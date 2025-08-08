open Effect
open Effect.Deep
open Custom_misc

type 'a promise_state =
| Pending
| Resolved of 'a
| Rejected of exn

type 'a promise = 'a promise_state Atomic.t

type task = unit -> unit

exception Pool_closed

type _ Effect.t += Suspend_task : unit promise -> unit Effect.t

module TSQueue = struct
  type 'a t = {
    queue : 'a Queue.t;
    mutex : Mutex.t;
    nonempty : Condition.t;
  }

  let create () = {
    queue = Queue.create ();
    mutex = Mutex.create ();
    nonempty = Condition.create ();
  }

  let add x q =
    Mutex.lock q.mutex;
    Queue.add x q.queue;
    Condition.signal q.nonempty;
    Mutex.unlock q.mutex

  let take q =
    Mutex.lock q.mutex;

    (* block domain until there is an element available *)
    while Queue.is_empty q.queue do
      Condition.wait q.nonempty q.mutex
    done;

    let elt = Queue.take q.queue in
    Mutex.unlock q.mutex;
    elt

  let take_opt q =
    Mutex.lock q.mutex;
    let ret =
      if Queue.is_empty q.queue then
        None
      else
        Some (Queue.take q.queue) in
    Mutex.unlock q.mutex;
    ret

  let is_empty q =
    Mutex.lock q.mutex;
    let ret = Queue.is_empty q.queue in
    Mutex.unlock q.mutex;
    ret
end

let rec await p =
  match Atomic.get p with
  | Pending -> Domain.cpu_relax (); await p
  | Resolved v -> v
  | Rejected exn -> begin
    dbg "[await] promise rejected with exception: %s\n" (Printexc.to_string exn);
    raise exn
  end

let try_resolve p =
  match Atomic.get p with
  | Pending -> None
  | Resolved v -> Some v
  | Rejected exn -> begin
    dbg "[try_resolve] promise rejected with exception: %s\n" (Printexc.to_string exn);
    raise exn
  end

module Pool = struct
  type pool_data = {
    domains : unit Domain.t array;
    task_queue : task TSQueue.t;
    suspended_tasks : ((unit, unit) continuation * unit promise) list ref;
    suspended_tasks_mutex : Mutex.t;
    active_tasks : int Atomic.t;
  }

  type t = pool_data option Atomic.t

  (* worker loop that is executed in each child domain *)
  let rec worker (pool : t) =
    match Atomic.get pool with
    | None -> () (* quit *)
    | Some p -> begin
      (* check if suspended tasks can be resumed *)
      Mutex.lock p.suspended_tasks_mutex;
      let pred = fun (_, promise) -> Atomic.get promise <> Pending in
      let (resumable_tasks, blocked_tasks) =
        List.partition pred !(p.suspended_tasks)
      in

      (* resume suspended tasks *)
      List.iter (fun (fiber, promise) ->
        let task =
          match Atomic.get promise with
          | Resolved v -> fun () -> continue fiber v
          | Rejected exn ->
            fun () ->
              dbg "[worker] rejecting task with exception: %s\n" (Printexc.to_string exn);
              discontinue fiber exn
          | Pending -> failwith "impossible" (* should have been filtered out *)
        in
        TSQueue.add task p.task_queue
      ) resumable_tasks;

      (* update the suspended tasks list *)
      p.suspended_tasks := blocked_tasks;
      Mutex.unlock p.suspended_tasks_mutex;

      (* wait for a new task or shutdown signal *)
      let task = TSQueue.take p.task_queue in
      try_with task ()
      {
        effc = (fun (type c) (eff: c Effect.t) ->
          match eff with
          | Suspend_task promise ->
            Some (fun (k: (c, _) continuation) ->
              (* store in suspended tasks list *)
              Mutex.lock p.suspended_tasks_mutex;
              p.suspended_tasks := (k, promise) :: !(p.suspended_tasks);
              Mutex.unlock p.suspended_tasks_mutex
            )
          | _ -> None
        )
      };
      worker pool
    end

  (* decorates a task with ability to update state after finishing *)
  let create num_domains =
    if num_domains <= 0 then
      raise (Invalid_argument "number of domains must be positive");

    let pool : t = Atomic.make (Some {
      domains = [||];  (* will be filled later *)
      task_queue = TSQueue.create ();
      suspended_tasks = ref [];
      suspended_tasks_mutex = Mutex.create ();
      active_tasks = Atomic.make 0;
    }) in

    (* now create the domains *)
    let make_new_domain = fun _ -> Domain.spawn (fun () -> worker pool) in
    let domains = Array.init num_domains make_new_domain in

    (* update the pool with the actual domains *)
    begin match Atomic.get pool with
    | Some pool_data -> Atomic.set pool (Some { pool_data with domains })
    | None -> failwith "impossible";
    end;

    pool

  let submit pool task =
    let promise = Atomic.make Pending in
    match Atomic.get pool with
    | None -> raise Pool_closed
    | Some p -> begin
        Atomic.incr p.active_tasks;
        let wrapped_task = fun () ->
          try
            let result = task () in
            Atomic.set promise (Resolved result);
            Atomic.decr p.active_tasks;
          with exn ->
            dbg "[Pool/submit] task failed with exception: %s\n" (Printexc.to_string exn);
            Printexc.print_backtrace stderr;
            Atomic.set promise (Rejected exn);
            Atomic.decr p.active_tasks
        in
        TSQueue.add wrapped_task p.task_queue;
      end;
    promise

  let join_and_shutdown pool =
    match Atomic.get pool with
    | None -> raise Pool_closed
    | Some p ->
      (* wait for all tasks to finish *)
      while Atomic.get p.active_tasks > 0 do
        Domain.cpu_relax ();
      done;

      Atomic.set pool None; (* mark the pool as closed *)

      (* temporary hack: wake up blocked workers *)
      for _ = 1 to Array.length p.domains do
        TSQueue.add (fun () -> ()) p.task_queue
      done;
      Array.iter Domain.join p.domains;
end

let suspend_on p = perform (Suspend_task p)
