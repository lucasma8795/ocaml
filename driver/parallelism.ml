type message =
| Task of (unit -> unit)
| Shutdown

(* A simple implementation of a message queue *)
module MessageQueue : sig

  type t

  val create : unit -> t

  val send : message -> t -> unit

  (* blocks domain until a message is available *)
  val recv : t -> message

end = struct

  type t = {
    queue : message Queue.t;
    mutex : Mutex.t;
    nonempty : Condition.t;
  }

  let create () = {
    queue = Queue.create ();
    mutex = Mutex.create ();
    nonempty = Condition.create ();
  }

  let send message mq =
    Mutex.lock mq.mutex;
    Queue.add message mq.queue;
    Condition.signal mq.nonempty;
    Mutex.unlock mq.mutex

  let recv mq =
    Mutex.lock mq.mutex;

    (* block until there is a message available *)
    while Queue.is_empty mq.queue do
      Condition.wait mq.nonempty mq.mutex
    done;

    let message = Queue.take mq.queue in
    Mutex.unlock mq.mutex;
    message
end

module ThreadPool = struct
  type pool_data = {
    domains : unit Domain.t array;
    message_queue : MessageQueue.t;
  }

  (* None iff pool is already closed
     Atomic.t should make the pool safe to pass between domains...? *)
  type t = pool_data option Atomic.t

  let rec worker mq =
    let msg = MessageQueue.recv mq in
    match msg with
    | Task f -> f (); worker mq
    | Shutdown -> ()

  let create num_domains =
    if num_domains <= 0 then
      raise (Invalid_argument "number of domains must be positive");

    let mq = MessageQueue.create () in
    let pool = {
      domains = Array.make num_domains (Domain.spawn (fun () -> worker mq));
      message_queue = mq;
    } in

    Atomic.make (Some pool)

  let submit pool task =
    match Atomic.get pool with
    | None -> raise (Invalid_argument "cannot submit to a closed pool")
    | Some pool ->
      MessageQueue.send (Task task) pool.message_queue

  let shutdown pool =
    begin match Atomic.get pool with
    | None -> raise (Invalid_argument "pool is already closed")
    | Some pool ->
      (* signal worker threads to shut down *)

      let num_domains = Array.length pool.domains in
      for _ = 0 to num_domains - 1 do
        MessageQueue.send Shutdown pool.message_queue;
      done;

      (* block until all domains finish *)
      Array.iter Domain.join pool.domains;
    end;

    (* mark the pool as closed *)
    Atomic.set pool None;
end
