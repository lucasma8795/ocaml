open Parallelism

let counter = Atomic.make 0

let () =
  let num_domains = Domain.recommended_domain_count () in
  Printf.printf "Creating a pool with %d domains...\n%!" num_domains;
  let pool = Pool.create num_domains in

  let x = ref 0 in
  let incr () =
    x := !x + 1;
    let promise = Pool.submit pool (fun () -> Atomic.incr counter) in
    suspend_on promise;
    !x
  in

  for _ = 1 to 10000 do
    let promise = Pool.submit pool incr in
    ignore promise
  done;

  Pool.join_and_shutdown pool;
  Printf.printf "x = %d\n%!" !x;
  Printf.printf "counter = %d\n%!" (Atomic.get counter);

(* ocamlopt -I +unix unix.cmxa driver/parallelism.mli driver/parallelism.ml demo.ml -I driver -o demo *)
