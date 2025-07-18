open Parallelism

let () =
  let num_domains = Domain.recommended_domain_count () in
  let pool = Pool.create num_domains in

  let x = ref 0 in
  let incr () =
    x := !x + 1;
    !x
  in

  for _ = 1 to 100000 do
    let promise = Pool.submit pool incr in
    ignore promise
  done;

  Pool.shutdown pool;
  Printf.printf "x = %d\n" !x;

(* ocamlopt -I +unix unix.cmxa driver/parallelism.mli driver/parallelism.ml demo.ml -I driver -o demo *)
