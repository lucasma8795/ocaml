let foo = 42

let () =
  let baz_sum = List.fold_left (+) 0 C.baz in
  Printf.printf "foo: %d, bar: %s, baz: %d\n%!" foo B.bar baz_sum

(* OCAMLRUNPARAM=b ./custom-ocamlc -g -c foo/A.ml foo/B.ml foo/C.ml foo/D.ml -I stdlib -I foo &> out *)
