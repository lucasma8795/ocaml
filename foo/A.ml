let foo = 42

let () =
  let baz_sum = List.fold_left (+) 0 C.baz in
  Printf.printf "foo: %d, bar: %s, baz: %d\n%!" foo B.bar baz_sum
