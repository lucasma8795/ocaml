open Effect_handler

let () =
  let run () = exit (Custom_maindriver.main Sys.argv Format.err_formatter) in
  base_effect_handler run

(*

CAML_LD_LIBRARY_PATH= ../v1/bin/ocamlc ocamlcommon.cma ocamlbytecomp.cma unix.cma \
  custom_ocamlc/custom_load_path.mli custom_ocamlc/custom_load_path.ml \
  custom_ocamlc/effect_handler.mli custom_ocamlc/effect_handler.ml \
  custom_ocamlc/parallelism_helper.mli custom_ocamlc/parallelism_helper.ml \
  custom_ocamlc/parallelism.mli custom_ocamlc/parallelism.ml \
  custom_ocamlc/custom_maindriver.mli custom_ocamlc/custom_maindriver.ml \
  custom_ocamlc/main.ml -I custom_ocamlc -I +compiler-libs -I +unix -o custom-ocamlc

*)
