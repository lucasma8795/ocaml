open Clflags

let dbg_lock = Mutex.create ()

let dbg fmt =
  Mutex.protect dbg_lock (fun () ->
    let ret = Printf.eprintf ("%d -> " ^^ fmt) (Domain.self () :> int) in
    flush stderr;
    ret
  )

let dbg_print_backtrace () =
  Mutex.protect dbg_lock (fun () ->
    Printexc.print_backtrace stderr;
    flush stderr
  )

let compile_implementation ctx impl_filename =
  dbg "[compile_implementation] entering (file=%s)\n" impl_filename;
  let Compenv.{
    log = ppf;
    compile_implementation;
    compile_interface;
    ocaml_mod_ext;
    ocaml_lib_ext;
  } = ctx in
  let impl ~start_from name =
    Compenv.readenv Format.err_formatter (Before_compile name);
    let opref = Compenv.output_prefix name in
    objfiles := (opref ^ ocaml_mod_ext) :: !objfiles;
   compile_implementation ~start_from ~source_file:name ~output_prefix:opref
  in
  impl ~start_from:Compiler_pass.Parsing impl_filename;
  dbg "[compile_implementation] finished compiling %s!\n" impl_filename

let compile_interface ctx mli_fullname =
  let Compenv.{
    log = ppf;
    compile_implementation;
    compile_interface;
    ocaml_mod_ext;
    ocaml_lib_ext;
  } = ctx in
  (* if mli_fullname = "foo/B.mli" then ignore (Sys.command "sleep 1"); *)
  dbg "[compile_interface] entering (file=%s)\n" mli_fullname;
  Compenv.readenv ppf (Before_compile mli_fullname);
  let opref = Compenv.output_prefix mli_fullname in
  compile_interface ~source_file:mli_fullname ~output_prefix:opref;
  if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles;
  dbg "[compile_interface] finished compiling %s!\n" mli_fullname
