open Effect.Deep
open Clflags
open Custom_load_path
open Parallelism_helper

type pid = int
type filename = string
type visibility = Load_path.visibility
type store = Local_store.store

module Dir = Load_path.Dir

type load_path_continuation =
  | Find of (filename, unit) continuation
  | Find_with_visibility of (filename * visibility, unit) continuation

(* State needed to resume the suspended compilation of a .ml file *)
type suspended_compilation = {
  store : store;
  continuation : load_path_continuation;
}

(* Represents the state of a process currently under parallel compilation *)
type compilation_state = {
  promise : int promise; (* exit code *)
  waiting_tasks : suspended_compilation list ref;
}

(* this holds the files currently under parallel compilation
   require fullname as key to prevent collisions *)
let compiling : (filename, compilation_state) Hashtbl.t = Hashtbl.create 16

let is_being_compiled fullname = Hashtbl.mem compiling fullname

let resolve_source_fullname cmi_file ~normalize =
  assert (Filename.check_suffix cmi_file ".cmi");
  let mli_file = (Filename.chop_suffix cmi_file ".cmi") ^ ".mli" in
  let mli_fullname =
    if normalize then
      Custom_load_path.find_normalized mli_file
    else
      Custom_load_path.find mli_file
  in
  assert (Sys.file_exists mli_fullname);
  mli_fullname

let compile_implementation ctx impl_filename =
  Printf.eprintf "[compile_implementation] entering (file=%s)\n" impl_filename;
  let Compenv.{
    log = ppf;
    compile_implementation;
    compile_interface;
    ocaml_mod_ext;
    ocaml_lib_ext;
  } = ctx in
  let impl ~start_from name =
    Compenv.readenv ppf (Before_compile name);
    let opref = Compenv.output_prefix name in
    objfiles := (opref ^ ocaml_mod_ext) :: !objfiles;
   compile_implementation ~start_from ~source_file:name ~output_prefix:opref
  in
  impl ~start_from:Compiler_pass.Parsing impl_filename;
  Printf.eprintf "[compile_implementation] finished compiling %s!\n" impl_filename

(* Takes in a resolved .mli filename, compiles it in a separate process,
   then returns the pid of the process that is compiling it *)
let compile_dependency mli_fullname =
  Printf.eprintf "* [compile_dependency] entering (file=%s)\n" mli_fullname;
  assert (Filename.check_suffix mli_fullname ".mli");

  try
    (* construct -I and -H args *)
    let load_path_args =
      List.flatten (List.map (fun d -> let path = Dir.path d in ["-I"; if path = "" then "." else path]) !visible_dirs) @
      List.flatten (List.map (fun d -> let path = Dir.path d in ["-H"; if path = "" then "." else path]) !hidden_dirs)
    in

    let prog = "boot/ocamlrun" in
    let args =
      ["boot/ocamlc"; "-c"; mli_fullname; "-nostdlib";
       "-use-prims"; "runtime/primitives"; "-g"; "-strict-sequence";
       "-principal"; "-absname"; "-w"; "+a-4-9-40-41-42-44-45-48";
       "-warn-error"; "+a"; "-bin-annot"; "-strict-formats"] @ load_path_args
    in

    (* compile the .mli file *)
    Printf.eprintf "* [Sys.command] %s %s\n" prog (String.concat " " args);

    let cmd = Filename.quote_command prog args in
    let exit_code = Sys.command cmd in
    let cmi_fullname = (Filename.chop_suffix mli_fullname ".mli") ^ ".cmi" in
    add_new_file_to_path cmi_fullname;
    exit_code

  with e -> begin
    Printf.eprintf "* [compile_dependency] failed to compile %s, giving up... \n" mli_fullname;
    raise Not_found
  end

let compile_ml_files env ml_files =
  Compmisc.init_path ();

  let pool = Pool.create 8 in
  List.iter (fun ml_file ->
    (* resolve source fullname *)
    let ml_fullname = find ml_file in

    try
      (* this holds the compilation's global state *)
      let store = Local_store.fresh ml_fullname in
      let compile () =
        let handle_not_found cmi_file resume k ~normalize =
          Printf.eprintf "[compile_ml_files] %s depends on %s but it is not found!\n" ml_fullname cmi_file;

          (* resolve source file *)
          let mli_fullname = resolve_source_fullname cmi_file ~normalize in
          let cmi_fullname = (Filename.chop_suffix mli_fullname ".mli") ^ ".cmi" in
          Printf.eprintf "[compile_ml_files] %s resolved to %s\n" cmi_file cmi_fullname;

          (* ensure it really isn't here *)
          if Sys.file_exists cmi_fullname then begin
            Printf.eprintf "[compile_ml_files] %s already exists (inconsistent load path state?)\n" cmi_fullname;
            add_new_file_to_path cmi_fullname;
            resume cmi_fullname

          end else
            begin match Hashtbl.find_opt compiling cmi_fullname with
            | None ->
              (* there is no process compiling the interface *)
              Printf.eprintf "[compile_ml_files] there is no process compiling %s! starting one...\n" cmi_fullname;
              let promise = Pool.submit pool (fun () -> compile_dependency mli_fullname) in

              Hashtbl.add compiling cmi_fullname {
                promise;
                waiting_tasks = ref [{ store; continuation = k }]
              }

            | Some { promise; waiting_tasks } -> (* this file is already being compiled *)
              Printf.eprintf "[compile_ml_files] %s is already being compiled! adding continuation to list...\n" cmi_fullname;
              waiting_tasks := { store; continuation = k } :: !waiting_tasks
            end
        in
        begin match compile_implementation env ml_fullname with
          | () -> (* return *)
            (* update the load path with the new .cmo file *)
            let cmo_fullname = (Filename.chop_suffix ml_fullname ".ml") ^ ".cmo" in
            add_new_file_to_path cmo_fullname

          | effect (Load_path.Find_path cmi_file), k ->
            begin
              assert (Filename.check_suffix cmi_file ".cmi");
              try
                let cmi_fullname = Custom_load_path.find cmi_file in

                try
                  continue k cmi_fullname
                with e -> begin
                  Printexc.print_backtrace stderr;
                  raise e
                end

              with Not_found ->
                let resume = continue k in
                handle_not_found cmi_file resume (Find k) ~normalize:false
            end

          | effect (Load_path.Find_normalized_with_visibility cmi_file), k ->
            begin
              assert (Filename.check_suffix cmi_file ".cmi");

              try
                let (cmi_fullname, visibility) = Custom_load_path.find_normalized_with_visibility cmi_file in

                try
                  continue k (cmi_fullname, visibility)
                with e -> begin
                  Printexc.print_backtrace stderr;
                  raise e
                end

              with Not_found ->
                let resume fn = continue k (fn, Load_path.Visible) in
                handle_not_found cmi_file resume (Find_with_visibility k) ~normalize:true
            end

          | exception exn ->
            Printf.eprintf "[compile_ml_files] fatal error: exception while compiling %s: %s\n" ml_fullname (Printexc.to_string exn);
            raise exn
        end
      in
      Local_store.with_store store compile

    with e -> begin
      Printf.eprintf "[compile_ml_files] Exception while compiling %s!\n%!" ml_fullname;
      Printf.eprintf "Fatal error: %s\n%!" (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      raise e
    end;

    (* raise Not_found *)
  ) ml_files;

  (* wait for all compilations to finish *)
  let rec wait_for_compilations () =
    let to_remove = ref [] in
    Hashtbl.iter (fun cmi_fullname { promise; waiting_tasks } ->
      match try_resolve promise with
      | None -> () (* process is still running *)

      | Some 0 -> (* resume all continuations for this process *)
        Printf.eprintf "[compile_ml_files] %s compiled successfully! resuming %d suspended compilations waiting on it... \n" cmi_fullname (List.length !waiting_tasks);
        List.iter (fun { store; continuation } ->
          Local_store.with_store store (fun () ->
            match continuation with
            | Find k -> continue k cmi_fullname
            | Find_with_visibility k ->
              continue k (cmi_fullname, Load_path.Visible)
          )
        ) !waiting_tasks;
        to_remove := cmi_fullname :: !to_remove

      | Some exit_code ->
        Printf.eprintf "[compile_ml_files] compilation of %s failed with abnormal exit code %d\n" cmi_fullname exit_code;
        exit 1

      | exception exn ->
        Printf.eprintf "[compile_ml_files] compilation of %s failed with exception: %s\n" cmi_fullname (Printexc.to_string exn);
        exit 1

    ) compiling;

    (* remove finished compilations *)
    List.iter (fun fn -> Hashtbl.remove compiling fn) !to_remove
  in

  (* wait for all compilations to finish *)
  while Hashtbl.length compiling > 0 do
    wait_for_compilations ();
  done
