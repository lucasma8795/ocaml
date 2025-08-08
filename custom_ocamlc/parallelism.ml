open Effect.Deep
open Clflags
open Custom_load_path
open Custom_misc
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
  promise : unit promise;
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

let compile_ml_files ctx ml_files =
  Compmisc.init_path ();

  let pool = Pool.create 1 in

  let promises = List.map (fun ml_file ->
    (* resolve source fullname *)
    let ml_fullname = find ml_file in

    let handle_not_found store cmi_file resume k ~normalize =
      assert (Filename.check_suffix cmi_file ".cmi");
      dbg "[handle_not_found] %s depends on %s but it is not found!\n" ml_fullname cmi_file;

      (* resolve source file *)
      let mli_fullname = resolve_source_fullname cmi_file ~normalize in
      let cmi_fullname = (Filename.chop_suffix mli_fullname ".mli") ^ ".cmi" in
      dbg "[handle_not_found] %s resolved to %s\n" cmi_file cmi_fullname;

      (* ensure it really isn't here *)
      if Sys.file_exists cmi_fullname then begin
        dbg "[handle_not_found] %s already exists (inconsistent load path state?)\n" cmi_fullname;
        add_new_file_to_path cmi_fullname;
        resume cmi_fullname

      end else
        dbg "[handle_not_found] compiling dependency %s...\n" cmi_fullname;

        (* effects don't cross domain boundaries, so we need to install a fresh handler *)
        let promise = Pool.submit pool (fun () ->
          try
            dbg "[task] starting work on %s...\n" cmi_fullname;
            let task () = compile_interface ctx mli_fullname in

            let new_store = Local_store.fresh cmi_fullname in
            Local_store.open_store new_store;
            Effect_handler.handle task;
            add_new_file_to_path cmi_fullname;
            Local_store.close_store new_store;

            dbg "[task] finished work on %s!\n" cmi_fullname

          with e ->
            dbg "[task] %s failed with exception: %s\n" cmi_fullname (Printexc.to_string e);
            dbg_print_backtrace ();
            raise e
        ) in

        Local_store.close_store store;
        suspend_on promise;
        dbg "[handle_not_found] resuming compilation of %s\n" ml_fullname;
        Local_store.open_store store;
        resume cmi_fullname
    in

    let task () =
      let store = Local_store.fresh ml_fullname in
      Local_store.open_store store;
      match compile_implementation ctx ml_fullname with
      | () -> (* return *)
        (* update the load path with the new .cmo file *)
        let cmo_fullname = (Filename.chop_suffix ml_fullname ".ml") ^ ".cmo" in
        add_new_file_to_path cmo_fullname;
        Local_store.close_store store

      | effect (Load_path.Find_path cmi_file), k ->
        dbg "[compile_ml_files/find] looking for %s...\n" cmi_file;
        assert (Filename.check_suffix cmi_file ".cmi");
        begin match Custom_load_path.find cmi_file with
        | cmi_fullname -> continue k cmi_fullname
        | exception Not_found ->
          let resume = continue k in
          handle_not_found store cmi_file resume (Find k) ~normalize:false
        end

      | effect (Load_path.Find_normalized_with_visibility cmi_file), k ->
        dbg "[compile_ml_files/find_normalized] looking for %s...\n" cmi_file;
        assert (Filename.check_suffix cmi_file ".cmi");
        begin match Custom_load_path.find_normalized_with_visibility cmi_file with
        | (cmi_fullname, visibility) ->
          continue k (cmi_fullname, visibility)
        | exception Not_found ->
          (* todo: don't always assume Visible *)
          let resume fn = continue k (fn, Load_path.Visible) in
          handle_not_found store cmi_file resume (Find_with_visibility k) ~normalize:true
        end

      | exception exn ->
        dbg "[compile_ml_files] fatal error: exception while compiling %s: %s\n" ml_fullname (Printexc.to_string exn);
        Printexc.print_backtrace stderr;
        raise exn
    in

    Pool.submit pool (fun () -> Effect_handler.handle task)
  ) ml_files

  in

  (* wait for all compilations to finish *)
  Pool.join_and_shutdown pool;
  List.iter (fun promise ->
    match await promise with
    | () -> ()
    | exception exn ->
      let ml_file = List.nth ml_files (List.length promises - 1) in
      dbg "[compile_ml_files] fatal error: exception while compiling %s: %s\n" ml_file (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      raise exn
  ) promises
