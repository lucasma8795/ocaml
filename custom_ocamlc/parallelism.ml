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

let pending_compilation : (string, unit promise) Hashtbl.t =
  Hashtbl.create 16


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

    let rec handle_not_found store cmi_file resume k ~normalize =
      assert (Filename.check_suffix cmi_file ".cmi");
      dbg "[handle_not_found] %s depends on %s but it is not found!\n" ml_fullname cmi_file;

      (* resolve source file *)
      let mli_fullname = resolve_source_fullname cmi_file ~normalize in
      let cmi_fullname = (Filename.chop_suffix mli_fullname ".mli") ^ ".cmi" in
      dbg "[handle_not_found] %s resolved to %s\n" cmi_file cmi_fullname;

      (* check that no one is compiling it already *)
      begin match Hashtbl.find_opt pending_compilation cmi_fullname with
      | Some promise ->
        dbg "[handle_not_found] %s is already being compiled!\n" cmi_fullname;
        Local_store.close_store store;
        suspend_on promise;
        Local_store.open_store store

      | None ->
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
              dbg "[handle_not_found] starting work on %s...\n" cmi_fullname;

              let store = Local_store.fresh cmi_fullname in

              (* todo: refactor out common logic *)
              let task () =
                match compile_interface ctx mli_fullname with
                | () -> (* return *)
                  (* update the load path with the new .cmo file *)
                  let cmi_fullname = (Filename.chop_suffix mli_fullname ".mli") ^ ".cmi" in
                  add_new_file_to_path cmi_fullname;
                  Local_store.close_store store;
                  dbg "[handle_not_found] finished work on %s!\n" cmi_fullname

                | effect (Load_path.Find_path cmi_file), k ->
                  dbg "[handle_not_found/find] looking for %s...\n" cmi_file;
                  assert (Filename.check_suffix cmi_file ".cmi");
                  begin match Custom_load_path.find cmi_file with
                  | cmi_fullname -> continue k cmi_fullname
                  | exception Not_found ->
                    let resume = continue k in
                    handle_not_found store cmi_file resume (Find k) ~normalize:false
                  end

                | effect (Load_path.Find_normalized_with_visibility cmi_file), k ->
                  dbg "[handle_not_found/find_normalized] looking for %s...\n" cmi_file;
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
                  dbg "[handle_not_found] fatal error: exception while compiling %s: %s\n" mli_fullname (Printexc.to_string exn);
                  Printexc.print_backtrace stderr;
                  Local_store.close_store store;
                  raise exn

              in
              Local_store.open_store store;
              Effect_handler.handle task

            with e ->
              dbg "[handle_not_found] %s failed with exception: %s\n" cmi_fullname (Printexc.to_string e);
              raise e
          ) in

          Local_store.close_store store;
          Hashtbl.add pending_compilation cmi_fullname promise;
          suspend_on promise;
          Hashtbl.remove pending_compilation cmi_fullname;
          Local_store.open_store store;
      end;

      dbg "[handle_not_found] resuming compilation of %s\n" ml_fullname;
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
      raise exn
  ) promises
