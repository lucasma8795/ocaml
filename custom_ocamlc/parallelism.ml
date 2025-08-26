open Effect.Deep
open Custom_load_path
open Custom_misc
open Parallelism_helper
open Effect_handler

type filename = string
type visibility = Load_path.visibility

module Dir = Load_path.Dir

let pending_compilation : (filename, unit promise) Hashtbl.t =
  Hashtbl.create 16
let pool = Pool.create 2

let resolve_source_fullname cmi_file ~normalize =
  assert (Filename.check_suffix cmi_file ".cmi");
  let mli_file = (Filename.chop_suffix cmi_file ".cmi") ^ ".mli" in
  let mli_fullname =
    if normalize then
      find_normalized mli_file
    else
      find mli_file
  in
  assert (Sys.file_exists mli_fullname);
  mli_fullname

(* effect handler to override default find behavior (default effect handler) *)
let rec handle (compile : Compenv.action_context -> filename -> unit)
  (ctx : Compenv.action_context) (store : Local_store.store)
  (fullname : filename)
=
  let f () =
    (* begin if Filename.check_suffix fullname ".ml" then
      let prefix = Filename.chop_suffix fullname ".ml" in
      let cmi_file = prefix ^ ".cmi" in
      Effect.perform (Load_path.Find_path cmi_file) |> ignore
    end; *)
    compile ctx fullname
  in
  let override () =
    match f () with
    | () -> ()

    | effect (Load_path.Find_path dep), k ->
      dbg "[compile_dependency/find] %s is looking for %s...\n" fullname dep;
      assert (Filename.check_suffix dep ".cmi");
      begin match find dep with
      | dep_fullname -> continue k dep_fullname
      | exception Not_found ->
        let resume (fn : filename) = continue k fn in
        compile_dependency ctx store fullname dep resume ~normalize:false
      end

    | effect (Load_path.Find_normalized_with_visibility dep), k ->
      dbg "[compile_dependency/find_normalized] %s is looking for %s...\n" fullname dep;
      assert (Filename.check_suffix dep ".cmi");
      begin match find_normalized_with_visibility dep with
      | (dep_fullname, visibility) -> continue k (dep_fullname, visibility)
      | exception Not_found ->
        (* todo: don't always assume Visible *)
        let resume (fn: filename) = continue k (fn, Load_path.Visible) in
        compile_dependency ctx store fullname dep resume ~normalize:true;
      end

    | exception exn ->
      dbg "[compile_dependency] fatal error: exception while compiling %s: %s\n" fullname (Printexc.to_string exn);
      Printexc.print_backtrace stderr;
      Local_store.close_store store;
      raise exn
  in
  base_effect_handler override

and

(* must be called within a task submitted to pool *)
compile_dependency (ctx : Compenv.action_context)
  (parent_store : Local_store.store) (parent : filename) (dep : filename)
  (resume : filename -> unit) ~(normalize: bool) : unit
=
  assert (Filename.check_suffix dep ".cmi");
  dbg "[compile_dependency] %s depends on %s but it is not found!\n" parent dep;

  (* resolve source file *)
  let mli_fullname = resolve_source_fullname dep ~normalize in
  let cmi_fullname = (Filename.chop_suffix mli_fullname ".mli") ^ ".cmi" in
  dbg "[compile_dependency] source of %s resolved to %s\n" dep mli_fullname;

  (* check that no one is compiling it already *)
  begin match Hashtbl.find_opt pending_compilation mli_fullname with
  | Some promise ->
    dbg "[compile_dependency] %s is already being compiled! backing off...\n" mli_fullname;
    Local_store.close_store parent_store;
    task_suspend_until promise (* back off *)

  | None ->
    (* ensure it really isn't here *)
    if Sys.file_exists cmi_fullname then begin
      dbg "[compile_dependency] %s already exists (inconsistent load path state?)\n" cmi_fullname;
      assert false
    end;

    dbg "[compile_dependency] submitting dependency %s to pool...\n" cmi_fullname;

    (* submit dependency compilation to pool *)
    let promise = Pool.submit pool (fun () ->
      dbg "[compile_dependency] entering %s... (requested by %s)\n" cmi_fullname parent;

      let store = Local_store.fresh cmi_fullname in
      Local_store.open_store store;

      (* effects don't cross domain boundaries, so we need to install a fresh handler
         todo: an optimization would be to install the handler only once per domain? *)
      handle compile_interface ctx store mli_fullname;

      (* update the load path with the new .cmi file *)
      assert (Sys.file_exists cmi_fullname);
      add_new_file_to_path cmi_fullname;
      Local_store.close_store store;
      dbg "[compile_dependency] finished compiling dependency %s!\n" cmi_fullname)
    in

    Local_store.close_store parent_store;
    Hashtbl.add pending_compilation mli_fullname promise;
    task_suspend_until promise; (* back off *)
    Hashtbl.remove pending_compilation mli_fullname
  end;

  dbg "[compile_dependency] resuming compilation of %s (was suspended on %s)\n" parent cmi_fullname;
  assert (Sys.file_exists cmi_fullname);
  Local_store.open_store parent_store;
  resume cmi_fullname

let compile_ml_files ctx ml_files =
  Compmisc.init_path ();
  Local_store.freeze ();

  let promises = List.map (fun ml_file ->
    (* resolve source fullname *)
    let ml_fullname = find ml_file in
    Pool.submit pool (fun () ->
      let store = Local_store.fresh ml_fullname in
      Local_store.open_store store;
      handle compile_implementation ctx store ml_fullname;

      (* update the load path with the new .cmo file *)
      (* let cmo_fullname = (Filename.chop_suffix ml_fullname ".ml") ^ ".cmo" in *)
      (* add_new_file_to_path cmo_fullname; *)
      Local_store.close_store store)
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
