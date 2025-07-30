open Clflags

module STbl = Misc.Stdlib.String.Tbl
module Dir = Load_path.Dir
module Options = Main_args.Make_bytecomp_options (Main_args.Default.Main)

type pid = int
type ('a, 'b) continuation = ('a, 'b) Effect.Deep.continuation
type filename = string
type visibility = Load_path.visibility

type load_path_continuation =
  | Find of (filename, unit) continuation
  | Find_normalized_with_visibility of (filename * visibility, unit) continuation

(* various state info originally in driver/Load_path.ml, I just ripped it out*)
type registry = string STbl.t

let visible_files : registry ref = ref (STbl.create 42)
let visible_files_uncap : registry ref = ref (STbl.create 42)

let hidden_files : registry ref = ref (STbl.create 42)
let hidden_files_uncap : registry ref = ref (STbl.create 42)

let visible_dirs : Dir.t list ref = ref []
let hidden_dirs  : Dir.t list ref = ref []

let find_file_in_cache (fn : string) =
  try (STbl.find !visible_files fn, Load_path.Visible) with
  | Not_found -> (STbl.find !hidden_files fn, Load_path.Hidden)

let find_file_in_cache_uncap (fn_uncap : string) =
  try (STbl.find !visible_files_uncap fn_uncap, Load_path.Visible) with
  | Not_found -> (STbl.find !hidden_files_uncap fn_uncap, Load_path.Hidden)

let is_basename fn = Filename.basename fn = fn

let get_visible_path_list () = List.rev_map Dir.path !visible_dirs
let get_hidden_path_list () = List.rev_map Dir.path !hidden_dirs

let append_dir dir =
  let update base fn visible_files hidden_files =
    if Dir.hidden dir && not (STbl.mem !hidden_files base) then
      STbl.replace !hidden_files base fn
    else if not (STbl.mem !visible_files base) then
      STbl.replace !visible_files base fn
  in
  List.iter
    (fun base ->
      Result.iter (fun ubase ->
          let fn = Filename.concat (Dir.path dir) base in
          update base fn visible_files hidden_files;
          update ubase fn visible_files_uncap hidden_files_uncap
        )
        (Misc.normalized_unit_filename base)
    )
    (Dir.files dir);
  if Dir.hidden dir then
    hidden_dirs := dir :: !hidden_dirs
  else
    visible_dirs := dir :: !visible_dirs

let get_path_list () =
  let acc = List.rev_map Dir.path !hidden_dirs in
  Misc.rev_map_end Dir.path !visible_dirs acc

let auto_include_libs libs alert find_in_dir fn =
  Printf.eprintf "[Load_path:auto_include] (entering with fn=%s)\n" fn;
  let scan (lib, lazy dir) =
    let file = find_in_dir dir fn in
    let alert_and_add_dir _ =
      alert lib;
      append_dir dir
    in
    Option.iter alert_and_add_dir file;
    file
  in
  match List.find_map scan libs with
  | Some base -> base
  | None -> begin
    Printf.eprintf "[Load_path:auto_include] failed, aborting (fn=%s)\n" fn;
    raise Not_found;
  end

let auto_include_otherlibs : (string -> unit) -> Load_path.auto_include_callback =
  (* Ensure directories are only ever scanned once *)
  let expand = Misc.expand_directory Config.standard_library in
  let otherlibs =
    let read_lib lib = lazy (Dir.create ~hidden:false (expand ("+" ^ lib))) in
    List.map (fun lib -> (lib, read_lib lib)) ["dynlink"; "str"; "unix"] in
  auto_include_libs otherlibs

let auto_include find_in_dir fn =
  if !Clflags.no_std_include then begin
    Printf.eprintf "[auto_include] Clflags.no_std_include is true, abort \n";
    raise Not_found
  end else begin
    Printf.eprintf "[auto_include] Clflags.no_std_include is false, continue\n";
    let alert = Location.auto_include_alert in
    auto_include_otherlibs alert find_in_dir fn
  end

let prepend_add (dir: Dir.t) =
  List.iter (fun base ->
    Result.iter (fun filename ->
      let fn = Filename.concat (Dir.path dir) base in
      if Dir.hidden dir then begin
        STbl.replace !hidden_files base fn;
        STbl.replace !hidden_files_uncap filename fn
      end else begin
        STbl.replace !visible_files base fn;
        STbl.replace !visible_files_uncap filename fn
      end)
    (Misc.normalized_unit_filename base)
  ) (Dir.files dir)

let find_path fn =
  try
    if is_basename fn && not !Sys.interactive then
      fst (find_file_in_cache fn)
    else
      (* this triggers file system calls, which may be slow... *)
      Misc.find_in_path (get_path_list ()) fn

  with Not_found -> begin
    Printf.eprintf "[Find_path] attempt to auto-include %s\n" fn;
    (* may throw Not_found again *)
    auto_include Dir.find fn
  end

let find_normalized_with_visibility fn =
  match Misc.normalized_unit_filename fn with
  | Error _ -> raise Not_found
  | Ok fn_uncap ->
    try
      if is_basename fn && not !Sys.interactive then
        find_file_in_cache_uncap fn_uncap
      else
        try
          (Misc.find_in_path_normalized (get_visible_path_list ()) fn, Load_path.Visible)
        with
        | Not_found ->
          (Misc.find_in_path_normalized (get_hidden_path_list ()) fn, Load_path.Hidden)

    with Not_found -> begin
      Printf.eprintf "[Find_normalized_with_visibility] attempt to auto-include %s\n" fn;
      (* may throw Not_found again *)
      auto_include Dir.find_normalized fn_uncap, Load_path.Visible
    end

let find_normalized fn = fst (find_normalized_with_visibility fn)

let add_new_file_to_path : string -> string -> unit = fun dirname base ->
  Printf.eprintf "[add_new_file_to_path] adding dirname=%s base=%s to global state\n" dirname base;
  let dir = List.find (fun d -> Dir.path d = dirname) !visible_dirs in

  let base_uncap = Misc.normalized_unit_filename base in
  let base_uncap = match base_uncap with
    | Error _ -> raise Not_found
    | Ok base_uncap -> base_uncap
  in

  let full_fn = Filename.concat dirname base in
  let full_fn_uncap = Filename.concat dirname base_uncap in

  (* update the global state *)
  if Dir.hidden dir then begin
    STbl.replace !hidden_files_uncap base_uncap full_fn_uncap;
    STbl.replace !hidden_files base full_fn
  end else begin
    STbl.replace !visible_files_uncap base_uncap full_fn_uncap;
    STbl.replace !visible_files base full_fn
  end;

  Dir.add_file dir base;

  (* sanity checks *)
  assert (find_path base = full_fn)


(* takes in a *.cm{i,o} file, finds it in the load path and compiles it *)
let rec compile_dependency_parallel : string -> string * pid = fun fn ->
  Printf.eprintf "[compile_dependency] entering (fn=%s)\n" fn;

  (* extract basename *)
  let base = Filename.basename fn in

  (* verify that fn is either a .cmi or a .cmo *)
  let ext = Filename.extension fn in
  if ext <> ".cmi" then begin
    Printf.eprintf "[compile_dependency] don't know what to do with %s\n" base;
    raise Not_found;
  end;

  (* this should now be the basename prefix, i.e. Foo for path/Foo.ml *)
  let name = Filename.chop_suffix base ext in

  (* fn may contain a path prefix, if we detect one, then use that directly *)
  (* if not found, this throws Not_Found *)
  let maybe_path = Filename.dirname fn in
  let source_ext = ".mli" in
  let full_source_file =
    if maybe_path = "" || maybe_path = "." then
      (* no path prefix, so we need to find it in the load path *)
      find_path (name ^ source_ext)
    else begin
      (* we have a path prefix, so use that directly *)
      Printf.eprintf "[compile_dependency] detected path %s, using it\n" maybe_path;
      Filename.concat maybe_path (name ^ source_ext)
    end
  in

  assert (Sys.file_exists full_source_file);

  (* attempt to resolve full path of file, this may throw Not_Found *)
  Printf.eprintf "[compile_dependency] source resolved to %s\n" full_source_file;

  (* if the .cm{i,o} file is already here, skip
     todo: can we read info off of cache instead? *)
  let full_compiled_file = (Filename.chop_suffix full_source_file source_ext) ^ ext in

  let compile : unit -> pid = fun () ->
    (* construct -I and -H args *)
    let load_path_args =
      List.flatten (List.map (fun d -> let path = Dir.path d in ["-I"; if path = "" then "." else path]) !visible_dirs) @
      List.flatten (List.map (fun d -> let path = Dir.path d in ["-H"; if path = "" then "." else path]) !hidden_dirs)
    in

    let prog = "boot/ocamlrun" in
    let args =
      ["boot/ocamlc"; "-c"; full_source_file; "-nostdlib";
       "-use-prims"; "runtime/primitives"; "-g"; "-strict-sequence";
       "-principal"; "-absname"; "-w"; "+a-4-9-40-41-42-44-45-48";
       "-warn-error"; "+a"; "-bin-annot"; "-strict-formats"] @ load_path_args
    in

    Printf.eprintf "[Unix.create_process] %s %s\n" prog (String.concat " " args);

    let args = Array.of_list (prog :: args) in
    Unix.create_process prog args Unix.stdin Unix.stdout Unix.stderr
  in

  (* compile the source file *)
  try
    let pid = compile () in (full_compiled_file, pid)
  with Not_found -> begin
    Printf.eprintf "[compile_dependency] failed to compile %s, quitting \n" full_source_file;
    raise Not_found
  end

let handle f =
  Effect.Deep.match_with f ()
  {
    retc = (fun ret -> ret);
    exnc = raise;
    effc = fun (type c) (eff: c Effect.t) ->
      match eff with
      (* find : string -> string *)
      | Load_path.Find_path fn ->
        Some (fun (k: (c, _) continuation) ->
          try
            Effect.Deep.continue k (find_path fn)
          with Not_found -> begin
            Effect.Deep.discontinue k Not_found (* give up :( *)
          end
        )

      (* find_normalized_with_visibility : string -> string * visibility *)
      | Load_path.Find_normalized_with_visibility fn ->
        Some (fun (k: (c, _) continuation) ->
          try
            Effect.Deep.continue k (find_normalized_with_visibility fn)
          with Not_found -> begin
            Effect.Deep.discontinue k Not_found
          end
        )

      (* append_dir : Dir.t -> unit *)
      | Load_path.Append_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          append_dir dir;
          Effect.Deep.continue k ()
        )

      (* auto_include_otherlibs : (string -> unit) -> auto_include_callback *)
      | Load_path.Auto_include_otherlibs alert ->
        Some (fun (k: (c, _) continuation) ->
          Effect.Deep.continue k (auto_include_otherlibs alert)
        )

      (* prepend_dir : Dir.t -> unit *)
      | Load_path.Prepend_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          prepend_add dir;
          if Dir.hidden dir then
            hidden_dirs := !hidden_dirs @ [dir]
          else
            visible_dirs := !visible_dirs @ [dir];
          Effect.Deep.continue k ()
        )

      (* remove_dir : Dir.t -> unit *)
      | Load_path.Remove_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          let visible = List.filter (fun d -> Dir.path d <> dir) !visible_dirs in
          let hidden  = List.filter (fun d -> Dir.path d <> dir) !hidden_dirs in
          if   List.compare_lengths visible !visible_dirs <> 0
            || List.compare_lengths hidden !hidden_dirs <> 0 then begin
            Local_store.reset ();
            visible_dirs := visible;
            hidden_dirs := hidden;
            List.iter prepend_add hidden;
            List.iter prepend_add visible
          end;
          Effect.Deep.continue k ()
        )

      (* reset : unit -> unit *)
      | Load_path.Reset_path ->
        Some (fun (k: (c, _) continuation) ->
          STbl.clear !hidden_files;
          STbl.clear !hidden_files_uncap;
          STbl.clear !visible_files;
          STbl.clear !visible_files_uncap;
          hidden_dirs := [];
          visible_dirs := [];
          Effect.Deep.continue k ()
        )

      (* init : auto_include_callback -> string list -> string list -> unit
         assumes Reset_path has previously been performed *)
      | Load_path.Init_path (visible, hidden) ->
        Some (fun (k: (c, _) continuation) ->
          let visible = List.filter (fun p -> not (String.ends_with ~suffix:"v1/lib/ocaml" p)) visible in
          let hidden = List.filter (fun p -> not (String.ends_with ~suffix:"v1/lib/ocaml" p)) hidden in
          visible_dirs := List.rev_map (fun path -> Dir.create ~hidden:false path) visible;
          hidden_dirs := List.rev_map (fun path -> Dir.create ~hidden:true path) hidden;
          List.iter prepend_add !hidden_dirs;
          List.iter prepend_add !visible_dirs;

          Printf.eprintf "[Init_path] visible_dirs: %s\n"
            (String.concat ", " (List.map (fun d -> Dir.path d) !visible_dirs));
          Printf.eprintf "[Init_path] hidden_dirs: %s\n"
            (String.concat ", " (List.map (fun d -> Dir.path d) !hidden_dirs));

          Effect.Deep.continue k ()
        )

      (* get_visible : unit -> Dir.t list *)
      | Load_path.Get_visible ->
        Some (fun (k: (c, _) continuation) ->
          Effect.Deep.continue k (List.rev !visible_dirs)
        )

      (* get_path_list : unit -> string list *)
      | Load_path.Get_path_list ->
        Some (fun (k: (c, _) continuation) ->
          Effect.Deep.continue k (get_path_list ())
        )

      (* get_paths : unit -> paths *)
      | Load_path.Get_paths ->
        Some (fun (k: (c, _) continuation) ->
          let ret = Load_path.{
            visible = List.rev_map (fun d -> Dir.path d) !visible_dirs;
            hidden = List.rev_map (fun d -> Dir.path d) !hidden_dirs
          }
          in
          Effect.Deep.continue k ret
        )

      | _ -> None
  }

let compile_implementation ctx impl_filename =
  let Compenv.{ log = ppf;
        compile_implementation;
        compile_interface;
        ocaml_mod_ext;
        ocaml_lib_ext;
      } = ctx in
  let impl ~start_from name =
    Compenv.readenv ppf (Before_compile name);
    let opref = Compenv.output_prefix name in
    compile_implementation ~start_from ~source_file:name ~output_prefix:opref;
    objfiles := (opref ^ ocaml_mod_ext) :: !objfiles
  in
  impl ~start_from:Compiler_pass.Parsing impl_filename;
  Printf.eprintf "[process_deferred_actions] finished compiling %s!\n" impl_filename

let process_deferred_actions (env : Compenv.action_context) =
  let final_output_name = !Clflags.output_name in
  (* Make sure the intermediate products don't clash with the final one
     when we're invoked like: ocamlopt -o foo bar.c baz.ml. *)
  if not !compile_only then Clflags.output_name := None;
  begin
    match final_output_name with
    | None -> ()
    | Some _output_name ->
        if !compile_only then begin
          if List.length (List.filter (function
              | Compenv.ProcessCFile _
              | Compenv.ProcessImplementation _
              | Compenv.ProcessInterface _ -> true
              | _ -> false) !Compenv.deferred_actions) > 1 then
            Compenv.fatal "Options -c -o are incompatible with compiling multiple files"
        end;
  end;
  if !make_archive then begin
    if List.exists (function
        | Compenv.ProcessOtherFile name -> Filename.check_suffix name ".cmxa"
        | _ -> false) !Compenv.deferred_actions then
      Compenv.fatal "Option -a cannot be used with .cmxa input files."
    end
  else if !Compenv.deferred_actions = [] then
    Compenv.fatal "No input files";

  (* List.iter (process_action env) (List.rev !Compenv.deferred_actions); *)
  let work_queue = Queue.create () in

  (* pending is a hashtable mapping filenames to (pid, snapshots) pairs.
     This is used to track which files are currently being compiled and
     to add continuations that need to be resumed once the compilation is done. *)
  let pending : (filename, pid * (Local_store.store * load_path_continuation) list ref) Hashtbl.t = Hashtbl.create 42 in

  let try_resume_continuations () =
    let to_remove = ref [] in

    Hashtbl.iter (fun resolved_intf_fn (pid, snapshots) ->
      match Unix.waitpid [Unix.WNOHANG] pid with
      (* still running, keep the continuation *)
      | (0, _) -> ()

      (* successfully compiled, resume continuation *)
      | (_, Unix.WEXITED 0) ->
        Printf.eprintf "[process_deferred_actions] resuming all continuations for %s\n" resolved_intf_fn;

        add_new_file_to_path (Filename.dirname resolved_intf_fn) (Filename.basename resolved_intf_fn);

        (* resume all continuations *)
        List.iter (fun (store, continuation) ->
          (* restore the local store *)
          Local_store.with_store store (fun () ->
            match continuation with
            | Find k -> Effect.Deep.continue k (resolved_intf_fn)
            | Find_normalized_with_visibility k ->
              Effect.Deep.continue k (resolved_intf_fn, Load_path.Visible))
        ) !snapshots;

        to_remove := resolved_intf_fn :: !to_remove;

      | (_, Unix.WEXITED code) ->
        (* compilation failed, quit *)
        Printf.eprintf "[process_deferred_actions] compilation of %s returned with abnormal exit code %d\n" resolved_intf_fn code;
        exit 1

      | (_, Unix.WSIGNALED signal) ->
        (* compilation was killed by a signal, quit *)
        Printf.eprintf "[process_deferred_actions] compilation of %s failed with WSIGNALED %d \n" resolved_intf_fn signal;
        exit 1

      | (_, Unix.WSTOPPED signal) ->
        (* compilation was stopped by a signal, quit *)
        Printf.eprintf "[process_deferred_actions] compilation of %s failed with WSTOPPED %d \n" resolved_intf_fn signal;
        exit 1
    ) pending;

    (* modify the hash table now, since modification inside iter is undefined *)
    List.iter (fun fn -> Hashtbl.remove pending fn) !to_remove
  in

  (* Initialize work queue *)
  List.iter (fun action ->
    match action with
    | Compenv.ProcessImplementation name -> Queue.add name work_queue
    | _ -> ()
  ) (List.rev !Compenv.deferred_actions);

  while (not (Queue.is_empty work_queue)) || Hashtbl.length pending > 0 do
    match Queue.take_opt work_queue with
    | Some impl_fn ->
      Printf.eprintf "[process_deferred_actions] starting work on %s...\n%!" impl_fn;

      let store = Local_store.fresh impl_fn in

      Local_store.with_store store (fun () ->
        Effect.Deep.try_with (compile_implementation env) impl_fn
        {
          effc = fun (type c) (eff: c Effect.t) ->
          match eff with
          (* find : string -> string *)
          | Load_path.Find_path intf_fn ->
            assert (Filename.check_suffix intf_fn ".cmi");
            Some (fun (k: (c, _) continuation) ->
              try
                Effect.Deep.continue k (find_normalized intf_fn)

              with Not_found -> begin
                (* at this point, we need to compile the dependency *)
                Printf.eprintf "[process_deferred_actions] %s depends on %s but is not found!\n" impl_fn intf_fn;

                let path = find_normalized ((Filename.chop_suffix intf_fn ".cmi") ^ ".mli") in
                let resolved_intf_fn = (Filename.chop_suffix path ".mli") ^ ".cmi" in

                Printf.eprintf "[process_deferred_actions] %s resolved to %s\n" intf_fn resolved_intf_fn;

                begin match Hashtbl.find_opt pending resolved_intf_fn with
                | None ->
                  (* there is no process compiling the interface! starting one... *)
                  Printf.eprintf "[process_deferred_actions] there is no process compiling %s! starting one...\n" resolved_intf_fn;
                  let (full, pid) = compile_dependency_parallel resolved_intf_fn in
                  assert (full = resolved_intf_fn);
                  Hashtbl.add pending resolved_intf_fn (pid, ref [(store, Find k)])

                | Some (pid, snapshots) ->
                  (* this process is already under compilation! *)
                  Printf.eprintf "[process_deferred_actions] %s is already under compilation (pid=%d)! adding continuation to list...\n" resolved_intf_fn pid;
                  snapshots := (store, Find k) :: !snapshots
                end;
              end
            )

          (* find_normalized_with_visibility : string -> string * visibility *)
          | Load_path.Find_normalized_with_visibility intf_fn ->
            let intf_fn = match Misc.normalized_unit_filename intf_fn with
              | Error _ -> raise Not_found
              | Ok fn_uncap -> fn_uncap
            in
            (* assert that intf_fn is a .cmi file *)
            assert (Filename.check_suffix intf_fn ".cmi");
            Some (fun (k: (c, _) continuation) ->
              try
                Effect.Deep.continue k (find_normalized_with_visibility intf_fn)

              with Not_found -> begin
                (* at this point, we need to compile the dependency *)
                Printf.eprintf "[process_deferred_actions] %s depends on %s but is not found!\n%!" intf_fn impl_fn;

                let path = find_normalized ((Filename.chop_suffix intf_fn ".cmi") ^ ".mli") in
                let resolved_intf_fn = (Filename.chop_suffix path ".mli") ^ ".cmi" in

                Printf.eprintf "[process_deferred_actions] %s resolved to %s\n" intf_fn resolved_intf_fn;

                begin match Hashtbl.find_opt pending resolved_intf_fn with
                | None ->
                  (* there is no process compiling the interface! starting one... *)
                  Printf.eprintf "[process_deferred_actions] there is no process compiling %s! starting one...\n%!" resolved_intf_fn;
                  let (full, pid) = compile_dependency_parallel resolved_intf_fn in
                  assert (full = resolved_intf_fn);
                  Hashtbl.add pending resolved_intf_fn (pid, ref [(store, Find_normalized_with_visibility k)])

                | Some (pid, snapshots) ->
                  (* this process is already under compilation! *)
                  Printf.eprintf "[process_deferred_actions] %s is already under compilation (pid=%d)! adding continuation to list...\n%!" resolved_intf_fn pid;
                  snapshots := (store, Find_normalized_with_visibility k) :: !snapshots
                end;
              end
            )
          | _ -> None
        };
      )

    | None -> ();


    try_resume_continuations ()
  done;

  Printf.eprintf "[process_deferred_actions] all done! exiting...\n%!";
  (* at this point, all continuations should have been resumed *)

  Clflags.output_name := final_output_name;
  Compenv.stop_early :=
    !compile_only ||
    !print_types ||
    match !stop_after with
    | None -> false
    | Some p -> Clflags.Compiler_pass.is_compilation_pass p

let maindriver argv ppf =
  let program = "ocamlc" in
  Clflags.add_arguments __LOC__ Options.list;
  Clflags.add_arguments __LOC__
    ["-depend", Arg.Unit Makedepend.main_from_option,
     "<options> Compute dependencies (use 'ocamlc -depend -help' for details)"];
  let exception Continue in
  match
    Compenv.readenv ppf Before_args;
    Compenv.parse_arguments (ref argv) Compenv.anonymous program;
    Compmisc.read_clflags_from_env ();
    if !Clflags.plugin then
      Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
    begin try
      (* here *)
      process_deferred_actions Compenv.{
        log = ppf;
        compile_implementation = Compile.implementation;
        compile_interface = Compile.interface;
        ocaml_mod_ext = ".cmo";
        ocaml_lib_ext = ".cma";
      }
    with Arg.Bad msg ->
      begin
        prerr_endline msg;
        Clflags.print_arguments program;
        exit 2
      end
    end;
    if Clflags.(should_stop_after Compiler_pass.Lambda)
      then raise Continue;
    Compenv.readenv ppf Before_link;
    if
      List.length
        (List.filter (fun x -> !x)
           [make_archive;make_package;Compenv.stop_early;output_c_object])
        > 1
    then begin
      let module P = Clflags.Compiler_pass in
      match !stop_after with
      | None ->
          Compenv.fatal
            "Please specify at most one of -pack, -a, -c, -output-obj";
      | Some ((P.Parsing | P.Typing | P.Lambda) as p) ->
        assert (P.is_compilation_pass p);
        Printf.ksprintf Compenv.fatal
          "Options -i and -stop-after (%s) \
           are  incompatible with -pack, -a, -output-obj"
          (String.concat "|"
             (P.available_pass_names ~filter:(fun _ -> true) ~native:false))
      | Some (P.Scheduling | P.Emit) -> assert false (* native only *)
    end;
    if !make_archive then begin
      Compmisc.init_path ();

      Bytelibrarian.create_archive
        (Compenv.get_objfiles ~with_ocamlparam:false)
        (Compenv.extract_output !output_name);
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Compmisc.init_path ();
      let extracted_output = Compenv.extract_output !output_name in
      let revd = Compenv.get_objfiles ~with_ocamlparam:false in
      Compmisc.with_ppf_dump ~file_prefix:extracted_output (fun ppf_dump ->
        Bytepackager.package_files ~ppf_dump (Compmisc.initial_env ())
          revd (extracted_output));
      Warnings.check_fatal ();
    end
    else if not !Compenv.stop_early && !objfiles <> [] then begin
      let target =
        if !output_c_object && not !output_complete_executable then
          let s = Compenv.extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll
            || Filename.check_suffix s ".c")
          then s
          else
            Compenv.fatal
              (Printf.sprintf
                 "The extension of the output file must be .c, %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          Compenv.default_output !output_name
      in
      Compmisc.init_path ();
      Bytelink.link (Compenv.get_objfiles ~with_ocamlparam:true) target;
      Warnings.check_fatal ();
    end;
  with
  | exception (Compenv.Exit_with_status n) ->
    n
  | exception Continue
  | () ->
    Compmisc.with_ppf_dump ~file_prefix:"profile"
      (fun ppf -> Profile.print ppf !Clflags.profile_columns);
    0
  | exception x ->
  Location.report_exception ppf x;
  2

(* entry point *)
let () =
  let run () = exit (maindriver Sys.argv Format.err_formatter) in
  handle run

(* CAML_LD_LIBRARY_PATH= ../v1/bin/ocamlc ocamlcommon.cma ocamlbytecomp.cma unix.cma custom_ocamlc.ml -I +compiler-libs -I +unix -o custom-ocamlc *)
