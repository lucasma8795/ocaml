module STbl = Misc.Stdlib.String.Tbl
module Dir = Load_path.Dir

(* various state info originally in driver/Load_path.ml, I just ripped it out*)
type registry = string STbl.t

let visible_files : registry ref = Local_store.s_table STbl.create 42
let visible_files_uncap : registry ref = Local_store.s_table STbl.create 42

let hidden_files : registry ref = Local_store.s_table STbl.create 42
let hidden_files_uncap : registry ref = Local_store.s_table STbl.create 42

let visible_dirs : Dir.t list ref = Local_store.s_ref []
let hidden_dirs  : Dir.t list ref = Local_store.s_ref []

let find_file_in_cache (fn : string) =
  try (STbl.find !visible_files fn, Load_path.Visible) with
  | Not_found -> (STbl.find !hidden_files fn, Load_path.Hidden)

let find_file_in_cache_uncap (fn_uncap : string) =
  try (STbl.find !visible_files_uncap fn_uncap, Load_path.Visible) with
  | Not_found -> (STbl.find !hidden_files_uncap fn_uncap, Load_path.Hidden)

let is_basename fn = Filename.basename fn = fn

let get_visible_path_list () = List.rev_map Dir.path !visible_dirs
let get_hidden_path_list () = List.rev_map Dir.path !hidden_dirs

let get_path_list () =
  let acc = List.rev_map Dir.path !hidden_dirs in
  Misc.rev_map_end Dir.path !visible_dirs acc

let auto_include find_in_dir fn =
  if !Clflags.no_std_include then begin
    Printf.eprintf "[Load_path:auto_include] Clflags.no_std_include is true, abort \n";
    raise Not_found
  end else begin
    Printf.eprintf "[Load_path:auto_include] Clflags.no_std_include is false, continue\n";
    let alert = Location.auto_include_alert in
    Load_path.auto_include_otherlibs alert find_in_dir fn
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
    Printf.eprintf "[Load_path:Find_path] attempt to auto-include %s\n" fn;
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
      Printf.eprintf "[Load_path:Find_normalized_with_visibility] attempt to auto-include %s\n" fn;
      (* may throw Not_found again *)
      auto_include Dir.find_normalized fn_uncap, Load_path.Visible
    end


let add_new_file_to_path : string -> string -> unit = fun dirname base ->
  Printf.eprintf "[Load_path:add_new_file_to_path] adding dirname=%s base=%s to global state\n" dirname base;
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
  assert (find_path base = full_fn);
  let hidden = if Dir.hidden dir then Load_path.Hidden else Load_path.Visible in
  assert (find_normalized_with_visibility base = (Filename.concat dirname base, hidden))


(* takes in a *.cm{i,o} file, finds it in the load path and compiles it *)
let rec compile_dependency : string -> string = fun fn ->
  Printf.eprintf "[Load_path:compile_dependency] entering (fn=%s)\n" fn;

  (* this will most definitely break for capitalized file names *)
  let base = Filename.basename fn in
  let base = match Misc.normalized_unit_filename base with
    | Error _ -> raise Not_found
    | Ok ubase -> ubase
  in

  (* verify that fn is either a .cmi or a .cmo *)
  let ext =
    if String.ends_with ~suffix:".cmi" base then
      ".cmi"
    else if String.ends_with ~suffix:".cmo" base then
      ".cmo"
    else begin
      Printf.eprintf "[Load_path:compile_dependency] %s is not a .cmi or a .cmo file??\n" base;
      raise Not_found
    end
  in

  (* this should now be the file prefix, i.e. Foo for path/Foo.ml *)
  let prefix = Filename.chop_suffix base ext in

  (* if fn is a .cmi, we want to find a .mli, otherwise a .ml *)
  let source_ext = if ext = ".cmi" then ".mli" else ".ml" in

  (* attempt to resolve full path of file, this may throw Not_Found *)
  let full_source_file = find_path (prefix ^ source_ext) in
  let dirname = Filename.dirname full_source_file in
  Printf.eprintf "[Load_path:compile_dependency] path resolved to %s\n" full_source_file;
  Printf.eprintf "[Load_path:compile_dependency] dirname resolved to %s\n" dirname;

  (* if the .cm{i,o} file is already here, skip
     todo: read info off of cache instead *)
  let full_compiled_file = Filename.concat dirname base in
  if Sys.file_exists full_compiled_file then begin
    Printf.eprintf "[Load_path:compile_dependency] %s is already here, skipping\n" base;
    full_compiled_file
  end

  else

  let compile : unit -> unit = fun () ->
    (* todo: dynamic args instead of inheriting load path? *)

    (* construct -I and -H args *)
    let load_path_args =
      List.flatten (List.map (fun d -> ["-I"; Dir.path d]) !visible_dirs) @
      List.flatten (List.map (fun d -> ["-H"; Dir.path d]) !hidden_dirs)
    in

    let args =
      ["./boot/ocamlc"; "-c"; full_source_file; "-nostdlib";
       "-use-prims"; "runtime/primitives"; "-g"; "-strict-sequence";
       "-principal"; "-absname"; "-w"; "+a-4-9-40-41-42-44-45-48";
       "-warn-error"; "+a"; "-bin-annot"; "-strict-formats"] @
      load_path_args
    in

    let cmd = Filename.quote_command "./boot/ocamlrun" args in
    Printf.eprintf "[Sys.command] %s\n" cmd;
    let exit_code = Sys.command cmd in
    Printf.eprintf "[Load_path:compile_dependency] exit code: %d\n" exit_code;

    (* todo: throw exception instead? *)
    assert (exit_code = 0)
  in

  (* if we are compiling a .ml, compile the .mli if it exists (it is ok if not)
     (otherwise we get inconsistent .cmi's from the auto-generated one
     via compilation of a .ml, and compilation of the .mli) *)
  if source_ext = ".ml" then begin
    try
      let cmi_file = prefix ^ ".cmi" in
      Printf.eprintf "[Load_path:compile_dependency] attempting to compile %s (ok if it doesn't exist)\n" cmi_file;
      ignore (compile_dependency cmi_file)
      (* we probably want exit code to be 0 as well... *)

    with Not_found ->
      Printf.eprintf "[Load_path:compile_dependency] no .mli file for %s, continuing...\n" full_source_file
  end;

  (* compile the source file *)
  let () = try
    compile ();
    Printf.eprintf "[Load_path:compile_dependency] compiled successfully!\n"
  with Not_found -> begin
    Printf.eprintf "[Load_path:compile_dependency] failed to compile %s, quitting \n" full_source_file;
    raise Not_found
  end

  in

  (* add the new source file to global state *)
  add_new_file_to_path dirname base;
  Printf.eprintf "[Load_path:compile_dependency] added %s to global state\n" (find_path base);

  full_compiled_file

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
          (* Printf.eprintf "[Load_path:Find_path] %s\n" fn; *)
          try
            let ret = find_path fn in
            (* great! nothing wrong *)
            Effect.Deep.continue k ret

          with Not_found -> begin
            (* at this point, we need to compile the dependency *)
            Printf.eprintf "[Load_path:Find_path] compiling dependency %s\n" fn;
            try
              let full_fn = compile_dependency fn in
              Effect.Deep.continue k full_fn

            (* where is the dependency?!?! *)
            with Not_found ->
              Effect.Deep.discontinue k Not_found (* give up :( *)
          end
        )

      (* find_normalized_with_visibility : string -> string * visibility *)
      | Load_path.Find_normalized_with_visibility fn ->
        Some (fun (k: (c, _) continuation) ->
          (* Printf.eprintf "[Load_path:Find_normalized_with_visibility] %s\n" fn; *)
          try
            Effect.Deep.continue k (find_normalized_with_visibility fn)
          with Not_found -> begin
            (* at this point, we need to compile the dependency *)
            Printf.eprintf "[Load_path:Find_normalized_with_visibility] compiling dependency %s\n" fn;
            try
              let full_fn = compile_dependency fn in
              Effect.Deep.continue k (full_fn, Load_path.Visible)

            (* where is the dependency?!?! *)
            with Not_found ->
              Effect.Deep.discontinue k Not_found (* give up :( *)
          end
        )

      (* append_dir : Dir.t -> unit *)
      | Load_path.Append_dir dir ->
        Some (fun (k: (c, _) continuation) ->
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
            visible_dirs := dir :: !visible_dirs;
          Effect.Deep.continue k ()
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
          visible_dirs := List.rev_map (fun path -> Dir.create ~hidden:false path) visible;
          hidden_dirs := List.rev_map (fun path -> Dir.create ~hidden:true path) hidden;
          List.iter prepend_add !hidden_dirs;
          List.iter prepend_add !visible_dirs;

          (* Printf.eprintf "[Load_path:Init_path] visible_dirs: %s\n"
            (String.concat ", " (List.map (fun d -> Dir.path d) !visible_dirs));
          Printf.eprintf "[Load_path:Init_path] hidden_dirs: %s\n"
            (String.concat ", " (List.map (fun d -> Dir.path d) !hidden_dirs)); *)

          Effect.Deep.continue k ();
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
