module STbl = Misc.Stdlib.String.Tbl
module Dir = Load_path.Dir

type registry = string STbl.t

let visible_files : registry ref = Local_store.s_table STbl.create 42
let visible_files_uncap : registry ref = Local_store.s_table STbl.create 42

let hidden_files : registry ref = Local_store.s_table STbl.create 42
let hidden_files_uncap : registry ref = Local_store.s_table STbl.create 42

let visible_dirs : Dir.t list ref = Local_store.s_ref []
let hidden_dirs  : Dir.t list ref = Local_store.s_ref []

let find_file_in_cache fn visible_files hidden_files =
  try (STbl.find !visible_files fn, Load_path.Visible) with
  | Not_found -> (STbl.find !hidden_files fn, Load_path.Hidden)

let is_basename fn = Filename.basename fn = fn

let get_visible_path_list () = List.rev_map Dir.path !visible_dirs
let get_hidden_path_list () = List.rev_map Dir.path !hidden_dirs

let get_path_list () =
  Misc.rev_map_end Dir.path !visible_dirs (List.rev_map Dir.path !hidden_dirs)

let auto_include find_in_dir fn =
  if !Clflags.no_std_include then begin
    Printf.eprintf "[Load_path:auto_include] Clflags.no_std_include is true, abort \n";
    raise Not_found
  end else begin
    Printf.eprintf "[Load_path:auto_include] Clflags.no_std_include is false, continue\n";
    let alert = Location.auto_include_alert in
    Load_path.auto_include_otherlibs alert find_in_dir fn
  end

let prepend_add dir =
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

let find_path_unchecked fn =
  if is_basename fn && not !Sys.interactive then
    fst (find_file_in_cache fn visible_files hidden_files)
  else
    Misc.find_in_path (get_path_list ()) fn

let find_path fn =
  try
    find_path_unchecked fn
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
        find_file_in_cache fn_uncap visible_files_uncap hidden_files_uncap
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

let compile_dependency fn =
  let fn = match Misc.normalized_unit_filename fn with
    | Error _ -> raise Not_found
    | Ok fn_uncap -> fn_uncap
  in

  Printf.eprintf "[Load_path:compile_dependency] entering (fn=%s)\n" fn;

  let maybe_prefix = Filename.chop_suffix_opt ~suffix:".cmi" fn in
  let prefix =
    match maybe_prefix with
    | Some p -> p
    | None -> begin
        Printf.eprintf "[Load_path:compile_dependency] %s is not a .cmi file??\n" fn;
        raise Not_found
      end
  in

  let compile source_file =
    let args =
      ["./ocamlc"; "-c"; source_file] @
      (List.flatten (List.map (fun d -> ["-I"; Dir.path d]) !visible_dirs)) @
      (if List.is_empty !hidden_dirs then [] else "-H" :: List.map Dir.path !hidden_dirs)
    in
    let cmd = Filename.quote_command "runtime/ocamlrun" args in
    Printf.eprintf "[Sys.command] %s\n" cmd;
    ignore (Sys.command cmd)
  in

  let try_compile ext compile_fn =
    let file = prefix ^ ext in
    try
      Printf.eprintf "[Load_path:compile_dependency] compiling %s\n" file;
      compile_fn file
    with Not_found ->
      Printf.eprintf "[Load_path:compile_dependency] %s not found%s\n"
        file (if ext = ".ml" then "!!" else ", skipping");
      if ext = ".ml" then raise Not_found
  in

  try_compile ".mli" (fun f -> compile (find_path_unchecked f));
  try_compile ".ml"  (fun f -> compile (find_path_unchecked f));

  (* or do we just return unit... *)
  let full_ml_path = find_path_unchecked (prefix ^ ".ml") in
  Filename.chop_suffix full_ml_path ".ml"

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
          Printf.eprintf "[Load_path:Find_path] %s\n" fn;
          try
            let ret = find_path fn in
            (* great! nothing wrong *)
            Effect.Deep.continue k ret

          with Not_found -> begin
            (* at this point, we need to compile the dependency *)
            Printf.eprintf "[Load_path:Find_path] compiling dependency %s\n" fn;
            try
              let prefix = compile_dependency fn in
              Effect.Deep.continue k (prefix ^ ".cmi")

            (* where is the dependency?!?! *)
            with Not_found ->
              Effect.Deep.discontinue k Not_found (* give up :( *)
          end
        )

      (* find_normalized_with_visibility : string -> string * visibility *)
      | Load_path.Find_normalized_with_visibility fn ->
        Some (fun (k: (c, _) continuation) ->
          Printf.eprintf "[Load_path:Find_normalized_with_visibility] %s\n" fn;
          try
            let ret = find_normalized_with_visibility fn in
            Effect.Deep.continue k ret
          with Not_found -> begin
            (* at this point, we need to compile the dependency *)
            Printf.eprintf "[Load_path:Find_normalized_with_visibility] compiling dependency %s\n" fn;
            try
              let prefix = compile_dependency fn in
              Effect.Deep.continue k (prefix ^ ".cmi", Load_path.Visible)

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
          let hidden = List.filter (fun d -> Dir.path d <> dir) !hidden_dirs in
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
          visible_dirs := List.rev_map (Dir.create ~hidden:false) visible;
          hidden_dirs := List.rev_map (Dir.create ~hidden:true) hidden;
          List.iter prepend_add !hidden_dirs;
          List.iter prepend_add !visible_dirs;

          Printf.eprintf "[Load_path:Init_path] visible_dirs: %s\n"
            (String.concat ", " (List.map Dir.path !visible_dirs));
          Printf.eprintf "[Load_path:Init_path] hidden_dirs: %s\n"
            (String.concat ", " (List.map Dir.path !hidden_dirs));

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
            visible = List.rev_map Dir.path !visible_dirs;
            hidden = List.rev_map Dir.path !hidden_dirs
          }
          in
          Effect.Deep.continue k ret
        )

      | _ -> None
  }
