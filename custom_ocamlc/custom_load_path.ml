open Load_path

module STbl = Misc.Stdlib.String.Tbl

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
  Printf.eprintf "[auto_include] (entering with fn=%s)\n" fn;
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
    Printf.eprintf "[auto_include] failed, aborting (fn=%s)\n" fn;
    raise Not_found;
  end

let auto_include_otherlibs =
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

let reset () =
  Printf.eprintf "[Reset_path] resetting load path state\n%!";
  STbl.clear !hidden_files;
  STbl.clear !hidden_files_uncap;
  STbl.clear !visible_files;
  STbl.clear !visible_files_uncap;
  hidden_dirs := [];
  visible_dirs := []

let init ~visible ~hidden =
  Printf.eprintf "[Init_path] initializing load path state with %d visible and %d hidden dirs\n%!"
    (List.length visible) (List.length hidden);

  let visible = List.filter (fun p -> not (String.ends_with ~suffix:"v1/lib/ocaml" p)) visible in
  let hidden = List.filter (fun p -> not (String.ends_with ~suffix:"v1/lib/ocaml" p)) hidden in

  begin try
    visible_dirs := List.rev_map (fun path -> Dir.create ~hidden:false path) visible;
    hidden_dirs := List.rev_map (fun path -> Dir.create ~hidden:true path) hidden;
    List.iter prepend_add !hidden_dirs;
    List.iter prepend_add !visible_dirs;

  with e ->
    Printf.eprintf "[Init_path] failed to initialize load path state: %s\n" (Printexc.to_string e);
    raise e
  end;

  Printf.eprintf "[Init_path] visible_dirs: %s\n"
      (String.concat ", " (List.map (fun d -> Dir.path d) !visible_dirs));
  Printf.eprintf "[Init_path] hidden_dirs: %s\n"
    (String.concat ", " (List.map (fun d -> Dir.path d) !hidden_dirs))

let find fn =
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

let add_new_file_to_path fullname =
  if not (Sys.file_exists fullname) then begin
    Printf.eprintf "[add_new_file_to_path] %s does not exist, giving up...\n" fullname;
    raise Not_found
  end;

  (* add the file to the directory *)
  Printf.eprintf "* [add_new_file_to_path] adding %s to global state\n" fullname;

  let dirname = Filename.dirname fullname in
  let basename = Filename.basename fullname in

  let dir =
    try
      List.find (fun d -> Dir.path d = dirname) !visible_dirs
    with Not_found -> begin
      Printf.eprintf "[add_new_file_to_path] %s not found in directory list??\n" dirname;
      raise Not_found
    end
  in

  let basename_uncap = match Misc.normalized_unit_filename basename with
    | Error _ -> raise Not_found
    | Ok uncap -> uncap
  in

  if Dir.hidden dir then begin
    STbl.replace !hidden_files_uncap basename_uncap fullname;
    STbl.replace !hidden_files basename fullname
  end else begin
    STbl.replace !visible_files_uncap basename_uncap fullname;
    STbl.replace !visible_files basename fullname
  end;

  Dir.add_file dir basename;

  (* sanity checks *)
  assert (find basename = fullname)
