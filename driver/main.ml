open Effect.Deep
open Load_path
(* open Printf *)
open Local_store

module STbl = Misc.Stdlib.String.Tbl

type registry = string STbl.t

let visible_files : registry ref = s_table STbl.create 42
let visible_files_uncap : registry ref = s_table STbl.create 42

let hidden_files : registry ref = s_table STbl.create 42
let hidden_files_uncap : registry ref = s_table STbl.create 42

let visible_dirs : Dir.t list ref = s_ref []
let hidden_dirs  : Dir.t list ref = s_ref []

let find_file_in_cache fn visible_files hidden_files =
  try (STbl.find !visible_files fn, Visible) with
  | Not_found -> (STbl.find !hidden_files fn, Hidden)

let is_basename fn = Filename.basename fn = fn

let get_visible_path_list () = List.rev_map Dir.path !visible_dirs
let get_hidden_path_list () = List.rev_map Dir.path !hidden_dirs

let auto_include find_in_dir fn =
  if !Clflags.no_std_include then
    raise Not_found
  else
    let alert = Location.auto_include_alert in
    Load_path.auto_include_otherlibs alert find_in_dir fn

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

let () =
  let run () = exit (Maindriver.main Sys.argv Format.err_formatter) in
  match_with run ()
  {
    retc = (fun ret -> ret);
    exnc = raise;
    effc = fun (type c) (eff: c Effect.t) ->
      match eff with
      (* find : string -> string *)
      | Load_path fn ->
        Some (fun (k: (c, _) continuation) ->
          let ret =
            try
              if is_basename fn && not !Sys.interactive then
                fst (find_file_in_cache fn visible_files hidden_files)
              else
                Misc.find_in_path (get_path_list ()) fn
            with Not_found ->
              auto_include Dir.find fn;
            in
          continue k ret
        )

      | Load_path_normalized fn ->
        Some (fun (k: (c, _) continuation) ->
          match Misc.normalized_unit_filename fn with
          | Error _ -> raise Not_found
          | Ok fn_uncap ->
          let ret =
            try
              if is_basename fn && not !Sys.interactive then
                find_file_in_cache fn_uncap
                  visible_files_uncap hidden_files_uncap
              else
                try
                  (Misc.find_in_path_normalized (get_visible_path_list ()) fn, Visible)
                with
                | Not_found ->
                  (Misc.find_in_path_normalized (get_hidden_path_list ()) fn, Hidden)
            with Not_found ->
              auto_include Dir.find_normalized fn_uncap, Visible
            in
          continue k ret
        )

      (* append_dir : Dir.t -> unit *)
      | Append_dir dir ->
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
          continue k ()
        )

      (* prepend_dir : Dir.t -> unit *)
      | Prepend_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          prepend_add dir;
          if Dir.hidden dir then
            hidden_dirs := !hidden_dirs @ [dir]
          else
            visible_dirs := !visible_dirs @ [dir];
          continue k ()
        )

      (* remove_dir : Dir.t -> unit *)
      | Remove_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          let visible = List.filter (fun d -> Dir.path d <> dir) !visible_dirs in
          let hidden = List.filter (fun d -> Dir.path d <> dir) !hidden_dirs in
          if   List.compare_lengths visible !visible_dirs <> 0
            || List.compare_lengths hidden !hidden_dirs <> 0 then begin
            reset ();
            visible_dirs := visible;
            hidden_dirs := hidden;
            List.iter prepend_add hidden;
            List.iter prepend_add visible
          end;
          continue k ()
        )

      (* reset : unit -> unit *)
      | Reset_path ->
        Some (fun (k: (c, _) continuation) ->
          STbl.clear !hidden_files;
          STbl.clear !hidden_files_uncap;
          STbl.clear !visible_files;
          STbl.clear !visible_files_uncap;
          hidden_dirs := [];
          visible_dirs := [];
          continue k ()
        )

      (* init : auto_include_callback -> string list -> string list -> unit
         assumes Reset_path has previously been performed *)
      | Init_path (visible, hidden) ->
        Some (fun (k: (c, _) continuation) ->
          visible_dirs := List.rev_map (Dir.create ~hidden:false) visible;
          hidden_dirs := List.rev_map (Dir.create ~hidden:true) hidden;
          List.iter prepend_add !hidden_dirs;
          List.iter prepend_add !visible_dirs;
          continue k ()
        )

      (* get_visible : unit -> Dir.t list *)
      | Get_visible ->
        Some (fun (k: (c, _) continuation) ->
          continue k (List.rev !visible_dirs)
        )

      (* get_path_list : unit -> string list *)
      | Get_path_list ->
        Some (fun (k: (c, _) continuation) ->
          let ret = Misc.rev_map_end Dir.path !visible_dirs (List.rev_map Dir.path !hidden_dirs) in
          continue k ret
        )

      (* get_paths : unit -> paths *)
      | Get_paths ->
        Some (fun (k: (c, _) continuation) ->
          let ret = {
            visible = List.rev_map Dir.path !visible_dirs;
            hidden = List.rev_map Dir.path !hidden_dirs
          }
          in
          continue k ret
        )

      | _ -> None
  }
