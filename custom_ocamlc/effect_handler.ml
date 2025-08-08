open Custom_load_path
open Custom_misc
open Effect.Deep

module Dir = Load_path.Dir

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
          match find fn with
          | fn -> continue k fn
          | exception e ->
            dbg "[effect_handler/find] exception while finding %s: %s\n" fn (Printexc.to_string e);
            discontinue k e
        )

      (* find_normalized_with_visibility : string -> string * visibility *)
      | Load_path.Find_normalized_with_visibility fn ->
        Some (fun (k: (c, _) continuation) ->
          match find_normalized_with_visibility fn with
          | (fn, visibility) -> continue k (fn, visibility)
          | exception e ->
            dbg "[effect_handler/find_normalized] exception while finding %s: %s\n" fn (Printexc.to_string e);
            discontinue k e
        )

      (* append_dir : Dir.t -> unit *)
      | Load_path.Append_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          match append_dir dir with
          | () -> continue k ()
          | exception e ->
            dbg "[effect_handler/append_dir] exception while appending %s: %s\n" (Dir.path dir) (Printexc.to_string e);
            discontinue k e
        )

      (* auto_include_otherlibs : (string -> unit) -> auto_include_callback *)
      | Load_path.Auto_include_otherlibs alert ->
        Some (fun (k: (c, _) continuation) ->
          match auto_include_otherlibs alert with
          | callback -> continue k callback
          | exception e ->
            dbg "[effect_handler/auto_include_otherlibs] exception while auto-including otherlibs: %s\n" (Printexc.to_string e);
            discontinue k e
        )

      (* prepend_dir : Dir.t -> unit *)
      | Load_path.Prepend_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          match prepend_dir dir with
          | dir -> continue k ()
          | exception e ->
            dbg "[effect_handler/prepend_dir] exception while prepending %s: %s\n" (Dir.path dir) (Printexc.to_string e);
            discontinue k e
        )

      (* remove_dir : Dir.t -> unit *)
      | Load_path.Remove_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          match remove_dir dir with
          | () -> continue k ()
          | exception e ->
            dbg "[effect_handler/remove_dir] exception while removing %s: %s\n" dir (Printexc.to_string e);
            discontinue k e
        )

      (* reset : unit -> unit *)
      | Load_path.Reset_path ->
        Some (fun (k: (c, _) continuation) ->
          match reset () with
          | () -> continue k ()
          | exception e ->
            dbg "[effect_handler/reset] exception while resetting load path: %s\n" (Printexc.to_string e);
            discontinue k e
        )

      (* init : auto_include_callback -> string list -> string list -> unit
         assumes Reset_path has previously been performed *)
      | Load_path.Init_path (visible, hidden) ->
        Some (fun (k: (c, _) continuation) ->
          match init ~visible ~hidden with
          | () -> continue k ()
          | exception e ->
            dbg "[effect_handler/init] exception while initializing load path: %s\n" (Printexc.to_string e);
            discontinue k e
        )

      (* get_visible : unit -> Dir.t list *)
      | Load_path.Get_visible ->
        Some (fun (k: (c, _) continuation) ->
          match get_visible () with
          | dirs -> continue k dirs
          | exception e ->
            dbg "[effect_handler/get_visible] exception while getting visible dirs: %s\n" (Printexc.to_string e);
            discontinue k e
        )

      (* get_path_list : unit -> string list *)
      | Load_path.Get_path_list ->
        Some (fun (k: (c, _) continuation) ->
          match get_path_list () with
          | paths -> continue k paths
          | exception e ->
            dbg "[effect_handler/get_path_list] exception while getting path list: %s\n" (Printexc.to_string e);
            discontinue k e
        )

      (* get_paths : unit -> paths *)
      | Load_path.Get_paths ->
        Some (fun (k: (c, _) continuation) ->
          match get_paths () with
          | paths -> continue k paths
          | exception e ->
            dbg "[effect_handler/get_paths] exception while getting paths: %s\n" (Printexc.to_string e);
            discontinue k e
        )

      | _ -> None
  }
