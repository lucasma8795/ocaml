open Custom_load_path

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
          try
            Effect.Deep.continue k (find fn)
          with e -> begin
            Printf.eprintf "[effect_handler/find] exception while finding %s: %s\n%!" fn (Printexc.to_string e);
            Effect.Deep.discontinue k e
          end
        )

      (* find_normalized_with_visibility : string -> string * visibility *)
      | Load_path.Find_normalized_with_visibility fn ->
        Some (fun (k: (c, _) continuation) ->
          try
            Effect.Deep.continue k (find_normalized_with_visibility fn)
          with e -> begin
            Printf.eprintf "[effect_handler/find_normalized] exception while finding %s: %s\n%!" fn (Printexc.to_string e);
            Effect.Deep.discontinue k e
          end
        )

      (* append_dir : Dir.t -> unit *)
      | Load_path.Append_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          try
            append_dir dir;
            Effect.Deep.continue k ()
          with e -> begin
            Effect.Deep.discontinue k e
          end
        )

      (* auto_include_otherlibs : (string -> unit) -> auto_include_callback *)
      | Load_path.Auto_include_otherlibs alert ->
        Some (fun (k: (c, _) continuation) ->
          try
            Effect.Deep.continue k (auto_include_otherlibs alert)
          with e -> begin
            Effect.Deep.discontinue k e
          end
        )

      (* prepend_dir : Dir.t -> unit *)
      | Load_path.Prepend_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          try
            prepend_add dir;
            if Dir.hidden dir then
              hidden_dirs := !hidden_dirs @ [dir]
            else
              visible_dirs := !visible_dirs @ [dir];
            Effect.Deep.continue k ()
          with e -> begin
            Effect.Deep.discontinue k e
          end
        )

      (* remove_dir : Dir.t -> unit *)
      | Load_path.Remove_dir dir ->
        Some (fun (k: (c, _) continuation) ->
          try
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
          with e -> begin
            Effect.Deep.discontinue k e
          end
        )

      (* reset : unit -> unit *)
      | Load_path.Reset_path ->
        Some (fun (k: (c, _) continuation) ->
          try
            Custom_load_path.reset ();
            Effect.Deep.continue k ()
          with e -> begin
            Effect.Deep.discontinue k e
          end
        )

      (* init : auto_include_callback -> string list -> string list -> unit
         assumes Reset_path has previously been performed *)
      | Load_path.Init_path (visible, hidden) ->
        Some (fun (k: (c, _) continuation) ->
          try
            init ~visible ~hidden;
            Effect.Deep.continue k ()
          with e -> begin
            Effect.Deep.discontinue k e
          end
        )

      (* get_visible : unit -> Dir.t list *)
      | Load_path.Get_visible ->
        Some (fun (k: (c, _) continuation) ->
          try
            Effect.Deep.continue k (List.rev !visible_dirs)
          with e -> begin
            Effect.Deep.discontinue k e
          end
        )

      (* get_path_list : unit -> string list *)
      | Load_path.Get_path_list ->
        Some (fun (k: (c, _) continuation) ->
          try
            Effect.Deep.continue k (get_path_list ())
          with e -> begin
            Effect.Deep.discontinue k e
          end
        )

      (* get_paths : unit -> paths *)
      | Load_path.Get_paths ->
        Some (fun (k: (c, _) continuation) ->
          try
            Effect.Deep.continue k Load_path.{
              visible = List.rev_map Dir.path !visible_dirs;
              hidden  = List.rev_map Dir.path !hidden_dirs
            }
          with e -> begin
            Effect.Deep.discontinue k e
          end
        )

      | _ -> None
  }
