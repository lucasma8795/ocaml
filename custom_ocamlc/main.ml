open Clflags
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
          with Not_found ->
            Effect.Deep.discontinue k Not_found
        )

      (* find_normalized_with_visibility : string -> string * visibility *)
      | Load_path.Find_normalized_with_visibility fn ->
        Some (fun (k: (c, _) continuation) ->
          try
            Effect.Deep.continue k (find_normalized_with_visibility fn)
          with Not_found ->
            Effect.Deep.discontinue k Not_found
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
          Custom_load_path.reset ();
          Effect.Deep.continue k ()
        )

      (* init : auto_include_callback -> string list -> string list -> unit
         assumes Reset_path has previously been performed *)
      | Load_path.Init_path (visible, hidden) ->
        Some (fun (k: (c, _) continuation) ->
          Custom_load_path.init ~visible ~hidden;
          Effect.Deep.continue k ()
        )

      (* get_visible : unit -> Dir.t list *)
      | Load_path.Get_visible ->
        Some (fun (k: (c, _) continuation) ->
          Effect.Deep.continue k (List.rev !Custom_load_path.visible_dirs)
        )

      (* get_path_list : unit -> string list *)
      | Load_path.Get_path_list ->
        Some (fun (k: (c, _) continuation) ->
          Effect.Deep.continue k (get_path_list ())
        )

      (* get_paths : unit -> paths *)
      | Load_path.Get_paths ->
        Some (fun (k: (c, _) continuation) ->
          Effect.Deep.continue k Load_path.{
            visible = List.rev_map Dir.path !visible_dirs;
            hidden  = List.rev_map Dir.path !hidden_dirs
          }
        )

      | _ -> None
  }

let () =
  let run () = exit (Custom_maindriver.main Sys.argv Format.err_formatter) in
  handle run

(*

CAML_LD_LIBRARY_PATH= ../v1/bin/ocamlc ocamlcommon.cma ocamlbytecomp.cma unix.cma \
  custom_ocamlc/custom_load_path.mli custom_ocamlc/custom_load_path.ml \
  custom_ocamlc/parallelism.mli custom_ocamlc/parallelism.ml \
  custom_ocamlc/custom_maindriver.mli custom_ocamlc/custom_maindriver.ml \
  custom_ocamlc/main.ml -I custom_ocamlc -I +compiler-libs -I +unix -o custom-ocamlc

*)
