(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Effect

module Dir = struct
  type t = {
    path : string;
    files : string list ref;
    hidden : bool;
  }

  let path t = t.path
  let files t = !(t.files)
  let hidden t = t.hidden

  let find t fn =
    if List.mem fn !(t.files) then
      Some (Filename.concat t.path fn)
    else
      None

  let find_normalized t fn =
    let fn = Misc.normalized_unit_filename fn in
    let search base =
      if Misc.normalized_unit_filename base = fn then
        Some (Filename.concat t.path base)
      else
        None
    in
    List.find_map search !(t.files)

  (* For backward compatibility reason, simulate the behavior of
     [Misc.find_in_path]: silently ignore directories that don't exist
     + treat [""] as the current directory. *)
  let readdir_compat dir =
    try
      Sys.readdir (if dir = "" then Filename.current_dir_name else dir)
    with Sys_error _ ->
      [||]

  let create ~hidden path =
    { path; files = ref (Array.to_list (readdir_compat path)); hidden }

  let add_file t fn =
    if not (List.mem fn !(t.files)) then
      t.files := fn :: !(t.files)
end

type auto_include_callback =
  (Dir.t -> string -> string option) -> string -> string

let no_auto_include _ _ = raise Not_found

type visibility = Visible | Hidden

type paths =
  { visible : string list;
    hidden : string list }

type _ Effect.t +=
  | Find_path : string -> string Effect.t
  | Find_normalized_with_visibility : string -> (string * visibility) Effect.t
  | Append_dir  : Dir.t -> unit Effect.t
  | Auto_include_otherlibs : (string -> unit) -> auto_include_callback Effect.t
  | Prepend_dir : Dir.t -> unit Effect.t
  | Remove_dir  : string -> unit Effect.t
  | Reset_path  : unit Effect.t
  | Init_path   : (string list * string list) -> unit Effect.t
  | Get_visible : Dir.t list Effect.t
  | Get_path_list : string list Effect.t
  | Get_paths : paths Effect.t

let reset () =
  assert (not Config.merlin || Local_store.is_bound ());
  perform Reset_path

let get_visible () = perform Get_visible

let get_path_list () = perform Get_path_list

let get_paths () = perform Get_paths

(* unused variable auto_include *)
let [@ocaml.warning "-27"] init ~auto_include ~visible ~hidden =
  reset ();
  perform (Init_path (visible, hidden))

let remove_dir dir =
  assert (not Config.merlin || Local_store.is_bound ());
  perform (Remove_dir dir)

(* General purpose version of function to add a new entry to load path: We only
   add a basename to the cache if it is not already present, in order to enforce
   left-to-right precedence. *)
let add (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
  perform (Append_dir dir)

let append_dir = add

let add_dir ~hidden dir = add (Dir.create ~hidden dir)

(* Add the directory at the start of load path - so basenames are
   unconditionally added. *)
let prepend_dir (dir : Dir.t) =
  assert (not Config.merlin || Local_store.is_bound ());
  perform (Prepend_dir dir)

let auto_include_otherlibs alert find_in_dir fn =
  let callback = perform (Auto_include_otherlibs alert) in
  callback find_in_dir fn

let find fn =
  assert (not Config.merlin || Local_store.is_bound ());
  (* let snapshot = Local_store.snapshot (string_of_int (Random.bits ())) in *)
  let ret = perform (Find_path fn) in
  (* Local_store.restore snapshot; *)
  ret

let find_normalized_with_visibility fn =
  assert (not Config.merlin || Local_store.is_bound ());
  (* let snapshot = Local_store.snapshot (string_of_int (Random.bits ())) in *)
  let ret = perform (Find_normalized_with_visibility fn) in
  (* Local_store.restore snapshot; *)
  ret

let find_normalized fn = fst (find_normalized_with_visibility fn)
