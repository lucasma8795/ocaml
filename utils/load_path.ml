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
    files : string list;
    hidden : bool;
  }

  let path t = t.path
  let files t = t.files
  let hidden t = t.hidden

  let find t fn =
    if List.mem fn t.files then
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
    List.find_map search t.files

  (* For backward compatibility reason, simulate the behavior of
     [Misc.find_in_path]: silently ignore directories that don't exist
     + treat [""] as the current directory. *)
  let readdir_compat dir =
    try
      Sys.readdir (if dir = "" then Filename.current_dir_name else dir)
    with Sys_error _ ->
      [||]

  let create ~hidden path =
    { path; files = Array.to_list (readdir_compat path); hidden }
end

type auto_include_callback =
  (Dir.t -> string -> string option) -> string -> string

let no_auto_include _ _ = raise Not_found
let auto_include_callback = ref no_auto_include

type visibility = Visible | Hidden

type paths =
  { visible : string list;
    hidden : string list }

type _ Effect.t +=
  | Load_path : (string * auto_include_callback ref) -> string Effect.t
  | Load_path_normalized :
      (string * auto_include_callback ref) -> (string * visibility) Effect.t
  | Append_dir  : Dir.t -> unit Effect.t
  | Prepend_dir : Dir.t -> unit Effect.t
  | Remove_dir  : string -> unit Effect.t
  | Reset_path  : unit Effect.t
  | Init_path   : (string list * string list) -> unit Effect.t
  | Get_visible : Dir.t list Effect.t
  | Get_path_list : string list Effect.t
  | Get_paths : paths Effect.t

let reset () =
  assert (not Config.merlin || Local_store.is_bound ());
  perform Reset_path;
  auto_include_callback := no_auto_include

let get_visible () = perform Get_visible

let get_path_list () = perform Get_path_list

let get_paths () = perform Get_paths

let init ~auto_include ~visible ~hidden =
  reset ();
  perform (Init_path (visible, hidden));
  auto_include_callback := auto_include

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

let auto_include_libs libs alert find_in_dir fn =
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
  | None -> raise Not_found

let auto_include_otherlibs =
  (* Ensure directories are only ever scanned once *)
  let expand = Misc.expand_directory Config.standard_library in
  let otherlibs =
    let read_lib lib = lazy (Dir.create ~hidden:false (expand ("+" ^ lib))) in
    List.map (fun lib -> (lib, read_lib lib)) ["dynlink"; "str"; "unix"] in
  auto_include_libs otherlibs

let find fn =
  assert (not Config.merlin || Local_store.is_bound ());
  perform (Load_path (fn, auto_include_callback))

let find_normalized_with_visibility fn =
  assert (not Config.merlin || Local_store.is_bound ());
  perform (Load_path_normalized (fn, auto_include_callback))

let find_normalized fn = fst (find_normalized_with_visibility fn)
