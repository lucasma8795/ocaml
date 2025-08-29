(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Frederic Bour, Tarides                          *)
(*                         Thomas Refis, Tarides                          *)
(*                                                                        *)
(*   Copyright 2020 Tarides                                               *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Dbg

module DLS = Domain.DLS

type ref_and_reset =
  | Table : { ref: 'a DLS.key; init: unit -> 'a } -> ref_and_reset
  | Ref : { ref: 'a DLS.key; mutable snapshot: 'a } -> ref_and_reset

type bindings = {
  mutable refs: ref_and_reset list;
  mutable frozen : bool;
}

let global_bindings =
  { refs = []; frozen = false }

let reset () =
  List.iter (function
    | Table { ref; init } -> DLS.set ref (init ())
    | Ref { ref; snapshot } -> DLS.set ref snapshot
  ) global_bindings.refs

let s_table create size =
  let init () = create size in
  let ref = DLS.new_key (fun () -> init ()) in
  assert (not global_bindings.frozen);
  global_bindings.refs <- (Table { ref; init }) :: global_bindings.refs;
  ref

let s_ref k =
  let ref = DLS.new_key (fun () -> k) in
  assert (not global_bindings.frozen);
  global_bindings.refs <-
    (Ref { ref; snapshot = k }) :: global_bindings.refs;
  ref

type slot = Slot : { ref : 'a DLS.key; mutable value : 'a } -> slot
type store = slot list * string

(* only call once, from main thread, before calling new_store *)
let freeze () =
  assert (not global_bindings.frozen);
  global_bindings.frozen <- true;
  List.iter (function
    | Table _ -> ()
    | Ref r -> r.snapshot <- DLS.get r.ref
  ) global_bindings.refs;
  dbg "[local_store:freeze] bound initial values of global state!\n%!"

let active_store_name = DLS.new_key (fun () -> None)

let is_bound () = DLS.get active_store_name <> None

let snapshot name : store =
  (* assert (DLS.get active_store_name <> None);
  DLS.set active_store_name None; *)
  let slots =
    List.map (function
      | Table { ref; _ } -> Slot { ref; value = DLS.get ref }
      | Ref { ref; _ }   -> Slot { ref; value = DLS.get ref }
    ) global_bindings.refs
  in
  (* make sure ref contents aren't physically shared *)
  reset ();
  dbg "[local_store:snapshot] snapshot %d slots to store '%s'\n%!" (List.length slots) name;
  slots, name

let restore (slots, name) =
  (* assert (DLS.get active_store_name = None);
  DLS.set active_store_name (Some name); *)
  List.iter (fun (Slot { ref; value }) -> DLS.set ref value) slots;
  dbg "[local_store:restore] restored %d slots from store '%s'\n%!" (List.length slots) name

(* let open_store (slots, name) =
  if (DLS.get active_store_name <> None) then
    dbg "[local_store:open_store] error: store with name '%s' is already active\n%!"
      (Option.get (DLS.get active_store_name));

  assert (DLS.get active_store_name = None);
  assert (not (is_bound ()));
  DLS.set global_bindings.is_bound true;
  DLS.set active_store_name (Some name);
  dbg "[local_store:open_store] restored %d slots for store '%s'\n%!" (List.length slots) name;
  List.iter (fun (Slot { ref; value }) -> DLS.set ref value) slots

let close_store (slots, name) =
  if (DLS.get active_store_name <> Some name) then begin
    match DLS.get active_store_name with
    | None ->
      dbg "[local_store:close_store] error: tried to close '%s' but there was no active store\n%!"
        name
    | Some active_name ->
        dbg "[local_store:close_store] error: store with name '%s' is active but tried to close '%s'!\n%!"
          active_name name
  end;

  assert (DLS.get active_store_name = Some name);
  assert (is_bound ());
  DLS.set global_bindings.is_bound false;
  DLS.set active_store_name None;

  dbg "[local_store:close_store] saved %d slots to store '%s'\n%!" (List.length slots) name;
  List.iter (fun (Slot s) -> s.value <- DLS.get s.ref) slots *)
