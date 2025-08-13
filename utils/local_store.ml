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

module DLS = Domain.DLS

type ref_and_reset =
  | Table : { ref: 'a DLS.key; init: unit -> 'a } -> ref_and_reset
  | Ref : { ref: 'a DLS.key; mutable snapshot: 'a } -> ref_and_reset

type bindings = {
  mutable refs: ref_and_reset list;
  mutable frozen : bool;
  is_bound: bool DLS.key;
}

let global_bindings =
  { refs = []; is_bound = DLS.new_key (fun () -> false); frozen = false }

let is_bound () = DLS.get global_bindings.is_bound

let reset () =
  assert (is_bound ());
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
  Printf.eprintf "%d -> [local_store:freeze] bound initial values of global state!\n%!" (Domain.self () :> int)

let fresh name : store =
  assert (global_bindings.frozen);
  let slots =
    List.map (function
      | Table { ref; init } -> Slot {ref; value = init ()}
      | Ref r -> Slot { ref = r.ref; value = r.snapshot }
    ) global_bindings.refs
  in
  Printf.eprintf "%d -> [local_store:fresh] created new store with %d slots\n%!" (Domain.self () :> int) (List.length slots);
  slots, name

let active_store_name = DLS.new_key (fun () -> None)

let open_store (slots, name) =
  if (DLS.get active_store_name <> None) then
    Printf.eprintf "%d -> [local_store:open_store] error: store with name '%s' is already active\n%!"
      (Domain.self () :> int) (Option.get (DLS.get active_store_name));

  assert (DLS.get active_store_name = None);
  assert (not (is_bound ()));
  DLS.set global_bindings.is_bound true;
  DLS.set active_store_name (Some name);
  Printf.eprintf "%d -> [local_store:open_store] restored %d slots for store '%s'\n%!" (Domain.self () :> int) (List.length slots) name;
  List.iter (fun (Slot { ref; value }) -> DLS.set ref value) slots

let close_store (slots, name) =
  if (DLS.get active_store_name <> Some name) then begin
    match DLS.get active_store_name with
    | None ->
      Printf.eprintf "%d -> [local_store:close_store] error: tried to close '%s' but there was no active store\n%!"
        (Domain.self () :> int) name
    | Some active_name ->
        Printf.eprintf "%d -> [local_store:close_store] error: store with name '%s' is active but tried to close '%s'!\n%!"
          (Domain.self () :> int) active_name name
  end;

  assert (DLS.get active_store_name = Some name);
  assert (is_bound ());
  DLS.set global_bindings.is_bound false;
  DLS.set active_store_name None;

  Printf.eprintf "%d -> [local_store:close_store] saved %d slots to store '%s'\n%!" (Domain.self () :> int) (List.length slots) name;
  List.iter (fun (Slot s) -> s.value <- DLS.get s.ref) slots
