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

type ref_and_reset =
  | Table : { ref: 'a ref; init: unit -> 'a } -> ref_and_reset
  | Ref : { ref: 'a ref; mutable snapshot: 'a } -> ref_and_reset

type bindings = {
  mutable refs: ref_and_reset list;
  mutable frozen : bool;
  mutable is_bound: bool;
}

let global_bindings =
  { refs = []; is_bound = false; frozen = false }

let is_bound () = global_bindings.is_bound

let reset () =
  assert (is_bound ());
  List.iter (function
    | Table { ref; init } -> ref := init ()
    | Ref { ref; snapshot } -> ref := snapshot
  ) global_bindings.refs

let s_table create size =
  let init () = create size in
  let ref = ref (init ()) in
  assert (not global_bindings.frozen);
  global_bindings.refs <- (Table { ref; init }) :: global_bindings.refs;
  ref

let s_ref k =
  let ref = ref k in
  assert (not global_bindings.frozen);
  global_bindings.refs <-
    (Ref { ref; snapshot = k }) :: global_bindings.refs;
  ref

type slot = Slot : { ref : 'a ref; mutable value : 'a } -> slot
type store = slot list * string

let fresh name =
  let slots =
    List.map (function
      | Table { ref; init } -> Slot {ref; value = init ()}
      | Ref r ->
          if not global_bindings.frozen then r.snapshot <- !(r.ref);
          Slot { ref = r.ref; value = r.snapshot }
    ) global_bindings.refs
  in
  Printf.eprintf "%d -> [local_store:fresh] created new store with %d slots\n%!" (Domain.self () :> int) (List.length slots);
  global_bindings.frozen <- true;
  slots, name

let with_store (slots, name) f =
  assert (not global_bindings.is_bound);
  global_bindings.is_bound <- true;
  List.iter (fun (Slot {ref;value}) -> ref := value) slots;
  Printf.eprintf "%d -> [local_store:with_store] restored %d slots for store '%s'\n%!" (Domain.self () :> int) (List.length slots) name;
  Fun.protect f ~finally:(fun () ->
    List.iter (fun (Slot s) -> s.value <- !(s.ref)) slots;
    global_bindings.is_bound <- false;
  )

let active_store_name = ref None

let open_store (slots, name) =
  if (!active_store_name <> None) then
    Printf.eprintf "%d -> [local_store:open_store] error: store with name '%s' is already active\n%!"
      (Domain.self () :> int) (Option.get !active_store_name);

  assert (!active_store_name = None);
  assert (not global_bindings.is_bound);
  global_bindings.is_bound <- true;
  active_store_name := Some name;
  Printf.eprintf "%d -> [local_store:open_store] restored %d slots for store '%s'\n%!" (Domain.self () :> int) (List.length slots) name;
  List.iter (fun (Slot {ref;value}) -> ref := value) slots

let close_store (slots, name) =
  if (!active_store_name <> Some name) then begin
    match !active_store_name with
    | None ->
      Printf.eprintf "%d -> [local_store:close_store] error: tried to close '%s' but there was no active store\n%!"
        (Domain.self () :> int) name
    | Some active_name ->
        Printf.eprintf "%d -> [local_store:close_store] error: store with name '%s' is active but tried to close '%s'!\n%!"
          (Domain.self () :> int) active_name name
  end;

  assert (!active_store_name = Some name);
  assert (global_bindings.is_bound);
  global_bindings.is_bound <- false;
  active_store_name := None;
  Printf.eprintf "%d -> [local_store:close_store] saved %d slots to store '%s'\n%!" (Domain.self () :> int) (List.length slots) name;
  List.iter (fun (Slot s) -> s.value <- !(s.ref)) slots
