(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Lambda

module DLS = Domain.DLS

(* Get oo primitives identifiers *)

let oo_prim = Lambda.transl_prim "CamlinternalOO"

(* Share blocks *)

let consts : (structured_constant, Ident.t) Hashtbl.t DLS.key =
  Local_store.s_table Hashtbl.create 17

let share c =
  match c with
    Const_block (_n, l) when l <> [] ->
      begin try
        Lvar (Hashtbl.find (DLS.get consts) c)
      with Not_found ->
        let id = Ident.create_local "shared" in
        Hashtbl.add (DLS.get consts) c id;
        Lvar id
      end
  | _ -> Lconst c

(* Collect labels *)

let cache_required = Local_store.s_ref false
let method_cache = Local_store.s_ref lambda_unit
let method_count = Local_store.s_ref 0
let method_table = Local_store.s_ref []

let meth_tag s = Lconst(Const_base(Const_int(Btype.hash_variant s)))

let next_cache tag =
  let n = DLS.get method_count in
  DLS.set method_count (n + 1);
  (tag, [DLS.get method_cache; Lconst(Const_base(Const_int n))])

let rec is_path = function
    Lvar _ | Lprim (Pgetglobal _, [], _) | Lconst _ -> true
  | Lprim (Pfield _, [lam], _) -> is_path lam
  | Lprim ((Parrayrefu _ | Parrayrefs _), [lam1; lam2], _) ->
      is_path lam1 && is_path lam2
  | _ -> false

let meth obj lab =
  let tag = meth_tag lab in
  if not (DLS.get cache_required && !Clflags.native_code) then (tag, []) else
  if not (is_path obj) then next_cache tag else
  try
    let r = List.assoc obj (DLS.get method_table) in
    try
      (tag, List.assoc tag !r)
    with Not_found ->
      let p = next_cache tag in
      r := p :: !r;
      p
  with Not_found ->
    let p = next_cache tag in
    DLS.set method_table ((obj, ref [p]) :: (DLS.get method_table));
    p

let reset_labels () =
  Hashtbl.clear (DLS.get consts);
  DLS.set method_count 0;
  DLS.set method_table []

(* Insert labels *)

let int n = Lconst (Const_base (Const_int n))

let prim_makearray =
  Primitive.simple ~name:"caml_array_make" ~arity:2 ~alloc:true

(* Also use it for required globals *)
let transl_label_init_general f =
  let expr = f () in
  let expr =
    Hashtbl.fold
      (fun c id expr -> Llet(Alias, Pgenval, id, Lconst c, expr))
      (DLS.get consts) expr
  in
  (*let expr =
    List.fold_right
      (fun id expr -> Lsequence(Lprim(Pgetglobal id, [], Location.none), expr))
      (Env.get_required_globals ()) expr
  in
  Env.reset_required_globals ();*)
  reset_labels ();
  expr

let transl_label_init_flambda f =
  assert(Config.flambda);
  let method_cache_id = Ident.create_local "method_cache" in
  DLS.set method_cache (Lvar method_cache_id);
  (* Calling f (usually Translmod.transl_struct) requires the
     method_cache variable to be initialised to be able to generate
     method accesses. *)
  let expr = f () in
  let expr =
    if DLS.get method_count = 0 then expr
    else
      Llet (Strict, Pgenval, method_cache_id,
        Lprim (Pccall prim_makearray,
               [int (DLS.get method_count); int 0],
               Loc_unknown),
        expr)
  in
  transl_label_init_general (fun () -> expr)

let transl_store_label_init glob size f arg =
  assert(not Config.flambda);
  assert(!Clflags.native_code);
  DLS.set method_cache (Lprim(Pfield (size, Pointer, Mutable),
                        (* XXX KC: conservative *)
                        [Lprim(Pgetglobal glob, [], Loc_unknown)],
                        Loc_unknown));
  let expr = f arg in
  let (size, expr) =
    if DLS.get method_count = 0 then (size, expr) else
    (size+1,
     Lsequence(
     Lprim(Psetfield(size, Pointer, Root_initialization),
           [Lprim(Pgetglobal glob, [], Loc_unknown);
            Lprim (Pccall prim_makearray,
                   [int (DLS.get method_count); int 0],
                   Loc_unknown)],
           Loc_unknown),
     expr))
  in
  size, transl_label_init_general (fun () -> expr)

let transl_label_init f =
  if !Clflags.native_code then
    transl_label_init_flambda f
  else
    transl_label_init_general f

(* Share classes *)

let wrapping = Local_store.s_ref false
let top_env = Local_store.s_ref Env.empty
let classes = Local_store.s_ref []
let method_ids = Local_store.s_ref Ident.Set.empty

let oo_add_class id =
  DLS.set classes (id :: DLS.get classes);
  (DLS.get top_env, DLS.get cache_required)

let oo_wrap_gen env req f x =
  if DLS.get wrapping then
    if DLS.get cache_required then f x else
      Misc.protect_refs [Misc.R' (cache_required, true)] (fun () ->
          f x
        )
  else
    Misc.protect_refs [Misc.R' (wrapping, true); Misc.R' (top_env, env)]
      (fun () ->
         DLS.set cache_required req;
         DLS.set classes [];
         DLS.set method_ids Ident.Set.empty;
         let lambda, other = f x in
         let lambda =
           List.fold_left
             (fun lambda id ->
                Llet(StrictOpt, Pgenval, id,
                     Lprim(Pmakeblock(0, Mutable, None),
                           [lambda_unit; lambda_unit; lambda_unit],
                           Loc_unknown),
                     lambda))
             lambda (DLS.get classes)
         in
         lambda, other
      )

let oo_wrap env req f x =
  let lam, () = oo_wrap_gen env req (fun x -> f x, ()) x in
  lam

let reset () =
  Hashtbl.clear (DLS.get consts);
  DLS.set cache_required false;
  DLS.set method_cache lambda_unit;
  DLS.set method_count 0;
  DLS.set method_table [];
  DLS.set wrapping false;
  DLS.set top_env Env.empty;
  DLS.set classes [];
  DLS.set method_ids Ident.Set.empty
