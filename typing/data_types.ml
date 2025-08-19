(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Picube, INRIA Paris                          *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types

(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
    cstr_uid: Uid.t;
   }

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_unboxed                        (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t * bool     (* Extension constructor
                                           true if a constant false if a block*)

let equal_tag t1 t2 =
  match (t1, t2) with
  | Cstr_constant i1, Cstr_constant i2 -> i2 = i1
  | Cstr_block i1, Cstr_block i2 -> i2 = i1
  | Cstr_unboxed, Cstr_unboxed -> true
  | Cstr_extension (path1, _), Cstr_extension (path2, _) ->
      Path.same path1 path2
  | (Cstr_constant _|Cstr_block _|Cstr_unboxed|Cstr_extension _), _ -> false

let equal_constr c1 c2 =
  equal_tag c1.cstr_tag c2.cstr_tag

let may_equal_constr c1 c2 =
  c1.cstr_arity = c2.cstr_arity
  && (match c1.cstr_tag,c2.cstr_tag with
     | Cstr_extension _,Cstr_extension _ ->
         (* extension constructors may be rebindings of each other *)
         true
     | tag1, tag2 ->
         equal_tag tag1 tag2)

let cstr_res_type_path cstr =
  let dbg s = Printf.eprintf "%d -> %s\n%!" (Domain.self () :> int) s in
  let rec pp_path (p : Path.t) =
    match p with
    | Pident id -> Ident.to_string id
    | Pdot(p, s) | Pextra_ty (p, Pcstr_ty s) ->
        Printf.sprintf "%s.%s" (pp_path p) s
    | Papply(p1, p2) -> Printf.sprintf "%s(%s)" (pp_path p1) (pp_path p2)
    | Pextra_ty (p, Pext_ty) -> pp_path p
  in
  match get_desc cstr.cstr_res with
  | Tconstr(p, _, _) -> p
  | Tsubst (expr, None) -> begin
      match expr.desc with
      | Tconstr (path, _, _) -> dbg (Printf.sprintf "got Tsubst (Tconstr (%s, ?, ?), None)" (pp_path path)); assert false
      | Tvar None -> dbg "got Tsubst (Tvar None, None)"; assert false
      | Tvar (Some id) -> dbg (Printf.sprintf "got Tsubst (Tvar (%s), None)" id); assert false
      | Ttuple _ -> dbg "got Tsubst (Ttuple _, None)"; assert false
      | Tobject _ -> dbg "got Tsubst (Tobject _, None)"; assert false
      | Tfield _ -> dbg "got Tsubst (Tfield _, None)"; assert false
      | Tnil -> dbg "got Tsubst (Tnil, None)"; assert false
      | Tlink _ -> dbg "got Tsubst (Tlink _, None)"; assert false
      | Tsubst _ -> dbg "got Tsubst (Tsubst _, None)"; assert false
      | Tvariant _ -> dbg "got Tsubst (Tvariant _, None)"; assert false
      | Tunivar _ -> dbg "got Tsubst (Tunivar _, None)"; assert false
      | Tpoly _ -> dbg "got Tsubst (Tpoly _, None)"; assert false
      | Tpackage _ -> dbg "got Tsubst (Tpackage _, None)"; assert false
      | _ -> dbg "got Tsubst (<something else>, None)"; assert false
    end
  | _ -> dbg "got something else"; assert false

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result (the record) *)
    lbl_arg: type_expr;                 (* Type of the argument
                                           (the field value) *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_atomic: atomic_flag;            (* Is this an atomic field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
   }

let lbl_res_type_path lbl =
  match get_desc lbl.lbl_res with
  | Tconstr (p, _, _) -> p
  | _ -> assert false
