(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

(* aliases *)
module Tv = QmlTypeVars

type ordered_quantif =
    (Tv.TypeVar.t list, Tv.RowVar.t list, Tv.ColVar.t list) Tv.generic_quantif
type ('t, 'c) tsc =
    {
      mutable freevars : Tv.quantif option ; (** optimization : in order not to recompute freevars each time *)
      mutable phantomvars : Tv.quantif option ; (** optimization : in order not to recompute phantomvars each time *)
      (* note : phantomvars + vars of the body = quantif + freevars*)
      quantif : Tv.quantif ;
      nf_constraint : 'c ; (* used in env of typers *)
      body : 't
    }

(** used for specialization of named-type *)
let export_ordered_quantif tsc =
  {
    Tv.typevar = Tv.TypeVarSet.elements tsc.quantif.Tv.typevar;
    Tv.rowvar = Tv.RowVarSet.elements tsc.quantif.Tv.rowvar;
    Tv.colvar = Tv.ColVarSet.elements tsc.quantif.Tv.colvar
  }

let import vars t c =
  {
    freevars = None ;
    phantomvars = None;
    quantif = vars ;
    nf_constraint = c ;
    body = t
  }

let export_unsafe tsc =
  (tsc.quantif, tsc.body, tsc.nf_constraint)

let freevars_with_cache count_t_c_freevars tsc =
  match tsc.freevars with
  | Some set -> set
  | None ->
      let free = count_t_c_freevars tsc.body tsc.nf_constraint in
      let free = Tv.FreeVars.diff free tsc.quantif in
      begin tsc.freevars <- Some free end;
      free

let phantomvars_with_cache count_t_c_freevars tsc =
  match tsc.phantomvars with
  | Some set -> set
  | None ->
      let free = count_t_c_freevars tsc.body tsc.nf_constraint in
      let phantom = Tv.FreeVars.diff tsc.quantif free in
      begin tsc.phantomvars <- Some phantom end;
      phantom

let export_vars t = t.quantif

let arity t = Tv.TypeVarSet.cardinal t.quantif.Tv.typevar

let is_empty t =
  Tv.TypeVarSet.is_empty t.quantif.Tv.typevar &&
    Tv.RowVarSet.is_empty t.quantif.Tv.rowvar &&
    Tv.ColVarSet.is_empty t.quantif.Tv.colvar

let full_arity t =
  (Tv.TypeVarSet.cardinal t.quantif.Tv.typevar,
   Tv.RowVarSet.cardinal t.quantif.Tv.rowvar,
   Tv.ColVarSet.cardinal t.quantif.Tv.colvar)

let map_body_unsafe f t =
  {t with body = f t.body}

let map_body_unsafe2 f t =
  let (t', v) = f t.body in
  ({t with body = t'}, v)
