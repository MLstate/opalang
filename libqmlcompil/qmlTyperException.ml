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

(**
   This file contains all the possible cases of Exception possibly returned by
   any Typer.
   If you want to add an exception, do it here, and provide the to_string which
   goes together.
   Remember that this file is the interface between the typer - guys and the
   user.

   This module has no mli because it would duplicate the definition of the
   type t.
*)


(* TODO: this almost the same as QmlError.location; use QmlError and refactor*)
type location =
  [`Expr_loc of QmlAst.expr
  |`Pat_loc of QmlAst.pat
  |`Ty_loc of QmlAst.ty
  |`No_loc]

module LocSet : (BaseSetSig.S with type elt = location)
  = BaseSet.Make (
    struct
      type t = location
      let compare l1 l2 =
        match l1, l2 with
        |`Expr_loc e1, `Expr_loc e2 -> Annot.compare (QmlAst.QAnnot.expr e1) (QmlAst.QAnnot.expr e2)
        |`Pat_loc e1, `Pat_loc e2 -> Annot.compare (QmlAst.QAnnot.pat e1) (QmlAst.QAnnot.pat e2)
        |`Ty_loc e1, `Ty_loc e2 -> Pervasives.compare e1(*.annot*) e2(*.annot*)
        |_ -> Pervasives.compare l1 l2
    end)

type loc_set = LocSet.t

let loc_set_empty = LocSet.empty
let loc_set_add = LocSet.add
let loc_set_union  = LocSet.union

type error_loc =
    (* the main location, of the subexpression being typed when error occured *)
    location *
    (* any extra related locations, e.g., an application in another let rec
       branch, which makes the currently typed application incorrect *)
    loc_set

let loc_empty = (`No_loc, LocSet.empty)

let loc_make main set =
  let set = LocSet.remove main set in
  (main, set)

let loc_add_main main (old_main, set) =
  (* [`No_loc] can enter the set, but there'll always be at most one copy *)
  let set = LocSet.add old_main set in
  loc_make main set

let loc_add_set set (main, old_set) =
  let set = LocSet.union old_set set in
  loc_make main set


type 'ty t =
  | InvalidExpression (* Invalid expression detected while typing (could have
      been caught earlier) -- expression is in the location *)
  | InvalidType of
      'ty * [`duplicate_field | `duplicate_field_with_diff_ty_in_sum_cases |
             `not_a_record | `record_not_closed | `abstract_in_ty_annotation |
             `other]
     (* Invalid type
      detected while typing:
         - coerce failure
         - record with several fields with the same name
         - several sum cases with a same field having different types
         - coerce to "external", i.e. into the constructor [QmlAst.TypeAbstract]
         - ... ? *)
  | InvalidTypeDefinition of 'ty * 'ty (* Corresponds to the restrictions which
      are specific to type definitions  e.g. type 'a t = ... 'b t ...,
      if a <> b gives InvalidTypeDefinition (['a], t, ... 'b t ...) *)
  | InvalidTypeUsage of QmlAst.typeident * QmlAst.typevar list * 'ty list (* The
      use of a typename does not agree with its definition (e.g. number of type
      parameters). *)
  | InvalidUnification of 'ty * 'ty * ('ty * 'ty) option (* The option is in
      case we have more precise information *)
  | InternalError of string
  | IdentifierNotFound of Ident.t * Ident.t list
      (* [IdentifierNotFound (missing, list_of_identifiers_at_this_point)].
         [list_of_identifiers_at_this_point] may be empty if we are in a context
         where the list of identifiers is unclear*)
  | TypeIdentNotFound of QmlAst.typeident
  | MatchNamedTypeProblem (* In pattern-matching, the typer sometimes tries to
      guess a typename; this exception corresponds to the case when it needed
      to guess a typename but didn't succeed; at time of writing, it means that
      several typenames are possible, and the typer refuses to create an
      overload (this may change very often). *)
  | UnableToTypeBypass of BslKey.t
  | NotImplementedYet of string
  | DuplicateTypeDefinitions of string (* An exception for QmlBlender and OPA,
      not thrown in the normal QML world. *)
  | ExpansiveExprAtTopLevel
  | Escaping_private_type of (Ident.t * 'ty)  (* A toplevel value definition has
      a type in which a @private type to the package appears and this definition
      is not marked @private, hence is exported outside the package. Since the
      @private type is not visible the toplevel definition must not be visible
      also. *)

type exn_t = error_loc * (QmlAst.ty t)
exception Exception of exn_t



(* val map : ('a -> 'b) -> 'a QmlTyperException.t -> 'b QmlTyperException.t *)
let map f_ty = function
  | InvalidExpression -> InvalidExpression
  | InvalidType (t,k) -> InvalidType (f_ty t, k)
  | InvalidTypeDefinition (ty1, ty2) ->
      InvalidTypeDefinition (f_ty ty1, f_ty ty2)
  | InvalidTypeUsage (tid, tvl, tyl) ->
      InvalidTypeUsage (tid, tvl, List.map f_ty tyl)
  | InvalidUnification (ty1, ty2, tys_opt) ->
      InvalidUnification
        (f_ty ty1, f_ty ty2,
         Option.map (fun (ty3, ty4) -> (f_ty ty3, f_ty ty4)) tys_opt)
  | InternalError s -> InternalError s
  | IdentifierNotFound _ as x -> x
  | TypeIdentNotFound x -> TypeIdentNotFound x
  | MatchNamedTypeProblem -> MatchNamedTypeProblem
  | UnableToTypeBypass x -> UnableToTypeBypass x
  | NotImplementedYet x -> NotImplementedYet x
  | DuplicateTypeDefinitions s -> DuplicateTypeDefinitions s
  | ExpansiveExprAtTopLevel -> ExpansiveExprAtTopLevel
  | Escaping_private_type (ident, ty) -> Escaping_private_type (ident, f_ty ty)
