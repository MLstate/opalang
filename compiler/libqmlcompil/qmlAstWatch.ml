(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
module Q = QmlAst

exception Bad_structure


(* assert false are forbidden !!! *)

let of_coerce = function
  | Q.Coerce (_, e, t) -> e, t
  | _ -> raise Bad_structure

let rec of_couple e = match e with
  | Q.Record (_, [("f1", e1); ("f2", e2) ]) -> (e1, e2)
  | _ -> of_couple (fst (of_coerce e) )

let is f e =
  try let _ = f e in true
  with Bad_structure -> false

let is_couple e = is of_couple e


module Pat =
struct
  type 'a util = Q.pat -> 'a

  let void_coerce ty =
    match ty with
    | Q.TypeName ([], ty) when let id = Q.TypeIdent.to_string ty in id = "void" || id = "unit" -> true
    | Q.TypeRecord (Q.TyRow ([], None)) -> true
    | _ -> false

  let bool_coerce ty =
    match ty with
    | Q.TypeName ([], ty) when Q.TypeIdent.to_string ty = "bool" -> true
    | _ -> false

  let is_void = function
    | Q.PatRecord (_, [], `closed) -> true
    | Q.PatCoerce (_, Q.PatRecord (_, [], _), ty) -> void_coerce ty
    | _ -> false

  let rec is_bool = function
    | Q.PatCoerce (_, pat, ty) -> if bool_coerce ty then is_bool pat else None
    | Q.PatRecord (_, [bool, void], `closed) ->
        if is_void void
        then (
          match bool with
          | "true" -> Some true
          | "false" -> Some false
          | _ -> None
        )
        else None
    | _ -> None
end

let uncons_ifthenelse if_ pats =
    match pats with
    | [ true_, then_ ; false_, else_ ] -> (
        if (Pat.is_bool true_) = (Some true) && (Pat.is_bool false_) = (Some false)
        then (
          match if_ with
          | Q.Coerce (_, if_, ty) ->
              if Pat.bool_coerce ty
              then Some (if_, then_, else_)
              else None
          | _ -> Some (if_, then_, else_)
        )
        else None
      )
    | _ -> None
