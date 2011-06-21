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
(*
  Authors: 2009, Vincent Benayoun         <Vincent.Benayoun@mlstate.com>
*)

type ident = string

type hoop =
  | HOAdd
  | HOSubs
  | HOMult
  | HODiv
      (* TODO : extend with mult, minus, ... *)

let hoop_arity o =
  match o with
  | HOAdd -> 2
  | HOSubs -> 2
  | HOMult -> 2
  | HODiv -> 2

type hopred =
  | HOEq
  | HONeq
  | HOLe
  | HOLt


(* implicit database properties *)
type hoterm =
      (* basic terms *)
  | HO_Const   of QmlAst.const_expr
  | HO_ApplyOp of hoop * (hoterm list)
      (* variables : term, property or predicate *)
  | HO_Var     of ident
      (* predifined predicates *)
  | HO_Pred    of hopred * (hoterm list)
      (* abstraction and application *)
  | HO_Lambda  of  ident * hoterm
  | HO_Apply   of hoterm * hoterm
      (* logical connectives *)
  | HO_True
  | HO_Not     of hoterm
  | HO_And     of hoterm * hoterm
  | HO_Or      of hoterm * hoterm
  | HO_Implies of hoterm * hoterm
  | HO_Forall  of  ident * hoterm
  | HO_Exists  of  ident * hoterm
      (* property on evaluation of possibly impure qml expression
	 in which database is implicit *)
  | HO_Exec    of QmlAst.expr * (QmlAst.expr option) * hoterm (* exec e1 [returns e2] -> f  *)
      (* if [e1] is evaluated (in the value [e2]), the property [f] is true *)

type pcode_elt =
  | Code_elt      of QmlAst.code_elt
  | Precondition  of string * string * hoterm
  | Postcondition of string * string * string * hoterm
  | Property      of string * hoterm
  | Invariant     of hoterm

type pcode = pcode_elt list


(* let pcode_to_code l = *)
(*   let fold_fun acc c = *)
(*     match c with *)
(*     | Code_elt c -> c::acc *)
(*     | _ -> acc *)
(*   in *)
(*   List.fold_left fold_fun [] l *)
