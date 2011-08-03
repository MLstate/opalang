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
    @author Rudy Sicard
**)


type directive_dep =
    [ `hybrid_value
    | `insert_server_value of Ident.t
    | `fun_action of QmlAst.fun_action_content option ]

val fold_directive_deps :
  (Ident.t -> 'a) -> (string -> 'a) -> directive_dep -> ('a -> 'b -> 'b) -> 'b -> 'b

val reorder :
  (string -> Ident.t) ->
  (Ident.t list) ->
  (Ident.t list IdentMap.t) ->
  ((int list) ->
    (int list IntMap.t) ->
    (int * IntSet.t) list ->
     (int * bool * IntSet.t) list * 'a) ->
  QmlAst.code ->
  QmlAst.code

(** functions meant for sliced cleaning *)

val get_unreachable_idents_of_code :
  (string -> Ident.t) ->
  Ident.t list ->
  QmlAst.code -> QmlAst.code ->
  IdentSet.t * QmlAst.code * QmlAst.code
(**
   This function returns the set of unreachable identifiers of the code
   so that you can clean not only the code but also the rest of the environment
*)
