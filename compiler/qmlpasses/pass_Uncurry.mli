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

(**
   [Pass_Uncurry] takes a lambda lifted code with eta expanded bypass
   and adds directive @closure_create and @closure_apply in the code
   that [Pass_Closure] with replace with actual code

   assumptions:
   renaming (identifiers are uniquely defined)
   lambda lifting has been done
   hoisting of bypass has been done

   @author Sebastien Briais
*)

val process_code:
  ?can_be_cleaned:(Ident.t -> bool) -> (* a predicate that tells if the closure of an identifier can be cleaned *)
  side:[`server|`client] ->
  typed:bool -> (* same remark as for Pass_LambdaLifting *)
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  (QmlTypes.gamma * QmlAst.annotmap) * Ident.t IdentMap.t (* maps the identifier of closures to the identifier of the implementation *)* QmlAst.code
