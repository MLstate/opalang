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
   This pass checks the validity of the recursive definitions in the whole code
   and implements recursive bindings with values

   Post condition: all recursive bindings contains only lambdas
*)

module Warning : sig
  val recval : WarningClass.wclass

  val recval_lambda : WarningClass.wclass

  val set : WarningClass.Set.t
end

val process_code :
 val_ : (string -> Ident.t) ->
 QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  QmlTypes.gamma * QmlAst.annotmap * QmlAst.code
