(*
    Copyright © 2011, 2012 MLstate

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
val ident_to_string : Ident.t -> string

val process :
  kind:[`adhoc | `ast] ->
  annotmap:QmlAst.annotmap ->
  stdlib_gamma:QmlTypes.gamma ->
  gamma:QmlTypes.gamma ->
  val_:(string -> Ident.t) ->
  generate:bool ->
  server_renaming:QmlRenamingMap.t ->
  client_renaming:QmlRenamingMap.t ->
  code:QmlAst.code ->
  QmlTypes.gamma * QmlAst.annotmap * QmlAst.code
