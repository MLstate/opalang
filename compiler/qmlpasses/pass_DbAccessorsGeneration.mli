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
   Pass: Generation of database accessors

   @author Louis Gesbert
   @author Vincent Benayoun
*)

val process_code :
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlDbGen.Schema.t ->
  QmlAst.code ->
  (QmlAlphaConv.t option) ->
  QmlDbGen.dbinfo StringListMap.t * QmlTypes.gamma * QmlAst.annotmap * QmlAst.code
