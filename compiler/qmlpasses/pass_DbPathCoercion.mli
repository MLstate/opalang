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
   Pass: Coercion of DB-Paths.

   @author Louis Gesbert
   @author Mathieu Barbin
*)

(**
   TODO(louis) What does this pass do very precislly (annotations, transformations)

   Historic:
   This was originally part of the Blender. The blender has been splitten in S3.
*)

val process_code :
  val_:(string -> QmlAst.ident) ->
  QmlDbGen.Schema.t -> QmlTypes.gamma -> QmlAst.annotmap -> QmlAst.code ->
  QmlAst.annotmap * QmlAst.code
