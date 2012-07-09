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
   Pass: Generation and Finalization of DB-Schema.

   @author Louis Gesbert
   @author Mathieu Barbin
   @author Mikolaj Konarski
*)

module Arg :
sig

  (**
     Concatenation o all specs in interaction with this module.
     + [--database name:options] override options for database [name]
         if no name is supplied, it assumes a single, anonymous database
     + [--print-dbschema] print the db-schema using PassTracker file system.
     + [--display-dbschema] enforce [--print-dbschema] fork and display the schema
  *)
  val options : (Base.Arg.key * Base.Arg.spec * Base.Arg.doc) list

end

(**
   About arguments:
   + [gamma] is used read-only, for finding type definitions.
   + [annotmap] is used for error messages only, for finding positions.
*)
val process_code :
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlDbGen.Schema.t ->
  QmlAst.code ->
  QmlTypes.gamma * QmlDbGen.Schema.t * QmlAst.code
