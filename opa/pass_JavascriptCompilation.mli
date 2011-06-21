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
   Compilation to Javascript, and reinsertion of the code in the qml.
   @author Mathieu Barbin
   @author Maxime Audoin
*)

(**

   The client code is compiled to [JsAst], and then serialized, using the runtime JsAst
   defined in the package [stdlib.js].
   The serialized js supports a runtime alpha-renaming and dead code elimination.
   This code is reinjected in the server code.

   Compositionality:
   + We should remember the list of extra libs and plugin already compiled and registred
   to avoid to register them several time.
*)

(**
   Process the code.

   FIXME:

   + passing options arround will probably become deprecated when we will be finished
   with the refactoring of private options with local Arg modules directly in passes.

   + the typing environment is probably needed as read-only.
   The reinjected code can be not typed, nobody cares.

   The returned value is the server with the client reinjected.
*)

(**
   {[
   a = register("key", b)
   ]}
   a -> b, "key"
*)

val process :
  options:OpaEnv.opa_options ->
  closure_map:Ident.t IdentMap.t ->
  renaming_server:QmlRenamingMap.t ->
  renaming_client:QmlRenamingMap.t ->
  client_roots:IdentSet.t ->
  typing:QmlTyper.env -> (* currently unused *)
  bsl_pp:(string -> string) ->
  bsl_client:BslLib.env_bsl ->
  server: QmlBlender.qml_milkshake ->
  client: QmlBlender.qml_milkshake ->
  QmlBlender.qml_milkshake
