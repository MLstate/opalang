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

(**
   Specific bsl processing for Ocaml files.

   This module implements part of the bslregister process.

   @author Mathieu Barbin
*)

(** *)
type ml_runtime = FBuffer.t
type ml_runtime_mli = FBuffer.t

(**
   Bsl Preprocessing for Ocaml files.

   This function is called at the end, once we have parsed all ocaml files,
   the fonction implements a fold on decorated_files, for building
   the runtime and the wanted interface (for checking).

   The function should also perform calls to the Register API,
   given as a record of type [BslPluginInterface.dynloader_interface],
   which contains support for registering types and primitives.

   The preprocess should respect the module hiearchy and keys names,
   for beeing coherent with Js, but the actual implementation is free,
   as long as the generated ml_runtime and ml_runtime_mli are coherent
   with the impl used as argument for calling the dynload interface.
*)
val preprocess :
  options:BslInterface.options ->
  plugins:BslPluginInterface.plugin list ->
  dynloader_interface:BslPluginInterface.dynloader_interface ->
  BslDirectives.bypasslang_decorated_file list ->
  BslPluginInterface.ocaml_env * ml_runtime option * ml_runtime_mli option
