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
   Specific bsl processing for Javascript files.

   This module implements part of the bslregister process.

   @author Mathieu Barbin
*)

(** {6 Type alias for clarty} *)
(** *)
type filename = string
type contents = string

(** We support two different syntaxes for JS bsl files: the classic
    one, with "##" directives, and another one that uses
    "documentation comments", such as those in jsdoc.
*)

type js_decorated_file = {
  directives: (FilePos.pos * BslTags.t * BslDirectives.Js.t) list;
  contents: JsAst.code;
  filename: filename;
}

type decorated_file =
| Classic of BslDirectives.bypasslang_decorated_file
| DocLike of js_decorated_file

(**
   Bsl Preprocessing for Javascript files.

   This function is called at the end, once we have parsed all javascript files,
   the function implements a fold on decorated_files, for building
   the runtime and the wanted interface (for checking).

   The function should also perform calls to the Register API,
   given as a record of type [BslPluginInterface.dynloader_interface],
   which contains support for registering types and primitives.

   The preprocess should respect the module hiearchy and key names,
   for being coherent with Ocaml, but the actual implementation is free,
   as long as the generated javascript is coherent
   with the impl used as argument for calling the dynload interface.

   The [options] are passed for checking issues.

   The [depends] is the js code of previous built plugins.
   If the js writen for a plugin depends on a previous plugin,
   its javascript code may depend on the javascirpt code of
   one of the previous plugins, so it is passed to the function
   preprocess to be used e.g. during checking to avoid unbound values.

   The returned js_code must be inserted into the plugin and escaped,
   so we need a string. Internally, the implementation does not use
   string concatenation, but a FBuffer, with a final [FBuffer.contents],
   before checking e.g.
*)
val preprocess :
  options:BslInterface.options ->
  plugins:BslPluginInterface.plugin list ->
  dynloader_interface:BslPluginInterface.dynloader_interface ->
  depends:(filename * contents) list ->
  lang:BslLanguage.t ->
  js_confs:BslJsConf.t ->
  decorated_file list ->
  BslPluginInterface.javascript_env * JsPackage.t
