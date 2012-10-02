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
   Loading bsl plugins (bypass)
   @author Mathieu Barbin
*)

(**
   Load the BSL and any plug-in.

   Plug-ins are .cma/.cmo/.cmxs files, as documented in the documentation of libbsl.
   Plug-ins are first registered and then properly loaded.

   Registration takes place in the following order:
   - Opa standard bypass library (defined in module {!OpabslPlugin})
   - files specified through command-line arguments {v *.opp v}, searched in the current
     directory and directories specified through command-line option {v -I v} -- if several files
     match the name specified, a warning is displayed and the first such file is used.
   - plugin specified through source files, using the import-plugin syntax

   Plugins are then sorted topologically by dependency.

   Finally, plug-ins are loaded both by the common BSL and a backend-specific loader and
   a dummy-backend bypass map is produced. This map contains all the bypasses -- if you
   need to restrict to a subset, use {restrict_any} or {restrict_all}.

   Loading is ensured by {!Dynlink} or {!Marshal}.
   This pass fails and exits if a module cannot be loaded or
   in case of circular dependencies.

   This pass returns an bsl environment, which is a bymap (indexed bypasses)
   and a list of plugins, sorted in a topologic order wrt their dependency order.
*)

val process :
  (OpaEnv.opa_back_end -> ((BslPluginInterface.plugin -> unit) option)) ->
  (OpaEnv.opa_back_end -> BslLanguage.t) ->
  options: OpaEnv.opa_options ->
  code:(((_, _) SurfaceAst.code_elt ObjectFiles.parsed_code) as 'code) ->
  OpaEnv.opa_options * 'code * BslLib.env_bsl
