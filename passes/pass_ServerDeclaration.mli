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
   This pass checks if one or several 'server' values are defined at the
   toplevel. If so, it updates the options and insert a \@server_entry_point
   directive for each 'server' value, on user code.  If a 'server' value is
   found in strandard library code, it raise an exception and stop the
   compilation.


   Assumptions: no alpha renaming

   Directives removed : [\[\]]

   Directives added : [\[ `server_entry_point \]]

 *)
val pass_check_server_entry_point :
  options:OpaEnv.opa_options ->
  (SurfaceAst.nonuid, [< SurfaceAst.all_directives > `server_entry_point ] as 'a) SurfaceAstPasses.env_both_lcodes ->
    (OpaEnv.opa_options * (string, 'a) SurfaceAstPasses.env_both_lcodes)

(**
  This pass replaces \@server_entry_point with call to a Bsl function which will
  store [service] produced by server maker.

  Directives removed : [\[ `server_entry_point \]]

  Directives added : [\[\]]
*)
val pass_resolve_server_entry_point :
  options:OpaEnv.opa_options ->
  (Ident.t, [< SurfaceAst.all_directives > `server_entry_point] as 'a) SurfaceAst.code ->
    (Ident.t, 'a) SurfaceAst.code


(**
  This pass add a toplevel declaration, which will launch the retrieve all
  services stored, combine them and launch the server.
  If retrieved services haven't same options (port, encryption, see [Server]), a
  runtime error will be produced.

  Directives removed : [\[ `server_entry_point \]]

  Directives added : [\[\]]
*)
val pass_adding_server :
  options:OpaEnv.opa_options ->
  (Ident.t, [< SurfaceAst.all_directives > `coerce ] as 'a) SurfaceAst.code ->
    (Ident.t, 'a) SurfaceAst.code
