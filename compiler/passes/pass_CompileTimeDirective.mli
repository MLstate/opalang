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

  For inserted values, we can use a extra directive, with a key in the stdlib.
  instead of inserting a identifier.

  myvariable = @compiletime("git-version")

  And we can document somewhere all available keys, so that the user may
  decide or not to introduce them in its code, and making less hacky the procedure.
*)

val infos_ident_names : string list

val process_code :
  options:OpaEnv.opa_options ->
  (Ident.t,([< SurfaceAst.all_directives > `coerce `compiletime] as 'dir)) SurfaceAst.code ->
  (Ident.t,'dir) SurfaceAst.code
