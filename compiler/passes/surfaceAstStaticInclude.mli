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
   This module gathers the passes that statically include some content
   in the source code
*)


val warning_set: WarningClass.Set.t

(**
   This pass replaces \@static_binary_content and \@static_source_content with
   the actual content of the file (in release mode, or with some code to load
   the file at run time otherwise)

   Assumptions: the files exist on the disk maybe?

   Directives removed : [\[ `static_binary_content | `static_source_content \]]

   Directives added : [\[\]]

   The type doesn't reflect that the directives have disappeared,
   see the comments at pass_collect_annotations for why it is so
 *)
val pass_static_inclusions :
  options:OpaEnv.opa_options ->
    (Ident.t,
      [< SurfaceAst.all_directives > `coerce `static_content `static_resource] as 'a) SurfaceAst.code ->
  (Ident.t, 'a) SurfaceAst.code

(**
    This pass replaces \@static_include_directory with the construction of a
    stringmap containing a binding between filename and a tuple of the mimetype
    and the content of the file.
    Mimetype and content are resolved on separates passes, on this one, only
    directives are inserted.
    By default, only  \@static_source_content is used.

    Directives removed : [\[ `static_file_mimetype \]]

    Directives added : [\[ `static_source_content \]]
*)
val pass_static_inclusion_directory :
  options:OpaEnv.opa_options ->
    (Ident.t,
      [< SurfaceAst.all_directives > `coerce `static_content `static_resource `static_content_directory `static_resource_directory ] as 'a) SurfaceAst.code ->
  (Ident.t, 'a) SurfaceAst.code

(* returns a map from files that will be included statically to their date of last modification, or None if the file couldn't be read *)
val pass_analyse_static_include_deps :
  options:OpaEnv.opa_options ->
  ('ident, [< SurfaceAst.all_directives > `static_content `static_resource `static_content_directory `static_resource_directory ] as 'a) SurfaceAst.code ->
  float option StringMap.t
