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
  This module contains the compilation passes that are executed
  in SurfaceAst (the very front end of the compiler)
  except the parsing pass (that is available in OpaParser)
 *)

open SurfaceAstPassesTypes
open SurfaceAst

(**
   This pass looks for Package declaration of each file, and loads these
   dependencies
   The node Package is removed from the code and never appears afterwards
*)
val pass_load_objects:
  options:options ->
  (([< SurfaceAst.all_directives > `static_content `static_content_directory `static_resource `static_resource_directory ] as 'b) parsed_file list * 'b parsed_file list) ->
  ((string, 'b) SurfaceAst.code_elt ObjectFiles.parsed_code -> unit) ->
  unit

(**
   This pass goes through the whole to transform the syntactic constructions
   parsers and xml_parser into real opa code
   Parser generation happens in trx_convert.ml
   Xml_parser generation happens in xml_pattern.ml

   Assumptions: no alpha renaming yet
                no directives other than `xml_parser contains an expression in its variant

   Directives removed : [\[ `xml_parser _ \]]

   Directives added : [\[\]]
 *)
val pass_parser_generation :
  options:options ->
  (string,parsing_directive) env_both_lcodes ->
  (string, renaming_directive) env_both_lcodes


(**
   This pass alpha renames the whole code
   It takes care of renaming types, type variables, and identifiers

   Assumptions:
   - every identifier that is allowed to be unbound should be given (first argument)
     for example, git_version, or release are defined afterwards by the compiler and must be given
     in the list
   - every type identifier that is allowed to be unbound should given as
     a second argument
     tuples are a special cases: tuple_%d, where d >= 0 is automatically defined once it is used
     afterwards, a mapping from integers to the corresponding tuple identifier can be retrieved
   - no directive bind variables in their subexpression, or somehow change the scope
     there is of course a few exceptions: SurfaceAst.alpha_renaming_directive
   - no variant of a directive contains expressions, patterns, types or anything that should be renamed

   Directives removed : [SurfaceAst.alpha_renaming_directive]

   Directives added : [\[ `local \]]
 *)
val pass_check_duplication :
  string list ->
  string list ->
  options:options ->
  (string, renaming_directive) env_both_lcodes ->
  (Ident.t, dependency_directive) env_both_lcodes


(**
   This pass insert the definitions of tuple_%d for every use that was recorded
   by the alpha renaming pass

   Assumption: alpha renaming has been performed already

   Directives removed : [\[\]]

   Directives added : [\[\]]
 *)
val pass_tuple_types :
  options:options ->
  (Ident.t, 'b) code ->
  (Ident.t, 'b) code
