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
(* rebel open *)
open SurfaceAst
open SurfaceAstHelper
open OpaEnv
open SurfaceAstPassesTypes

(* refactoring in progress *)

(* alias *)
module C = SurfaceAstCons.ExprIdentCons
module CS = SurfaceAstCons.StringCons



let pass_load_objects ~options (special_parsed_files, user_parsed_files) k =
  let extract_package_decl = function
    | (Package ((`declaration | `import) as kind, name), label) ->
        Some (kind, name, label.QmlLoc.pos)
    | _ -> None in
  let extract_current_package_name = function
    | Package (`declaration, name), _label -> Some(name)
    | _ -> None in
  let package_name code = Base.List.find_map extract_current_package_name code in
  let label s = {QmlLoc.pos=FilePos.nopos ("i18n, import "^s); QmlLoc.notes=SurfaceAstCons.Fresh.id()} in
  let map { parsedFile_lcode = code; parsedFile_filename = name; parsedFile_content = content } =
    let exists = ObjectFiles.exists_package ~extrapath:options.OpaEnv.extrapath in
    let package = package_name code in
    (* adding internationalisation import *)
    let i18n_to_import = I18nAndComputedString.may_import_package ?package ~exists ~options in
    let imports =  BaseList.map (fun s -> Package(`import, s), label s) i18n_to_import in
    (name,content,imports @ code)
  in
  let main_file (entry : Qml2jsOptions.extra_lib) =
    match entry with
    | `client (file, _) -> file
    | `server (nodejs_module, _) -> Filename.concat nodejs_module "main.js"
  in
  ObjectFiles.set_relative_stdlib
    (Printf.sprintf "stdlib.%s" (OpaEnv.string_of_available_back_end options.OpaEnv.back_end));
  ObjectFiles.set_extrapaths ~no_stdlib:(not options.OpaEnv.stdlib) options.OpaEnv.extrapath;
  ObjectFiles.load
    ~parallelism:options.parallelism
    ~extrajs:(
      (*
        TODO(if needed): we can patch ObjectFiles for passing the conf as well
      *)
      BaseList.map main_file options.OpaEnv.extrajs
    )
    ~no_stdlib:(not options.OpaEnv.stdlib)
    extract_package_decl
    (SurfaceAstStaticInclude.pass_analyse_static_include_deps ~options)
    (BaseList.map map special_parsed_files @ BaseList.map map user_parsed_files)
    k

let pass_parser_generation
    ~options:_ (env : (string,parsing_directive) env_both_lcodes)
    : (string,renaming_directive) env_both_lcodes =
  let rewrite code =
    (* map_down because the directive `xml_parser may contain parsers *)
    SurfaceAstTraversal.ExprTraverse.Heterogeneous.lift_map_down_to_fixpoint
      (function (* the filter function, to make the typer happy *)
         | #renaming_directive as x -> x
         | #parsing_directive -> assert false)
      (function (* the actual mapping function *)
         | (Directive (`parser_ e, [], _), label) ->
             SurfaceAstCons.with_label' label SurfaceAstTrx.translate_rule e
         | (Directive (`xml_parser xml_parser, [], _), label) as e ->
             SurfaceAstCons.with_label' label (SurfaceAstXmlPattern.process_parser e) xml_parser
         | e -> e) code in
  { env with
      lcodeNotUser = rewrite env.lcodeNotUser;
      lcodeUser = rewrite env.lcodeUser;
  }

(**
   Check for duplication of idents and some more.

   This pass
   - checks for level-0 identifiers with two definitions -- having two definitions of the same level-0 identifier
   is either a warning or an error, depending on options
   - renames all identifiers to make them unique

   If the option [--warn-error duplicateL0] is set, having two level-0 identifiers with the same name is cause for
   an error. Otherwise, it's an warning.
*)

let pass_check_duplication
    compiler_inserted_names compiler_inserted_types ~options:_
    (env : (string,renaming_directive) env_both_lcodes)
    : (Ident.t,dependency_directive) env_both_lcodes =
  let envs = SurfaceAstRenaming.init_env compiler_inserted_names compiler_inserted_types in
  let envs = SurfaceAstRenaming.load_env envs in
  let envs, lcodeNotUser = SurfaceAstRenaming.code envs env.lcodeNotUser in
  let envs, lcodeUser = SurfaceAstRenaming.code envs env.lcodeUser in
  SurfaceAstRenaming.save_env envs;
  {
    env with
      lcodeNotUser ;
      lcodeUser ;
      lcodeTypeRenaming = SurfaceAstRenaming.extract_types_in_scope envs;
      exported_values_idents = SurfaceAstRenaming.get_exported_values envs
  }


let pass_tuple_types ~options:_  lcode =
  SurfaceAstCons.with_builtin_position
    (fun () ->
       let intmap = SurfaceAstRenaming.get_tuple_int_map () in
       let typedefs =
         IntMap.fold
           (fun n ident acc ->
              let name = Printf.sprintf "tuple_%d" n in
              let var d = Printf.sprintf "%s_%d" name d in
              let vars = BaseList.init n (fun n -> Ident.next (var n)) in
              C.T.typedef
                SurfaceAst.TDV_public ident ~tyvs: (BaseList.map flatvar vars)
                (C.T.tuple (BaseList.map C.T.var vars))
              :: acc)
           intmap [] in
       let defs = BaseList.map C.C.newtype typedefs in
       defs @ lcode)
