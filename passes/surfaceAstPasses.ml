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
(* rebel open *)
open SurfaceAst
open SurfaceAstHelper
open OpaEnv

(* refactoring in progress *)

(* alias *)
module C = SurfaceAstCons.ExprIdentCons
module CS = SurfaceAstCons.StringCons

type options = OpaEnv.opa_options

type ('a,'b) env_both_lcodes = {
  lcodeNotUser : ('a,'b) code ;
  lcodeUser : ('a,'b) code ;
  lcodeTypeRenaming : (Ident.t * FilePos.pos) StringMap.t ;
  exported_values_idents : IdentSet.t ;
  env_bsl : BslLib.env_bsl ;
}

type 'a parsed_file = {
  parsedFile_filename : string ;
  parsedFile_lcode : (string,'a) code ;
  parsedFile_content : string ;
}

let pass_load_objects ~options (special_parsed_files, user_parsed_files) k =
  let extract_package_decl = function
    | (Package ((`declaration | `import) as kind, name), label) ->
        Some (kind, name, label.QmlLoc.pos)
    | _ -> None in
  let map { parsedFile_lcode = code; parsedFile_filename = name; parsedFile_content = content } =
    (name, content, code) in
  ObjectFiles.set_extrapaths ~no_stdlib:(not options.OpaEnv.stdlib) options.OpaEnv.extrapath;
  ObjectFiles.load
    ~extrajs:(
      (*
        TODO(if needed): we can patch ObjectFiles for passing the conf as well
      *)
      List.map fst options.OpaEnv.extrajs
    )
    ~no_stdlib:(not options.OpaEnv.stdlib)
    extract_package_decl
    (SurfaceAstStaticInclude.pass_analyse_static_include_deps ~options)
    (List.map map special_parsed_files @ List.map map user_parsed_files)
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
              let vars = List.init n (fun n -> Ident.next (var n)) in
              C.T.typedef
                SurfaceAst.TDV_public ident ~tyvs: (List.map flatvar vars)
                (C.T.tuple (List.map C.T.var vars))
              :: acc)
           intmap [] in
       let defs = List.map C.C.newtype typedefs in
       defs @ lcode)
