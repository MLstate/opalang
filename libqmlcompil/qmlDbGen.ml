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
(*
    @author Louis Gesbert
**)

(* Exported module with reduced interface *)

(* Because of a lack of functionality in OCaml module handling, we can't use include *)

module Sch = Schema_private
module Schema = struct
  type t = Sch.meta_schema
  let mapi = Sch.mapi
  let initial = Sch.initial
  let is_empty = Sch.is_empty_or_unused
  let register_path = Sch.register_path
  let register_default = Sch.register_default
  let register_db_declaration = Sch.register_db_declaration
  let register_new_db_value = Sch.register_new_db_value
(*   let get_type_of_path = get_type_of_path *)
  (* let preprocess_path = preprocess_path *)
  let preprocess_paths_expr = Sch.preprocess_paths_expr
  let preprocess_paths_code_elt = Sch.preprocess_paths_code_elt
  let preprocess_paths_ast = Sch.preprocess_paths_ast
  let finalize = Sch.finalize
  let of_package = Sch.of_package
  let merge = Sch.merge
  let map_types = Sch.map_types
  let map_expr = Sch.map_expr
  let fold_expr = Sch.fold_expr
  let foldmap_expr = Sch.foldmap_expr
  let from_gml s =
    StringListMap.singleton []
      ({ Sch.ident = None;
         Sch.path_aliases = [];
         Sch.options = [];
         Sch.schema = Schema_io.from_gml_string s;
         Sch.virtual_path = Sch.PathMap.empty;
       })
  let to_dot t chan =
    StringListMap.iter
      (fun key db_def ->
         output_string chan (String.concat "/" key);
         output_char chan '\n';
         Schema_io.to_dot db_def.Sch.schema chan)
      t

  let find_db_def t db_ident_opt =
    if StringListMap.size t = 1 && db_ident_opt = None
    then StringListMap.min t
    else
      StringListMap.min (* may raise Not_found *)
        (StringListMap.filter_val
           (fun db_def -> db_ident_opt = Option.map Ident.original_name db_def.Sch.ident)
           t)
  let db_to_dot t db_ident_opt chan =
    let _, db_def = find_db_def t db_ident_opt in
    Schema_io.to_dot db_def.Sch.schema chan
  let db_to_gml t db_ident_opt chan =
    let _, db_def = find_db_def t db_ident_opt in
    Schema_io.to_gml db_def.Sch.schema chan

  module HacksForPositions = Sch.HacksForPositions
end

module type S = sig include DbGenByPass.S end

type dbinfo = DbGen_private.dbinfo

let merge_dbinfo = DbGen_private.merge_dbinfo

module DbGen ( Arg : DbGenByPass.S ) = struct

  module Access = DbGen_private.DatabaseAccess (Arg)
  let initialize = Access.initialize
  let replace_path_exprs = Access.replace_path_exprs
  let replace_path_code_elt = Access.replace_path_code_elt
  let replace_path_ast = Access.replace_path_ast
end

module DbGenByPass = DbGenByPass

let warning_set =
  WarningClass.Set.create_from_list [
    WarningClass.dbgen;
    WarningClass.dbgen_schema;
  ]
