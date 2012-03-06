(*
    Copyright © 2011, 2012 MLstate

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
(* depends *)
module List = BaseList

(* shorthands *)
module SA = SurfaceAst
module Db = QmlDbGen
module DbAst = QmlAst.Db

(* ******************************************************************)
(* Separated compilation ********************************************)
(* ******************************************************************)
module S =
struct
  (* Original ident * stub ident, stub type, expanded stub *)
  type t = DbAst.engine list

  let pass = "DbEngineImporation"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R = struct
  include ObjectFiles.Make(S)
  let save backends = save backends
end

let label = FilePos.nopos "Built in pass DbEngineImportation"

let r = ref []

let import_packages engine =
  let package = match engine with
    | `db3   -> "stdlib.database.db3"
    | `mongo -> "stdlib.database.mongo"
  in
  ObjectFiles.add_compiler_package package

let process_code ~stdlib code =
  if stdlib then
    let engines =
      match QmlDbGen.Args.get_engine () with
      | None -> []
      | Some engine -> [engine]
    in
    let engines =
      R.fold_with_name ~optional:true ~deep:true
        (fun _name acc t -> t@acc)
        engines
    in
    let engines = engines @ !r in
    let padecl, dbdecl, engines = List.fold_left
      (fun (padecl, dbdecl, engines) -> function
       | (SA.Database (_, id::_, opt), _) ->
           padecl,
           Option.map (fun dbdecl -> StringSet.add id dbdecl) dbdecl,
           (opt.DbAst.backend :: engines)
       | (SA.Database (_, _, opt), _) ->
           padecl, None, opt.DbAst.backend :: engines
       | (SA.NewDbDef (DbAst.Db_TypeDecl ((DbAst.Decl_fld p::_), _)), _) ->
           StringSet.add p padecl, dbdecl, engines
       | _ -> padecl, dbdecl, engines
      ) (StringSet.empty, Some StringSet.empty, engines) code
    in
    let engines =
      match dbdecl with
      | None -> engines (* Case if default database *)
      | Some dbdecl ->
          if StringSet.is_empty (StringSet.diff padecl dbdecl) then engines
          else `db3 :: engines (* Some path are not included in a database,
                                  load default engine. *)
    in
    let engines = List.uniq_unsorted engines in
    r := engines

let finalize ~stdlib =
  if stdlib then (
    List.iter import_packages !r;
    match ObjectFiles.compilation_mode() with
    | `compilation -> R.save !r
    | _ -> ()
  )

let get_engines () = !r

