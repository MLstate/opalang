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
(* depends *)
module List = BaseList
module Format = BaseFormat

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
  let pp f l =
    let aux = function
      |`db3 -> "db3"
      |`mongo -> "mongo"
    in
    List.iter (fun x -> Format.fprintf f "%s;" (aux x)) l
end

module R = struct
  include ObjectFiles.Make(S)
  let save backends = save backends
end

let label = FilePos.nopos "Built in pass DbEngineImportation"

let r = ref []

let import_packages engines =
  let packages =
    List.map
      (function
       | `db3   -> "stdlib.database.db3"
       | `mongo -> "stdlib.database.mongo")
      engines
  in
  ObjectFiles.add_compiler_packages packages

let process_code ~stdlib code =
  if stdlib then
    let default_engine = QmlDbGen.get_engine () in
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
       | (SA.NewDbDef
            ( DbAst.Db_TypeDecl ((DbAst.Decl_fld p::_), _)
            | DbAst.Db_Default ((DbAst.Decl_fld p::_), _)
            | DbAst.Db_Constraint ((DbAst.Decl_fld p::_), _)), _) ->
           StringSet.add p padecl, dbdecl, engines
       | _ -> padecl, dbdecl, engines
      ) (StringSet.empty, Some StringSet.empty, engines) code
    in
    let engines =
      match dbdecl with
      | None ->
          (* Case if default database *)
          if StringSet.is_empty padecl then engines
          else  default_engine :: engines
      | Some dbdecl ->
          if StringSet.is_empty (StringSet.diff padecl dbdecl) then engines
          else default_engine :: engines
            (* Some path are not included in a database,
               load default engine. *)
    in
    let engines = List.uniq_unsorted engines in
    r := engines

let finalize ~stdlib =
  if stdlib then (
    import_packages !r;
    match ObjectFiles.compilation_mode() with
    | `compilation ->
        ObjectFiles.resave ();
        R.save !r
    | _ -> ()
  )

let get_engines () = !r

