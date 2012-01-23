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

(* shorthands *)
module S = SurfaceAst
module Db = QmlDbGen

let builtinpos = FilePos.nopos "Built in pass DbEngineImportation"

let process_code code =
  if List.exists
    (function
       | (S.Database _), _ | (S.NewDbDef _), _ -> true
       | _ -> false)
    code then (
      let package = match QmlDbGen.Args.get_engine () with
      | Db.Db3   -> "stdlib.database.db3"
      | Db.Mongo -> "stdlib.database.mongo"
      in
      ObjectFiles.import_package package builtinpos;
      ObjectFiles.add_compiler_package package;
    )
  ;
  code
