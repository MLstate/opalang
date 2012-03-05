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

  let pass = "ResolveRemoteCalls"
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
  ObjectFiles.import_package package label;
  ObjectFiles.add_compiler_package package

let process_code ~stdlib code =
  if stdlib then
    let engines =
      match QmlDbGen.Args.get_engine () with
      | None -> []
      | Some engine -> [engine]
    in
    let engines =
      R.fold_with_name ~deep:true
        (fun _ acc t -> t@acc)
        engines
    in
    let engines = List.fold_left
      (fun acc -> function
       | (SA.Database (_, _, opt), _) -> opt.DbAst.backend :: acc
       | _ -> acc
      ) engines code
    in
    let engines = List.uniq_unsorted engines in
    r := engines;
    List.iter import_packages engines;
    if ObjectFiles.compilation_mode() = `compilation then R.save engines

let get_engines () = !r

