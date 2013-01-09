(*
    Copyright © 2011-2013 MLstate

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

module Q = QmlAst
module C = QmlAstCons.TypedExpr

module DbAst = QmlAst.Db
module DbSchema = QmlDbGen.Schema

module Api =
struct

  module Db = Opacapi.DbPostgres

end

let label = Annot.nolabel "PostgresCodeGeneration"

module Generator =
struct

  let open_database gamma annotmap name =
    let annotmap, name = C.string annotmap name in
    let annotmap, open_ = OpaMapToIdent.typed_val ~label Api.Db.open_ annotmap gamma in
    C.apply gamma annotmap open_ [name]

end

let init_database gamma annotmap schema =
  List.fold_left
    (fun (annotmap, newvals) database ->
       if database.DbSchema.options.DbAst.backend = `postgres
         && database.DbSchema.package = ObjectFiles.get_current_package_name () then
           let ident = database.DbSchema.ident in
           let name = database.DbSchema.name in
           let (annotmap, open_) = Generator.open_database gamma annotmap name in
           (annotmap, (Q.NewVal (label, [ident, open_]))::newvals)
       else (annotmap, newvals)
    )
    (annotmap, []) (DbSchema.get_db_declaration schema)

let process_code ~stdlib_gamma gamma annotmap schema code =
  ignore(stdlib_gamma, gamma, annotmap, schema, code);
  match ObjectFiles.compilation_mode () with
  | `init -> (annotmap, code)
  | _ ->
      let (annotmap, vals) = init_database stdlib_gamma annotmap schema in
      (annotmap, vals@code)

