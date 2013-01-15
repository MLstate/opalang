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

module BSLDbGen = QmlDbGen.DbGenByPass.BSLDbGenAlphaOpa
module DbGen = QmlDbGen.DbGen ( BSLDbGen )

let clean_code gamma schema code =
  List.filter
    (function
     | Q.Database (_, _, _, opt) when opt.Q.Db.backend = `db3 -> false
     | Q.NewDbValue (_, (
                       Q.Db.Db_TypeDecl   (Q.Db.Decl_fld prefix::_, _)
                     | Q.Db.Db_Default    (Q.Db.Decl_fld prefix::_, _)
                     | Q.Db.Db_Alias      (Q.Db.Decl_fld prefix::_, _)
                     | Q.Db.Db_Constraint (Q.Db.Decl_fld prefix::_, _)
                     | Q.Db.Db_Virtual    (Q.Db.Decl_fld prefix::_, _))
                    ) when let open QmlDbGen.Schema in
       (get_node gamma schema [Q.Db.FldKey prefix]).database.options.Q.Db.backend = `db3
           -> false
     | _ -> true
    ) code

let process_code gamma annotmap schema code alpha_opt =
  (* 1°: Remove db3 database declaration and db3 path declaration *)
  let code = clean_code gamma schema code in

  (* 2°: generate database accessors from the schema *)
  let dbinfo, gamma, annotmap_opt, dbgen_init_code, dbgen_accessors_code =
    DbGen.initialize
      ~annotmap:(Some annotmap)
      ~valinitial_env:alpha_opt
      gamma
      schema
  in

  (* 3°: adding annotmap of dbGen code *)
  let annotmap =
    Option.default_map annotmap
      (fun am -> QmlAnnotMap.merge (QmlTypes.process_typenames_annotmap ~gamma:gamma am)
         annotmap) annotmap_opt
  in

  (* 4°: inserting dbgen code *)
  let code =
    dbgen_init_code
    @ dbgen_accessors_code
    @ code
  in

  dbinfo, gamma, annotmap, code
