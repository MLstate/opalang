(*
    Copyright © 2011-2013 MLstate

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


(**
  Pass for Dropbox database backend

  @author Quentin Bourgerie
  @author Cedric Soulas
*)

(* shorthands *)
module Q = QmlAst
module C = QmlAstCons.TypedExpr

module Api = struct
  let some = Opacapi.some

  module Types = Opacapi.Types

  module Db = Opacapi.DbDropbox
end

module DbAst = QmlAst.Db
module DbSchema = QmlDbGen.Schema
module List = BaseList
module Format = BaseFormat

let label = Annot.nolabel "DropBoxCodeGeneration"
let backend = `dropbox

module Generator = struct

  (** TODO - ITS A COMMON UTILS *)
  let dot_select field update =
    match update with
    | DbAst.SFlds fields ->
        List.find_map
          (fun (fields, u) -> match fields with
           | [`string t] when t = field ->
               Some (if u = DbAst.SNil then DbAst.SStar else u)
           | [_t] -> None
           | _t::_q -> assert false
           | _ -> None)
          fields
    | DbAst.SNil -> Some DbAst.SNil
    | DbAst.SStar -> Some DbAst.SStar
    | _ -> None

  let dot_update gamma annotmap field update =
    match update with
    | DbAst.UExpr e ->
        let annotmap, e = C.dot gamma annotmap e field in
        Some (annotmap, DbAst.UExpr e)
    | DbAst.UFlds fields ->
        List.find_map
          (fun (fields, u) -> match fields with
           | `string t::q when t = field -> Some (annotmap, DbAst.UFlds [q, u])
           | _ -> None)
          fields
    | _ -> None

  (** ****************************************************)



  let ty_database = Q.TypeVar (QmlTypeVars.TypeVar.next ())

  let open_database gamma annotmap name =
    let annotmap, name = C.string annotmap name in
    let annotmap, open_ = OpaMapToIdent.typed_val ~label Api.Db.open_ annotmap gamma in
    let annotmap, open_ = C.apply gamma annotmap open_ [name] in
    (annotmap, open_)

  let node_to_dbexpr _gamma annotmap node =
    C.ident annotmap node.DbSchema.database.DbSchema.ident ty_database

  let expr_to_field gamma annotmap expr
      ?(ty=QmlAnnotMap.find_ty (Annot.annot (QmlAst.Label.expr expr)) annotmap)
      ()
      =
    let annotmap, expr_to_field =
      OpaMapToIdent.typed_val ~label ~ty:[ty] Opacapi.DbDropbox.expr_to_field annotmap gamma
    in
    C.apply gamma annotmap expr_to_field [expr]

  let expr_of_strpath gamma annotmap strpath =
    let annotmap, path = List.fold_left
      (fun (annotmap, acc) key ->
         let annotmap, e = C.string annotmap key in
         annotmap, e::acc
      ) (annotmap, []) strpath
    in
    C.rev_list (annotmap, gamma) path

  let expr_of_strexprpath gamma annotmap path =
    let annotmap, path = List.fold_left
      (fun (annotmap, acc) key ->
         match key with
         | `string key ->
             let annotmap, e = C.string annotmap key in
             annotmap, e::acc
         | `expr e ->
             let annotmap, e = expr_to_field gamma annotmap e () in
             annotmap, e::acc
      ) (annotmap, []) path
    in
    C.rev_list (annotmap, gamma) path

  let update_to_expr gamma annotmap ty = function
    | DbAst.UExpr expr ->
        let (annotmap, serialize) =
          OpaMapToIdent.typed_val ~label ~ty:[ty] Opacapi.OpaSerialize.serialize annotmap gamma
        in C.apply gamma annotmap serialize [expr]
    | u ->
        OManager.error "This kind of update is not yet implemented by dropbox backend : %a"
          (QmlAst.Db.pp_update QmlPrint.pp#expr) u

  let get_node ~context gamma schema path =
    try
      DbSchema.get_node gamma schema path
    with Base.NotImplemented s ->
      QmlError.error context
        "Can't generates mongo access because : %s is not yet implemented"
        s

  let rec compose_path ~context gamma annotmap schema dbname kind subs select =
    let subkind =
      match kind with
      | DbAst.Update _
      | DbAst.Ref -> DbAst.Ref
      | _ -> DbAst.Valpath
    in
    let subs =
      List.filter_map
        (function (field, sub) ->
           match dot_select field select with
           | Some select -> Some (field, select, sub)
           | None -> None
        ) subs
    in
    let annotmap, elements =
      C.list_map
        (fun annotmap (field, select, sub) ->
           let (annotmap, path) =
             string_path ~context gamma annotmap schema (subkind, dbname::sub) select
           in
           let (annotmap, field) = C.string annotmap field in
           C.opa_tuple_2 (annotmap, gamma) (field, path)
        ) (annotmap, gamma) subs
    in
    let builder, pathty =
      match subkind with
      | DbAst.Ref -> Api.Db.build_rpath_compose, Api.Types.DbDropbox.ref_path
      | DbAst.Valpath -> Api.Db.build_vpath_compose, Api.Types.DbDropbox.val_path
      | _ -> assert false
    in
    (annotmap, [elements], builder, pathty)

  and gen_string_path ~context expr_of_path gamma annotmap schema node (kind, strpath) select =
    match node.DbSchema.kind with
    | DbSchema.SetAccess (setkind, path, query, _todo) ->
        dbset_path ~context gamma annotmap schema (kind, path) setkind node query [] select
    | _ ->
        let dataty = node.DbSchema.ty in
        let dbname = node.DbSchema.database.DbSchema.name in
        match kind with
        | DbAst.Update (u, o) ->
            begin match node.DbSchema.kind with
            | DbSchema.Plain ->
                let annotmap, path = expr_of_path gamma annotmap strpath in
                let annotmap, uexpr = update_to_expr gamma annotmap dataty u in
                let annotmap, database = node_to_dbexpr gamma annotmap node in
                let annotmap, update =
                  OpaMapToIdent.typed_val ~label Api.Db.update_path annotmap gamma in
                C.apply gamma annotmap update [database; path; uexpr]
            | DbSchema.Partial (sum, rpath, partial) ->
                if sum then QmlError.serror context "Update inside a sum path is forbidden";
                let annotmap, path =
                  let rpath = List.map (fun s -> `string s ) rpath in
                  expr_of_path gamma annotmap (`string dbname::rpath) in
                let partial = List.map (fun s -> `string s) partial in
                let annotmap, uexpr =
                  update_to_expr gamma annotmap dataty (DbAst.UFlds [partial, u])
                in
                let annotmap, database = node_to_dbexpr gamma annotmap node in
                let annotmap, update =
                  OpaMapToIdent.typed_val ~label Api.Db.update_path annotmap gamma in
                C.apply gamma annotmap update [database; path; uexpr]
            | DbSchema.Compose c ->
                (* TODO - Warning non atocmic update ??*)
                let annotmap, sub =
                  List.fold_left_filter_map
                    (fun annotmap (field, subpath) ->
                       match dot_update gamma annotmap field u with
                       | Some (annotmap, subu) ->
                           begin match dot_select field select with
                           | Some select ->
                               let annotmap, sube =
                                 string_path ~context gamma annotmap schema
                                   (DbAst.Update (subu, o), dbname::subpath)
                                   select
                               in (annotmap, Some (Ident.next "_", sube))
                           | None -> annotmap, None
                           end
                       | None -> annotmap, None
                    ) annotmap c
                in
                let annotmap, unit = C.unit annotmap in
                C.letin annotmap sub unit
            | _ -> assert false
            end
        | _ ->
            (* All other kind access are factorized bellow *)
            let annotmap, path = expr_of_path gamma annotmap strpath in
            let (annotmap, args, builder, pathty) =
              match node.DbSchema.kind with
              | DbSchema.Compose subs ->
                  compose_path ~context gamma annotmap schema dbname kind subs select

              | DbSchema.Partial (sum, rpath, partial) ->
                  let annotmap, partial = C.list_map
                    (fun annotmap fragment -> C.string annotmap fragment)
                    (annotmap, gamma) partial
                  in let annotmap, rpath = C.list_map
                    (fun annotmap fragment -> C.string annotmap fragment)
                    (annotmap, gamma) (dbname::rpath)
                  in begin match kind with
                  | DbAst.Ref ->
                      if sum then QmlError.serror context "Update inside a sum path is forbidden";
                      annotmap, [rpath; partial], Api.Db.build_rpath_sub, Api.Types.DbDropbox.ref_path
                  | _ ->
                      annotmap, [rpath; partial], Api.Db.build_vpath_sub, Api.Types.DbDropbox.val_path
                  end
              | DbSchema.Plain ->
                  (match kind with
                   | DbAst.Update _
                   | DbAst.Ref -> (annotmap, [], Api.Db.build_rpath, Api.Types.DbDropbox.ref_path)
                   | _ -> (annotmap, [], Api.Db.build_vpath, Api.Types.DbDropbox.val_path))
              | _ -> assert false
            in
            let (annotmap, build) =
              OpaMapToIdent.typed_val ~label ~ty:[dataty] builder annotmap gamma in
            let (annotmap, database) = node_to_dbexpr gamma annotmap node in
            let ty = OpaMapToIdent.specialized_typ ~ty:[dataty] pathty gamma in
            let (annotmap, default) = node.DbSchema.default ~select annotmap in
            let (annotmap, path) = C.apply ~ty gamma annotmap build
              ([database; path; default] @ args) in
            let again =
              match kind with
              | DbAst.Default -> Some Api.Db.read
              | DbAst.Option -> Some Api.Db.option
              | _ -> None
            in
            let (annotmap, path) =
              match again with
              | None -> (annotmap, path)
              | Some again ->
                  let (annotmap, again) =
                    OpaMapToIdent.typed_val ~label ~ty:[QmlAstCons.Type.next_var (); dataty]
                      again annotmap gamma in
                  C.apply gamma annotmap again [path]
            in annotmap, path

  and string_path ~context gamma annotmap schema (kind, strpath) select =
    let node =
      let strpath = List.map (fun k -> DbAst.FldKey k) strpath in
      get_node ~context gamma schema strpath in
    let strpath = List.map (fun s -> `string s) strpath in
    gen_string_path ~context expr_of_strexprpath gamma annotmap schema node (kind, strpath) select

  and dbset_path ~context gamma annotmap schema (kind, path) setkind node query0 embed select0 =
    match setkind with
    | DbSchema.DbSet _ -> QmlError.error context "DbSet are not yet implemented by dropbox backend"
    | DbSchema.Map _ ->
        let path = List.rev_map (fun s -> `string s) path in
        let path =
          match query0 with
          | None -> path
          | Some (_, (DbAst.QEq key, _)) -> `expr key :: path
          | _ -> QmlError.error context
              "This kind of query is not yet implemented by dropbox backend"
        in
        let path =
          let rec aux path embed = match embed with
            | [] -> List.rev path
            | t::q -> match t with
              | DbAst.FldKey k -> aux ((`string k)::path) q
              | DbAst.ExprKey e
              | DbAst.Query (DbAst.QEq e, _) -> aux ((`expr e)::path) q
              | _ -> QmlError.error context
                  "This kind of sub path is not yet implemented by dropbox backend"
          in aux path embed
        in
        let node = { node with DbSchema.kind = DbSchema.Plain } in
        gen_string_path ~context expr_of_strexprpath gamma annotmap schema node (kind, path) select0


  let path ~context gamma annotmap schema (label, dbpath, kind, select) =
    let node = get_node ~context gamma schema dbpath in
    match node.DbSchema.database.DbSchema.options.DbAst.backend with
    | `dropbox -> (
        let annotmap, mongopath =
          match node.DbSchema.kind with
          | DbSchema.SetAccess (setkind, path, query, embed) ->
              dbset_path ~context gamma annotmap schema (kind, path)
                setkind node query (Option.default [] embed) select
          | _ ->
              let strpath = List.map
                (function
                 | DbAst.FldKey k -> k
                 | _ -> assert false
                ) dbpath in
              string_path ~context gamma annotmap schema (kind, strpath) select
        in
        match kind with
        | DbAst.Ref | DbAst.Valpath ->
            let annotmap, p2p =
              OpaMapToIdent.typed_val ~label
                ~ty:[QmlAstCons.Type.next_var (); node.DbSchema.ty]
                Api.Db.path_to_path annotmap gamma in
            C.apply gamma annotmap p2p [mongopath]
        | _ -> annotmap, mongopath
      )
    | _ -> annotmap, Q.Path (label, dbpath, kind, select)

end

let init_database gamma annotmap schema =
  List.fold_left
    (fun (annotmap, newvals) database ->
       match database.DbSchema.options.DbAst.backend with
       | `dropbox ->
         let ident = database.DbSchema.ident in
         let name = database.DbSchema.name in
         let (annotmap, open_) = Generator.open_database gamma annotmap name in
         (annotmap, (Q.NewVal (label, [ident, open_]))::newvals)
       | _ -> (annotmap, newvals)
    )
    (annotmap, []) (DbSchema.get_db_declaration schema)

let clean_code _gamma annotmap _schema code =
  List.fold_left_filter_map
    (fun annotmap -> function
       | Q.Database _ -> annotmap, None
       | Q.NewDbValue (_label, DbAst.Db_TypeDecl (p, _ty)) ->
           begin match p with
           | (DbAst.Decl_fld _)::p ->
               let rec aux rpath p =
                 match p with
                 | (DbAst.Decl_set _lidx)::[] -> assert false
                 | (DbAst.Decl_set _lidx)::_ -> assert false
                 | (DbAst.Decl_fld str)::p -> aux (str::rpath) p
                 | [] -> annotmap, None
                 | _ -> assert false
               in aux [] p
           | _ -> annotmap, None
           end
       | Q.NewDbValue _ -> annotmap, None
       | elt -> annotmap, Some elt)
    annotmap code

let process_path gamma annotmap schema code =
  let fmap tra annotmap = function
    | Q.Path (label, path, kind, select) as expr ->
        let context = QmlError.Context.annoted_expr annotmap expr in
        let annotmap, result =
          Generator.path ~context gamma annotmap schema (label, path, kind, select) in
        tra annotmap result
    | e -> tra annotmap e
  in
  QmlAstWalk.CodeExpr.fold_map
    (fun annotmap expr ->
       let annotmap, expr = QmlAstWalk.Expr.traverse_foldmap fmap annotmap expr in
       fmap (fun a e -> a,e) annotmap expr)
    annotmap code


let process_code ~stdlib_gamma gamma annotmap schema code =
  match ObjectFiles.compilation_mode () with
  | `init -> (annotmap, code)
  | _ ->
      let gamma = QmlTypes.Env.unsafe_append stdlib_gamma gamma in
      let (annotmap, code) = clean_code gamma annotmap schema code in
      let (annotmap, code) =
        let (annotmap, vals) = init_database stdlib_gamma annotmap schema in
        (annotmap, vals@code)
      in
      let (annotmap, code) = process_path gamma annotmap schema code in
      (annotmap, code)

