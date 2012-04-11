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

(* shorthands *)
module Q = QmlAst
module C = QmlAstCons.TypedExpr

module Api = struct
  let some = Opacapi.some

  module Types = Opacapi.Types

  module Db = Opacapi.DbMongo

  module DbSet = Opacapi.DbSet
end

module DbAst = QmlAst.Db
module DbSchema = QmlDbGen.Schema
module List = BaseList
module Format = BaseFormat

type db_access = {
  engines : Ident.t StringMap.t
}

let label = Annot.nolabel "MongoAccessGeneration"

module Generator = struct

  let ty_is_const gamma ty =
    match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
    | Q.TypeConst _ -> true
    | _ -> false

  (* With mongo db we don't consider list as a sum *)
  let ty_is_sum gamma ty =
    match ty with
    | Q.TypeName ([_], name) when Q.TypeIdent.to_string name = "list" -> false
    | ty ->
        match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
        | Q.TypeSum _ -> true
        | _ -> false

  let ty_database = Q.TypeVar (QmlTypeVars.TypeVar.next ())

  let open_database gamma annotmap name host port =
    let annotmap, name = C.string annotmap name in
    let annotmap, host =
      match host with
      | None -> C.none annotmap gamma
      | Some host ->
          let annotmap, host = C.string annotmap host in
          C.some annotmap gamma host
    in
    let annotmap, port =
      match port with
      | None -> C.none annotmap gamma
      | Some port ->
          let annotmap, port = C.int annotmap port in
          C.some annotmap gamma port
    in
    let annotmap, open_ = OpaMapToIdent.typed_val ~label Api.Db.open_ annotmap gamma in
    let annotmap, open_ = C.apply gamma annotmap open_ [name; host; port] in
    (annotmap, open_)

  let node_to_dbexpr _gamma annotmap node =
    C.ident annotmap node.DbSchema.database.DbSchema.ident ty_database

  let opa2doc gamma annotmap expr
      ?(ty=QmlAnnotMap.find_ty (Annot.annot (QmlAst.Label.expr expr)) annotmap)
      ()
      =
    let (annotmap, opa2doc) =
      OpaMapToIdent.typed_val ~label ~ty:[ty] Api.DbSet.opa2doc annotmap gamma
    in
    C.apply gamma annotmap opa2doc [expr]

  let add_to_document gamma annotmap name expr
      ?(ty=QmlAnnotMap.find_ty (Annot.annot (QmlAst.Label.expr expr)) annotmap)
      doc =
    let (annotmap, add_to_document) =
      OpaMapToIdent.typed_val ~label ~ty:[ty] Api.DbSet.add_to_document annotmap gamma
    in
    let (annotmap, name) = C.string annotmap name in
    let (annotmap, opaty) =
      Pass_ExplicitInstantiation.ty_to_opaty
        ~memoize:false
        ~val_:OpaMapToIdent.val_ ~side:`server
        annotmap gamma ty in
    C.apply gamma annotmap add_to_document [doc; name; expr; opaty]

  let expr_of_strpath gamma annotmap strpath =
    let annotmap, path = List.fold_left
      (fun (annotmap, acc) key ->
         let annotmap, e = C.string annotmap key in
         annotmap, e::acc
      ) (annotmap, []) strpath
    in
    C.rev_list (annotmap, gamma) path

  let rec prepare_query query =
    match query with
    | DbAst.QEq  _
    | DbAst.QGt  _
    | DbAst.QLt  _
    | DbAst.QGte _
    | DbAst.QLte _
    | DbAst.QNe  _
    | DbAst.QMod _
    | DbAst.QIn  _ -> query
    | DbAst.QFlds flds -> DbAst.QFlds (List.map (fun (f, q) -> (f, prepare_query q)) flds)
    | DbAst.QAnd (q1, q2) -> DbAst.QAnd (prepare_query q1, prepare_query q2)
    | DbAst.QOr (q1, q2)  -> DbAst.QOr  (prepare_query q1, prepare_query q2)
    | DbAst.QNot DbAst.QEq  e -> DbAst.QNe  e
    | DbAst.QNot DbAst.QGt  e -> DbAst.QLte e
    | DbAst.QNot DbAst.QLt  e -> DbAst.QGte e
    | DbAst.QNot DbAst.QGte e -> DbAst.QLt  e
    | DbAst.QNot DbAst.QLte e -> DbAst.QGt  e
    | DbAst.QNot DbAst.QNe  e -> DbAst.QEq  e
    | DbAst.QNot (DbAst.QIn _ | DbAst.QMod _) -> query
    | DbAst.QNot (DbAst.QNot query) -> query
    | DbAst.QNot (DbAst.QFlds flds) ->
        DbAst.QFlds (List.map (fun (f, q) -> (f, prepare_query (DbAst.QNot q))) flds)
    | DbAst.QNot (DbAst.QOr (q1, q2)) ->
        DbAst.QAnd (prepare_query (DbAst.QNot q1), prepare_query (DbAst.QNot q2))
    | DbAst.QNot (DbAst.QAnd (q1, q2)) ->
        DbAst.QOr (prepare_query (DbAst.QNot q1), prepare_query (DbAst.QNot q2))

  let empty_query gamma annotmap = C.list (annotmap, gamma) []

  let query_to_expr gamma annotmap query =
    match query with
    | None -> empty_query gamma annotmap
    | Some (_todo, query) ->
        let query = prepare_query query in
        let rec aux annotmap query =
          match query with
          | DbAst.QEq e ->
              let (annotmap, e) = C.shallow_copy annotmap e in
              opa2doc gamma annotmap e ()
          | DbAst.QMod _ -> assert false
          | DbAst.QGt e | DbAst.QLt e | DbAst.QGte e | DbAst.QLte e | DbAst.QNe e | DbAst.QIn e ->
              let name =
                match query with
                | DbAst.QGt _  -> "$gt"
                | DbAst.QLt _  -> "$lt"
                | DbAst.QGte _ -> "$gte"
                | DbAst.QLte _ -> "$lte"
                | DbAst.QNe _  -> "$ne"
                | DbAst.QIn _  -> "$in"
                | _ -> assert false
              in
              let annotmap, query = empty_query gamma annotmap in
              add_to_document gamma annotmap name e query
          | DbAst.QFlds flds ->
              List.fold_left
                (fun (annotmap, acc) (fld, query) ->
                   let name = BaseFormat.sprintf "%a" QmlAst.Db.pp_field fld in
                   match query with
                   | DbAst.QEq e -> add_to_document gamma annotmap name e acc
                   | _ ->
                       let annotmap, query = aux annotmap query in
                       add_to_document gamma annotmap name query acc
                )
                (empty_query gamma annotmap)
                flds
          | DbAst.QNot query ->
              let annotmap, query = aux annotmap query in
              let annotmap, empty = empty_query gamma annotmap in
              add_to_document gamma annotmap "$not" query empty
          | DbAst.QAnd (q1, q2)
          | DbAst.QOr  (q1, q2) ->
              let name =
                match query with
                | DbAst.QAnd _ -> "$and"
                | DbAst.QOr  _ -> "$or"
                | _ -> assert false
              in
              let annotmap, q1 = aux annotmap q1 in
              let annotmap, q2 = aux annotmap q2 in
              let ty =
                QmlAnnotMap.find_ty (Annot.annot (QmlAst.Label.expr q1)) annotmap
              in
              let annotmap, query = C.list ~ty (annotmap, gamma) [q1; q2] in
              let annotmap, empty = empty_query gamma annotmap in
              add_to_document gamma annotmap name query empty
        in aux annotmap query

  let select_to_expr gamma annotmap select =
    let rec aux prev_fld ((annotmap, acc) as aacc) select =
      let get_name () = BaseFormat.sprintf "%a" QmlAst.Db.pp_field prev_fld in
      match select with
      | DbAst.SFlds flds ->
          List.fold_left
            (fun aacc (fld, select) -> aux (prev_fld @ fld) aacc select)
            aacc
            flds
      | DbAst.SNil | DbAst.SStar ->
          let name = get_name () in
          let annotmap, one = C.int annotmap 1 in
          add_to_document gamma annotmap name one acc
      | DbAst.SSlice (e1, e2) ->
          let name = get_name () in
          let tyint = (Q.TypeConst Q.TyInt) in
          let limitid = Ident.next "limit" in
          let annotmap, pvar = QmlAstCons.TypedPat.var annotmap limitid tyint in
          let annotmap, ko_expr =
            let annotmap, empty = empty_query gamma annotmap in
            add_to_document gamma annotmap "$slice" e1 empty
          in
          let annotmap, ok_expr =
            let annotmap, empty = empty_query gamma annotmap in
            let annotmap, limit = C.ident annotmap limitid tyint in
            let annotmap, sklim = C.list ~ty:tyint (annotmap, gamma) [limit; e1] in
            add_to_document gamma annotmap "$slice" sklim empty
          in
          let annotmap, slice =
            QmlAstCons.TypedPat.match_option annotmap gamma e2 pvar ok_expr ko_expr
          in
          let annotmap, empty = empty_query gamma annotmap in
          add_to_document gamma annotmap name slice empty
    in aux [] (empty_query gamma annotmap) select

  let query_add_order gamma annotmap order query =
    match order with
    | None -> annotmap, query
    | Some order ->
        let annotmap, eorder =
          List.fold_left
            (fun (annotmap, acc) (fld, expr) ->
               let name = BaseFormat.sprintf "%a" QmlAst.Db.pp_field fld in
               let annotmap, expr =
                 let annotmap, up = C.int annotmap 1 in
                 let annotmap, pup =
                   let annotmap, any = QmlAstCons.TypedPat.any annotmap in
                   QmlAstCons.TypedPat.record annotmap ["up", any] in
                 let annotmap, down = C.int annotmap (-1) in
                 let annotmap, pdown =
                   let annotmap, any = QmlAstCons.TypedPat.any annotmap in
                 QmlAstCons.TypedPat.record annotmap ["down", any] in
                 C.match_ annotmap expr [(pup, up); (pdown, down)]
               in add_to_document gamma annotmap name expr acc)
            (empty_query gamma annotmap) order
        in
        let annotmap, metaquery = empty_query gamma annotmap in
        let annotmap, metaquery = add_to_document gamma annotmap "$query" query metaquery in
        add_to_document gamma annotmap "$orderby" eorder metaquery

  let update_to_expr ?(set=true) gamma annotmap = function
    | DbAst.UExpr e ->
        let annotmap, uexpr = opa2doc gamma annotmap e () in
        if set then
          let annotmap, empty = C.list (annotmap, gamma) [] in
          add_to_document gamma annotmap "$set" uexpr empty
        else annotmap, uexpr
    | update ->
        let addset = set in
        let rec collect fld (inc, set, other, annotmap) update =
          let rfld = if fld = "" then "value" else fld in
          match update with
          | DbAst.UExpr e -> (inc, (rfld, e)::set, other, annotmap)
          | DbAst.UIncr i -> ((rfld, i)::inc, set, other, annotmap)
          | DbAst.UFlds fields ->
              List.fold_left
                (fun (inc, set, other, annotmap) (f, u) ->
                   let fld =
                     let dot = match fld with | "" -> "" | _ -> "." in
                     BaseFormat.sprintf "%s%s%a" fld dot QmlAst.Db.pp_field f in
                   collect fld (inc, set, other, annotmap) u)
                (inc, set, other, annotmap) fields
          | DbAst.UAppend     e -> (inc, set, (rfld, "$push", e)::other, annotmap)
          | DbAst.UAppendAll  e -> (inc, set, (rfld, "$pushAll", e)::other, annotmap)
          | DbAst.URemove     e -> (inc, set, (rfld, "$pull", e)::other, annotmap)
          | DbAst.URemoveAll  e -> (inc, set, (rfld, "$pullAll", e)::other, annotmap)
          | DbAst.UPop   ->
              let annotmap, e = C.int annotmap (-1) in
              (inc, set, (rfld, "$pop", e)::other, annotmap)
          | DbAst.UShift ->
              let annotmap, e = C.int annotmap 1 in
              (inc, set, (rfld, "$pop", e)::other, annotmap)
        in let (inc, set, other, annotmap) = collect "" ([], [], [], annotmap) update in
        let annotmap, uexpr = C.list (annotmap, gamma) [] in
        let annotmap, uexpr =
          match inc with
          | [] -> annotmap, uexpr
          | _ ->
              let ty = Q.TypeConst Q.TyInt in
              let rec aux ((annotmap, doc) as acc) inc =
                match inc with
                | [] -> acc
                | (field, value)::q ->
                    let (annotmap, value) = C.int annotmap value in
                    aux (add_to_document gamma annotmap field value ~ty doc) q
              in
              let annotmap, iexpr = aux (C.list (annotmap, gamma) []) inc in
              add_to_document gamma annotmap "$inc" iexpr uexpr
        in
        let annotmap, uexpr =
          match set with
          | [] -> annotmap, uexpr
          | ["", e] -> add_to_document gamma annotmap "value" e uexpr
          | _ ->
              let rec aux ((annotmap, doc) as acc) set =
                match set with
                | [] -> acc
                | (field, value)::q ->
                    (*Special case for _id fields we can't modify.
                      Mongo restriction : TODO ?*)
                    if field = "_id" then
                      aux acc q
                    else
                      aux (add_to_document gamma annotmap field value doc) q
              in
              if addset then (
                let annotmap, sexpr = aux (C.list (annotmap, gamma) []) set in
                add_to_document gamma annotmap "$set" sexpr uexpr
              ) else (
                aux (C.list (annotmap, gamma) []) set
              )
        in
        let annotmap, uexpr =
          List.fold_left
            (fun (annotmap, uexpr) (fld, name, request) ->
               let annotmap, empty = C.list (annotmap, gamma) [] in
               let annotmap, request = add_to_document gamma annotmap fld request empty in
               add_to_document gamma annotmap name request uexpr
            ) (annotmap, uexpr) other
        in annotmap, uexpr

  let dot_update gamma annotmap field update =
    match update with
    | DbAst.UExpr e ->
        let annotmap, e = C.dot gamma annotmap e field in
        Some (annotmap, DbAst.UExpr e)
    | DbAst.UFlds fields ->
        List.find_map
          (fun (fields, u) -> match fields with
           | t::q when t = field -> Some (annotmap, DbAst.UFlds [q, u])
           | _ -> None)
          fields
    | _ -> None

  let dot_select field update =
    match update with
    | DbAst.SFlds fields ->
        List.find_map
          (fun (fields, u) -> match fields with
           | [t] when t = field ->
               Some (if u = DbAst.SNil then DbAst.SStar else u)
           | [_t] -> None
           | _t::_q -> assert false
           | _ -> None)
          fields
    | DbAst.SNil -> Some DbAst.SNil
    | DbAst.SStar -> Some DbAst.SStar
    | _ -> None

  let get_node ~context schema path =
    try
      DbSchema.get_node schema path
    with Base.NotImplemented s ->
      QmlError.error context
        "Can't generates mongo access because : %s is not yet implemented"
        s

  let dbMongoSet_to_dbSet gamma annotmap set dataty imap =
    let setident = Ident.next "mongoset" in
    let annotmap, identset =
      let tyset = OpaMapToIdent.specialized_typ ~ty:[dataty]
        Api.Types.DbMongoSet.engine gamma in
      C.ident annotmap setident tyset
    in
    let annotmap, iterator =
      let annotmap, iterator =
        OpaMapToIdent.typed_val ~label ~ty:[dataty]
          Api.DbSet.iterator annotmap gamma
      in
      imap (C.apply ~ty:dataty gamma annotmap iterator [identset])
    in
    let annotmap, genset =
      let annotmap, identset = C.copy annotmap identset in
      C.record annotmap [("iter", iterator); ("engine", identset)]
    in
    C.letin annotmap [setident, set] genset

  let get_read_map setkind dty uniq annotmap gamma =
    let aty = QmlAstCons.Type.next_var () in
    match setkind, uniq with
    | DbSchema.Map (_kty, _), true ->
        OpaMapToIdent.typed_val ~label ~ty:[aty; dty] Api.DbSet.map_to_uniq annotmap gamma
    | DbSchema.Map (kty, _), false ->
        let annotmap, to_map =
          OpaMapToIdent.typed_val ~label ~ty:[aty; dty; dty; kty;]
            Api.DbSet.to_map annotmap gamma
        in
        let annotmap, identity =
          let idx = Ident.next "x" in
          let annotmap, x = C.ident annotmap idx dty in
          C.lambda annotmap [idx, dty] x
        in
        let idx = Ident.next "x" in
        let annotmap, x = C.ident annotmap idx dty in
        let annotmap, body = C.apply gamma annotmap to_map [x; identity] in
        let annotmap, body = C.some annotmap gamma body in
        C.lambda annotmap [idx, aty] body
    | DbSchema.DbSet _, true ->
        OpaMapToIdent.typed_val ~label ~ty:[dty] Api.DbSet.set_to_uniq annotmap gamma
    | DbSchema.DbSet dataty, false ->
        let idset = Ident.next "set" in
        let tyset = OpaMapToIdent.specialized_typ ~ty:[dataty]
          Api.Types.DbMongoSet.engine gamma in
        let annotmap, set = C.ident annotmap idset tyset in
        let annotmap, set = dbMongoSet_to_dbSet gamma annotmap set dty (fun x -> x) in
        let annotmap, set = C.some annotmap gamma set in
        C.lambda annotmap [idset, tyset] set

  let apply_postmap gamma kind dataty postmap =
    match postmap with
    | None -> (fun x -> x)
    | Some (map, postty) ->
        match kind with
        | DbAst.Default ->
            (fun (annotmap, expr) -> C.apply ~ty:postty gamma annotmap map [expr])
        | DbAst.Option ->
            (fun (annotmap, expr) ->
               let id = Ident.next "data" in
               let annotmap, pvar = QmlAstCons.TypedPat.var annotmap id postty in
               let annotmap, ko_expr = C.none annotmap gamma in
               let annotmap, ok_expr =
                 let annotmap, eid = C.ident annotmap id dataty in
                 let annotmap, result = C.apply ~ty:postty gamma annotmap map [eid] in
                 C.some annotmap gamma result
               in
               QmlAstCons.TypedPat.match_option annotmap gamma expr pvar ok_expr ko_expr
            )
        | _ -> assert false

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
      | DbAst.Ref -> Api.Db.build_rpath_compose, Api.Types.DbMongo.ref_path
      | DbAst.Valpath -> Api.Db.build_vpath_compose, Api.Types.DbMongo.val_path
      | _ -> assert false
    in
    (annotmap, [elements], builder, pathty)

  and string_path ~context gamma annotmap schema (kind, strpath) select =
    let node =
      let strpath = List.map (fun k -> DbAst.FldKey k) strpath in
      get_node ~context schema strpath in
    match node.DbSchema.kind with
    | DbSchema.SetAccess (setkind, path, query, _todo) ->
        dbset_path ~context gamma annotmap (kind, path) setkind node query None select
    | _ ->
        let dataty = node.DbSchema.ty in
        let dbname = node.DbSchema.database.DbSchema.name in
        match kind with
        | DbAst.Update (u, o) ->
            begin match node.DbSchema.kind with
            | DbSchema.Plain ->
                let annotmap, path = expr_of_strpath gamma annotmap strpath in
                let annotmap, uexpr =
                  if ty_is_sum gamma dataty then (
                    let annotmap, uexpr = update_to_expr ~set:false gamma annotmap u in
                    (* Special case for upsert without '$' modifier, needs
                       _id to the update query. *)
                    let _id =
                      (Format.sprintf "/%a"
                         (Format.pp_list "/" Format.pp_print_string) strpath)
                    in
                    let annotmap, _id = C.string annotmap _id in
                    add_to_document gamma annotmap "_id" _id uexpr
                  ) else (
                    let u =
                      if ty_is_const gamma dataty then DbAst.UFlds [["value"], u]
                      else u
                    in
                    update_to_expr gamma annotmap u)
                in
                let annotmap, database = node_to_dbexpr gamma annotmap node in
                let annotmap, update =
                  OpaMapToIdent.typed_val ~label Api.Db.update_path annotmap gamma in
                C.apply gamma annotmap update [database; path; uexpr]
            | DbSchema.Partial (sum, rpath, partial) ->
                if sum then QmlError.serror context "Update inside a sum path is forbidden";
                let annotmap, path = expr_of_strpath gamma annotmap (dbname::rpath) in
                let annotmap, uexpr = update_to_expr gamma annotmap (DbAst.UFlds [partial, u]) in
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
            let annotmap, path = expr_of_strpath gamma annotmap strpath in
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
                      annotmap, [rpath; partial], Api.Db.build_rpath_sub, Api.Types.DbMongo.ref_path
                  | _ ->
                      annotmap, [rpath; partial], Api.Db.build_vpath_sub, Api.Types.DbMongo.val_path
                  end
              | DbSchema.Plain ->
                  let annotmap, const = C.bool (annotmap, gamma) (ty_is_const gamma dataty) in
                  (match kind with
                   | DbAst.Update _
                   | DbAst.Ref -> (annotmap, [const], Api.Db.build_rpath, Api.Types.DbMongo.ref_path)
                   | _ -> (annotmap, [const], Api.Db.build_vpath, Api.Types.DbMongo.val_path))
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

  and dbset_path ~context gamma annotmap (kind, path) setkind node query0 embed select0 =
    let ty = node.DbSchema.ty in
    let annotmap, skip, limit, query, order, uniq =
      match query0 with
      | None ->
          let annotmap, limit = C.int annotmap 0 in
          let annotmap, skip  = C.int annotmap 0 in
          annotmap, skip, limit, None, None, false
      | Some ((uniq, (query, opt)) as _x) ->
          let annotmap, limit =
            match opt.DbAst.limit with
            | None -> C.int annotmap 0
            | Some i -> annotmap, i
          in let annotmap, skip =
            match opt.DbAst.skip with
            | None -> C.int annotmap 0
            | Some i -> annotmap, i
          in let query = Some (
            match setkind with
            | DbSchema.Map _ ->
                let rec insert_id query = match query with
                  | DbAst.QEq  _
                  | DbAst.QGt  _
                  | DbAst.QLt  _
                  | DbAst.QGte _
                  | DbAst.QLte _
                  | DbAst.QNe  _
                  | DbAst.QMod _
                  | DbAst.QIn  _ -> DbAst.QFlds [(["_id"], query)]
                  | DbAst.QFlds flds -> DbAst.QFlds (List.map (fun (flds, q) -> ("_id"::flds, q)) flds)
                  | DbAst.QNot q -> DbAst.QNot (insert_id q)
                  | DbAst.QAnd (q1, q2) -> DbAst.QAnd (insert_id q1, insert_id q2)
                  | DbAst.QOr (q1, q2) -> DbAst.QOr (insert_id q1, insert_id q2)
                in
                uniq, insert_id query
            | _ -> uniq, query
          )
          in
          annotmap, skip, limit, query, opt.DbAst.sort, uniq
    in
    match query0, kind with
    | None, DbAst.Update (DbAst.UExpr e, _options) (* TODO : options *) ->
        (* Just reuse ref path on collections if 0 query *)
        let annotmap, refpath =
          dbset_path ~context gamma annotmap (DbAst.Ref, path) setkind node query0 embed select0 in
        let annotmap, more = C.dot gamma annotmap refpath "more" in
        let annotmap, write = C.dot gamma annotmap more "write" in
        let annotmap, apply = C.apply gamma annotmap write [e] in
        let annotmap, ignore =
          let i = Ident.next "_ignore" in
          let annotmap, v = C.cheap_void annotmap gamma in
          C.letin annotmap [(i, apply)] v
        in annotmap, ignore

    | _ ->
        (* Preprocessing of the embedded path, for select only useful data. *)
        let select0, postdot =
          let dot str ty =
            match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
            | Q.TypeRecord ((Q.TyRow (row, _)) as tyrow) ->
                begin try List.assoc str row
                with Not_found -> OManager.i_error "Selection : %s not found in %a"
                  str QmlPrint.pp#tyrow tyrow
                end
            | ty -> OManager.i_error "Selection : %s not found in non record : %a"
                str QmlPrint.pp#ty ty
          in
          match embed with
          | None -> select0, None
          | Some embed ->
              let select0, postdot =
                List.fold_right
                  (fun fragment (select, post) ->
                     match fragment with
                     | DbAst.FldKey str ->
                         DbAst.SFlds [[str], select],
                         (fun ((annotmap, expr), ty) ->
                            let ty = dot str ty in
                            let ae = C.dot gamma annotmap expr str in
                            post (ae, ty)
                         )
                     | DbAst.ExprKey _
                     | DbAst.NewKey _
                     | DbAst.Query _ ->
                         QmlError.error context
                           "This kind of sub selection is not yet implemented by mongo generator")
                  embed
                  (select0, (fun x -> x))
              in select0, Some postdot
        in
        (* Type of the data after selection *)
        let dataty =
          let ty =
            match setkind with
            | DbSchema.DbSet ty -> ty
            | DbSchema.Map (_, ty) -> ty
          in QmlDbGen.Utils.type_of_selected gamma ty select0
        in
        (* DbSet.build *)
        let (annotmap, build, query, args) =
          match kind with
          | DbAst.Default
          | DbAst.Valpath
          | DbAst.Ref
          | DbAst.Option ->
              (* query *)
              let annotmap, query = query_to_expr gamma annotmap query in
              let annotmap, query = query_add_order gamma annotmap order query in
              let annotmap, default = node.DbSchema.default ~select:select0 annotmap in
              let annotmap, select =
                match select0 with
                | DbAst.SNil | DbAst.SStar -> C.none annotmap gamma
                | select ->
                    let annotmap, select = select_to_expr gamma annotmap select in
                    C.some annotmap gamma select
              in
              begin match kind with
              | DbAst.Default | DbAst.Option ->
                  let annotmap, build =
                    OpaMapToIdent.typed_val ~label ~ty:[dataty] Api.DbSet.build annotmap gamma in
                  (annotmap, build, query, [default; skip; limit; select])
              | DbAst.Valpath ->
                  let annotmap, build =
                    OpaMapToIdent.typed_val ~label ~ty:[QmlAstCons.Type.next_var (); dataty]
                      Api.DbSet.build_vpath annotmap gamma
                  in
                  let annotmap, read_map = get_read_map setkind dataty uniq annotmap gamma in
                  (annotmap, build, query, [default; skip; limit; select; read_map])
              | DbAst.Ref ->
                  let annotmap, read_map = get_read_map setkind dataty uniq annotmap gamma in
                  let build_rpath, (annotmap, write_map) =
                    match setkind, uniq with
                    | DbSchema.DbSet _, true ->
                        let iarg  = Ident.next "data" in
                        let annotmap, earg = C.ident annotmap iarg dataty in
                        let annotmap, doc = opa2doc ~ty:dataty gamma annotmap earg () in
                        Api.DbSet.build_rpath, C.lambda annotmap [(iarg, dataty)] doc
                    | DbSchema.Map (_kty, _dty), true ->
                        let iarg  = Ident.next "data" in
                        let annotmap, earg = C.ident annotmap iarg dataty in
                        let annotmap, doc = opa2doc ~ty:dataty gamma annotmap earg () in
                        Api.DbSet.build_rpath, C.lambda annotmap [(iarg, dataty)] doc
                    | DbSchema.DbSet _, false ->
                        QmlError.warning ~wclass:WarningClass.dbgen_mongo
                          context "Reference path on database set is not advised";
                        Api.DbSet.build_rpath_collection,
                        OpaMapToIdent.typed_val ~label ~ty:[dataty]
                          Api.DbSet.set_to_docs annotmap gamma

                    | DbSchema.Map (kty, _), false ->
                        QmlError.warning ~wclass:WarningClass.dbgen_mongo
                          context "Reference path on database map is not advised";
                        Api.DbSet.build_rpath_collection,
                        OpaMapToIdent.typed_val ~label ~ty:[kty; dataty]
                          Api.DbSet.map_to_docs annotmap gamma
                  in
                  let annotmap, build =
                    OpaMapToIdent.typed_val ~label ~ty:[dataty; dataty] build_rpath annotmap gamma
                  in
                  (annotmap, build, query, [default; skip; limit; select; read_map; write_map])
              | _ -> assert false
              end

          | DbAst.Update (u, o) ->
              let (annotmap, query) = query_to_expr gamma annotmap query in
              let (annotmap, update) =
                let u =
                  (* Hack : When map value is simple, adding the "value" field *)
                  match setkind with
                  | DbSchema.Map (_, tyval) when ty_is_const gamma tyval -> DbAst.UFlds [["value"], u]
                  | _ -> u
                in
                update_to_expr gamma annotmap u
              in
              let annotmap, upsert =
                if o.DbAst.ifexists then C._false (annotmap, gamma)
                else C._true (annotmap, gamma)
              in
              let (annotmap, build) =
                OpaMapToIdent.typed_val ~label Api.DbSet.update annotmap gamma
              in
              (annotmap, build, query, [update; upsert])
        in
        (* database *)
        let (annotmap, database) = node_to_dbexpr gamma annotmap node in
        (* path : list(string) *)
        let (annotmap, path) =
          let (annotmap, path) = List.fold_left
            (fun (annotmap, acc) key ->
               let annotmap, e = C.string annotmap key in
               annotmap, e::acc
            ) (annotmap, []) path
          in
          C.rev_list (annotmap, gamma) path in
        (* dbset = DbSet.build(database, path, query, ...) *)
        let (annotmap, set) =
          C.apply  gamma annotmap build
            ([database; path; query] @ args) in
        let ty =
          (*FIXME : We should project the resulted ty according to the selection *)
          ty
        in
        (* Final convert *)
        let (annotmap, set) =
          match kind with
          | DbAst.Default | DbAst.Option ->
              let annotmap, postmap =
                match postdot with
                | None -> annotmap, None
                | Some postdot ->
                    let data = Ident.next "data" in
                    let (annotmap, map), postty =
                      postdot ((C.ident annotmap data dataty), dataty)
                    in
                    let annotmap, map = C.lambda annotmap [(data, dataty)] map in
                    annotmap, Some (map, postty)
              in
              (match setkind, uniq with
               | DbSchema.DbSet _, false ->
                   let imap = function (annotmap, iterator) ->
                     match postmap with
                     | None -> annotmap, iterator
                     | Some (map, postty) ->
                         let annotmap, imap =
                           OpaMapToIdent.typed_val ~label ~ty:[dataty; postty]
                             Api.DbSet.iterator_map annotmap gamma
                         in C.apply ~ty gamma annotmap imap [map; iterator]
                   in
                   dbMongoSet_to_dbSet gamma annotmap set dataty imap
               | DbSchema.Map (keyty, _), false ->
                   let (annotmap, postdot), postty =
                     match postmap with
                     | None ->
                         let id = Ident.next "x" in
                         let annotmap, idx = C.ident annotmap id dataty in
                         (C.lambda annotmap [id, dataty] idx), dataty
                     | Some (map, postty) -> (annotmap, map), postty
                   in
                   let annotmap, to_map =
                     OpaMapToIdent.typed_val ~label
                       ~ty:[QmlAstCons.Type.next_var (); dataty; postty; keyty;]
                       Api.DbSet.to_map annotmap gamma in
                   let annotmap, map =
                     C.apply ~ty gamma annotmap to_map [set; postdot] in
                   begin match kind with
                   | DbAst.Option ->
                       (* TODO - Actually we consider map already exists *)
                       C.some annotmap gamma map
                   | _ -> (annotmap, map)
                   end
               | DbSchema.DbSet _, true ->
                   let (annotmap, set_to_uniq) =
                     let set_to_uniq = match kind with
                       | DbAst.Default -> Api.DbSet.set_to_uniq_def
                       | DbAst.Option -> Api.DbSet.set_to_uniq
                       | _ -> assert false
                     in
                     OpaMapToIdent.typed_val ~label ~ty:[dataty] set_to_uniq annotmap gamma in
                   apply_postmap gamma kind dataty postmap
                     (C.apply ~ty gamma annotmap set_to_uniq [set])
               | DbSchema.Map (_keyty, _), true ->
                   let (annotmap, map_to_uniq) =
                     let map_to_uniq = match kind with
                       | DbAst.Default -> Api.DbSet.map_to_uniq_def
                       | DbAst.Option -> Api.DbSet.map_to_uniq
                       | _ -> assert false
                     in
                     OpaMapToIdent.typed_val ~label ~ty:[QmlAstCons.Type.next_var (); dataty]
                       map_to_uniq annotmap gamma in
                   apply_postmap gamma kind dataty postmap
                     (C.apply ~ty gamma annotmap map_to_uniq [set])
              )
          | _ -> (annotmap, set)
        in
        (annotmap, set)


  let path ~context gamma annotmap schema (label, dbpath, kind, select) =
    let node = get_node ~context schema dbpath in
    match node.DbSchema.database.DbSchema.options.DbAst.backend with
    | `mongo -> (
        let annotmap, mongopath =
          match node.DbSchema.kind with
          | DbSchema.SetAccess (setkind, path, query, embed) ->
              dbset_path ~context gamma annotmap (kind, path) setkind node query embed select
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
    | `db3 -> annotmap, Q.Path (label, dbpath, kind, select)

  let indexes gamma annotmap _schema node rpath lidx =
    let (annotmap, database) =
      node_to_dbexpr gamma annotmap node in
    let (annotmap, build) =
      OpaMapToIdent.typed_val ~label Api.DbSet.indexes annotmap gamma in
    let (annotmap, path) =
      C.rev_list_map
        (fun annotmap fragment -> C.string annotmap fragment)
        (annotmap, gamma) rpath
    in
    let (annotmap, lidx) =
      List.fold_left_map
        (fun annotmap idx ->
           C.list_map
             (fun annotmap fragment -> C.string annotmap fragment)
             (annotmap, gamma) idx)
        annotmap lidx
    in
    let (annotmap, lidx) = C.list (annotmap, gamma) lidx
    in C.apply gamma annotmap build [database; path; lidx]



end

let init_database gamma annotmap schema =
  List.fold_left
    (fun (annotmap, newvals) database ->
       if database.DbSchema.options.DbAst.backend = `mongo then
         let ident = database.DbSchema.ident in
         let name = database.DbSchema.name in
         let (annotmap, open_) = Generator.open_database gamma annotmap name None None in
         (annotmap, (Q.NewVal (label, [ident, open_]))::newvals)
       else (annotmap, newvals)
    )
    (annotmap, []) (DbSchema.get_db_declaration schema)

let clean_code gamma annotmap schema code =
  List.fold_left_filter_map
    (fun annotmap -> function
       | Q.Database _ -> annotmap, None
       | Q.NewDbValue (_label, DbAst.Db_TypeDecl (p, _ty)) ->
           let fake_path =
             match p with
             | DbAst.Decl_fld k::_ -> [DbAst.FldKey k]
             | _ -> []
           in
           begin match p with
           | (DbAst.Decl_fld _)::p ->
               let rec aux rpath p =
                 match p with
                 | (DbAst.Decl_set lidx)::[] ->
                     let (annotmap, init) =
                       let fake_node = DbSchema.get_node schema fake_path in
                       Generator.indexes gamma annotmap schema fake_node rpath lidx
                     in
                     let id = Ident.next "_index_setup" in
                     annotmap, Some (Q.NewVal (label, [id, init]))
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

