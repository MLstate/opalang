(* shorthands *)
module Q = QmlAst
module C = QmlAstCons.TypedExpr

module Api = Opacapi

module DbAst = QmlAst.Db
module DbSchema = QmlDbGen.Schema
module List = BaseList

type db_access = {
  engines : Ident.t StringMap.t
}

let label = Annot.nolabel "MongoAccessGeneration"


module Generator = struct

  let ty_database = Q.TypeVar (QmlTypeVars.TypeVar.next ())

  let open_database gamma annotmap host port =
    let (annotmap, name) = C.string annotmap "test" in
    let (annotmap, host) = C.string annotmap host in
    let (annotmap, port) = C.int annotmap port in
    let (annotmap, open_) = OpaMapToIdent.typed_val ~label Opacapi.Db.open_ annotmap gamma in
    let (annotmap, open_) = C.apply gamma annotmap open_ [name; host; port] in
    (annotmap, open_)

  let dbname_to_expr _gamma annotmap schema dbname =
    let (database, _) =
      try
        QmlDbGen.Schema.db_declaration schema dbname
      with Not_found ->
        OManager.i_error "\"%s\" database declaration was not found on database schema" dbname
    in C.ident annotmap database ty_database

  let rec compose_path gamma annotmap schema kind subs =
    ignore (match kind with
            | DbAst.Ref -> assert false
            | _ -> kind);
    let annotmap, subs =
      List.fold_left_map
        (fun annotmap (field, sub) ->
           let (annotmap, path) = string_path gamma annotmap schema (DbAst.Valpath, sub) in
           annotmap, (field, path))
        annotmap subs
    in
    let annotmap, read =
      let annotmap, res =
        let (annotmap, subsread) = List.fold_left_map
          (fun annotmap (field, subpath) ->
             let (annotmap, read) =
               OpaMapToIdent.typed_val ~label Api.Db.read annotmap gamma in
             let (annotmap, read) = C.apply gamma annotmap read [subpath] in
             annotmap, (field, read)
          ) annotmap subs
        in C.record annotmap subsread
      in
      let annotmap, res =  C.record annotmap [("some", res)] in
      C.lambda annotmap [] res
    in
    let annotmap, more = C.cheap_void annotmap gamma in
    let pathty = Api.Types.val_path in
    (annotmap, [read; more], Api.Db.build_path_raw, pathty)

    and string_path gamma annotmap schema (kind, strpath) =
    (* vv FIXME !?!?! vv *)
    let (annotmap2, node) =
      let strpath = List.map (fun k -> DbAst.FldKey k) strpath in
      DbSchema.get_node annotmap schema strpath in
    let annotmap = QmlAnnotMap.merge annotmap annotmap2 in
    (* ^^ FIXME !?!?! ^^ *)

    let (annotmap, args, builder, pathty) =
      match node.DbSchema.kind with
      | DbSchema.Compose subs ->
          compose_path gamma annotmap schema kind subs

      | DbSchema.Partial (rpath, partial) ->
          let annotmap, partial = C.list_map
            (fun annotmap fragment -> C.string annotmap fragment)
            (annotmap, gamma) partial
          in let annotmap, rpath = C.list_map
            (fun annotmap fragment -> Printf.eprintf "%s\n%!" fragment; C.string annotmap fragment)
            (annotmap, gamma) rpath
          in annotmap, [rpath; partial], Api.Db.build_vpath_sub, Api.Types.val_path
      | DbSchema.Plain ->
          (match kind with
           | DbAst.Update _
           | DbAst.Ref -> (annotmap, [], Api.Db.build_rpath, Api.Types.ref_path)
           | _ -> (annotmap, [], Api.Db.build_vpath, Api.Types.val_path))
      | _ -> assert false
    in
    let dbname, strpath =
      match strpath with
      | k::path -> (k, path)
      | _ -> assert false
    in
    let (annotmap, path) = List.fold_left
      (fun (annotmap, acc) key ->
         let annotmap, e = C.string annotmap key in
         annotmap, e::acc
      ) (annotmap, []) strpath
    in
    let dataty = node.DbSchema.ty in
    let (annotmap, build) =
      OpaMapToIdent.typed_val ~label ~ty:[dataty] builder annotmap gamma in
    let (annotmap, path) = C.rev_list (annotmap, gamma) path in
    let (annotmap, database) = dbname_to_expr gamma annotmap schema dbname in
    let ty = OpaMapToIdent.specialized_typ ~ty:[dataty] pathty gamma in
    let (annotmap, path) = C.apply ~ty gamma annotmap build
      ([database; path; node.DbSchema.default] @ args) in
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
    in match kind with
    | DbAst.Update u -> (
        match u with
        | DbAst.UExpr e ->
            let (annotmap, write) =
              OpaMapToIdent.typed_val ~label ~ty:[dataty] Api.Db.write annotmap gamma in
            C.apply gamma annotmap write [path; e]
        | _ -> assert false
      )
    | _ -> annotmap, path

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

  let query_to_expr gamma annotmap query =
    let empty_query annotmap = C.list (annotmap, gamma) [] in
    match query with
    | None -> empty_query annotmap
    | Some (_todo, query) ->
        let query = prepare_query query in
        let rec aux annotmap query =
          match query with
          | DbAst.QEq e ->
              let a = Annot.annot (QmlAst.Label.expr e) in
              let ty = QmlAnnotMap.find_ty a annotmap in
              let (annotmap, opa2doc) =
                OpaMapToIdent.typed_val ~label ~ty:[ty] Api.DbSet.opa2doc annotmap gamma
              in
              let (annotmap, e) = C.shallow_copy annotmap e in
              C.apply gamma annotmap opa2doc [e]
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
              let annotmap, query = empty_query annotmap in
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
                (empty_query annotmap)
                flds
          | DbAst.QNot query ->
              let annotmap, query = aux annotmap query in
              let annotmap, empty = empty_query annotmap in
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
              let annotmap, empty = empty_query annotmap in
              add_to_document gamma annotmap name query empty
        in aux annotmap query

  let update_to_expr gamma annotmap update =
    let rec collect fld (inc, set, other, annotmap) update =
      match update with
      | DbAst.UExpr e -> (inc, (fld, e)::set, other, annotmap)
      | DbAst.UIncr i -> ((fld, i)::inc, set, other, annotmap)
      | DbAst.UFlds fields ->
          List.fold_left
            (fun (inc, set, other, annotmap) (f, u) ->
               let fld =
                 let dot = match fld with | "" -> "" | _ -> "." in
                 BaseFormat.sprintf "%s%s%a" fld dot QmlAst.Db.pp_field f in
               collect fld (inc, set, other, annotmap) u)
            (inc, set, other, annotmap) fields
      | DbAst.UAppend     e -> (inc, set, (fld, "$push", e)::other, annotmap)
      | DbAst.UAppendAll  e -> (inc, set, (fld, "$pushAll", e)::other, annotmap)
      | DbAst.UPrepend    _e -> assert false
      | DbAst.UPrependAll _e -> assert false
      | DbAst.UPop   ->
          let annotmap, e = C.int annotmap (-1) in
          (inc, set, (fld, "$pop", e)::other, annotmap)
      | DbAst.UShift ->
          let annotmap, e = C.int annotmap 1 in
          (inc, set, (fld, "$pop", e)::other, annotmap)
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
      | ["", e] -> add_to_document gamma annotmap "$set" e uexpr
      | _ ->
          let rec aux ((annotmap, doc) as acc) set =
            match set with
            | [] -> acc
            | (field, value)::q ->
                aux (add_to_document gamma annotmap field value doc) q
          in
          let annotmap, sexpr = aux (C.list (annotmap, gamma) []) set in
          add_to_document gamma annotmap "$set" sexpr uexpr
    in
    let annotmap, uexpr =
      List.fold_left
        (fun (annotmap, uexpr) (fld, name, request) ->
           let annotmap, empty = C.list (annotmap, gamma) [] in
           let annotmap, request = add_to_document gamma annotmap fld request empty in
           add_to_document gamma annotmap name request uexpr
        ) (annotmap, uexpr) other
    in annotmap, uexpr
          (* let *)

    (* assert false *)

  let dbset_path gamma annotmap schema (kind, path) setkind node query =
    let ty = node.DbSchema.ty in
    let dbname = node.DbSchema.database.DbSchema.name in
    let query = Option.map
      (function (partial, query) as x ->
         match setkind with
         | DbSchema.Map _ -> partial, DbAst.QFlds [(["_id"], query)]
         | _ -> x)
      query
    in
    (* DbSet.build *)
    let (annotmap, build, query, args) =
      (match kind with
       | DbAst.Default ->
           let dataty =
             match setkind with
             | DbSchema.DbSet ty -> ty
             | DbSchema.Map _ -> QmlAstCons.Type.next_var () (* Dummy type variable, should never use*)
           in
           let (annotmap, build) =
             OpaMapToIdent.typed_val ~label ~ty:[dataty] Api.DbSet.build annotmap gamma in
           (* query *)
           let (annotmap, query) = query_to_expr gamma annotmap query in
           (annotmap, build, query, [])
       | DbAst.Update u ->
           let (annotmap, query) = query_to_expr gamma annotmap query in
           let (annotmap, update) = update_to_expr gamma annotmap u in
           let (annotmap, build) =
             OpaMapToIdent.typed_val ~label Api.DbSet.update annotmap gamma
           in
           (annotmap, build, query, [update])
       | _ -> assert false)
    in
    (* database *)
    let (annotmap, database) = dbname_to_expr gamma annotmap schema dbname in
    (* path : list(string) *)
    let (annotmap, path) =
      let (annotmap, path) = List.fold_left
        (fun (annotmap, acc) key ->
           let annotmap, e = C.string annotmap key in
           annotmap, e::acc
        ) (annotmap, []) path
      in
      C.rev_list (annotmap, gamma) path in
    (* dbset = DbSet.build(database, path, query) *)
    let (annotmap, set) = C.apply ~ty gamma annotmap build ([database; path; query] @ args) in
    (* if is a map convert *)
    let (annotmap, set) =
      match kind with
      | DbAst.Default ->
          begin match setkind with
          | DbSchema.DbSet _ -> (annotmap, set)
          | DbSchema.Map (keyty, dataty) ->
              Format.eprintf "Instantiate with %a\n" QmlPrint.pp#ty dataty;
              let (annotmap, to_map) =
                OpaMapToIdent.typed_val ~label
                  ~ty:[QmlAstCons.Type.next_var (); keyty; dataty]
                  Api.DbSet.to_map annotmap gamma in
              C.apply ~ty gamma annotmap to_map [set]
          end
      | _ -> (annotmap, set)
    in
    (annotmap, set)


  let path gamma annotmap schema (kind, dbpath) =
    let (_, node) = DbSchema.get_node annotmap schema dbpath in
    Format.eprintf "%a\n%!" DbSchema.pp_node node;
    match node.DbSchema.kind with
    | DbSchema.SetAccess (setkind, path, query) ->
        dbset_path gamma annotmap schema (kind, path) setkind node query
    | _ ->
        let strpath = List.map
          (function
             | DbAst.FldKey k -> k
             | _ -> assert false
          ) dbpath in
        string_path gamma annotmap schema (kind, strpath)

  let indexes gamma annotmap schema dbname rpath lidx =
    let (annotmap, database) =
      dbname_to_expr gamma annotmap schema dbname in
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
    (fun (annotmap, newvals) (ident, _name, opts) ->
       match opts with
       | [`engine (`client (Some host, Some port))] ->
           let (annotmap, open_) = Generator.open_database gamma annotmap host port in
           (annotmap, (Q.NewVal (label, [ident, open_]))::newvals)
       | _ ->
           let (annotmap, open_) = Generator.open_database gamma annotmap "localhost" 27017 in
           (annotmap, (Q.NewVal (label, [ident, open_]))::newvals)
    )
    (annotmap, []) (QmlDbGen.Schema.get_db_declaration schema)

let clean_code gamma annotmap schema code =
  List.fold_left_filter_map
    (fun annotmap -> function
       | Q.Database _ -> annotmap, None
       | Q.NewDbValue (_label, DbAst.Db_TypeDecl (p, _ty)) ->
           begin match p with
           | [] -> annotmap, None
           | (DbAst.Decl_fld dbname)::p ->
               let rec aux rpath p =
                 match p with
                 | (DbAst.Decl_set lidx)::[] ->
                     let (annotmap, init) =
                       Generator.indexes gamma annotmap schema dbname rpath lidx
                     in
                     let id = Ident.next "_index_setup" in
                     annotmap, Some (Q.NewVal (label, [id, init]))
                 | (DbAst.Decl_set _lidx)::_ -> assert false
                 | (DbAst.Decl_fld str)::p -> aux (str::rpath) p
                 | [] -> annotmap, None
                 | _ -> assert false
               in aux [] p
           | _ -> assert false
           end
       | Q.NewDbValue _ -> annotmap, None
       | elt -> annotmap, Some elt)
    annotmap code

let process_path gamma annotmap schema code =
  let fmap tra annotmap = function
    | Q.Path (_label, path, kind) ->
        Generator.path gamma annotmap schema (kind, path)
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

