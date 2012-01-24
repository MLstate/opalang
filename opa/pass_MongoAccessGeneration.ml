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
    let (annotmap, database) =
      let (database, _) =
        try
          QmlDbGen.Schema.db_declaration schema dbname
        with Not_found ->
          OManager.i_error "\"%s\" database declaration was not found on database schema" dbname
      in
      C.ident annotmap database ty_database
    in
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
      )
    | _ -> annotmap, path

  let query_to_expr gamma annotmap query =
    let rec aux annotmap query =
      match query with
      | DbSchema.Empty -> C.list (annotmap, gamma) []
      | _ -> assert false
    in aux annotmap query

  let update_to_expr gamma annotmap update =
    let rec aux annotmap update =
      match update with
      | DbAst.UExpr e ->
          let a = Annot.annot (QmlAst.Label.expr e) in
          let ty = QmlAnnotMap.find_ty a annotmap in
          match ty with
          | QmlAst.TypeRecord _ ->
              let (annotmap, opa2doc) =
                OpaMapToIdent.typed_val ~label ~ty:[ty] Api.DbSet.opa2doc annotmap gamma
              in
              C.apply gamma annotmap opa2doc [e]
          | _ -> assert false
              (* | _ -> assert false *)
    in aux annotmap update

  let dbset_path gamma annotmap schema (kind, path) node query =
    let ty = node.DbSchema.ty in
    let dbname = node.DbSchema.database.DbSchema.name in
    (* DbSet.build *)
    let (annotmap, build, args) =
      (match kind with
       | DbAst.Default ->
           let tydbset = OpaMapToIdent.typ Api.Types.dbset in
           let dataty =
             match ty with
             | Q.TypeName ([dataty], tyident) when Q.TypeIdent.compare tydbset tyident = 0
                 -> dataty
             | _ -> assert false
           in
           let (annotmap, build) =
             OpaMapToIdent.typed_val ~label ~ty:[dataty] Api.DbSet.build annotmap gamma in
           (annotmap, build, [])
       | DbAst.Update u ->
           let (annotmap, update) = update_to_expr gamma annotmap u in
           let (annotmap, build) =
             OpaMapToIdent.typed_val ~label Api.DbSet.update annotmap gamma
           in
           (annotmap, build, [update])
       | _ -> assert false)
    in
    (* database *)
    let (annotmap, database) =
      let (database, _) =
        try
          QmlDbGen.Schema.db_declaration schema dbname
        with Not_found ->
          OManager.i_error "\"%s\" database declaration was not found on database schema" dbname
      in
      C.ident annotmap database ty_database
    in
    (* path : list(string) *)
    let (annotmap, path) =
      let (annotmap, path) = List.fold_left
        (fun (annotmap, acc) key ->
           let annotmap, e = C.string annotmap key in
           annotmap, e::acc
        ) (annotmap, []) path
      in
      C.rev_list (annotmap, gamma) path in
    (* query *)
    let (annotmap, query) = query_to_expr gamma annotmap query in
    (* dbset = DbSet.build(database, path, query) *)
    let (annotmap, set) = C.apply ~ty gamma annotmap build ([database; path; query] @ args) in
    (annotmap, set)

  let path gamma annotmap schema (kind, dbpath) =
    let (_, node) = DbSchema.get_node annotmap schema dbpath in
    Format.eprintf "%a\n%!" DbSchema.pp_node node;
    match node.DbSchema.kind with
    | DbSchema.SetAccess (path, _, query) ->
        dbset_path gamma annotmap schema (kind, path) node query
    | _ ->
        let strpath = List.map
          (function
             | DbAst.FldKey k -> k
             | _ -> assert false
          ) dbpath in
        string_path gamma annotmap schema (kind, strpath)

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

let clean_code code =
  List.filter
    (function
       | Q.Database _ -> false
       | Q.NewDbValue _ -> false
       | _ -> true)
    code

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
      let code = clean_code code in
      let (annotmap, code) =
        let (annotmap, vals) = init_database stdlib_gamma annotmap schema in
        (annotmap, vals@code)
      in
      let gamma = QmlTypes.Env.unsafe_append stdlib_gamma gamma in
      let (annotmap, code) = process_path gamma annotmap schema code in
      (annotmap, code)

