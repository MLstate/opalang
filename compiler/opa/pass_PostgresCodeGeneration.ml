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

module Format = BaseFormat
module List = BaseList

module Q = QmlAst
module QD = Q.Db
module S = QmlDbGen.Schema

module C = QmlAstCons.TypedExpr

module Api =
struct

  module Db = Opacapi.DbPostgres

end

let label = Annot.nolabel "PostgresCodeGeneration"

module QueryMap = BaseMap.Make(
  struct
    type t = S.sqlquery
    let compare = Pervasives.compare
  end
)

module UpdateMap = BaseMap.Make(
  struct
    type t = (S.query option * Q.expr QD.update)
    let compare = Pervasives.compare
  end
)

type env = {
  ty_init : [ `foreign of string | `type_ of (string * string) ] StringListMap.t;
  (* List of queries which create table *)
  tb_init : string list;
  (* (id of prepared statement, string query) *)
  q_prepared : (string * string) QueryMap.t;
  (* (id of prepared statement, string query) *)
  u_prepared : (string * string) UpdateMap.t;
  gamma : QmlTypes.gamma;
  annotmap : Q.annotmap;
  schema : S.t
}

module Generator =
struct

  let make_env gamma annotmap schema = {
    tb_init = ["CREATE LANGUAGE plpgsql"];
    ty_init = StringListMap.empty;
    q_prepared = QueryMap.empty;
    u_prepared = UpdateMap.empty;
    gamma; annotmap; schema;
  }

  let get_node ~context gamma schema path =
    try
      S.get_node gamma schema path
    with Base.NotImplemented s ->
      QmlError.error context
        "Can't generates postgres access because : %s is not yet implemented"
        s

  let pp_postgres_field =
    Format.pp_list "."
      (fun fmt -> function | `string s -> Format.pp_print_string fmt s | _ -> assert false)

  let pp_table_name = (Format.pp_list "_" Format.pp_print_string)

  let opa_to_data gamma annotmap expr =
    let ty = QmlAnnotMap.find_ty (Annot.annot (Q.Label.expr expr)) annotmap in
    match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
    | Q.TypeConst c ->
        let fld = match c with
          | Q.TyNull   -> assert false
          | Q.TyFloat  -> "Float"
          | Q.TyInt    -> "Int"
          | Q.TyString -> "String"
        in
        C.record annotmap [fld, expr]
    | ty -> OManager.i_error
        "expression of type @{<bright>%a@} in sql query are not yet implemented or unexpected"
          QmlPrint.pp#ty ty

  let database
      ({gamma; annotmap; tb_init; q_prepared; u_prepared; _} as env)
      name =
    let annotmap, open_ = OpaMapToIdent.typed_val ~label Api.Db.open_ annotmap gamma in
    let annotmap, name = C.string annotmap name in
    let annotmap, tables =
      let annotmap, tables =
        List.fold_left
          (fun (annotmap, tables) table ->
             let annotmap, table = C.string annotmap table in
             annotmap, table::tables
          ) (annotmap, []) tb_init
      in
      C.list (annotmap, gamma) tables
    in
    let annotmap, statements =
      let annotmap, statements =
        QueryMap.fold
          (fun _prepared (qid, query) (annotmap, statements) ->
             let annotmap, qid = C.string annotmap qid in
             let annotmap, query = C.string annotmap query in
             (* TODO: Optimized types *)
             let annotmap, types = C.list (annotmap, gamma) [] in
             let annotmap, statement =
               C.record annotmap [
                 "id", qid;
                 "query", query;
                 "types", types;
               ]
             in
             annotmap, statement::statements
          ) q_prepared (annotmap, []) in
      C.list (annotmap, gamma) statements
    in
    let annotmap, queries =
      let annotmap, queries =
        UpdateMap.fold
          (fun _ (_, query) (annotmap, queries) ->
             let annotmap, query = C.string annotmap query in
             annotmap, query::queries
          ) u_prepared (annotmap, [])
      in
      C.list (annotmap, gamma) queries
    in
    let annotmap, pgdb = C.apply gamma annotmap open_ [name; tables; statements; queries] in
    {env with annotmap}, pgdb

  let pp_type_as_pgtype ?(path=[]) env fmt ty =
    match StringListMap.find_opt path env.ty_init with
    | Some (`foreign _) -> assert false
    | Some (`type_ (name,_)) -> Format.pp_print_string fmt name
    | None ->
        match QmlTypesUtils.Inspect.follow_alias_noopt_private env.gamma ty with
        | Q.TypeConst Q.TyFloat  -> Format.fprintf fmt "FLOAT8"
        | Q.TypeConst Q.TyInt    -> Format.fprintf fmt "INT8"
        | Q.TypeConst Q.TyString -> Format.fprintf fmt "TEXT"
        | _ ->
            Format.eprintf "Not_found path [%a]:%a\n%!"
              (Format.pp_list "," Format.pp_print_string) path
              QmlPrint.pp#ty ty;
            raise Not_found

  (* ******************************************************)
  (* QUERYING *********************************************)
  (* ******************************************************)
  let pp_postgres_genquery pp_expr fmt (q:(_, _) QmlAst.Db.query) =
    let rec aux fmt q =
      let pp x = Format.fprintf fmt x in
      match q with
      | QD.QEq   e    -> pp " = %a" pp_expr e
      | QD.QGt   e    -> pp " > %a" pp_expr e
      | QD.QLt   e    -> pp " < %a" pp_expr e
      | QD.QGte  e    -> pp " >= %a" pp_expr e
      | QD.QLte  e    -> pp " <= %a" pp_expr e
      | QD.QNe   e    -> pp " <> %a" pp_expr e
      | QD.QIn   e    -> pp " IN %a" pp_expr e
      | QD.QMod  _    -> assert false
      | QD.QExists false   -> pp " = NULL"
      | QD.QExists true    -> pp " <> NULL"
      | QD.QOr  (q0, q1) ->
          pp "%a OR %a"
            aux q0
            aux q1
      | QD.QAnd (q0, q1) ->
          pp "%a AND %a"
            aux q0
            aux q1
      | QD.QNot  _     -> assert false
      | QD.QFlds flds  ->
          List.iter
            (fun (f, q) ->
               pp "%a %a"
                 pp_postgres_field f
                 aux q
            ) flds
    in
    match q with
    | QD.QFlds [] -> ()
    | _ ->
        let pp x = Format.fprintf fmt x in
        pp " WHERE ";
        aux fmt q

  let pp_postgres_sqlquery fmt q =
    let pos = ref 0 in
    let pp x = Format.fprintf fmt x in
    pp "SELECT ";
    (match q.QD.sql_fds with
     | [] -> pp "* "
     | _ ->
         (BaseFormat.pp_list ","
            (fun fmt (db, field) ->
               (match db with "" -> ()
                | _ -> Format.fprintf fmt "%s." db);
               Format.fprintf fmt "%s" field
            ))
           fmt q.QD.sql_fds
    );
    pp " FROM ";
    (BaseFormat.pp_list "," Format.pp_print_string) fmt q.QD.sql_tbs;
    match q.QD.sql_ops with
    | None -> ()
    | Some sql_ops ->
        pp_postgres_genquery
          (fun fmt -> function
           | `expr _ -> incr pos; Format.fprintf fmt "$%d" !pos
           | `bind s -> Format.pp_print_string fmt s
          ) fmt sql_ops

  let prepared_statement_for_query =
    let fresh_id =
      let fresh = Fresh.fresh_factory (fun x -> x) in
      fun () -> Format.sprintf "query_%d" (fresh ())
    in
    fun
      ({annotmap; q_prepared; _} as env)
      ((sqlquery, options) as query) ->
      let buffer = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buffer in
      pp_postgres_sqlquery fmt sqlquery;
      (* TODO OPTIONS *)
      ignore options;
      Format.pp_print_flush fmt ();
      let qid = fresh_id () in
      let q_prepared = QueryMap.add query (qid, Buffer.contents buffer) q_prepared in
      {env with annotmap; q_prepared}

  let execute_statement
      ({gamma; annotmap; q_prepared; _} as env)
      node (uniq, query) =
    let qid, _ = try QueryMap.find query q_prepared with
        Not_found -> OManager.i_error "Can't found prepared statement"
    in
    let annotmap, database =
      C.ident annotmap node.S.database.S.ident node.S.database.S.dbty in
    let annotmap, qid = C.string annotmap qid in
    let annotmap, args =
      match (fst query).QD.sql_ops with
      | None -> annotmap, []
      | Some sql_ops ->
          (* see type Postgres.data *)
          QmlAstWalk.DbWalk.Query.self_traverse_fold
            (fun self tra ((annotmap, args) as acc) -> function
             | QD.QEq     (`expr e)
             | QD.QGt     (`expr e)
             | QD.QLt     (`expr e)
             | QD.QGte    (`expr e)
             | QD.QLte    (`expr e)
             | QD.QNe     (`expr e)
             | QD.QIn     (`expr e) ->
                 let annotmap, arg = opa_to_data gamma annotmap e in
                 annotmap, arg::args
             | QD.QAnd (q0, q1)
             | QD.QOr  (q0, q1) ->
                 self (self acc q0) q1
             | x -> tra acc x
            ) (annotmap, []) sql_ops
    in
    let annotmap, args = C.rev_list (annotmap, gamma) args in
    let build = if uniq then Api.Db.build_uniq else Api.Db.build_dbset in
    let annotmap, build =
      OpaMapToIdent.typed_val ~label ~ty:[node.S.ty]
        build annotmap gamma
    in
    let annotmap, dbset = C.apply gamma annotmap build [database; qid; args] in
    {env with annotmap}, dbset


  (* ******************************************************)
  (* UPDATING *********************************************)
  (* ******************************************************)
  let pp_update ~tbl pp_expr fmt (u:_ QmlAst.Db.update) =
    Format.fprintf fmt "UPDATE %s SET " tbl;
    match u with
    | QD.UFlds flds ->
        Format.pp_list ", "
          (fun fmt (s, u) ->
             Format.fprintf fmt "%a = " pp_postgres_field s;
             match u with
             | QD.UIncr e -> Format.fprintf fmt "%a + %a" pp_postgres_field s pp_expr e;
             | QD.UExpr e -> Format.fprintf fmt "%a" pp_expr e;
             | _ -> assert false
          ) fmt flds
    | QD.UExpr _
    | QD.UIncr _ -> assert false
    | QD.UId _
    | QD.UAppend _
    | QD.UAppendAll _
    | QD.URemove _
    | QD.URemoveAll _
    | QD.UPop
    | QD.UShift ->
        (* External table? Embedded? Mixed? *)
        assert false

  let pp_insert ~tbl pp_expr fmt (u:_ QmlAst.Db.update) =
    match u with
    | QD.UFlds flds ->
        Format.fprintf fmt "INSERT INTO %s(%a) VALUES(" tbl
          (Format.pp_list ", "
             (fun fmt (s, _u) -> pp_postgres_field fmt s))
          flds;
        Format.pp_list ", "
          (fun fmt (_s, u) ->
             match u with
             | QD.UIncr e
             | QD.UExpr e -> pp_expr fmt e;
             | _ -> assert false
          ) fmt flds;
        Format.fprintf fmt ")"
    | QD.UExpr _
    | QD.UIncr _ -> assert false
    | QD.UId _
    | QD.UAppend _
    | QD.UAppendAll _
    | QD.URemove _
    | QD.URemoveAll _
    | QD.UPop
    | QD.UShift ->
        (* External table? Embedded? Mixed? *)
        assert false

  let pp_postgres_insert_or_update env ~name ~tbl fmt q (u:_ QmlAst.Db.update) =
    let annotmap = env.annotmap in
    let aset = ref AnnotSet.empty in
    let pp_annot fmt i = Format.fprintf fmt "x%d" (Annot.to_int i) in
    let pp_expr fmt e =
      let annot = Annot.annot (Q.Label.expr e) in
      aset := AnnotSet.add annot !aset;
      pp_annot fmt annot
    in
    let pp x = Format.fprintf fmt x in
    pp "CREATE OR REPLACE FUNCTION %s(" name;
    let buffer = Buffer.create 256 in
    let fmt2 = Format.formatter_of_buffer buffer in
    let pp2 x = Format.fprintf fmt2 x in
    pp2 ")RETURNS VOID AS\n";
    pp2 "$$\n";
    pp2 "BEGIN\n";
    pp2 "  LOOP\n";
    pp2 "    %a " (pp_update ~tbl pp_expr) u;
    begin match q with
    | Some (q, _) -> pp_postgres_genquery pp_expr fmt2 q;
    | _ -> ()
    end;
    pp2 ";\n";
    pp2 "    IF found THEN\n";
    pp2 "      RETURN;\n";
    pp2 "    END IF;\n";
    pp2 "    BEGIN\n";
    pp2 "      %a;\n" (pp_insert ~tbl pp_expr) u;
    pp2 "    EXCEPTION WHEN unique_violation THEN\n";
    pp2 "    END;\n";
    pp2 "  END LOOP;\n";
    pp2 "END;\n";
    pp2 "$$\n";
    pp2 "LANGUAGE plpgsql;\n";
    Format.pp_print_flush fmt2 ();
    let () =
      let pp_elt fmt a =
        Format.fprintf fmt "%a %a"
          pp_annot a
          (pp_type_as_pgtype env) (QmlAnnotMap.find_ty a annotmap)
      in
      let max = AnnotSet.max_elt !aset in
      let set = AnnotSet.remove max !aset in
      AnnotSet.pp ", " pp_elt fmt set;
      pp_elt fmt max
    in
    Format.pp_print_string fmt (Buffer.contents buffer);
    ()

  let prepared_statement_for_update =
    let fresh_id =
      let fresh = Fresh.fresh_factory (fun x -> x) in
      fun () -> Format.sprintf "update_%d" (fresh ())
    in
    fun
      ({annotmap; u_prepared; _} as env)
      ~tbl
      query
      (update, update_options) ->
        let buffer = Buffer.create 256 in
        let fmt = Format.formatter_of_buffer buffer in
        let uid = fresh_id () in
        pp_postgres_insert_or_update env ~name:uid ~tbl fmt query update;
        (* TODO OPTIONS *)
        ignore (update_options);
        Format.pp_print_flush fmt ();
        Format.eprintf "%s\n%!" (Buffer.contents buffer);
        let u_prepared = UpdateMap.add (query, update) (uid, Buffer.contents buffer) u_prepared in
        {env with annotmap; u_prepared}

  let execute_statement_for_update =
    fun
      ({gamma; annotmap; u_prepared; _} as env)
      node query (update, _update_options) ->
        let procname, _ = UpdateMap.find (query, update) u_prepared in
        let amap = AnnotMap.empty in
        let amap =
          match query with
          | None -> amap
          | Some (query , _) ->
              QmlAstWalk.DbWalk.Query.fold
                (fun amap -> function
                 | QD.QEq e | QD.QGt e | QD.QLt e | QD.QGte e | QD.QLte e
                 | QD.QNe e | QD.QIn e ->
                     let annot = Annot.annot (Q.Label.expr e) in
                     AnnotMap.add annot e amap
                 | _ -> amap
                ) AnnotMap.empty query
        in
        let amap =
          QmlAstWalk.DbWalk.Update.fold
            (fun amap -> function
             | QD.UExpr e
             | QD.UIncr e
             | QD.UAppend    e
             | QD.UAppendAll e
             | QD.URemove    e
             | QD.URemoveAll e
             | QD.UId (e, _) ->
                 let annot = Annot.annot (Q.Label.expr e) in
                 AnnotMap.add annot e amap
             | _ -> amap
            ) amap update
        in
        let annotmap, database =
          C.ident annotmap node.S.database.S.ident node.S.database.S.dbty in
        let annotmap, procname =
          C.string annotmap procname in
        let annotmap, args =
          AnnotMap.fold
            (fun _ expr (annotmap, args) ->
               let annotmap, arg = opa_to_data gamma annotmap expr in
               annotmap, arg::args
            ) amap (annotmap, [])
        in
        let annotmap, args =
          C.rev_list (annotmap, gamma) args in
        let annotmap, update_or_insert =
          OpaMapToIdent.typed_val ~label Api.Db.update_or_insert annotmap gamma in
        let annotmap, res =
          C.apply gamma annotmap update_or_insert [database; procname; args] in
        {env with annotmap}, res

  let query_to_sqlquery tbl query =
    let rec aux q =
      let binop q0 q1 rb = rb (aux q0) (aux q1) in
      match q with
      | QD.QEq e  -> QD.QEq (`expr e)
      | QD.QGt e  -> QD.QGt (`expr e)
      | QD.QLt e  -> QD.QLt (`expr e)
      | QD.QGte e -> QD.QGte (`expr e)
      | QD.QLte e -> QD.QLte (`expr e)
      | QD.QNe e  -> QD.QNe (`expr e)
      | QD.QIn e  -> QD.QIn (`expr e)
      | QD.QMod i ->  QD.QMod i
      | QD.QOr  (q0, q1) -> binop q0 q1 (fun q0 q1 -> QD.QOr (q0, q1))
      | QD.QAnd (q0, q1) -> binop q0 q1 (fun q0 q1 -> QD.QAnd (q0, q1))
      | QD.QNot  q -> QD.QNot (aux q)
      | QD.QFlds flds ->
          let flds = List.map (fun (s,q) -> (s, aux q)) flds in
          QD.QFlds flds
      | QD.QExists b -> QD.QExists b
    in
    {QD. sql_ops = Option.map aux query; sql_tbs = [tbl]; sql_fds = []}

  let resolve_sqlaccess env node (uniq, query) =
    (* TODO  - Prepare for uniq ? *)
    let env = prepared_statement_for_query env query in
    execute_statement env node (uniq, query)

  let path ~context
      ({gamma; schema; _} as env)
      (label, dbpath, kind, select)
      =
    let node = get_node ~context gamma schema dbpath in
    match node.S.database.S.options.QD.backend with
    | `postgres ->
        begin
          match kind, node.S.kind with
          | QD.Default, S.SqlAccess query ->
              resolve_sqlaccess env node (false, query)
          | QD.Default, S.SetAccess (S.DbSet _, [tbl], query, _) ->
              let uniq, query =
                match query with
                | None ->
                    false, (query_to_sqlquery tbl None, QD.default_query_options)
                | Some (uniq, (q, o)) ->
                    uniq, (query_to_sqlquery tbl (Some q), o)
              in
              resolve_sqlaccess env node (uniq, query)
          | QD.Update (upd, opt), S.SetAccess (S.DbSet _, [tbl], query, _) ->
              let query =
                match query with
                | None -> None
                | Some (true, q) -> Some q
                | _ -> assert false
              in
              let env = prepared_statement_for_update env ~tbl query (upd, opt) in
              execute_statement_for_update env node query (upd, opt)
          | _ -> assert false
        end
    | _ -> env, Q.Path (label, dbpath, kind, select)

  let table
      ({gamma; tb_init; _} as env)
      path ty lidx =
    Format.eprintf "Generating table %a with %a\n%!"
      pp_table_name path
      QmlPrint.pp#ty ty;
    let rec type_from_ty env tpath ty =
      match ty with
      | Q.TypeRecord Q.TyRow (fields, _) ->
          let tra env = List.fold_left
            (fun env (s, t) -> type_from_ty env (s::tpath) t)
            env fields
          in
          begin match tpath with
          | [] -> tra env (*First level: don't create a composite type *)
          | _ ->
              let env = tra env in
              let buffer = Buffer.create 256 in
              let fmt = Format.formatter_of_buffer buffer in
              let tpath = List.rev (tpath@path) in
              let name = Format.sprintf "%a" pp_table_name tpath in
              Format.fprintf fmt "CREATE TYPE %s AS (" name;
              Format.pp_list ","
                (fun fmt (s, t) ->
                   Format.fprintf fmt "%s %a" s (pp_type_as_pgtype ~path:(tpath@[s]) env) t;
                ) fmt fields;
              Format.fprintf fmt ")";
              Format.pp_print_flush fmt ();
              let q = Buffer.contents buffer in
              Format.eprintf "Added path [%a]\n%!" (Format.pp_list "," Format.pp_print_string) tpath;
              {env with ty_init = StringListMap.add tpath (`type_ (name, q)) env.ty_init}
          end
      | Q.TypeName _ ->
          type_from_ty env tpath (QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty)
      | Q.TypeConst _ -> env
      | _ -> OManager.i_error
          "Type %a is not yet handled by postgres generator\n"
            QmlPrint.pp#ty ty
    in
    let rec table_from_ty env ty =
      match ty with
      | Q.TypeRecord Q.TyRow (fields , None) ->
          let buffer = Buffer.create 256 in
          let fmt = Format.formatter_of_buffer buffer in
          Format.fprintf fmt "CREATE TABLE %a("
            pp_table_name path;
          let rec aux_field fmt (s, ty) =
            Format.fprintf fmt "%s %a" s (pp_type_as_pgtype ~path:(List.rev (s::path)) env) ty
          in
          let env = List.fold_left
            (fun env (field, ty) ->
               Format.fprintf fmt "%a, " aux_field (field, ty);
               env
            ) env fields
          in
          Format.pp_list ","
            (fun fmt idx ->
               Format.fprintf fmt " PRIMARY KEY(%a)"
                 (Format.pp_list "," Format.pp_print_string) idx)
            fmt lidx;
          Format.fprintf fmt ")";
          Format.pp_print_flush fmt ();
          {env with tb_init = (Buffer.contents buffer)::tb_init}
      | Q.TypeRecord _ -> assert false
      | Q.TypeName _ ->
          table_from_ty env (QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty)
      | _ -> assert false
    in
    table_from_ty (type_from_ty env [] ty) ty

end

let process_path env code =
  let fmap tra env = function
    | Q.Path (label, path, kind, select) as expr ->
        let context = QmlError.Context.annoted_expr env.annotmap expr in
        let env, result =
          Generator.path ~context env (label, path, kind, select) in
        tra env result
    | e -> tra env e
  in
  QmlAstWalk.CodeExpr.fold_map
    (fun env expr ->
       let env, expr = QmlAstWalk.Expr.traverse_foldmap fmap env expr in
       fmap (fun a e -> a,e) env expr)
    env code

let init_database env =
  List.fold_left
    (fun (env, newvals) database ->
       if database.S.options.QD.backend = `postgres
         && database.S.package = ObjectFiles.get_current_package_name () then
           let ident = database.S.ident in
           let name = database.S.name in
           let env, open_ = Generator.database env name in
           (env, (Q.NewVal (label, [ident, open_]))::newvals)
       else (env, newvals)
    )
    (env, []) (S.get_db_declaration env.schema)

let init_declaration ({gamma; schema; _} as env) code =
  List.fold_left_filter_map
    (fun env -> function
     | Q.Database _ -> env, None
     | Q.NewDbValue
         (_, (QD.Db_TypeDecl (p, _)
          | QD.Db_Default    (p, _)
          | QD.Db_Alias      (p, _)
          | QD.Db_Constraint (p, _)
          | QD.Db_Virtual    (p, _) as decl)) ->
         begin match p with
         | QD.Decl_fld k::_ ->
             if (S.get_node gamma schema [QD.FldKey k]).S.database.S.options.QD.backend
               <> `postgres then env, None
             else (
               match decl with
               | QD.Db_TypeDecl ((QD.Decl_fld _)::p, ty) ->
                   let rec aux rpath p = match p with
                     | (QD.Decl_set lidx)::[] ->
                         Generator.table env rpath ty lidx, None
                     | (QD.Decl_set _lidx)::_ -> assert false
                     | (QD.Decl_fld str)::p -> aux (str::rpath) p
                     | [] -> env, None
                     | _ -> assert false
                   in aux [] p
               | _ -> env, None
             )
         | _ -> assert false
         end
     | x -> env, Some x) env  code

let process_code ~stdlib_gamma gamma annotmap schema code =
  match ObjectFiles.compilation_mode () with
  | `init -> (annotmap, code)
  | _ ->
      let gamma = QmlTypes.Env.unsafe_append stdlib_gamma gamma in
      let env = Generator.make_env gamma annotmap schema in
      let env, code = init_declaration env code in
      let env, code = process_path env code in
      let env, vals = init_database env in
      (env.annotmap, vals@code)

