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

type env = {
  (* (id of prepared statement, string query) *)
  prepared : (string * string) QueryMap.t;
  gamma : QmlTypes.gamma;
  annotmap : Q.annotmap;
  schema : S.t
}

module Generator =
struct

  let make_env gamma annotmap schema = {
    prepared = QueryMap.empty;
    gamma; annotmap; schema;
  }

  let get_node ~context schema path =
    try
      S.get_node schema path
    with Base.NotImplemented s ->
      QmlError.error context
        "Can't generates postgres access because : %s is not yet implemented"
        s

  let pp_postgres_field =
    Format.pp_list "."
      (fun fmt -> function | `string s -> Format.pp_print_string fmt s | _ -> assert false)

  let pp_postgres_query fmt (q:QmlAst.expr QmlAst.Db.sqlquery) =
    let pos = ref 0 in
    let rec aux fmt q =
      let pp x = Format.fprintf fmt x in
      let pp_expr fmt = function
        | `expr _ -> incr pos; pp "$%d" !pos
        | `bind s -> Format.pp_print_string fmt s
      in
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
    pp " WHERE ";
    aux fmt q.QD.sql_ops

  let database
      ({gamma; annotmap; prepared; _} as env)
      name =
    let annotmap, open_ = OpaMapToIdent.typed_val ~label Api.Db.open_ annotmap gamma in
    let annotmap, name = C.string annotmap name in
    let annotmap, statements =
      let annotmap, statements = QueryMap.fold
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
        ) prepared (annotmap, [])
      in
      C.list (annotmap, gamma) statements
    in
    let annotmap, pgdb = C.apply gamma annotmap open_ [name; statements] in
    {env with annotmap}, pgdb

  let prepared_statement =
    let fresh_id =
      let fresh = Fresh.fresh_factory (fun x -> x) in
      fun () -> Format.sprintf "prepared_%d" (fresh ())
    in
    fun
      ({gamma=_; annotmap; prepared; _} as env)
      ((sqlquery, options) as query) ->
      let buffer = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer buffer in
      pp_postgres_query fmt sqlquery;
      (* TODO OPTIONS *)
      ignore options;

      Format.pp_print_flush fmt ();
      let qid = fresh_id () in
      let prepared = QueryMap.add query (qid, Buffer.contents buffer) prepared in
      {env with annotmap; prepared}

  let execute_statement
      ({gamma; annotmap; prepared; _} as env)
      node query =
    let qid, _ = try QueryMap.find query prepared with
        Not_found -> OManager.i_error "Can't found prepared statement"
    in
    let annotmap, database =
      C.ident annotmap node.S.database.S.ident node.S.database.S.dbty in
    let annotmap, qid = C.string annotmap qid in
    let annotmap, args = (* see type Pack.u *)
      QmlAstWalk.DbWalk.Query.self_traverse_fold
        (fun self tra ((annotmap, args) as acc) -> function
         | QD.QEq     (`expr e)
         | QD.QGt     (`expr e)
         | QD.QLt     (`expr e)
         | QD.QGte    (`expr e)
         | QD.QLte    (`expr e)
         | QD.QNe     (`expr e)
         | QD.QIn     (`expr e) ->
             let ty = QmlAnnotMap.find_ty (Annot.annot (Q.Label.expr e)) annotmap in
             begin match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
             | Q.TypeConst c ->
                 let fld = match c with
                   | Q.TyNull   -> assert false
                   | Q.TyFloat  -> "Float"
                   | Q.TyInt    -> "Int"
                   | Q.TyString -> "Cstring"
                 in
                 let annotmap, arg = C.record annotmap [fld, e] in
                 annotmap, arg::args
             | ty -> OManager.i_error "expression of type @{<bright>%a@} in sql query are not yet implemented or unexpected" QmlPrint.pp#ty ty
             end
         | QD.QAnd (q0, q1)
         | QD.QOr  (q0, q1) ->
             self (self acc q0) q1
         | x -> tra acc x
        ) (annotmap, []) (fst query).QD.sql_ops
    in
    let annotmap, args = C.rev_list (annotmap, gamma) args in
    let annotmap, build =
      OpaMapToIdent.typed_val ~label ~ty:[node.S.ty]
        Api.Db.build_dbset annotmap gamma
    in
    let annotmap, dbset = C.apply gamma annotmap build [database; qid; args] in
    {env with annotmap}, dbset

  let path ~context
      ({schema; _} as env)
      (label, dbpath, kind, select)
      =
    let node = get_node ~context schema dbpath in
    match node.S.database.S.options.QD.backend with
    | `postgres ->
        begin
          match node.S.kind with
          | S.SqlAccess query ->
              let env = prepared_statement env query in
              execute_statement env node query
          | _ -> assert false
        end
    | _ -> env, Q.Path (label, dbpath, kind, select)

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

let clean_declaration {schema; _} code=
  List.filter
    (function
     | Q.Database _ -> false
     | Q.NewDbValue
         (_, (QD.Db_TypeDecl (p, _)
          | QD.Db_Default    (p, _)
          | QD.Db_Alias      (p, _)
          | QD.Db_Constraint (p, _)
          | QD.Db_Virtual    (p, _))) ->
         begin match p with
         | QD.Decl_fld k::_ ->
             (S.get_node schema [QD.FldKey k]).S.database.S.options.QD.backend <> `postgres
         | _ -> assert false
         end
     | _ -> true) code

let process_code ~stdlib_gamma gamma annotmap schema code =
  match ObjectFiles.compilation_mode () with
  | `init -> (annotmap, code)
  | _ ->
      let gamma = QmlTypes.Env.unsafe_append stdlib_gamma gamma in
      let env = Generator.make_env gamma annotmap schema in
      let code = clean_declaration env code in
      let env, code = process_path env code in
      let env, vals = init_database env in
      (env.annotmap, vals@code)

