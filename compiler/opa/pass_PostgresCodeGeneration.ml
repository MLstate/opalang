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

  module Option = Opacapi.Option

  module DbSet = Opacapi.DbSet

  let serialize = Opacapi.OpaSerialize.serialize

  module Types = Opacapi.Types

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
  ty_init : [ `foreign of string | `type_ of ([`enum | `composite of string list] * string * string) | `blob ] StringListMap.t;
  (* List of queries which create table *)
  tb_init : string list;
  (* tb_default *)
  tb_default : Q.ty StringListMap.t;
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

  let pg_types = [
    (Api.Types.binary,     "Bytea",           "bytea");
    (Api.Types.bool,       "Bool",            "boolean");
  ]

  let get_pg_native_type gamma ty =
    let rec aux t =
      match t with
      | Q.TypeName ([a], s) when Q.TypeIdent.to_string s = Api.Types.list ->
          let list_error () =
            failwith
              (Format.sprintf
                 "list of %a are not yet handled by postgres generator"
                 QmlPrint.pp#ty a)
          in
          begin match aux a with
          | Some (_, "TEXT") -> Some ("StringArray1", "TEXT[]")
          | Some (_, "FLOAT") -> Some ("FloatArray1", "FLOAT[]")
          | Some (_, "INT8") -> Some ("IntArray1", "INT8[]")
          | _ -> list_error ()
          end
      | Q.TypeName (l, s) ->
          let st = Q.TypeIdent.to_string s in
          begin match List.find_opt (fun (x,_,_) -> x = st) pg_types with
          | None ->
              aux (QmlTypesUtils.Inspect.find_and_specialize gamma s l)
          | Some (_,x,y) -> Some (x, y)
          end
      | Q.TypeConst Q.TyFloat  -> Some ("Float", "FLOAT")
      | Q.TypeConst Q.TyInt    -> Some ("Int", "INT8")
      | Q.TypeConst Q.TyString -> Some ("String", "TEXT")
      | _ -> None
    in
    aux ty

  let make_env gamma annotmap schema = {
    tb_init = ["CREATE LANGUAGE plpgsql"];
    ty_init = StringListMap.empty;
    tb_default = StringListMap.empty;
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

  let opa_to_data
      ({gamma; annotmap; ty_init; _} as env)
      path expr =
    let ty = QmlAnnotMap.find_ty (Annot.annot (Q.Label.expr expr)) annotmap in
    match StringListMap.find_opt path ty_init with
    | Some (`type_ (`enum, _, _)) ->
        let annotmap, sum_to_enum =
          OpaMapToIdent.typed_val ~label ~ty:[ty] Api.Db.sum_to_enum annotmap gamma in
        let annotmap, enum =
          C.apply gamma annotmap sum_to_enum [expr] in
        let annotmap, e = C.record annotmap ["String", enum] in
        {env with annotmap}, e
    | Some `blob ->
        let annotmap, ser = OpaMapToIdent.typed_val ~ty:[ty] ~label Api.serialize annotmap gamma in
        let annotmap, e = C.apply gamma annotmap ser [expr] in
        let annotmap, e = C.record annotmap ["String", e] in
        {env with annotmap}, e
    | Some _ -> assert false
    | None ->
        match get_pg_native_type gamma ty with
        | Some (fld, _) ->
            let annotmap, e = C.record annotmap [fld, expr] in
            {env with annotmap}, e
        | None ->
            Format.eprintf "%a %a\n%!" QmlPrint.pp#ty ty (Format.pp_list "," Format.pp_print_string) path;
            raise Not_found

  let pp_type_as_pgtype ?(path=[]) env fmt ty =
    match StringListMap.find_opt path env.ty_init with
    | Some (`foreign _) -> assert false
    | Some (`type_ (_,name,_)) -> Format.pp_print_string fmt name
    | Some (`blob) -> Format.pp_print_string fmt "TEXT"
    | None ->
        match get_pg_native_type env.gamma ty with
        | Some (_, t) -> Format.pp_print_string fmt t
        | None ->
            Format.eprintf "%a\n%!" (Format.pp_list "." Format.pp_print_string) path;
            raise Not_found


  (* ******************************************************)
  (* QUERYING *********************************************)
  (* ******************************************************)
  let pp_postgres_genquery pp_expr fmt (q:(_, _) QmlAst.Db.query) =
    let rec aux fmt q =
      let pp x = Format.fprintf fmt x in
      match q with
      | QD.QEq   e    -> pp " = %a" (pp_expr []) e
      | QD.QGt   e    -> pp " > %a" (pp_expr []) e
      | QD.QLt   e    -> pp " < %a" (pp_expr []) e
      | QD.QGte  e    -> pp " >= %a" (pp_expr []) e
      | QD.QLte  e    -> pp " <= %a" (pp_expr []) e
      | QD.QNe   e    -> pp " <> %a" (pp_expr []) e
      | QD.QIn   e    -> pp " = ANY (%a)" (pp_expr []) e
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
         BaseFormat.pp_list "," (BaseFormat.pp_list "." Format.pp_print_string)
           fmt q.QD.sql_fds
    );
    pp " FROM ";
    (BaseFormat.pp_list "," Format.pp_print_string) fmt q.QD.sql_tbs;
    match q.QD.sql_ops with
    | None -> ()
    | Some sql_ops ->
        pp_postgres_genquery
          (fun _ fmt -> function
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
      node (kpath, query) =
    let qid, _ = try QueryMap.find query q_prepared with
        Not_found -> OManager.i_error "Can't found prepared statement"
    in
    let annotmap, database =
      C.ident annotmap node.S.database.S.ident node.S.database.S.dbty in
    let annotmap, qid = C.string annotmap qid in
    let env = {env with annotmap} in
    let ({annotmap; _} as env), args =
      match (fst query).QD.sql_ops with
      | None -> env, []
      | Some sql_ops ->
          (* see type Postgres.data *)
          QmlAstWalk.DbWalk.Query.self_traverse_fold
            (fun self tra ((env, args) as acc) -> function
             | QD.QEq     (`expr e)
             | QD.QGt     (`expr e)
             | QD.QLt     (`expr e)
             | QD.QGte    (`expr e)
             | QD.QLte    (`expr e)
             | QD.QNe     (`expr e)
             | QD.QIn     (`expr e) ->
                 let env, arg = opa_to_data env [] e in
                 env, arg::args
             | QD.QAnd (q0, q1)
             | QD.QOr  (q0, q1) ->
                 self (self acc q0) q1
             | x -> tra acc x
            ) (env, []) sql_ops
    in
    let annotmap, args = C.rev_list (annotmap, gamma) args in
    let annotmap, def = node.S.default annotmap in
    let build = match kpath with
      | `dbset   -> Api.Db.build_dbset
      | `uniq    -> Api.Db.build_uniq
      | `option    -> Api.Db.build_option
    in
    let annotmap, build =
      OpaMapToIdent.typed_val ~label ~ty:[node.S.ty]
        build annotmap gamma
    in
    let annotmap, dbset = C.apply gamma annotmap build [database; qid; args; def] in
    {env with annotmap}, dbset


  (* ******************************************************)
  (* UPDATING *********************************************)
  (* ******************************************************)

  let preprocess_update ~tbl ({gamma; annotmap; ty_init; _} as env) u =
    let rec aux path (annotmap, bindings) u =
      match StringListMap.find_opt (List.rev path) ty_init with
      | Some (`type_ (`enum, _, _) | `blob) ->
          let rec to_expr annotmap u =
            match u with
            | QD.UExpr e -> annotmap, e
            | QD.UFlds (flds:(Q.expr, Q.expr QD.update) QD.fields) ->
                let annotmap, flds = List.fold_left_map
                  (fun annotmap (s, u) ->
                     let annotmap, e = to_expr annotmap u in
                     let s = match s with [`string s] -> s | _ -> assert false in
                     annotmap, (s, e)
                  ) annotmap flds
                in C.record annotmap flds
            | _ -> assert false
          in let annotmap, e = to_expr annotmap u in
          (annotmap, bindings), QD.UExpr (e:Q.expr)
      | Some `foreign _ -> assert false
      | _ -> (
          match u with
          | QD.UFlds flds ->
              let (annotmap, bindings), flds = List.fold_left_map
                (fun (annotmap, bindings) (s, u) ->
                   let (annotmap, bindings), u =
                     let p = match s with | [`string s] -> s | _ -> assert false in
                     aux (p::path) (annotmap, bindings) u in
                   (annotmap, bindings), (s, u))
                (annotmap, bindings) flds
              in
              (annotmap, bindings), QD.UFlds flds
          | QD.UExpr e ->
              let ty = QmlAnnotMap.find_ty (Annot.annot (Q.Label.expr e)) annotmap in
              begin match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
              | Q.TypeRecord (Q.TyRow (flds, _)) ->
                  let (annotmap, bindings), flds =
                    List.fold_left_map
                      (fun (annotmap, bindings) (s, ty) ->
                         let annotmap, e = C.dot gamma annotmap e s in
                         let i = Ident.next "udot" in
                         let annotmap, ie = C.ident annotmap i ty in
                         let acc, u = aux (s::path) (annotmap, (i,e)::bindings) (QD.UExpr ie) in
                         acc, ([`string s], u)
                      )
                      (annotmap, bindings) flds
                  in (annotmap, bindings), QD.UFlds flds
              | _ -> (annotmap, bindings), QD.UExpr e
              end
          | QD.UIncr _
          | QD.UId _
          | QD.UAppend _
          | QD.UAppendAll _
          | QD.URemove _
          | QD.URemoveAll _
          | QD.UPop
          | QD.UShift -> (annotmap, bindings), u
        )
    in
    let (annotmap, bindings), u = aux [tbl] (annotmap, []) u in
    {env with annotmap}, bindings, u

  let flatten_fields flds =
    let rec aux rpath acc flds =
      List.fold_left
        (fun acc (s, u) ->
           let s = match s with [`string s] -> s | _ -> assert false in
           match u with
           | QD.UFlds flds -> aux (s::rpath) acc flds
           | u -> (List.rev (s::rpath), u)::acc)
        acc flds
    in aux [] [] flds

  let lexi_fields {ty_init; _} path flds =
    let rec aux  path acc flds =
      List.fold_left
        (fun acc (s, u) ->
           let s = match s with [`string s] -> s | _ -> assert false in
           let path = s::path in
           match StringListMap.find_opt (List.rev path) ty_init with
           | Some (`type_ (`enum, _, _) | `blob) -> StringMap.add s (`op u) acc
           | _ ->
               match StringMap.find_opt s acc with
               | None ->
                   begin match u with
                   | QD.UFlds flds -> StringMap.add s (`sub (aux path StringMap.empty flds)) acc
                   | _ -> StringMap.add s (`op u) acc
                   end
               | Some (`sub sub) ->
                   begin match u with
                   | QD.UFlds flds -> StringMap.add s (`sub (aux path sub flds)) acc
                   | _ -> assert false
                   end
               | Some (`op _) -> assert false
        ) acc flds
    in aux path StringMap.empty flds

  let pp_update ~tbl pp_expr fmt (u:_ QmlAst.Db.update) =
    Format.fprintf fmt "UPDATE %s SET " tbl;
    match u with
    | QD.UFlds flds ->
        let flds = flatten_fields flds in
        Format.pp_list ","
          (fun fmt (p, u) ->
             match u with
             | QD.UIncr e ->
                 Format.fprintf fmt "%a = %a + %a"
                   (Format.pp_list "." Format.pp_print_string) p
                   (Format.pp_list "." Format.pp_print_string) p
                   (pp_expr (tbl::p)) e
             | QD.UExpr e ->
                 Format.fprintf fmt "%a = %a"
                   (Format.pp_list "." Format.pp_print_string) p
                   (pp_expr (tbl::p)) e
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

  let pp_insert env ~tbl pp_expr fmt (u:_ QmlAst.Db.update) =
    Format.eprintf "Type: %a\n%!" (QD.pp_update QmlPrint.pp#expr) u;
    match u with
    | QD.UFlds flds ->
        let start = ref true in
        let lexmap = lexi_fields env [tbl] flds in
        Format.fprintf fmt "INSERT INTO %s(%a) VALUES(" tbl
          (StringMap.pp ""
             (fun fmt s _ ->
                (if !start then start:=false else Format.fprintf fmt " ,");
                Format.pp_print_string fmt s))
          lexmap;
        let rec aux path n =
          match n with
          | `op (QD.UIncr e | QD.UExpr e) ->
              pp_expr (List.rev path) fmt e
          | `op _ -> assert false
          | `sub lexmap ->
              match StringListMap.find_opt (List.rev path) env.ty_init with
              | Some (`type_ (`composite flds, _, _)) ->
                  Format.fprintf fmt "ROW(%a)"
                    (Format.pp_list ", "
                       (fun fmt s ->
                          match StringMap.find_opt s lexmap with
                          | None -> Format.pp_print_string fmt "NULL"
                          | Some n ->
                              aux (s::path) n)
                    ) flds
              | _ -> Format.eprintf "Not found %a : %a\n%!" (Format.pp_list "." Format.pp_print_string) path (StringListMap.pp ":::" (fun fmt k _ -> (Format.pp_list "." Format.pp_print_string) fmt k)) env.ty_init; assert false (* ? *)
        in
        let start = ref true in
        StringMap.pp ""
          (fun fmt s n ->
             (if !start then start:=false else Format.fprintf fmt " ,");
             aux [s;tbl] n)
          fmt lexmap;
        Format.pp_print_string fmt ")"

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
    let aset = ref AnnotMap.empty in
    let pp_annot fmt i = Format.fprintf fmt "x%d" (Annot.to_int i) in
    let pp_expr path fmt e =
      let annot = Annot.annot (Q.Label.expr e) in
      aset := AnnotMap.add annot path !aset;
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
    pp2 "      %a;\n" (pp_insert env ~tbl pp_expr) u;
    pp2 "    EXCEPTION WHEN unique_violation THEN\n";
    pp2 "    END;\n";
    pp2 "  END LOOP;\n";
    pp2 "END;\n";
    pp2 "$$\n";
    pp2 "LANGUAGE plpgsql;\n";
    Format.pp_print_flush fmt2 ();
    let () =
      let pp_elt fmt a path =
        Format.fprintf fmt "%a %a"
          pp_annot a
          (pp_type_as_pgtype ~path env) (QmlAnnotMap.find_ty a annotmap)
      in
      let (max, p) = AnnotMap.max !aset in
      let set = AnnotMap.remove max !aset in
      AnnotMap.pp ", " pp_elt fmt set;
      pp_elt fmt max p
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
      ~tbl
      node query (update, _update_options) ->
        let procname, _ = UpdateMap.find (query, update) u_prepared in
        let (amap:(string list * Q.expr) AnnotMap.t) = AnnotMap.empty in
        let amap =
          match query with
          | None -> amap
          | Some (query , _) ->
              let _, amap =
                QmlAstWalk.DbWalk.Query.self_traverse_fold
                  (fun self tra (path, (amap:(string list * Q.expr) AnnotMap.t)) -> function
                   | QD.QEq e | QD.QGt e | QD.QLt e | QD.QGte e | QD.QLte e
                   | QD.QNe e | QD.QIn e ->
                       let annot = Annot.annot (Q.Label.expr e) in
                       path, AnnotMap.add annot (List.rev path, e) amap
                   | QD.QFlds flds ->
                       path, List.fold_left
                         (fun amap (f, q) ->
                            let s = match f with | [`string s] -> s | _ -> assert false in
                            let _, amap = self (s::path, amap) q in
                            amap
                         ) amap flds
                   | q -> tra (path, amap) q
                  ) ([], AnnotMap.empty) query
              in amap
        in
        let _, amap =
          QmlAstWalk.DbWalk.Update.self_traverse_fold
            (fun self tra (path, amap) -> function
             | QD.UExpr e
             | QD.UIncr e
             | QD.UAppend    e
             | QD.UAppendAll e
             | QD.URemove    e
             | QD.URemoveAll e
             | QD.UId (e, _) ->
                 let annot = Annot.annot (Q.Label.expr e) in
                 path, AnnotMap.add annot (List.rev path, e) amap
             | QD.UFlds flds ->
                 path, List.fold_left
                   (fun amap (f, q) ->
                      let s = match f with | [`string s] -> s | _ -> assert false in
                      let _, amap = self (s::path, amap) q in
                      amap
                   ) amap flds
             | q -> tra (path, amap) q
            ) ([], amap) update
        in
        let annotmap, database =
          C.ident annotmap node.S.database.S.ident node.S.database.S.dbty in
        let annotmap, procname =
          C.string annotmap procname in
        let ({annotmap; _} as env), args =
          AnnotMap.fold
            (fun _ (path, expr) (env, args) ->
               let env, arg = opa_to_data env (tbl::path) expr in
               env, arg::args
            ) amap ({env with annotmap}, [])
        in
        let annotmap, args =
          C.rev_list (annotmap, gamma) args in
        let annotmap, update_or_insert =
          OpaMapToIdent.typed_val ~label Api.Db.update_or_insert annotmap gamma in
        let annotmap, res =
          C.apply gamma annotmap update_or_insert [database; procname; args] in
        {env with annotmap}, res

  let post_projection gamma ~ty path annotmap =
    match path with
    | [] -> None
    | _ -> Some (
        let arg = Ident.next "x" in
        let rec aux path =
          match path with
          | [] -> C.ident annotmap arg ty
          | t::q ->
              let annotmap, e = aux q in
              C.dot gamma annotmap e t
        in
        let annotmap, dots = aux path in
        C.lambda annotmap [(arg, ty)] dots
      )

  let apply_post_projection env postproj kpath expr =
    match postproj env.annotmap with
    | None -> (env, expr)
    | Some (annotmap, f) ->
        let annotmap, r =
          match kpath with
          | `uniq   -> C.apply env.gamma annotmap f [expr]
          | `option ->
              let annotmap, map =
                OpaMapToIdent.typed_val ~label Api.Option.map annotmap env.gamma in
              C.apply env.gamma annotmap map [f; expr]
          | `dbset  ->
              let annotmap, map =
                OpaMapToIdent.typed_val ~label Api.DbSet.map annotmap env.gamma in
              C.apply env.gamma annotmap map [f; expr]
        in {env with annotmap}, r

  let query_to_sqlquery ~ty tbl query select embed =
    let postproj, select = match embed with
      | None ->
          (fun env _kpath expr -> (env, expr)), select
      | Some x ->
          let path = List.map (function QD.FldKey s -> s | _ -> assert false) x in
          let fld = List.map (function QD.FldKey s -> `string s | _ -> assert false) x in
          (fun env kpath expr ->
             apply_post_projection env
               (post_projection ~ty env.gamma path)
               kpath expr
          ),
          QD.SFlds [(fld, select)]
    in
    let sql_fds =
      match select with
      | QD.SId _ | QD.SSlice _->
          OManager.printf "This kind of projection is not yet implemented by PG driver";
          assert false
      | QD.SStar | QD.SNil -> []
      | QD.SFlds flds ->
          List.map
            (fun (field, s) ->
               begin match s with
               | QD.SStar | QD.SNil -> ()
               | _ -> OManager.printf "This kind of projection is not yet implemented by PG driver";
                   assert false
               end;
               tbl::
                 (List.map
                    (function `string s -> s
                     | _ -> OManager.printf "This kind of projection is not yet implemented by PG driver"; assert false
                    ) field
                 )
            ) flds
    in
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
    postproj, {QD. sql_ops = Option.map aux query; sql_tbs = [tbl]; sql_fds}

  let resolve_sqlaccess env node (kpath, query) =
    (* TODO  - Prepare for uniq ? *)
    let env = prepared_statement_for_query env query in
    execute_statement env node (kpath, query)

  let resolve_sqlupdate ~tbl env node query (upd, opt) =
    let env, bindings, upd = preprocess_update ~tbl env upd in
    let env = prepared_statement_for_update env ~tbl query (upd, opt) in
    let env, e = execute_statement_for_update env ~tbl node query (upd, opt) in
    let annotmap, e =
      List.fold_left
        (fun (annotmap, letin) binding ->
           C.letin annotmap [binding] letin
        ) (env.annotmap, e) bindings
    in
    {env with annotmap}, e

  let path ~context
      ({gamma; schema; _} as env)
      (label, dbpath, kind, select)
      =
    let node = get_node ~context gamma schema dbpath in
    match node.S.database.S.options.QD.backend with
    | `postgres ->
        begin
          let setaccess_to_sqlacces tbl query embed =
            let ty = node.S.ty in
            match query with
            | None ->
                let post, query = query_to_sqlquery ~ty tbl None select embed in
                `dbset, post, (query, QD.default_query_options)
            | Some (uniq, (q, o)) ->
                let post, query = query_to_sqlquery ~ty tbl (Some q) select embed in
                (if uniq then `uniq else `dbset), post, (query, o)
          in
          let string_path () =
            List.map
              (function | QD.FldKey str -> str | _ -> assert false)
              (List.tl dbpath)
          in
          let plain_to_sqlaccess node =
            let path = string_path () in
            let ty =
              let rec aux = function
                  [] -> node.S.ty
                | t::q -> Q.TypeRecord (Q.TyRow ([(t, aux q)], None))
              in aux path
            in
            let node = {node with
              S.ty;
              default = fun ?select annotmap ->
                (* TODO: fix select *)
                let rec aux annotmap = function
                  | [] -> node.S.default ?select annotmap
                  | t::q ->
                      let annotmap, e = aux annotmap q in
                      C.record annotmap [(t, e)]
                in aux annotmap path
                       }
            in
            (fun env kpath expr ->
               apply_post_projection env
                 (post_projection env.gamma ~ty path)
                 kpath expr
            ),
            node,
            ({QD. sql_ops=None; sql_tbs=["_default"]; sql_fds=[path]}, QD.default_query_options)
          in
          match kind, node.S.kind with
          | QD.Default, S.SqlAccess query ->
              resolve_sqlaccess env node (`dbset, query)
          | QD.Default, S.SetAccess (S.DbSet _, [tbl], query, embed) ->
              let kpath, post, query = setaccess_to_sqlacces tbl query embed in
              let env, access = resolve_sqlaccess env node (kpath, query) in
              post env kpath access
          | QD.Option, S.SetAccess (S.DbSet _, [tbl], query, embed) ->
              let kpath, post, query = setaccess_to_sqlacces tbl query embed in
              assert (kpath = `uniq);
              let env, access = resolve_sqlaccess env node (`option, query) in
              post env kpath access
          | QD.Update (upd, opt), S.SetAccess (S.DbSet _, [tbl], query, _) ->
              let query =
                match query with
                | None -> None
                | Some (true, q) -> Some q
                | _ -> assert false
              in resolve_sqlupdate ~tbl env node query (upd, opt)

          | QD.Default, S.Plain ->
              let post, node, query = plain_to_sqlaccess node in
              let env, access = resolve_sqlaccess env node (`uniq, query) in
              post env `uniq access
          | QD.Option, S.Plain ->
              let post, node, query = plain_to_sqlaccess node in
              let env, access = resolve_sqlaccess env node (`option, query) in
              post env `option access
          | QD.Update (upd, opt), S.Plain ->
              let strpath = string_path () in
              let annotmap, _0 = C.int env.annotmap 0 in
              let upd = QD.UFlds [
                [`string "_id"], QD.UExpr _0;
                (List.map (fun s -> `string s) strpath), upd
              ] in
              let query = QD.QFlds [[`string "_id"], QD.QEq _0], QD.default_query_options in
              resolve_sqlupdate ~tbl:"_default" {env with annotmap} node (Some query) (upd, opt)
          | _ -> assert false
        end
    | _ -> env, Q.Path (label, dbpath, kind, select)

  let rec type_from_ty env path tpath ty =
    match ty with
    | Q.TypeRecord Q.TyRow (fields, _) ->
        let tra env = List.fold_left
          (fun env (s, t) -> type_from_ty env path (s::tpath) t)
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
            let fields = List.map fst fields in
            Format.eprintf "Added COMPOSITE at [%a]\n%!" (Format.pp_list "," Format.pp_print_string) tpath;
            {env with ty_init = StringListMap.add tpath (`type_ (`composite fields, name, q)) env.ty_init}
        end
    | Q.TypeName (l, s) ->
        if Option.is_some (get_pg_native_type env.gamma ty) then env
        else
          type_from_ty env path tpath (QmlTypesUtils.Inspect.find_and_specialize env.gamma s l)
    | Q.TypeSum (Q.TyCol (cols, _)) ->
        let tpath = List.rev (tpath@path) in
        begin try
          let buffer = Buffer.create 256 in
          let fmt = Format.formatter_of_buffer buffer in
          let name = Format.sprintf "%a" pp_table_name tpath in
          Format.fprintf fmt "CREATE TYPE %s AS ENUM (" name;
          Format.pp_list ","
            (fun fmt flds ->
               match flds with
               | [(case, ty)] when QmlTypesUtils.Inspect.is_type_void env.gamma ty ->
                   Format.fprintf fmt "'%s'" case;
               | _ -> raise Not_found
            ) fmt cols;
          Format.fprintf fmt ")";
          Format.pp_print_flush fmt ();
          let q = Buffer.contents buffer in
          Format.eprintf "Added ENUM at [%a]\n%!" (Format.pp_list "," Format.pp_print_string) tpath;
          {env with ty_init = StringListMap.add tpath (`type_ (`enum, name, q)) env.ty_init}
        with Not_found ->
          Format.eprintf "Added BLOB at [%a]\n%!" (Format.pp_list "," Format.pp_print_string) tpath;
          {env with ty_init = StringListMap.add tpath (`blob) env.ty_init}
        end
    | Q.TypeConst _ -> env
    | _ -> OManager.i_error
        "Type %a is not yet handled by postgres generator\n"
          QmlPrint.pp#ty ty

  let rec table_from_ty
      ({gamma; tb_init; _} as env)
      path ty lidx =
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
        Format.eprintf "TABLE FROM TY %s\n%!" (Buffer.contents buffer);
        {env with tb_init = (Buffer.contents buffer)::tb_init}
    | Q.TypeRecord _ -> assert false
    | Q.TypeName _ ->
        table_from_ty env path (QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty) lidx
    | _ -> assert false

  let database env name =
    (* Create the default table *)
    let ({gamma; annotmap; ty_init; tb_init; q_prepared; u_prepared; _} as env) =
      let mty =
        StringListMap.fold
          (fun path ty acc ->
             Format.eprintf "PATH%a:%a\n%!" (Format.pp_list "." Format.pp_print_string) path QmlPrint.pp#ty ty;
             let rec aux path (acc:[`sub of _ | `ty of _] StringMap.t)=
               match path with
               | [] -> acc
               | [t] -> StringMap.add t (`ty ty) acc
               | t::q ->
                   match StringMap.find_opt t (acc:[`sub of _ | `ty of _] StringMap.t) with
                   | None -> StringMap.add t (`sub (aux q StringMap.empty)) acc
                   | Some (`sub sub) -> StringMap.add t (`sub (aux q sub)) acc
                   | _ -> assert false
             in aux path acc)
          env.tb_default StringMap.empty
      in
      let mty = StringMap.add "_id" (`ty (Q.TypeConst Q.TyInt)) mty in
      let rec aux mty =
        let lty = StringMap.to_list mty in
        Format.eprintf "HERE:%d\n%!" (List.length lty);
        let lty = List.map
          (fun (f, x) -> f, (match x with | `ty ty -> ty | `sub mty -> aux mty))
          lty
        in
        Q.TypeRecord (Q.TyRow (lty, None))
      in
      let ty = aux mty in
      table_from_ty env ["_default"] ty [["_id"]]
    in
    let annotmap, open_ = OpaMapToIdent.typed_val ~label Api.Db.open_ annotmap gamma in
    let annotmap, name = C.string annotmap name in
    let annotmap, tables =
      List.fold_left
        (fun (annotmap, tables) table ->
           let annotmap, table = C.string annotmap table in
           annotmap, table::tables
        ) (annotmap, []) tb_init
    in
    let annotmap, tables =
      StringListMap.fold
        (fun _ kind (annotmap, tys) ->
           match kind with
           | `type_ (_, _, q) ->
               let annotmap, q = C.string annotmap q in
               annotmap, q::tys
           | _ -> (annotmap, tys)
        ) ty_init (annotmap, tables)
    in
    let annotmap, tables = C.list (annotmap, gamma) tables in
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

  let table_default env path ty =
    let env = type_from_ty env path [] ty in
    {env with tb_default = StringListMap.add path ty env.tb_default}

  let table env path ty lidx =
    Format.eprintf "Generating table %a with %a\n%!"
      pp_table_name path
      QmlPrint.pp#ty ty;
    table_from_ty (type_from_ty env path [] ty) path ty lidx

end

let process_path env code =
  let fmap tra env = function
    | Q.Path (label, path, kind, select) as expr ->
        (* (try *)
          let context = QmlError.Context.annoted_expr env.annotmap expr in
          let env, result =
            Generator.path ~context env (label, path, kind, select) in
          tra env result
        (* with e -> *)
        (*   OManager.serror "Error while generates postgres path: %a\n" QmlPrint.pp#expr expr; *)
        (*   raise e) *)
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
                     | [] ->
                         Generator.table_default env (List.rev rpath) ty , None
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

