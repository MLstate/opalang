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
(*
    @author Louis Gesbert
**)

#<Debugvar:DBGEN_DEBUG>

module Format = BaseFormat

module List = BaseList

module Graph = SchemaGraphLib.SchemaGraph.SchemaGraph0

module DbAst = QmlAst.Db

module C = DbGen_common

type engine = [`db3 | `mongo]

let settyp = DbGen_common.settyp

module Args = C.Args

let get_engine = C.get_default

module Sch = Schema_private
module Schema = struct

  type t = Sch.meta_schema

  type database = {
    name : string;
    ident : Ident.t;
    dbty : QmlAst.ty;
    options : QmlAst.Db.options;
    package : ObjectFiles.package_name;
  }

  type query = QmlAst.expr DbAst.query * QmlAst.expr DbAst.query_options

  type set_kind =
    | Map of QmlAst.ty * QmlAst.ty
    | DbSet of QmlAst.ty

  type node_kind =
    | Compose of (string * string list) list
    | Plain
    | Partial of bool (* Inside sum*) * string list * string list
    | SetAccess of set_kind * string list * (bool (*is_unique*) * query) option * QmlAst.path option

  type node = {
    ty : QmlAst.ty;
    kind : node_kind;
    database : database;
    default : ?select:QmlAst.expr DbAst.select -> QmlAst.annotmap -> (QmlAst.annotmap * QmlAst.expr);
  }

  let pp_query fmt = function
    | None -> ()
    | Some (u, (q, o)) ->
        Format.fprintf fmt "[%a%a]/* uniq : %b */"
          (QmlAst.Db.pp_query QmlPrint.pp#expr) q
          (QmlAst.Db.pp_options QmlPrint.pp#expr) o
          u

  let pp_set_kind fmt = function
    | DbSet ty -> Format.fprintf fmt "dbset(%a)" QmlPrint.pp#ty ty
    | Map (kt, vt) -> Format.fprintf fmt "map(%a, %a)" QmlPrint.pp#ty kt QmlPrint.pp#ty vt

  let pp_kind fmt kind =
    let pp_path fmt p =
      List.iter (Format.fprintf fmt "/%s") p;
      Format.fprintf fmt "/";
    in
    match kind with
    | Plain -> Format.fprintf fmt "plain"
    | Partial (b, p0, p1) -> Format.fprintf fmt "partial (%b, %a, %a)" b pp_path p0 pp_path p1
    | Compose cmp ->
        Format.fprintf fmt "compose(%a)"
          (Format.pp_list "; " (fun fmt (f, p) -> Format.fprintf fmt "%s:[%a]" f pp_path p)) cmp
    | SetAccess (sk, path, query, epath) ->
        Format.fprintf fmt "@[<hov>access to %a : %a @. with : %a @. embedded path : %a@]"
          pp_path path
          pp_set_kind sk
          pp_query query
          (Option.pp (DbAst.pp_path_elts QmlPrint.pp#expr)) epath

  let pp_node fmt node =
    Format.fprintf fmt "{@[<hov>type : %a; @. kind : %a; ...@]}"
      QmlPrint.pp#ty node.ty
      pp_kind node.kind

  let mapi = Sch.mapi
  let initial = Sch.initial
  let is_empty = Sch.is_empty_or_unused
  let register_path = Sch.register_path
  let register_default = Sch.register_default
  let register_db_declaration = Sch.register_db_declaration
  let register_new_db_value = Sch.register_new_db_value
(*   let get_type_of_path = get_type_of_path *)
  (* let preprocess_path = preprocess_path *)
  let preprocess_paths_expr = Sch.preprocess_paths_expr
  let preprocess_paths_code_elt = Sch.preprocess_paths_code_elt
  let preprocess_paths_ast = Sch.preprocess_paths_ast
  let finalize = Sch.finalize
  let of_package = Sch.of_package
  let merge = Sch.merge
  let map_types = Sch.map_types
  let map_expr = Sch.map_expr
  let fold_expr = Sch.fold_expr
  let foldmap_expr = Sch.foldmap_expr
  let from_gml s =
    StringListMap.singleton []
      ({ Sch.ident = Ident.next "dummy_from_gml";
         Sch.ty = C.Db.t ();
         Sch.context = QmlError.Context.pos (FilePos.nopos "built from gml");
         Sch.path_aliases = [];
         Sch.options = {DbAst.backend = `db3};
         Sch.schema = Schema_io.from_gml_string s;
         Sch.package = "dummy_from_gml";
         Sch.virtual_path = Sch.PathMap.empty;
       })
  let to_dot t chan =
    StringListMap.iter
      (fun _key db_def ->
         (* output_string chan (String.concat "/" key); *)
         (* output_char chan '\n'; *)
         Schema_io.to_dot db_def.Sch.schema chan)
      t

  let find_db_def t db_ident_opt =
    if StringListMap.size t = 1 && db_ident_opt = None
    then StringListMap.min t
    else
      StringListMap.min (* may raise Not_found *)
        (StringListMap.filter_val
           (fun db_def -> db_ident_opt = Some (Ident.original_name db_def.Sch.ident))
           t)
  let db_to_dot t db_ident_opt chan =
    let _, db_def = find_db_def t db_ident_opt in
    Schema_io.to_dot db_def.Sch.schema chan
  let db_to_gml t db_ident_opt chan =
    let _, db_def = find_db_def t db_ident_opt in
    Schema_io.to_gml db_def.Sch.schema chan

  let db_declaration = Sch.db_declaration

  let decl_to_db name decl =
    {
      name;
      ident = decl.Sch.ident;
      dbty = decl.Sch.ty;
      options = decl.Sch.options;
      package = decl.Sch.package;
    }

  let get_db_declaration schema =
    let decls = Sch.get_db_declaration schema in
    List.map (fun (decl, name) -> decl_to_db name decl) decls

  let get_database schema name =
    let declaration = db_declaration schema name in
    decl_to_db name declaration

  exception Vertex of Graph.vertex

  let get_root schema = try
    Graph.iter_vertex (fun v -> if SchemaGraphLib.is_root v then (raise (Vertex v))) schema;
    OManager.i_error "Don't find the root node on database schema";
  with Vertex v -> v


  (** Get the next node on given [schema] according to the path
      [fragment]. *)
  let rec next schema node fragment =
    let can_succ e =
      match (fragment, e.C.label) with
      | (DbAst.FldKey s0, C.Field (s1, _)) when s0 = s1 -> true
      | (DbAst.FldKey _s0, _) -> false
      | (DbAst.ExprKey _, C.Multi_edge _) -> true
      | (DbAst.Query _, C.Multi_edge _) -> true
      | (DbAst.NewKey, _) -> true
      | _ -> assert false (* TODO *)
    in
    let v = match (Graph.V.label node).C.nlabel with
    | C.Sum ->
        Graph.fold_succ
          (fun node acc -> try
             let e = next schema node fragment in
             match acc with
             | None -> Some e
             | Some _ -> assert false
           with Not_found -> acc)
          schema node None
    | _ ->
        let edge = Graph.fold_succ_e
          (fun edge ->
             let (_, e, _) = edge in
             function
               | None when can_succ e -> Some edge
               | Some _ when can_succ e -> assert false
               | x -> x
          ) schema node None
        in Option.map Graph.E.dst edge
    in
    match v with
    | None -> raise Not_found
    | Some v ->
        match (Graph.V.label v).C.nlabel with
        | C.Hidden -> SchemaGraphLib.SchemaGraph.unique_next schema v
        | _ -> v

  let is_sum node =
    match (Graph.V.label node).C.nlabel with
    | C.Sum -> true
    | _ -> false

  let get_node (schema:t) path =
    #<If>
      Format.eprintf "Get node : @[with path %a@]@\n" QmlPrint.pp#path_elts path;
    #<End>;
    let dbname, declaration, path =
      try
        let dbname, path= match path with
        | DbAst.FldKey k::path -> k, path
        | _ -> assert false (* TODO *)
        in
        dbname, db_declaration schema dbname, path
      with Not_found ->
        "_no_name", db_declaration schema "_no_name", path
    in
    let database = get_database schema dbname in
    let llschema = declaration.Sch.schema in
    let find_next_step (node, kind, path) fragment =
      let next = next llschema node fragment in
      let get_setkind schema node =
        match Graph.succ_e schema node with
        | [edge] ->
            let next = Graph.E.dst edge in
            begin match (Graph.E.label edge).C.label with
            | C.Multi_edge C.Kint ->
                Map (QmlAst.TypeConst QmlAst.TyInt, next.C.ty)
            | C.Multi_edge C.Kstring ->
                Map (QmlAst.TypeConst QmlAst.TyString, next.C.ty)
            | C.Multi_edge (C.Kfields _) -> DbSet next.C.ty
            | _ -> assert false
            end
        | [] -> OManager.i_error "Found any successors from a multi node"
        | _ -> OManager.i_error "Found multiple successors from a multi node"
      in
      match fragment with
      | DbAst.ExprKey expr ->
          let setkind = get_setkind llschema node in
          let options = {DbAst.limit = None; skip = None; sort = None} in
          let kind = SetAccess (setkind, path, Some (true, (DbAst.QEq expr, options)), None) in
          (next, kind, path)

      | DbAst.FldKey key ->
          let kind =
            let nlabel = Graph.V.label next in
            match nlabel.C.nlabel with
            | C.Multi -> SetAccess (get_setkind llschema next, key::path, None, None)
            | _ ->
                match kind, nlabel.C.plain with
                | Compose _, true -> Plain
                | Compose c, false -> Compose c
                | Partial (sum, path, part), _ ->
                    Partial (sum && is_sum node, path, key::part)
                | Plain, _ -> Partial (is_sum node, path, key::[])
                | SetAccess _, _ -> raise (Base.NotImplemented "Selection inside a multi node")
          in let path = key::path
          in (next, kind, path)
      | DbAst.Query (query, options) ->
          begin match kind with
          | SetAccess (_k, path, None, _) ->
              let uniq = Sch.is_uniq llschema node query in
              let kind = SetAccess (get_setkind llschema node, path,
                                    Some (uniq, (query, options)), None) in
              (next, kind, path)
          | SetAccess (_, _path, Some _, _) ->
              raise (Base.NotImplemented "Selection inside a multi node")
          | _ ->
              raise (Base.NotImplemented "Query in a non multi node")
          end
      | DbAst.NewKey -> raise (Base.NotImplemented "New key")
    in
    let node, kind =
      let rec find path ((node, kind, _) as x) =
        match (path, kind) with
        | [], _ -> node, kind
        | _::_, SetAccess (k, p, (Some _ as q), None) -> node, SetAccess(k, p, q, Some path)
        | t::q, _ -> find q (find_next_step x t)
      in find path (get_root llschema, Compose [], [])
    in
    let kind =
      match kind with
      | Compose _ -> (
          match (Graph.V.label node).C.nlabel with
          | C.Product ->
              let path = List.map
                (function
                   | DbAst.FldKey k -> k
                   | _ -> assert false) path in
              Compose (List.map
                         (fun edge ->
                            let sname = SchemaGraphLib.fieldname_of_edge edge
                            in sname, path @ [sname])
                         (Graph.succ_e llschema node)
                      )
          | _ -> assert false
        )
      | Partial (sum, path, part) ->
          Partial (sum, List.rev path, List.rev part)
      | SetAccess (k, path, query, epath) ->
          SetAccess (k, List.rev path, query, epath)
      | Plain -> Plain
    in
    let default =
      let node =
        match kind with
        | SetAccess (_, _, None, _) -> SchemaGraphLib.SchemaGraph.unique_next llschema node
        | _ -> node
      in
      fun ?select annotmap ->
      let (annotmap2, expr) =
        DbGen_private.Default.expr
          ?select
          annotmap
          llschema
          node
      in
      QmlAnnotMap.merge annotmap annotmap2, expr
    in
    let r = {
      database; kind; default;
      ty = node.DbGen_common.ty;
    } in
    #<If>
      Format.eprintf "@[Got : %a@]@\n" pp_node r;
    #<End>;
    r


  module HacksForPositions = Sch.HacksForPositions
end

module Utils = struct

  let rec type_of_selected gamma ty select =
    let res = match select with
      | DbAst.SNil | DbAst.SStar | DbAst.SSlice _ -> ty
      | DbAst.SId (_id, s) ->
        begin match QmlTypesUtils.Inspect.follow_alias_noopt_private ~until:"ordered_map" gamma ty with
          | QmlAst.TypeName ([_; dty; _], _) ->  type_of_selected gamma dty s
          | ty2 -> OManager.i_error "Try to select an id on %a %a" QmlPrint.pp#ty ty QmlPrint.pp#ty ty2
          end

      | DbAst.SFlds sflds ->
          let ty = QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty in
          match ty with
          | QmlAst.TypeRecord (QmlAst.TyRow (rflds, rv)) ->
              QmlAst.TypeRecord
                (QmlAst.TyRow
                   ((List.filter_map
                       (fun (rfld, ty) ->
                          match List.find_map
                            (fun (sfld, s) -> if sfld = [rfld] then Some s else None)
                            sflds
                          with | None -> None
                          | Some s -> Some (rfld, type_of_selected gamma ty s)
                       )
                       rflds)
                      , rv)
                )
          | ty -> OManager.i_error "Try to select fields on %a" QmlPrint.pp#ty ty
    in
    #<If>
      Format.eprintf "@[Type selection : %a.%a => %a@]@\n"
      QmlPrint.pp#ty ty
      (QmlAst.Db.pp_select (fun _ _ -> ())) select
      QmlPrint.pp#ty res;
    #<End>;
    res


end

module type S = sig include DbGenByPass.S end

type dbinfo = DbGen_private.dbinfo

let merge_dbinfo = DbGen_private.merge_dbinfo

module DbGen ( Arg : DbGenByPass.S ) = struct

  module Access = DbGen_private.DatabaseAccess (Arg)
  let initialize = Access.initialize

  let replace_path_exprs = Access.replace_path_exprs
  let replace_path_code_elt = Access.replace_path_code_elt
  let replace_path_ast = Access.replace_path_ast
end

module DbGenByPass = DbGenByPass

let warning_set =
  WarningClass.Set.create_from_list [
    WarningClass.dbgen;
    WarningClass.dbgen_schema;
  ]
