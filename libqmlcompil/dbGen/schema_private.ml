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

(**
   Private module for DB-Schema manipulation.
   @author Louis Gesbert
   @author Mathieu Barbin (errors report)
*)

(* This module makes use of OCamlGraph: http://ocamlgraph.lri.fr/doc *)


(* depends *)
module Format = Base.Format
module List = BaseList
module String = BaseString

(* refactoring in progress *)

(* shorthands *)
module Q = QmlAst
module Db = Q.Db
module C = DbGen_common
module H = DbGenHelpers

(* alias *)
module V = SchemaGraphLib.V
module E = SchemaGraphLib.E
module ExprIdent = Ident
module SchemaGraph0 = SchemaGraphLib.SchemaGraph0
module SchemaGraph = SchemaGraphLib.SchemaGraph

module HacksForPositions =
struct
  let annotmap = ref ( QmlAnnotMap.empty : Q.annotmap )
  let set_annotmap map = annotmap := map
  let free_annotmap () = annotmap := QmlAnnotMap.empty
  (*
    Map the context, and add the current annotmap.
    Should be used each time a context is created.
  *)
  let map context =
    let c = QmlError.Context.annotmap !annotmap in
    QmlError.Context.merge2 context c
end

module PathMap = BaseMap.Make
  (struct
     type t = Db.path_decl
     let compare = Pervasives.compare
   end)


#<< let debugtag = DebugTracer.Debug.create "Schema" >>#;
#<< let sch_debug = QmlAst.QmlDebugInterface.debug "QmlDbGen.Schema" debugtag >>#;

let internal_error fmt = OManager.i_error fmt

let (@*) = InfixOperator.(@*)

type t = SchemaGraphLib.SchemaGraph0.t
type vertex = SchemaGraphLib.SchemaGraph0.vertex
type edge = SchemaGraphLib.SchemaGraph0.edge

type database_def = {
  ident: Q.ident;
  ty:Q.ty;
  context: QmlError.context;
  path_aliases: (Q.path * Q.path) list;
  (* eg. [(/a,/b); (/alias,/deep/data)] *)
  options: Db.options;
  schema: t;
  package : ObjectFiles.package_name;
  virtual_path : (Q.ident * Q.ty * Q.ty) PathMap.t;
}
type meta_schema = database_def StringListMap.t

let mapi f = StringListMap.mapi
  (fun key def ->
     let ident, options = f key (def.ident, def.options) in
     { def with ident=ident; options=options; })

(* ---------------------------------------------------------------------- *)
(* A selection of ways to type/coerce the returned results. Choose wisely *)
(* ---------------------------------------------------------------------- *)

let get_type_from_name ~context gamma tylst tid =
  match
    QmlTypes.Env.TypeIdent.findi_opt ~visibility_applies: false tid gamma with
  | Some (tid, (ts, _)) ->
      QmlTypes.Scheme.specialize ~typeident:tid ~ty:tylst ts
  | None ->
      QmlError.error context
        "Type @{<bright>%s@} is not defined" (Q.TypeIdent.to_string tid)

let key_kind ~context gamma ty =
  let rec aux seen = function
    | Q.TypeConst Q.TyInt      -> C.Kint
    | Q.TypeConst Q.TyString   -> C.Kstring
    | Q.TypeName (tylst,tid) -> let ty = get_type_from_name ~context gamma tylst tid in
        if not (List.mem tid seen) then aux (tid::seen) ty else failwith "key_kind"
    | _ -> failwith "key_kind"
  in aux [] ty

(* puts extensible* records as types upwards from node *)
let rec type_upwards t node =
  let n = SchemaGraphLib.get_parent_node t node in
  let chld_types = List.map (fun e -> SchemaGraphLib.fieldname_of_edge e, (V.label (E.dst e)).C.ty) (SchemaGraph0.succ_e t n) in
  let ty = Q.TypeRecord (QmlAstCons.Type.Row.make ~extend:false chld_types) in
  (* * ~extend:false makes the life of the schema cleanup easier, but results in
     wrong types until all the db definitions have been read. However, with the new
     preprocessing, we don't need those anymore so it's ok *)
  let ty = match (V.label n).C.ty with
    | Q.TypeRecord _ -> ty (* todo: Check that we can't already have an inconsistent record type *)
    | ty' ->
        internal_error
          "Inconsistent typing of node @{<bright>%s@}: nodes with type @{<bright>%a@} cannot have children"
          (SchemaGraphLib.string_path_of_node t n)
          QmlPrint.pp#ty ty'
  in
  let context =
    QmlError.Context.merge (V.label node).C.context
      (List.map (fun n -> (V.label n).C.context) (SchemaGraph0.succ t n)) in
  let context = HacksForPositions.map context
  in
  let t,n = SchemaGraphLib.set_node_label t n C.Product in
  let t,n = SchemaGraphLib.set_node_type t n ty in
  let t,n = SchemaGraphLib.set_node_context t n context
  in
    if SchemaGraphLib.is_root n then t else type_upwards t n

(** Inserts a new node above [node], Multi if key_type was specified, Hidden
    otherwise. Does not work for indexed sets *)
let insert_multi t ?key_kind ?multi_type node =
  let multi_type = match multi_type with Some t -> t | None -> SchemaGraphLib.type_of_node node in
  let parent = SchemaGraphLib.get_parent_edge t node in
  let cstr = if SchemaGraphLib.is_node_abstract node then [Db.C_Private] else [] in
  let multi_node, down_edge_label = match key_kind with
    | None -> SchemaGraphLib.new_node C.Hidden multi_type None cstr node.C.context, C.Hidden_edge
    | Some kind ->
        SchemaGraphLib.new_node C.Multi multi_type None cstr node.C.context, C.Multi_edge kind in
  let down_edge = E.create multi_node { C.label = down_edge_label; C.is_main = true } node in
  let t = SchemaGraph0.add_edge_e t down_edge in
  SchemaGraph0.remove_edge_e (SchemaGraph0.add_edge_e t (E.create (E.src parent) (E.label parent) multi_node)) parent

(** Some magic to detect handled pervasive types
    We need to find a better way to do this, it's ugly
    @return (t, n option) option, t is present if it was modified (and the type handled)
    n, if present, is the node that was added
*)
let rec manage_pervasive_type ~context t gamma n =
  let manage_tymap n ty1 ty2 =
#<< sch_debug "Map type found in the DB definitions: mapping to internal map"; >>#;
    let tymap = SchemaGraphLib.type_of_node n in
    let t,n = SchemaGraphLib.set_node_type t n ty2 in
    try
      let kind = key_kind ~context gamma ty1 in
      let t = insert_multi t ~key_kind:kind ~multi_type:tymap n in
      Some (t, Some n)
    with Failure "key_kind" ->
       QmlError.warning ~wclass:WarningClass.dbgen_schema context
        "@[<2>this kind of @{<bright>map@} is not handled by the database,@ elements won't be reachable directly@ (at %s)@]"
        (SchemaGraphLib.string_path_of_node t n)
      ;
      None
  in
  let rec aux ty =
    match QmlTypesUtils.Inspect.get_deeper_typename gamma ty with
      | Q.TypeName ([ty1;ty2],tid)
          when (match Q.TypeIdent.to_string tid with
                  | ( "make_map.t" (* for qml *)
                    | "map" (* for opa, old style *)
                    | "Map_private.map" (* for opa, new style *)) -> true
                  | _ -> false) ->
          manage_tymap n ty1 ty2
      | Q.TypeName ([ty1;ty2;tyorder],tid)
          when (Q.TypeIdent.to_string tid = "ordered_map")  ->
          (match QmlTypesUtils.Inspect.get_deeper_typename gamma tyorder with
             | Q.TypeName ([],tid)
                 when (match Q.TypeIdent.to_string tid with
                         | "String.order" | "Int.order" | "Order.default" -> true
                         | _ -> false) ->
                 manage_tymap n ty1 ty2
             | _ ->
                 QmlError.warning ~wclass:WarningClass.dbgen_schema context
                 "This map uses an ordering that is unsupported by the database.\nAccess to elements by key will be disabled (at %s)"
                 (SchemaGraphLib.string_path_of_node t n);
                 None)
      | Q.TypeName ([],tid) when Q.TypeIdent.to_string tid = "binary" ->
          let t, _n = SchemaGraphLib.set_node_label t n (C.Leaf C.Leaf_binary) in
          Some (t, None)
      | _ -> None
  in aux (SchemaGraphLib.type_of_node n)

let rec has_dflt_in_parents t n =
  try
    let p = SchemaGraphLib.get_parent_node t n in
    (V.label p).C.nlabel = C.Product && ((V.label n).C.default <> None || has_dflt_in_parents t p)
  with Not_found -> false

let rec subtree_score t n =
  let (+) x y = match (x,y) with Some x, Some y -> Some (x+y) | _ -> None in
  let min x y = match (x,y) with Some x, Some y -> Some (min x y) | Some x, _ | _, Some x -> Some x | _ -> None in
  let subscores =
    List.map
      (fun e -> if (E.label e).C.is_main then subtree_score t (E.dst e) else None)
      (SchemaGraph0.succ_e t n)
  in
  match (V.label n).C.nlabel with (* A silly heuristic ;) *)
    | C.Product -> List.fold_left (+) (Some (List.length subscores)) subscores
    | C.Leaf _ -> Some 4
    | C.Sum -> Some (List.length (List.filter_map (fun x -> x) subscores) - 1) + List.fold_left min None subscores
    | C.Multi | C.Hidden -> List.fold_left (+) (Some 1000) subscores

(* Returns the descending edge that is non-recursive and has the smallest-scoring childs *)
(* Assumes a sum-node (hence record-node childs) *)
let find_nonrec_child_edge t n =
  let print_warning e =
    let str_path = SchemaGraphLib.string_path_of_node t n in
    (#<If:DBGEN_FLAGS$flag "nodefault"> QmlError.warning ~wclass:WarningClass.dbgen_schema
     #<Else> QmlError.serror #<End>)
      (V.label n).C.context
      (
        "You must specify a default value for path @{<bright>%s@}@\n" ^^
          "@[<2>@{<bright>Hint@}:@\nAdd for example:@\n" ^^
          "db %s = { %s }@]"
      )
      str_path
      str_path
      (String.concat_map "; "
         (fun e -> SchemaGraphLib.fieldname_of_edge e
            ^ (match (E.dst e).C.nlabel with
               | C.Leaf C.Leaf_int -> " = 0"
               | C.Leaf C.Leaf_text -> " = \"\""
               | C.Leaf C.Leaf_float -> " = 0."
               | C.Product when SchemaGraph0.succ_e t (E.dst e) = [] -> ""
               | _ -> " = <...>"))
         (SchemaGraph0.succ_e t (E.dst e)))
  in
  let aux t n =
    let es = SchemaGraph0.succ_e t n in
    let es =
      List.sort (* to get a deterministic hint message *)
        (fun e1 e2 -> compare (SchemaGraphLib.fieldid_of_edge e1) (SchemaGraphLib.fieldid_of_edge e2))
        es
    in
    let es_score = List.map (fun e -> e, subtree_score t (E.dst e)) es in
    let es_score = List.filter_map (fun (e,s) -> match s with Some s -> Some (e,s) | None -> None) es_score in
    let es_score = List.sort (fun (_e1,s1) (_e2,s2) -> compare s1 s2) es_score in
    match es_score with
    | [] ->
        QmlError.error (V.label n).C.context (
          "@[<2>Recursive database definition without a terminal case:@\n%s@]"
        )
          (SchemaGraphLib.string_path_of_node t n)
    | (e1,s1)::(_e2,s2)::_ when s1 >= 4 || s2 < 4 ->
        if not (SchemaGraphLib.is_node_private t n) then print_warning e1; e1
    | (e1,_)::_ -> e1
  in
  aux t n

(** Given a node in the graph, an edge label and a type, adds the subgrapgh
    corresponding to that type at that point *)
let rec add_subgraph ?(is_plain = false) ~context ?(boundnames = []) gamma t parent edge ty =
  (* aux goes down from an already added, childless node *)
  let rec aux boundnames t ?ty cur_node =
    assert (0 = SchemaGraph0.out_degree t cur_node);
    let ty = match ty with Some ty -> ty | None -> (V.label cur_node).C.ty in
    match ty with
      | Q.TypeConst c ->
          let node_typ = C.Leaf
            (match c with
               | Q.TyInt -> C.Leaf_int
               | Q.TyFloat -> C.Leaf_float
               | Q.TyString -> C.Leaf_text
               | _ ->
                   QmlError.error context (
                     "Invalid constant type in DB definition@\n@[<2>@{<bright>Hint@}:@\n"^^
                     "Handled constant type are: @{<bright>int@}, @{<bright>float@}, @{<bright>string@}@]"
                   )
            )
          in
          let t,_nnode = SchemaGraphLib.set_node_label t cur_node node_typ in t
      | Q.TypeName (ty_params, ty_name) ->
          (* Management of @private and @abstract types. For paths with such
             types, we anyway need to known their representation. However, we
             must know they are not "visible" type structures to forbid partial
             writes in paths with these types in databases. *)
          let is_abstract_or_private_ty =
            let (_, _, vis) = QmlTypes.Env.TypeIdent.raw_find ty_name gamma in
            (match vis with
             | QmlAst.TDV_public -> false
             | QmlAst.TDV_abstract _ | QmlAst.TDV_private _ ->
                 (* A priori, we make so that partial writes on abstract and
                    private type even when we are in the package defining them.
                    If this is too restrictive, we may consider relaxing this
                    fact. Let's see at usage. *)
                 true) in
          let (t, cur_node) =
            if SchemaGraphLib.is_node_abstract cur_node ||
              not is_abstract_or_private_ty then (t, cur_node)
            else SchemaGraphLib.add_node_cstr t cur_node Db.C_Private
          in
          (match List.find_opt (fun (ty',_) -> Q.EqualsTy.equal ty' ty) boundnames with
           | Some (_, linkto) when V.equal (SchemaGraphLib.find_path_path_of_node t ~node:linkto) cur_node ->
               (* Already went through that node, unfold *)
               aux boundnames
                 t ~ty:(get_type_from_name ~context gamma ty_params ty_name) cur_node
           | Some (_, linkto) ->
               let linkto = SchemaGraphLib.find_path_path_of_node t ~node: linkto in
                #<If:DBGEN_DEBUG>
                  OManager.printf "Recursive type detected at %s: linking@."
                    (SchemaGraphLib.string_path_of_node t cur_node)
                #<End>;
                let edge = SchemaGraphLib.get_parent_edge t cur_node in
                let t = SchemaGraph0.add_edge_e t (E.create (E.src edge) { (E.label edge) with C.is_main = false } linkto) in
                SchemaGraph0.remove_vertex t cur_node
            | None ->
                match manage_pervasive_type ~context t gamma cur_node with
                  | Some (t,Some n) -> (* was managed and returned a new node *)
                      aux boundnames t n
                  | Some (t,None) -> t (* managed and no new node, we're finished *)
                  | None -> (* not managed, unfold it and add to boundnames to detect recursion *)
                      aux ((ty, cur_node)::boundnames) t
                        ~ty:(get_type_from_name ~context gamma ty_params ty_name)
                        cur_node)
        | Q.TypeRecord tr ->
            assert (0 = SchemaGraph0.out_degree t cur_node);
            let t,node = SchemaGraphLib.set_node_label t cur_node C.Product in
              List.fold_left
                (fun t (rid, rty) ->
                   add_subgraph ~context ~boundnames gamma t node (C.Field (rid,0)) rty)
                t (QmlAstWalk.Row.ordered_elements tr) (* Need to be ordered ? *)
        | Q.TypeSum tycol ->
            assert (0 = SchemaGraph0.out_degree t cur_node);
            let t,node = SchemaGraphLib.set_node_label t cur_node C.Sum in
            QmlAstWalk.Col.fold_records
              (fun t ty ->
                 add_subgraph ~context ~boundnames gamma t node (C.SumCase 0) ty)
              t tycol
        | Q.TypeSumSugar _ -> assert false (* normally the type went through type_of_type *)
(*
        | Q.TypePath p ->
            assert (0 = SchemaGraph0.out_degree t cur_node);
            let linkto = skip_path t (SchemaGraphLib.find_path t p) in
            let edge = SchemaGraphLib.get_parent_edge t cur_node in
            let t = SchemaGraph0.add_edge_e t (E.create (E.src edge) { (E.label edge) with is_main = false } linkto) in
              SchemaGraph0.remove_vertex t cur_node
                (*     | Some (TypeChecker.TypeVar tvar) -> (\* FIXME: replace variables beforehand *\) *)
                (*         (match TypeVar.value tvar with *)
                (*            | TypeVar.Instantiated e -> *)
                (*                let t,cur_node = SchemaGraphLib.set_node_type t cur_node e in *)
                (*                  add_subgraph ~boundnames gamma t cur_node *)
                (*            | _ -> error (Printf.sprintf "uninstantiated variable found in db definition")) *)
                (*     | Some (TypeChecker.TypeVar tvar as ty) -> *)
                (*         let t,node = SchemaGraphLib.set_node_type t cur_node (TypeEnv.val_of_type ty) *)
                (*         in add_subgraph ~boundnames gamma t node *)
                (*         error (Printf.sprintf "invalid tvar perennial type") *)
                (*     | Some (TypeChecker.TypeName atid) -> *)
                (*         error (Printf.sprintf "invalid abstract perennial type") *)
*)
        | ty ->
            QmlError.error context
              "Elements of type @{<bright>%a@} cannot be stored in the database" QmlPrint.pp#ty ty
  in

  let t, cur_node = SchemaGraphLib.add_unknown_node ~context ~ty t parent edge in
  let t, cur_node = SchemaGraphLib.set_node_plain t cur_node is_plain in
  aux boundnames t cur_node

(* Rq: invariants de l'arbre: 1 arc C.is_main entrant et un seul pour tout noeud (sauf la racine), les arcs Field sortant sont distincts *)

let remove_dups l =
  let rec aux acc = function
    | x::r -> if List.mem x acc then aux acc r else aux (x::acc) r
    | [] -> List.rev acc
  in aux [] l


(* detect loops in the graph to turn them into Multi *)
(* -- the algorithm goes as follows:
   find loops
   for each loop: find the upper bounds (nodes closest to the root)
   insert a Multi node above them, taking their incoming edges
   choose a key (?): list -> int with implicit 0 | champ "key" du record ?
*)
let unroll_graph gamma t root =
  #<If:DBGEN_FLAGS$flag "noflatten"> t #<Else>
  assert (SchemaGraph0.mem_vertex t root);
  let make_multi _gamma t node =
    match (V.label (SchemaGraphLib.get_parent_node t node)).C.nlabel with
      | C.Hidden _ -> t
      | _ -> insert_multi t node
  in
  let loops: E.t list list = SchemaGraph.detect_loops t root in
#<<     List.iter (fun l -> sch_debug (Printf.sprintf "Loop detected: %s}" (List.fold_left (fun a e -> a^"/"^(SchemaGraphLib.string_of_edge e)) (SchemaGraphLib.string_path_of_node t (E.src (List.hd l)) ^"{") l))) loops; >>#;
    let summits: V.t list = List.fold_left (fun acc loop -> SchemaGraphLib.upper_nodes t loop @ acc) [] loops in
    let summits = remove_dups summits in
    #<> if summits <> [] then sch_debug (Printf.sprintf "Multiple nodes detected:%s" (List.fold_left (fun a b -> a^" "^(SchemaGraphLib.string_path_of_node t b)) "" summits));
    List.fold_left (make_multi gamma) t summits
  #<End>

(** Extends the tree according to a given type at a given path*)
let add_path ~context gamma t path0 ty =
  let root = SchemaGraphLib.get_root t in
  (* Follows the path until the point where it isn't defined already *)
  let rec follow n path =
    match (V.label n).C.nlabel, path with
      | C.Product, k::path ->
          (try follow (SchemaGraphLib.find_raw_path ~context t n [k]) path
           with Not_found -> n, k::path)
      | _, k::_path ->
          QmlError.error context
            "@[<2>Path specification with something else than field names unhandled yet@\nat %s (in %s)@]"
            (Db.path_decl_to_string path0)
            (Db.path_decl_key_to_string k)
      | _, [] ->
          QmlError.error context
            "Path %s was already defined"
            (Db.path_decl_to_string path0)
  in
  let rec build t n path =
    match path with
      | (Db.Decl_fld str)::[] ->
          let t = add_subgraph ~is_plain:true ~context gamma t n (C.Field (str,0)) ty in
          let n =
            try SchemaGraphLib.find_raw_path ~context t root path0
            with Not_found -> failwith (SchemaGraphLib.string_path_of_node t n ^ " || " ^ Db.path_decl_to_string path) (* the node may have changed, look it up again *)
          in
          let t = type_upwards t n in (* updates the type of parent records with the added field *)
          unroll_graph gamma t n (* unrolls loops to turn recursive types into maps *)
      | (Db.Decl_fld str)::path ->
          (* If the path has several levels of yet undefined fields, insert product nodes *)
          let t, next_node = SchemaGraphLib.add_unknown_node t n (C.Field (str,0)) ~context in
          build t next_node path
      | (Db.Decl_set lidx)::[] ->
          let t,n = SchemaGraphLib.set_node_label t n C.Multi in
          let t,n = SchemaGraphLib.set_node_type t n (C.tydbset ty) in
          add_subgraph ~is_plain:true ~context gamma t n (C.Multi_edge (C.Kfields lidx)) ty
      | (Db.Decl_set [])::_path ->
          QmlError.error context
            "Path specification inside sets unhandled yet (%s)"
            (Db.path_decl_to_string path0)
      | (Db.Decl_set _lidx)::_path ->
          QmlError.error context
            "Invalied path specification (%s)"
            (Db.path_decl_to_string path0)
      | _::_path ->
          QmlError.error context
            "Path specification with something else than field names unhandled yet (%s)"
            (Db.path_decl_to_string path0)
      | [] -> assert false
  in
  let n, path = follow root path0 in
  build t n path



(*----------------- Functions for building schema in a progressive way -----------------*)
(* and be able to get types before the whole process is finished -> needed by the typer *)

let initial = StringListMap.empty

(* We assume that if any databases with mount points are defined,
   they are "used",  even if their schemas are totally undefined
   (the at least one edge from the root is defined, in a way) *)
let is_empty_or_unused t =
  try
    let def = StringListMap.find [] t in
    (SchemaGraph0.nb_vertex def.schema = 1)
  with
  | Not_found ->
      StringListMap.is_empty t


let apply_aliases aliases path =
  let rec skip n l = if n <= 0 then l else match l with _::r -> skip (n-1) r | [] -> [] in
  let rec max_prefix = function
    | field::rest ->
        (match field with Db.FldKey _ -> field :: max_prefix rest | _ -> [])
    | [] -> []
  in
  let rec aux pfx sfx =
    try
      (List.assoc pfx aliases) @ sfx
    with Not_found ->
      if pfx = []
      then sfx
      else
        let pfx_but_last,last = Base.List.extract_last pfx in
        aux pfx_but_last (last::sfx)
  in
  let pfx = max_prefix path in
  let sfx = skip (List.length pfx) path in
  let path = aux pfx sfx in
  path



(* returns the prefix, database definition and the path with prefix removed;
   parameter f is used to extract strings from the beginning of the path *)
let get_database_def ~context t (f: 'a -> string option) (path: 'a list) =
  let error () =
    QmlError.error context (
      "This path doesn't seem to belong to any of the defined databases@\n"^^
        "@[<2>@{<bright>Hint@}:@\n"^^
        "According to the databases defined, the path should start with one of@\n%a@]@\n"
    )
      (StringListMap.pp "@ " (fun f p _ -> Format.fprintf f "%s" (String.sconcat ~left:"/" "/" p)))
      t
  in
  let rec max_prefix = function
    | field::rest ->
        (match f field with Some field -> field :: max_prefix rest | None -> [])
    | [] -> []
  in
  let rec aux pfx =
    try
      pfx, StringListMap.find pfx t
    with Not_found ->
      if pfx = [] then error() else aux (Base.List.remove_last pfx)
  in
  let rec skip n l = if n <= 0 then l else match l with _::r -> skip (n-1) r | [] -> [] in
  let pfx = max_prefix path in
  let pfx, def = aux pfx in
  let path = skip (List.length pfx) path in
  pfx, def, path

let database_def_of_path_def ~context t path =
  get_database_def ~context t (function Db.Decl_fld f -> Some f | _ -> None) path

let database_def_of_path_expr ~context t path =
  let pfx, def, path = get_database_def ~context t (function Db.FldKey f -> Some f | _ -> None) path in
  pfx, def, path

let map_database_def ~context f t path =
  let prefix, def, path = database_def_of_path_def ~context t path in
  StringListMap.add prefix (f def path) t

let register_path ~context t gamma path ty =
  map_database_def ~context
    (fun def path -> { def with schema = add_path ~context gamma def.schema path ty })
    t path

let find_path_manydb ~context t path =
  let pfx, def, path = database_def_of_path_def ~context t path in
  try
    pfx, def, SchemaGraphLib.find_path def.schema path
  with Not_found ->
    QmlError.error context
      "@[<2>This path definition is invalid: %s@\n@]"
      (Db.path_decl_to_string path)

let rec path_decl2expr ~context path =
  let map_fun elt =
    match elt with
      | Db.Decl_fld str -> Db.FldKey str
      | _ ->
        QmlError.error context
          "Path aliases can only contain record fields."
  in
  List.map map_fun path

let register_alias ~context t path target_path =
  let prefix, db_def, _n = find_path_manydb ~context t target_path in
  let path        = path_decl2expr ~context path in
  let target_path = path_decl2expr ~context target_path in
  let path_aliases = (path, target_path)::db_def.path_aliases in
  (* todo: check if the alias is not already defined
     or maybe we want that path aliases can be overridden ?
     in this case, the old the previous declaration could be deleted  *)
  StringListMap.add prefix { db_def with path_aliases = path_aliases } t

let maybe_name_expr ~name_default_values dflt =
  if name_default_values then
    let extract_db_expr = Ident.next "pull_db_expr_out" in
    let binding = (extract_db_expr, dflt) in
    let dflt = Q.Ident (Annot.refresh (Q.Label.expr dflt), extract_db_expr) in
    dflt, Some extract_db_expr, Some (binding, dflt)
  else
    dflt, None, None

let register_default ~name_default_values ~context t path dflt =
  let context =
    let c = QmlError.Context.expr dflt in
    let c = QmlError.Context.merge2 context c in
    c
  in
  if QmlAstWalk.UseDb.expr dflt
  then
    QmlError.error context
      "Database expressions are not allowed at this point"
  ;
  let prefix, db_def, n = find_path_manydb ~context t path in
  let n =
    match (V.label n).C.nlabel with
    | C.Multi  ->
        QmlError.error context
          "@[<2>You can't define a default value for this path@\n%s@]"
          (Db.path_decl_to_string path)
    | C.Hidden -> List.hd (SchemaGraph0.succ db_def.schema n)
    | _ -> n
  in
  if (V.label n).C.default <> None
  then
    QmlError.error context
      "@[<2>Redefinition of default database value for path@\n%s@\n@]"
      (Db.path_decl_to_string path)
  ;
  let dflt = QmlAstCons.UntypedExpr.coerce dflt (SchemaGraphLib.type_of_node n) in
  let dflt, _ident_opt, o = maybe_name_expr ~name_default_values dflt in
  let s, _n = SchemaGraphLib.set_node_dflt db_def.schema n dflt in
  StringListMap.add prefix { db_def with schema = s } t, o

(* FIXME: once the refactoring is finished, we can remove the context argument *)
let register_constraint ~context t p cstr =
  let prefix, db_def, n = find_path_manydb ~context t p in
  let n =
    match (V.label n).C.nlabel with
    | C.Hidden -> List.hd (SchemaGraph0.succ db_def.schema n)
    | _ -> n
  in
  match cstr with
  | Db.C_Ordering e ->
      let context =
        let c = QmlError.Context.expr e in
        let c = QmlError.Context.merge2 context c in
        let c = HacksForPositions.map c in
        c
      in
      if QmlAstWalk.UseDb.expr e
      then
        QmlError.error context
          "Database expressions are not allowed within this constraint specification"
      ;
      if (V.label n).C.nlabel <> C.Multi
      then
        QmlError.error context
          "@[<2>Ordering constraint on a path that is not of type map in:@\n%s@]"
          (Db.path_decl_to_string p)
      ;
      let (s, _n) = SchemaGraphLib.add_node_cstr db_def.schema n cstr in
      StringListMap.add prefix { db_def with schema = s } t
  | Db.C_Private -> (* must be propagated to the whole subtree *)
      let rec aux s n =
        let (s, n) =
          if SchemaGraphLib.is_node_abstract n then (s, n)
          else SchemaGraphLib.add_node_cstr s n Db.C_Private
        in
        List.fold_left
          (fun s e -> if (E.label e).C.is_main then aux s (E.dst e) else s)
          s (SchemaGraph0.succ_e s n)
      in
      StringListMap.add prefix { db_def with schema = aux db_def.schema n } t
  | _ -> internal_error "Sorry, this kind of constraint is not handled yet !"

let register_virtual_path ~name_default_values ~context t path expr =
  let o, ident =
    match maybe_name_expr ~name_default_values expr with
    | _, None, _ -> OManager.error "TODO add a path expr = ident condition"
    | _, Some ident, o -> o, ident in
  let sch =
    map_database_def ~context
      (fun def path ->
         {def with virtual_path =
             let tyread = QmlAstCons.Type.next_var () in
             let tywrite = QmlAstCons.Type.next_var () in
             PathMap.add path (ident, tyread, tywrite)  def.virtual_path})
      t path in
  sch, o

(*
  This function returns:
  1. the new schema
  2. the new value (because registering it adds coercion sometimes
                    and you don't want the coercion to be only in the schema)
*)
let register_new_db_value ~name_default_values t gamma (label, value) =
  let context = QmlError.Context.code_elt (Q.NewDbValue (label, value)) in
  let context = HacksForPositions.map context in
  (* We assume all database declarations already taken into account *)
  let t =
    if StringListMap.is_empty t then
      (* No database declaration found, so add the schema for the default db.
         Note that the db identifier is then not user-accessible. *)
      StringListMap.add [] { ident = Ident.next "database";
                             ty = C.Db.t ();
                             context;
                             path_aliases = [];
                             options = {
                               Q.Db.backend = C.Args.get_engine ();
                             };
                             schema = SchemaGraphLib.initial_schema ~context;
                             package = ObjectFiles.get_current_package_name ();
                             virtual_path = PathMap.empty } t
    else
      t
  in
  match value with
  | Db.Db_TypeDecl (p,ty) ->
      let ty =
        try fst (QmlTypes.type_of_type gamma ty) with
        | (QmlTyperException.Exception _) as exn ->
            QmlError.error context
              "@[<2>Type error in DB definition:@\n%a@]"
              (QmlTyperErrHandling.pp_report_from_typer_exception
                 QmlAnnotMap.empty)
              exn in
      register_path ~context t gamma p ty, None
  | Db.Db_Alias (p,p') ->
    register_alias ~context t p p', None
  | Db.Db_Default (p,dflt) ->
      let s, o = register_default ~name_default_values ~context t p dflt in
      let new_value =
        match o with
        | None -> None
        | Some (binding, new_dflt) ->
            Some (binding, Db.Db_Default (p,new_dflt)) in
      s, new_value
  | Db.Db_Constraint (p,cstr) ->
      register_constraint ~context t p cstr, None
  | Db.Db_Virtual (p, e) ->
      let s, o = register_virtual_path ~name_default_values ~context t p e in
      let new_value =
        match o with
        | None -> None
        | Some (binding, e) ->
            Some (binding, Db.Db_Virtual (p, e)) in
      s, new_value

let register_db_declaration t (label, ident, p, opts) =
  let context = QmlError.Context.code_elt (Q.Database (label, ident, p, opts)) in
  let context = HacksForPositions.map context in
  let error msg =
    QmlError.i_error None context msg
  in
  begin match p with
  | [] ->
      (StringListMap.add [] { ident = ident;
                              ty = C.Db.t ();
                              context = context;
                              path_aliases = [];
                              options = opts;
                              schema = SchemaGraphLib.initial_schema ~context;
                              package = ObjectFiles.get_current_package_name ();
                              virtual_path = PathMap.empty;
                            } t)
  | [Db.Decl_fld point] ->
      (StringListMap.add [point] { ident = ident;
                                   ty = C.Db.t ();
                                   context = context;
                                   path_aliases = [];
                                   options = opts;
                                   schema = SchemaGraphLib.initial_schema ~context;
                                   package = ObjectFiles.get_current_package_name ();
                                   virtual_path = PathMap.empty;
                                 } t)
  | _ -> error "Unhandled DB definition"
  end

let get_error decl msg =
  QmlError.i_error None decl.context msg

let get_db_declaration t =
  StringListMap.fold
    (fun name decl acc ->
       match name with
       | [name] -> (decl, name)::acc
       | [] -> (decl, "_no_name")::acc
       | _ -> get_error decl "Unhandled Db definition"
    )
    t []

let db_declaration t name =
  let name =
    match name with
    | "_no_name" -> []
    | _ -> [name] in
  let decl = StringListMap.find name t in
  decl

exception Formatted of unit Format.pprinter

let rec dots gamma field ty =
  match field with
  | [] -> ty
  | f::t ->
      match QmlTypesUtils.Inspect.follow_alias_noopt_private gamma ty with
      | Q.TypeRecord (Q.TyRow (row, _var) as tyrow) ->
          let ty =
            try List.assoc f row with Not_found ->
              raise (Formatted (fun fmt () ->
                       Format.fprintf fmt "@{<bright>'%s'@} is not found inside row @{<bright>{%a}@}"
                         f QmlPrint.pp#tyrow tyrow))
          in dots gamma t ty
      | Q.TypeSum (Q.TyCol (flds, _) as tysum) ->
          begin match List.find_map (List.assoc_opt f) flds with
          | Some ty -> ty
          | None ->
              raise (Formatted
                       (fun fmt () ->
                          Format.fprintf fmt
                            "@{<bright>'%s'@} is not found inside sum @{<bright>{%a}@}"
                            f QmlPrint.pp#tysum tysum)
                    )
          end
      | ty2 ->
          raise (Formatted  (fun fmt () ->
                   let more =
                     match ty2 with
                     | Q.TypeSum _ -> Some "Update inside a sum type is ambiguous."
                     | _ -> None in
                   Format.fprintf fmt
                     "can't through type @{<bright>%a@} with field(s) @{<bright>'%a'@}%a"
                     QmlPrint.pp#ty ty Db.pp_field field
                     (fun fmt () -> match more with
                      | None -> ()
                      | Some msg -> Format.fprintf fmt "\n@{<bright>Hint@} : %s" msg) ())
                )

let is_uniq t node query =
  let keyty = SchemaGraphLib.type_of_key t node in
  let rec aux query ty =
    match ty with
    | Q.TypeRecord (Q.TyRow (rows, _)) ->
        begin match query with
        | Q.Db.QFlds (flds) ->
            let (flds : string list) =
              List.filter_map
                (fun (f, q) -> match f, q with | [f], Q.Db.QEq _ -> Some f | _ -> None)
                flds
            in
            List.for_all
              (fun (f, _) ->
                 List.exists (fun fs -> f = fs) flds
              ) rows
        | _ -> false
        end
    | Q.TypeConst _ -> true
    | _ -> false
  in aux query keyty


let coerce_query_element ~context gamma ty (query, options) =
  let coerce new_annots wrap ty expr =
    let e = QmlAstCons.UntypedExpr.coerce expr ty in
    Q.QAnnot.expr e::new_annots, wrap e
  in
  let a, options =
    let a = [] in
    let optmap f a o = match o with
    | None -> a, None
    | Some o -> let x, y = f a o in x, Some y in
    let a, limit =
      optmap
        (fun a -> coerce a (fun x -> x) (Q.TypeConst Q.TyInt))
        a options.Db.limit
    in let a, skip =
      optmap
        (fun a -> coerce a (fun x -> x) (Q.TypeConst Q.TyInt))
        a options.Db.skip
    in let a, sort =
      let ty =
        Q.TypeSum (
          let void = Q.TypeRecord (QmlAstCons.Type.Row.make []) in
          QmlAstCons.Type.Col.make [
            [("down", void)];
            [("up", void)];
          ]
        )
      in
      optmap
        (fun a fields ->
           List.fold_left_map
             (fun a (flds, e) -> coerce a (fun e -> (flds, e)) ty e)
             a fields
        ) a options.Db.sort
    in
    (a, {Db.limit; skip; sort})
  in
  let rec aux new_annots ty query =
    let coerce = coerce new_annots in
    let aux2 wrap ty (q1, q2) =
      let new_annots, q1 = aux new_annots ty q1 in
      let new_annots, q2 = aux new_annots ty q2 in
      new_annots, wrap (q1, q2)
    in
    match query with
    | Db.QEq  expr -> coerce (fun e -> Db.QEq  e) ty expr
    | Db.QGt  expr -> coerce (fun e -> Db.QGt  e) ty expr
    | Db.QLt  expr -> coerce (fun e -> Db.QLt  e) ty expr
    | Db.QGte expr -> coerce (fun e -> Db.QGte e) ty expr
    | Db.QLte expr -> coerce (fun e -> Db.QLte e) ty expr
    | Db.QNe  expr -> coerce (fun e -> Db.QNe  e) ty expr
    | Db.QIn  expr ->
        let ty = QmlAst.TypeName ([ty], Q.TypeIdent.of_string Opacapi.Types.list) in
        coerce (fun e -> Db.QIn e) ty expr
    | Db.QOr  (q1, q2)  -> aux2 (fun (q1, q2) -> Db.QOr  (q1, q2)) ty (q1, q2)
    | Db.QAnd (q1, q2)  -> aux2 (fun (q1, q2) -> Db.QAnd (q1, q2)) ty (q1, q2)
    | Db.QNot query ->
        let new_annots, query = aux new_annots ty query in
        new_annots, (Db.QNot query)
    | Db.QMod _ when ty = Q.TypeConst Q.TyInt -> new_annots, query
    | Db.QMod _ -> QmlError.error context "mod is avialable only on integers"
    | Db.QFlds flds ->
        try
          let new_annots, flds =
            List.fold_left_map
              (fun acc (field, q) ->
                 let acc, q = aux acc (dots gamma field ty) q in
                 acc, (field, q))
              new_annots flds
          in new_annots, Db.QFlds flds
        with Formatted p ->
          QmlError.error context "This querying is invalid because %a\n%!" p ()

  in let a, query = aux a ty query in
  a, (query, options)

(** @return (new_annots_list, pppath) *)
let rec convert_dbpath ~context t gamma node kind path0 path =
  let context = QmlError.Context.merge2 context (V.label node).C.context in
  let context = HacksForPositions.map context in
  let cerror fmt =
    QmlError.error context (
      "in path access %a@\n"^^fmt
    )
      QmlPrint.pp#path (path0, kind)
  in
  let rec valid_keys () = match (V.label node).C.nlabel with
    | C.Product ->
        "one of " ^ String.concat_map ", "
          (fun e -> "/" ^ SchemaGraphLib.fieldname_of_edge e) (SchemaGraph0.succ_e t node)
    | C.Sum ->
        "one of " ^ String.concat ", "
          (List.uniq
             (List.fold_left (List.merge compare) []
                (List.map (List.map
                             (fun e -> "/" ^ SchemaGraphLib.fieldname_of_edge e)
                           @* SchemaGraph0.succ_e t) (SchemaGraph0.succ t node))))
    | C.Multi ->
        "a key of type " ^ (Format.to_string QmlPrint.pp#ty (SchemaGraphLib.type_of_key t node))
    | _ -> assert false
  in
  let invalid_entry entry value =
    cerror "Invalid %s @{<bright>%s@} (was expecting @{<bright>%s@})"
      entry value (valid_keys())
  in
  if (V.label node).C.nlabel = C.Hidden
  then convert_dbpath ~context t gamma (SchemaGraph.unique_next t node) kind path0 path
  else
    match path with
    | [] -> [],[]
    | (Db.FldKey fld)::path -> (
        let next =
          try E.dst (SchemaGraphLib.find_field_edge t node fld)
          with Not_found | Invalid_argument _ ->
            if SchemaGraphLib.is_root node
            then
              cerror "The db root @{<bright>%s@} is undefined" fld
            else
              invalid_entry "field" fld
        in
        match (V.label node).C.nlabel with
        | C.Product ->
            let new_annots, epath = convert_dbpath ~context t gamma next kind path0 path in
            new_annots, Db.FldKey fld :: epath
        | C.Sum ->
            (* Format.eprintf "Sum case on %a => %a\n%!" QmlPrint.pp#path (path0, kind) QmlPrint.pp#path (path, kind); *)
            if kind <> Q.Db.Ref then
              convert_dbpath ~context t gamma next kind path0 ((Db.FldKey fld)::path)
            else
            cerror "Direct write access to a sub node of a sum node is forbidden"
        | _ ->
            invalid_entry "field" fld
        )

    | (Db.ExprKey e)::path ->
        let _ = match (V.label node).C.nlabel with C.Multi -> ()
          | _ ->
              invalid_entry "key" (Format.to_string QmlPrint.pp#expr e)
        in
        let keytyp = match e with
        | Q.Record (_, keys) ->
            (* Keys can be partial on syntaxical record. *)
            let tykeys = fst (SchemaGraphLib.type_of_partial_key keys t node) in
            Q.TypeRecord (QmlAstCons.Type.Row.make ~extend:false tykeys)
        | _ -> SchemaGraphLib.type_of_key t node in

        let new_annots, e = match e with
          | Q.Coerce (_, _,ty) when ty = keytyp -> [], e
          | e ->
              let e' = QmlAstCons.UntypedExpr.coerce e keytyp in
              [Q.QAnnot.expr e'], e' in
        let new_annots', epath = convert_dbpath ~context t gamma (SchemaGraph.unique_next t node) kind path0 path in
        new_annots @ new_annots', Db.ExprKey e :: epath

    | Db.NewKey::path ->
        assert (SchemaGraphLib.multi_key t node = C.Kint);
        let new_annots, epath = convert_dbpath ~context t gamma (SchemaGraph.unique_next t node) kind path0 path in
        new_annots, Db.NewKey :: epath

    | Db.Query (query, options)::[] ->
        let new_annots, (query, options) =
          let ty =
            match SchemaGraphLib.type_of_node node with
            | Q.TypeName ([setparam], name) when Q.TypeIdent.to_string name = "dbset" -> setparam
            | _ ->  SchemaGraphLib.type_of_key t node
          in
          coerce_query_element ~context gamma ty (query, options)
        in
        new_annots, [Db.Query (query, options)]

    | Db.Query _::_path -> QmlError.error context "sub path after query is not handler yet"

let get_virtual_path vpath epath =
  let rec aux acc = function
    | ((Db.Decl_fld f1)::q1, ((Db.FldKey f2) as e)::q2) when f1 = f2 ->
        aux (e::acc) (q1, q2)
    | ((Db.Decl_int | Db.Decl_string | Db.Decl_set _)::q1, ((Db.ExprKey _) as e)::q2) ->
        aux (e::acc) (q1, q2)
    | [], p -> Some (List.rev acc, p)
    | _ -> None
  in
  PathMap.fold
    (fun dpath (ident, tyread, tywrite) acc ->
       match aux [] (dpath, epath) with
       | None -> acc
       | Some (p, r) ->
           (* OManager.printf "A virtual path (%s) is find for path : %a\n%!" *)
           (*   (Db.path_decl_to_string dpath) *)
           (*   QmlPrint.pp#path_elts epath; *)
           (p, r, (ident, tyread, tywrite))::acc
    ) vpath []

let rec find_exprpath_aux ?context t ?(node=SchemaGraphLib.get_root t) ?(kind=Db.Option) ?epath0 vpath epath =
  let context = match context with
    | Some context -> QmlError.Context.merge2 context (V.label node).C.context
    | None -> (V.label node).C.context
  in
  let epath0 = match epath0 with Some p -> p | None -> epath in (* for error messages *)
  if SchemaGraphLib.is_node_private t node && epath <> []
  then
    QmlError.error context
      "Direct access to private contents is forbidden at @{<bright>%s@} (in path access @{<bright>%a@})"
      (SchemaGraphLib.string_path_of_node t node)
      QmlPrint.pp#path_elts epath0
  ;
  match epath, (V.label node).C.nlabel with
  | path, C.Hidden ->
      find_exprpath_aux ~context t ~node:(SchemaGraph.unique_next t node) ~kind ~epath0 vpath path
  | [], C.Multi -> (
      match node.C.ty with
      | Q.TypeName ([setparam], name) as ty when Q.TypeIdent.to_string name = "dbset" ->
          ty, node, `virtualset (setparam, ty, true, None)
      | ty -> ty, node, `realpath
    )
  | [],_ -> node.C.ty, node, `realpath
  | (Db.FldKey fld)::epath, C.Product ->
      let next =
        try E.dst (List.find (SchemaGraphLib.edge_is_fld fld) (SchemaGraph0.succ_e t node))
        with Not_found ->
          QmlError.error context
            "Invalid field @{<bright>%s@} in path access @{<bright>%a@}"
            fld
            QmlPrint.pp#path_elts epath0
      in
      find_exprpath_aux ~context t ~node:next ~kind ~epath0 vpath epath
  | (Db.Query (query, _))::epath, C.Multi ->
      let setty = node.C.ty in
      (match epath with
       | [] -> ()
       | _ -> QmlError.error context "Path after queries is not yet allowed");
      (match setty with
       | Q.TypeName ([setparam], name)
           when Q.TypeIdent.to_string name = "dbset" ->
           let node, partial, tyread = node, not (is_uniq t node query), setty in
           node.C.ty, node, `virtualset (setparam, tyread, partial, None)
       | _ ->
           let keyty = SchemaGraphLib.type_of_key t node in
           let partial = not (is_uniq t node query) in
           let valty, node, _x =
             find_exprpath_aux ~context t ~node:(SchemaGraph.unique_next t node)
               ~kind ~epath0 vpath []
           in Q.TypeName ([keyty; valty], Q.TypeIdent.of_string Opacapi.Types.map), node,
           `virtualset (valty, valty, partial, None)
      )

  | (Db.ExprKey _e)::epath, C.Multi ->
      find_exprpath_aux ~context t ~node:(SchemaGraph.unique_next t node) ~kind ~epath0 vpath epath
  | (Db.FldKey fld)::_rp, C.Sum ->
      let e = SchemaGraphLib.find_field_edge t node fld in
      find_exprpath_aux ~context t ~node:(E.dst e) ~kind ~epath0 vpath epath
  | Db.NewKey::epath, C.Multi when SchemaGraphLib.multi_key t node = C.Kint ->
      find_exprpath_aux ~context t ~node:(SchemaGraph.unique_next t node) ~kind ~epath0 vpath epath
  | k::_,_ ->
      internal_error
        "Failed to lookup path %a at \"%a\""
        QmlPrint.pp#path_elts epath0
        QmlPrint.pp#path_elt k

let find_exprpath ?context t ?(node=SchemaGraphLib.get_root t) ?(kind=Db.Option) vpath epath =
  let context = match context with
  | Some context -> QmlError.Context.merge2 context (V.label node).C.context
  | None -> (V.label node).C.context
  in
  match get_virtual_path vpath epath with
  | [] -> find_exprpath_aux ~context t ~node ~kind vpath epath
  | [(_p, [], (ident, tyread, tywrite))] ->
      (match find_exprpath_aux ~context t ~node ~kind vpath epath with
       | ty, n, `realpath -> ty, n, `virtualpath (ident, tyread, tywrite)
       | _, _, `virtualset _ -> QmlError.error context
           "Can't make a virtual path on a dbset"
      )
  | [(p, _l, _e)] ->
      QmlError.error context
        "You can't direct access to the virtual path %a"
        QmlPrint.pp#path_elts p
  | _::_::_ -> assert false

let preprocess_kind ~context gamma kind ty virtual_ =
  match kind with
  | Db.Option | Db.Default | Db.Ref | Db.Valpath -> kind
  | Db.Update u ->
      let ty =
        match virtual_ with
        | `realpath -> ty
        | `virtualset (r, _, _, _) -> r
        | _ -> assert false
      in
      let coerce e ty = QmlAstCons.UntypedExpr.coerce e ty in
      let coerce_list e ty =
        match ty with
        | Q.TypeName ([param], name) when Q.TypeIdent.to_string name = "list" ->
            coerce e param
        | _ ->
            QmlError.error context "You use a database update operator which performs on 'list', but you used it on a path of '%a'"
              QmlPrint.pp#ty  ty
      in
      let rec update (ty:QmlAst.ty) u =
        let error fmt0 fmt =
          QmlError.error context ("You can't update "^^fmt0^^" because "^^fmt)
        in
        match u with
        | Db.UExpr e -> Db.UExpr (coerce e ty)
        | Db.UFlds fields ->
            Db.UFlds
              (List.map
                 (function (field, u) ->
                    let subty =
                      try
                        dots gamma field ty
                      with Formatted prt ->
                        error "the field @{<bright>'%a'@}" "%a" Db.pp_field field prt ()
                    in
                    (field, update subty u))
                 fields)
        | Db.UAppend     e -> Db.UAppend (coerce_list e ty)
        | Db.UPrepend    e -> Db.UPrepend  (coerce_list e ty)
        | Db.UAppendAll  e -> Db.UAppendAll  (coerce e ty)
        | Db.UPrependAll e -> Db.UPrependAll (coerce e ty)
        | Db.UIncr _ when (
            match ty with (* TODO - unify! *)
            | Q.TypeConst Q.TyInt -> true
            | _ -> false
          ) -> u
        | (Db.UPop | Db.UShift) when (
            match ty with (* TODO - unify???! *)
            | Q.TypeName ([_], name) when Q.TypeIdent.to_string name = "list" -> true
            | _ -> false
          ) -> u
        | Db.UPop -> error "" "pop is not available on %a" QmlPrint.pp#ty ty
        | Db.UShift -> error "" "shift is not available on %a" QmlPrint.pp#ty ty
        | Db.UIncr _ -> error "" "incr is not available on %a (only on int)" QmlPrint.pp#ty ty
      in Db.Update (update ty u)

let preprocess_path ~context t gamma prepath kind =
  let prefix, db_def, prepath = database_def_of_path_expr ~context t prepath in
  let prepath = apply_aliases db_def.path_aliases prepath in
  let root = SchemaGraphLib.get_root db_def.schema in
  let new_annots, epath = convert_dbpath ~context db_def.schema gamma root kind prepath prepath in
  let ty, _node, virtual_ = find_exprpath ~context db_def.schema db_def.virtual_path ~node:root ~kind epath in
  let label = Annot.nolabel "dbgen.preprocess_path" in
  let kind = preprocess_kind ~context gamma kind ty virtual_ in
  new_annots, Q.Path (label, List.map (fun f -> Db.FldKey f) prefix @ epath, kind), ty, virtual_



let preprocess_paths_expr ?(val_=(fun _ -> assert false)) t gamma e =
  QmlAstWalk.Expr.foldmap_up
    (fun annottrack e -> match e with
     | Q.Path (label, p, kind)  ->
         let a = Annot.annot label in
         let context = QmlError.Context.expr e in (* FIXME: we don't get a valid position here. *)
         let context = HacksForPositions.map context in
         let new_annots, p, realty, virtual_ = preprocess_path ~context t gamma p kind in
         let exprty =
           let dataty = match virtual_ with
             |`realpath -> realty
             |`virtualset (_, _, true, _) -> realty
             |`virtualset (d, _, false, _) -> d
             | _ -> OManager.i_error "Virtual path are NYI"
           in
           match kind with
           | Db.Option -> H.typeoption dataty
           | Db.Default -> dataty
           | Db.Valpath -> C.Db.val_path_ty dataty
           | Db.Ref -> C.Db.ref_path_ty dataty
           | Db.Update _u -> H.tyunit
         in
         let e =
           (* Bind type variable of virtual path handler with virtual
              path expression... *)
           match virtual_ with
           | `virtualpath (id, _, _) ->
               let coerce =
                 match kind with
                 | Db.Default -> Opacapi.DbVirtual.hack_coerce_default
                 | Db.Option ->  Opacapi.DbVirtual.hack_coerce_option
                 | Db.Valpath -> Opacapi.DbVirtual.hack_coerce_vvpath
                 | Db.Ref -> Opacapi.DbVirtual.hack_coerce_vrpath
                 | _ -> assert false (* TODO - ...*)
               in
               let coerce = QmlAstCons.UntypedExpr.ident (val_ coerce) in
               let id = QmlAstCons.UntypedExpr.ident id in
               QmlAstCons.UntypedExpr.apply coerce [id; e]
           | _ ->
               let path = Q.QAnnot.New.expr p a in
               QmlAstCons.UntypedExpr.coerce path exprty
         in
         List.rev_append (List.rev_map (fun a' -> a,a') ((Q.QAnnot.expr e)::new_annots)) annottrack,
         e
     | e -> annottrack, e)
    [] e

let preprocess_paths_code_elt ?(val_=(fun _ -> assert false)) annottrack t gamma =
  QmlAstWalk.Top.fold_map_expr
    (fun annottrack e -> let at, e = preprocess_paths_expr ~val_ t gamma e in
     List.rev_append at annottrack, e)
    annottrack

let preprocess_paths_ast ?(val_=(fun _ -> assert false)) t gamma =
  List.fold_left_map
    (fun annottrack elt ->
       let elt =
         match elt with
         | Q.NewDbValue (label, Db.Db_Virtual (p, e)) ->
             let annot = Q.QAnnot.expr e in
             let context = QmlError.Context.label label in
             let _, dbdef, _ = database_def_of_path_def ~context t p in
             let _, rty, wty = PathMap.find p dbdef.virtual_path in
             let coerce =
               let realty = (SchemaGraphLib.find_path dbdef.schema p).C.ty in
               let rty = Q.TypeArrow ([realty], rty) in
               let wty = Q.TypeArrow ([wty], realty) in
               let ty = Q.TypeRecord (Q.TyRow ([("read", rty);("write", wty)], None)) in
               let e_annot = Q.QAnnot.New.expr e annot in
               QmlAstCons.UntypedExpr.coerce e_annot ty in
             Q.NewVal (label, [(Ident.next "dbvirtual", coerce)])
         | _ -> elt
       in
       preprocess_paths_code_elt ~val_ annottrack t gamma elt) []

let preprocess_paths_code_elt ?(val_=(fun _ -> assert false)) t = preprocess_paths_code_elt ~val_ [] t

(* let find_dbpath t node path0 path = *)
(*   fst (find_dbpath' t [] node path0 path) *)

(* let _get_type_of_path_ t (path: Db.t) = *)
(*   let n,unify_list = find_dbpath' t [] (SchemaGraphLib.get_root t) path path.Db.path in *)
(*   let Some ty = (V.label n).C.ty *)
(*   in *)
(*     (match path.Db.kind with *)
(*        | Db.Default -> ty *)
(*        | Db.Option -> Q.TypeName ([ty], Q.TypeIdent.of_string "option")), *)
(*     unify_list *)

(* used for conversion *)
(* let has_fields t _ path = *)
(*   let n = find_dbpath t (get_root t) path path.Db.path in *)
(*     match (V.label n).C.nlabel with *)
(*       | Unknown -> assert false *)
(*       | Product | Sum -> true *)
(*       | _ -> false *)

(* used for conversion *)
(* let is_key_int t _ path = *)
(*   let n = find_dbpath t (get_root t) path path.Db.path in *)
(*     match (V.label n).C.nlabel with *)
(*       | Multi (Multi_int None, _) -> true *)
(*       | Multi (_,_) -> false *)
(*       | _ -> assert false *)

let renumber_edge i lbl =
  { lbl with C.label = match lbl.C.label with C.Field (s,_) -> C.Field (s,i) | C.SumCase _ -> C.SumCase i | lbl -> lbl }

module EdgeTbl = Base.Hashtbl.Make(SchemaGraph0.V)
let map_vertex f t = (* map_vertex from ocamlgraph is buggy *)
  (* this table is needed because we don't want to call f several times on the same nodes *)
  let h = EdgeTbl.create 997 (* FIXME? use a size linked to the size of the graph *) in
  let acc =
    SchemaGraph0.fold_vertex
      (fun n acc ->
         let n' = f n in
         EdgeTbl.add h n n';
         SchemaGraph0.add_vertex acc n
      ) t SchemaGraph0.empty in
  SchemaGraph0.fold_edges_e
    (fun e acc ->
       let n1 = EdgeTbl.find h (E.src e) in
       let label = E.label e in
       let n2 = EdgeTbl.find h (E.dst e) in
       let edge = E.create n1 label n2 in
       SchemaGraph0.add_edge_e acc edge
    ) t acc

(* We first need a canonical order of edges *)
let sort_edges t el =
  let compare_edges e e' =
    let r = compare (E.label e).C.label (E.label e').C.label in
    if r <> 0 then r else (* Happens for SumCase, never Field *)
      let fields e = List.sort compare (List.map SchemaGraphLib.fieldname_of_edge (SchemaGraph0.succ_e t (E.dst e)))
      in compare (fields e) (fields e')
  in
  List.sort compare_edges el

let cleanup_nonempty t =
  let package_name = ObjectFiles.get_current_package_name() in
  (* Create a canonical renumbering of nodes *)
  let rec get_ids ids n =
    let es = List.filter (fun e -> SchemaGraphLib.package_of_node (E.dst e) = package_name) (SchemaGraph0.succ_e t n) in
    let es = sort_edges t es in
    let esmain = List.filter (fun e -> (E.label e).C.is_main) es in
    (V.label n).C.nodeid :: List.fold_left get_ids ids (List.map E.dst esmain)
  in
  let m =
    let mapi_fun i id = id,(string_of_int(i+1) ^ package_name) in
    let ids = get_ids [] (SchemaGraphLib.get_root t) in
    StringMap.from_list ((List.hd ids, "root")::(List.mapi mapi_fun (List.tl ids)))
  in
  let node_map n =
    if SchemaGraphLib.package_of_node n <> package_name then n else
    V.create { (V.label n) with C.nodeid = StringMap.find (V.label n).C.nodeid m }
  in
  (* Create a new graph with a copy of all nodes, renumbered *)
  (* -- unneeded: adding the edges adds the nodes automatically *)
  (* let new_t = *)
  (*   SchemaGraph0.fold_vertex (fun n new_t -> add_vertex new_t (node_map n)) t empty *)
  (* in *)
  (* Copy all edges (renumbered) to the new graph *)
  let new_t =
      SchemaGraph0.fold_vertex
      (fun n new_t ->
         let eid = ref 0 in
         let es =
           List.map
             (fun e ->
                if SchemaGraphLib.package_of_node (E.dst e) <> package_name then e else
                  let e =
                    E.create (node_map (E.src e)) (renumber_edge !eid (E.label e)) (node_map (E.dst e))
                  in incr eid; e)
             (sort_edges t (SchemaGraph0.succ_e t n))
         in
         List.fold_left (fun new_t e -> SchemaGraph0.add_edge_e new_t e) new_t es)
      t SchemaGraph0.empty
  in
(* Not needed with records created with ~extend: false *)
(*   let t = map_vertex *)
(*     (fun n -> V.create { (V.label n) with ty = match (V.label n).C.ty with *)
(*                            | Some (TypeRecord tr) -> *)
(*                                Some (TypeRecord (QmlTypes.FreeVars.strip_deep_polymorphism tr)) *)
(*                            | ty -> ty }) t *)
(*   in *)
  new_t

(** Assuming nodes belonging to other packages have already been cleaned up:
    @post All field or sumcase edges going out of a single node hold different indices
    @post The root has id "root"
    @post The numberings are canonical (after cleanup, two equivalent graphs are equal) *)
let cleanup t =
  if SchemaGraph0.is_empty t then
    t
  else
    cleanup_nonempty t

(* replace e by e' in t *)
let replace_edge_e t e e' =
  let t = SchemaGraph0.remove_edge_e t e in
  SchemaGraph0.add_edge_e t e'

let renumber_root_edges t =
  let root = SchemaGraphLib.get_root t in
  let fold_fun t e i =
    let e' = E.create (E.src e) (renumber_edge i (E.label e)) (E.dst e) in
    replace_edge_e t e e'
  in
  let root_edges =
    List.sort
      (fun e1 e2 ->
         let c = compare (SchemaGraphLib.package_of_node (E.dst e1)) (SchemaGraphLib.package_of_node (E.dst e2)) in
         if c = 0 then compare (E.label e1) (E.label e2) else c)
      (SchemaGraph0.succ_e t root)
  in
  List.fold_left_i fold_fun t root_edges

let finalize t =
  if StringListMap.is_empty t then
    #<> let _ = sch_debug (Printf.sprintf "Program doesn't use the database") in
    None
  else
    let package_name = ObjectFiles.get_current_package_name() in
    let f db_def =
      let s = cleanup db_def.schema in
      let _check =
        (* Check if default values are properly defined *)
        SchemaGraph0.iter_vertex
          (fun n ->
             if SchemaGraphLib.package_of_node n = package_name && not (SchemaGraphLib.is_node_private s n) then
               match V.label n with
               | { C.nlabel = C.Sum; C.default = None } ->
                   ignore (find_nonrec_child_edge s n)
               | _ -> ()) s
      in
      { db_def with schema = s }
    in
    Some (StringListMap.map f t)

let def_of_package def package_name =
  { def with schema = SchemaGraphLib.schema_of_package def.schema package_name }
let of_package t package_name =
  StringListMap.map (fun def -> def_of_package def package_name) t

let package_name_of_def def =
  let schema = def.schema in
  let root = SchemaGraphLib.get_root schema in
  SchemaGraphLib.package_of_node root

let merge_database_def def def' =
  (* TODO: handle the case where two compilation units define a database with the same prefix *)
  { def with schema = SchemaGraphLib.merge_schema def.schema def'.schema }
let merge mt mt' = StringListMap.merge merge_database_def mt mt'

let map_types f t =
  StringListMap.map
    (fun db_def ->
       { db_def with schema =
           map_vertex
             (fun n ->
                let lbl = V.label n in
                let ty = f lbl.C.ty in
                if ty == lbl.C.ty then n
                else V.create { lbl with C.ty = ty })
             db_def.schema })
    t

let map_expr f t =
  StringListMap.map
    (fun db_def ->
       { db_def with schema =
           map_vertex
             (fun n ->
                let lbl = V.label n in
                let default = Option.map f lbl.C.default in
                let constraints =
                  Base.List.map_stable
                    (function
                     | Db.C_Validation e -> Db.C_Validation (f e)
                     | Db.C_Ordering e -> Db.C_Ordering (f e)
                     | (Db.C_Inclusion _ | Db.C_Inverse _ | Db.C_Private) as cstr -> cstr)
                    lbl.C.constraints
                in
                if default == lbl.C.default && constraints == lbl.C.constraints
                then n
                else V.create { lbl with C.default = default; C.constraints = constraints })
             db_def.schema })
    t

let fold_expr f acc t =
  let f n acc =
    let acc = List.fold_left (fun acc cstr ->
      match cstr with
      | Db.C_Validation e | Db.C_Ordering e -> f acc e
      | Db.C_Inclusion _ | Db.C_Inverse _ | Db.C_Private -> acc)
      acc (V.label n).C.constraints
    in
    match (V.label n).C.default with
    | Some e -> f acc e
    | None -> acc
  in
  let f _prefix db_def acc = SchemaGraph0.fold_vertex f db_def.schema acc in
  StringListMap.fold f t acc

let foldmap_expr f acc t =
  let f n (acc, t) =
    let label = V.label n in
    let acc, cstrs =
      Base.List.fold_left_map_stable
        (fun acc cstr ->
           match cstr with
           | Db.C_Validation e ->
               let acc,e = f acc e in
               acc, Db.C_Validation e
           | Db.C_Ordering e ->
               let acc,e = f acc e in
               acc, Db.C_Ordering e
           | Db.C_Inclusion _ | Db.C_Inverse _ | Db.C_Private ->
               acc, cstr)
        acc label.C.constraints
    in
    let t, n = if cstrs == label.C.constraints then t, n else SchemaGraphLib.set_node_cstrs t n cstrs in
    match (V.label n).C.default with
    | Some e ->
        let acc, e' = f acc e in
        acc, if e == e' then t else fst (SchemaGraphLib.set_node_dflt t n e)
    | None -> acc, t
  in
  let f _ db_def acc =
    let (acc, s) = SchemaGraph0.fold_vertex f db_def.schema (acc, db_def.schema) in
    acc, { db_def with schema = s }
  in
  StringListMap.fold_map f t acc
