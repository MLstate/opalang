(*
    Copyright © 2011, 2012 MLstate

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

(**
   Private module for DB-Schema manipulation.
   @author Louis Gesbert
   @author Vincent Benayoun (refactoring)
*)

(* This module gathers generic functions for schema graphs. *)


(* shorthand *)
module Q = QmlAst
module C = DbGen_common

(* alias *)
module Db = Q.Db

let internal_error fmt = OManager.i_error fmt

module Vertices: Graph.Sig.COMPARABLE with type t = C.schema_node = struct
  type t = C.schema_node
  let equal n1 n2 = (n1.C.nodeid = n2.C.nodeid)
  let hash n = Hashtbl.hash n.C.nodeid
  let compare n1 n2 = Pervasives.compare n1.C.nodeid n2.C.nodeid
end
module Edges: Graph.Sig.ORDERED_TYPE_DFT with type t = C.schema_edge = struct
  type t = C.schema_edge
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
  let default = { C.label = C.Field ("",0); C.is_main = false }
end

module SchemaGraph = GraphLib.SchemaGraph (Vertices) (Edges)
module V = SchemaGraph.V
module E = SchemaGraph.E
module SchemaGraph0 = SchemaGraph.SchemaGraph0

let is_root n = (V.label n).C.nodeid = "root"

let get_parent_edge t node =
  if is_root node then raise Not_found else
    match SchemaGraph0.pred_e t node with
    | [] -> internal_error "get_parent_edge: node in DB schema has no parent"
    | p::[] -> p
    | pl ->
        match List.find_all (fun e -> (E.label e).C.is_main) pl with
        | [p] -> p
        | [] -> internal_error "get_parent_edge: node in DB schema has no distinguished parent"
        | _ -> internal_error "get_parent_edge: node in DB schema has multiple distinguished parents"

let get_parent_node t node = E.src (get_parent_edge t node)

let string_of_edge e = match (E.label e).C.label with
  | C.Field (s,_) -> s
  | C.SumCase _ -> "."
  | C.Hidden_edge -> ""
  | C.Multi_edge _ -> "_"

let node_label n = (V.label n).C.nlabel

let type_of_node n = (V.label n).C.ty

let package_of_node n = (V.label n).C.from_package

let type_of_leaf = function
  | C.Leaf_int -> Q.TypeConst Q.TyInt
  | C.Leaf_float -> Q.TypeConst Q.TyFloat
  | C.Leaf_text -> Q.TypeConst Q.TyString
  | C.Leaf_binary -> Q.TypeConst Q.TyString

let leaves = [C.Leaf_int; C.Leaf_float; C.Leaf_text; C.Leaf_binary]


(***)

let fieldname_of_edge e = match (E.label e).C.label with
  | C.Field (f,_) -> f
  | _ -> assert false

let fieldid_of_edge e = match (E.label e).C.label with
  | C.SumCase id | C.Field (_,id) -> id
  | _ -> assert false

let path_of_node_one_step ~acc t ?(dir=None) node =
  if Some node = dir || is_root node then None
  else
    let pred_e = get_parent_edge t node in
    let acc =
      match (E.label pred_e).C.label with
      | C.Field (s,_) -> Db.Decl_fld s :: acc
      | C.SumCase _ -> acc
      | C.Hidden_edge -> acc
      | C.Multi_edge C.Kint -> Db.Decl_int :: acc
      | C.Multi_edge C.Kstring -> Db.Decl_string :: acc
      | C.Multi_edge (C.Kfields _) -> Db.Decl_set [] :: acc in
    Some (E.src pred_e, acc)

let rec path_of_node acc t ?dir node =
  match path_of_node_one_step ~acc t ?dir node with
  | None -> acc
  | Some (node, acc) -> path_of_node acc t ?dir node

let path_to_string = String.concat "/"

let string_path_of_node t ?dir node =
  (* (match dir with Some n when not is_root n -> "" | _ -> "/") ^ *)
  Db.path_decl_to_string (path_of_node [] t ~dir node)

let edge_is_fld fld e = match (E.label e).C.label with
  | C.Field (s,_) -> s = fld
  | _ -> false


(* handles only record paths (as they are in the initial tree). Raises Not_found *)
(** Follow a path in a schema started from a given node *)
let rec find_raw_path ~context t cur_node path = match path with
  | [] -> cur_node
  | (Db.Decl_fld str)::next_path ->
      let chlds = SchemaGraph0.succ_e t cur_node in
      let edge = List.find (edge_is_fld str) chlds
      in find_raw_path ~context t (E.dst edge) next_path
  | (Db.Decl_set [])::_next_path ->
      QmlError.error context
        "Path specification with set index being implemented in (find_raw_path)"
  | _ ->
      QmlError.error context
        "Path specification with something else than field names unhandled yet"

let rec find_field_edge t node field =
  match
    match (V.label node).C.nlabel with
      | C.Product ->
          List.find_all (edge_is_fld field) (SchemaGraph0.succ_e t node)
      | C.Sum ->
          List.find_all
            (fun e ->
               try ignore (find_field_edge t (E.dst e) field); true
               with Not_found -> false)
            (SchemaGraph0.succ_e t node)
      | _ -> raise (Invalid_argument "find_field_edge")
  with
    | [e] -> e
    | [] -> raise Not_found
    | _ ->
        QmlError.error (V.label node).C.context (
          "@[<2>This data query is ambiguous (it may match several cases in a sum type):@\n"^^
          "%s/%s@]"
        )
          (string_path_of_node t node)
          field


(** @return the key of a Mult node *)
let multi_key t n =
  match List.map E.label (SchemaGraph0.succ_e t n) with
    | [{C.label = C.Multi_edge k}] -> k
    | _ -> raise Not_found

(** @return the key type of a Mult node *)
let type_of_key t n = match multi_key t n with
  | C.Kint    -> Q.TypeConst Q.TyInt
  | C.Kstring -> Q.TypeConst Q.TyString
  | C.Kfields [fldlist] -> (* todo *)
      let node_of_elts = List.hd (SchemaGraph0.succ t n) in
      let chld_types = List.map (fun fld -> fld, (E.dst (find_field_edge t node_of_elts fld)).C.ty) fldlist in
      Q.TypeRecord (QmlAstCons.Type.Row.make ~extend:false chld_types)
  | C.Kfields _ -> assert false (* todo: handle multiple keys of sets *)

(** [type_of_partial_key fields schema node] For a set [node] and a
    key that contains [fields] returns the type of key fields and the
    type of free fields. *)
let type_of_partial_key fields t n = match multi_key t n with
| C.Kfields [fldlist] ->
    let node_of_elts = List.hd (SchemaGraph0.succ t n) in
    let fields =
      List.sort
        (fun x y -> String.compare (fst x) (fst y))
        fields in
    let fldlist = List.sort String.compare fldlist in
    let _, tykeys, tyfreekeys =
      List.fold_left
        (fun (fields, tykeys, tyfreekeys) fld ->
           let tyfld =
             (fld, (E.dst (find_field_edge t node_of_elts fld)).C.ty) in
           match fields with
           | [] -> ([], tykeys, tyfld::tyfreekeys)
           | (f,_)::rfields when f = fld -> (rfields, tyfld::tykeys, tyfreekeys)
           | _ -> (fields, tykeys, tyfld::tyfreekeys)
        ) (fields, [], []) fldlist in
    tykeys, tyfreekeys
| C.Kint | C.Kstring | C.Kfields _ -> internal_error "type_of_partial_key"


let rec get_root ?n t =
  let n = match n with Some n -> n | None -> SchemaGraph.get_node t in
  if is_root n then n else get_root ~n:(E.src (get_parent_edge t n)) t

(** Finds the node pointed by a given path *)
let rec find_path t ?(node=get_root t) path = match path with
  | [] -> node
  (* | "."::path -> find_path t ~node path *)
  | it::path ->
      match (V.label node).C.nlabel, it with
        | C.Leaf _, _ ->
            raise Not_found
        | C.Product, Db.Decl_fld fld ->
            let next_e =
              List.find (edge_is_fld fld) (SchemaGraph0.succ_e t node)
            in find_path t ~node:(E.dst next_e) path
        | C.Sum, Db.Decl_fld fld ->
            let e = find_field_edge t node fld in
              find_path t ~node:(E.dst e) (it::path)
        | C.Hidden, _ ->
            find_path t ~node:(SchemaGraph.unique_next t node) (it::path)
        | C.Multi, decl ->
            (match decl, multi_key t node with
               | Db.Decl_int, C.Kint | Db.Decl_string, C.Kstring | Db.Decl_set _, C.Kfields _
               | Db.Decl_set [], _ -> (* we allow /path[] whatever the key should be for find_path *)
                   find_path t ~node:(SchemaGraph.unique_next t node) path
               | _, _ -> raise Not_found)
        | _, _ -> raise Not_found
(*               else try *)
(*                 match next_e.label with *)
(*                   | C.Multi_int None -> ignore (int_of_string it); find_path ~generic t (E.dst next_e) path *)
(*                   | C.Multi_float None -> ignore (float_of_string it); find_path ~generic t (E.dst next_e) path *)
(*                   | C.Multi_string None -> ; find_path ~generic t (E.dst next_e) path *)
(*                   | _ -> find_path ~generic t (E.dst next_e) (it::path) (\* there is a default value *\) *)
(*               with Failure _ -> sch_error (sprintf "invalid key type in path %s" *)
(*                                          (string_path_of_node node)) *)

(*
   [find_path_path_of_node schema node] has the same behaviour as
   [find_path schema ~node (path_of_node schema node)] except that
   it won't fail when path_of_node returns an ambiguous path
   as is the case in [db /plop : {a:list(string); b:string} / {a:list(string)}]
   (because of /plop/a)
   This function is not the identity because it goes (for instance) through Hidden
   nodes
*)
let find_path_path_of_node t ~node =
  match path_of_node_one_step ~acc:[] t node with
  | None -> node
  | Some (node, path) -> find_path t ~node path

let new_node label ty default constraints context =
  V.create {
    C.from_package = ObjectFiles.get_current_package_name();
    C.nodeid = SchemaGraph.new_nodeid();
    C.nlabel = label;
    C.ty = ty;
    C.default = default;
    C.constraints = constraints;
    C.context = context;
    C.plain = false;
  }

let has_C_Private bool cstr = List.mem (Db.C_Private bool) cstr
let is_node_C_Private bool node = List.mem (Db.C_Private bool) (V.label node).C.constraints
let is_node_abstract node = is_node_C_Private false node
let is_node_full node = is_node_C_Private true node

(* A node is considered private when its _parent_ is abstract (eg has the
   private constraint). The parent may still be seen, but the 'private' child
   should be invisible *)
let is_node_private t node =
  if is_root node then false
  else if is_node_full node then true
  else if is_node_abstract (get_parent_node t node) then (package_of_node node) <> (ObjectFiles.get_current_package_name())
  else false

(** @param n a Mult node
    @return true if n is a set node *)
let is_node_set t n =
  match multi_key t n with
  | C.Kfields _ -> true
  | _ -> false

let add_unknown_node ?(ty=Q.TypeRecord (Q.TyRow ([], None))) ?dflt ?(cstr=[]) ~context t parent edgelbl =
  (* assumes lbl is not already taken *)
  let prop_private bool cstr =  if not (has_C_Private bool cstr) &&  is_node_C_Private bool parent then (Db.C_Private bool)::cstr else cstr in
  let cstr = prop_private true cstr in
  let cstr = prop_private false cstr in
  let new_node = new_node C.Product ty dflt cstr context in
  let new_edge = E.create parent { C.label = edgelbl; C.is_main = true } new_node in
    SchemaGraph0.add_edge_e t new_edge, new_node

let set_node_label t node lbl =
  if lbl = (V.label node).C.nlabel || ((V.label node).C.nlabel = C.Product && SchemaGraph0.succ_e t node = [])
  then
    let nnode = V.create { (V.label node) with C.nlabel = lbl } in
    SchemaGraph.replace_node t node nnode, nnode
  else
    internal_error "re-setting a node which is already set: %s" (string_path_of_node t node)

let set_node_id t node id =
  let nnode = V.create { (V.label node) with C.nodeid = id } in
    SchemaGraph.replace_node t node nnode, nnode

let set_node_type t node ty =
  let nnode = V.create { (V.label node) with C.ty = ty } in
    SchemaGraph.replace_node t node nnode, nnode

let set_node_dflt t node expr =
  let nnode = V.create { (V.label node) with C.default = Some expr } in
    SchemaGraph.replace_node t node nnode, nnode

let set_node_context t node context =
  let nnode = V.create { (V.label node) with C.context = context } in
    SchemaGraph.replace_node t node nnode, nnode

let set_node_cstrs t node cstrs =
  let nnode = V.create { (V.label node) with C.constraints = cstrs } in
    SchemaGraph.replace_node t node nnode, nnode

let set_node_plain t node plain =
  let nnode = V.create { (V.label node) with C.plain = plain } in
  SchemaGraph.replace_node t node nnode, nnode

let add_node_cstr t node cstr =
  (* fixme: check for duplicate/inconsistent constraints *)
  set_node_cstrs t node ((V.label node).C.constraints @ [cstr])

let get_node_type node = (V.label node).C.ty

(** @return n, the succesor of [node] pointed by the edge labeled [key]. *)
let get_field_chld t key node =
  E.dst (List.find (edge_is_fld key) (SchemaGraph0.succ_e t node))

let upper_nodes t (loop: E.t list) : V.t list =
  let nodes = List.map E.src loop in
    List.filter (fun n -> not (List.mem (get_parent_node t n) nodes)) nodes

let rec equal_subtree (t1,root1) (t2,root2) =
  V.label root1 = V.label root2 &&
  let es1 = SchemaGraph0.succ_e t1 root1 and es2 = SchemaGraph0.succ_e t2 root2 in
  try
    List.fold_left2
      (fun acc e1 e2 -> acc && E.label e1 = E.label e2 &&
          ((E.label e1).C.is_main && equal_subtree (t1,E.dst e1) (t2,E.dst e2)) ||
          (V.label (E.dst e1) = (V.label (E.dst e2)) (* links -> point to same node *)))
      true es1 es2
  with Invalid_argument _ -> false

(*----------------- Functions for building schema in a progressive way -----------------*)
(* and be able to get types before the whole process is finished -> needed by the typer *)

let initial_root ~context = V.create {
  C.from_package = ObjectFiles.get_current_package_name();
  C.nodeid = "root";
  C.nlabel = C.Product;
  C.ty = Q.TypeRecord (QmlAstCons.Type.Row.make ~extend:true []);
  C.default = None;
  C.constraints = [];
  C.context = context;
  C.plain = false;
}

let initial_schema ~context = SchemaGraph0.add_vertex SchemaGraph0.empty (initial_root ~context)

(* Adds the subtree of t' rooted at n to t. The two graphs should be disjoint,
   except for n that must exist in both *)
let rec add_subtree t t' n =
  SchemaGraph0.fold_succ_e
    (fun e t ->
       let t = SchemaGraph0.add_edge_e t e in
       if (E.label e).C.is_main then add_subtree t t' (E.dst e) else t)
    t' n t

let schema_of_package t package_name =
  SchemaGraph.filter (fun n -> n.C.from_package = package_name || is_root n) t

let merge_schema t1 t2 =
  if SchemaGraph0.nb_vertex t1 <= 1 then t2
  else if SchemaGraph0.nb_vertex t2 <= 1 then t1
  else
    let root1, root2  = (get_root t1, get_root t2) in
    let labels1 =
      SchemaGraph0.fold_succ_e (fun e acc -> StringSet.add (fieldname_of_edge e) acc) t1 root1 StringSet.empty in
    SchemaGraph0.fold_succ_e
      (fun e t ->
         if StringSet.mem (fieldname_of_edge e) labels1
         then
           QmlError.error
             (QmlError.Context.merge2
                (V.label (E.dst (find_field_edge t1 root1 (fieldname_of_edge e)))).C.context
                (V.label (E.dst e)).C.context)
             "Redefinition of the database root /%s. Two packages can not define database roots with the same name."
             (fieldname_of_edge e)
         else
           let t = SchemaGraph0.add_edge_e t (E.create root1 (E.label e) (E.dst e)) in
           add_subtree t t2 (E.dst e))
      t2 root2 t1
