(*
    Copyright © 2011 MLstate

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

(* depends *)
module String = BaseString
module List = BaseList

(* Shorthands *)
module G = Gml_parser

(** This module imports graphs as stored by the db (cf
    libqmlcompil/dbGen/schema_io.ml). The definition is close but different from
    that in dbGen_common.ml (only information that is meaningful at run-time is available).
    Although, now that this module is out of the BSL, it would be quite nice to factorise
    at least the printer/parser between compile-time and run-time.

    We have two types for the schema below: one using sets of vertices and
    edges, useful when (un-)serialising; and one that looks like a tree, much
    easier to manipulate.
*)

let version = 9 (* Should be the same as in libqmlcompil/dbGen/DbGen_common *)

type leaf = Leaf_int | Leaf_float | Leaf_text | Leaf_binary

type node = Multi | Hidden | Sum | Product | Leaf of leaf

type multi_key = Kint | Kstring | Kfields of string list list

type edge_label = Multi_edge of multi_key | Hidden_edge | SumCase of int | Field of string * int | Dead of int
  (** Dead is for edge ids that have already been used in the past, and should therefore not be used
      again if we want a consistent history *)

type edge = { src: string; dst: string; primary: bool; lbl: edge_label }

(* First representation of schemas used for (un)serialization
   cf. type tree for the other representation *)
(* nodeid of Root has to be "root" *)
type schema = {
  nodes: node StringMap.t;
  edges: edge list;
}

module StringOf = struct
  let leaf = function
    | Leaf_int -> "int" | Leaf_float -> "float" | Leaf_text -> "text" | Leaf_binary -> "binary"
  let node = function
    | Multi -> "Multi"
    | Hidden -> "Hidden"
    | Sum -> "Sum"
    | Product -> "Product"
    | Leaf lf -> Printf.sprintf "Leaf %s" (leaf lf)
  let multi_key = function
    | Kint -> "Kint"
    | Kstring -> "Kstring"
    | Kfields flds -> Printf.sprintf "Kfields %s"
        (String.concat_map ~left:"[ " ~right:" ]" "; "
           (String.concat_map ~left:"[ " ~right:" ]" "; " (fun x -> x))
           flds)
  let edge_label = function
    | Multi_edge mk -> Printf.sprintf "Multi_edge %s" (multi_key mk)
    | Hidden_edge -> "Hidden_edge"
    | SumCase i -> Printf.sprintf "SumCase %d" i
    | Field (s,i) -> Printf.sprintf "Field %s %d" s i
    | Dead i -> Printf.sprintf "Dead %d" i
end

let node s n = StringMap.find n s.nodes

let check s = List.fold_left
  (* uncomplete (missing: connected, edges with distinct ids, one incoming primary, root...) *)
  (fun acc -> function { src = src; dst = dst; lbl = lbl } ->
     acc && StringMap.mem src s.nodes && StringMap.mem dst s.nodes && 
       (match lbl with
          | Multi_edge _ -> node s src = Multi
          | Hidden_edge -> node s src = Hidden
          | SumCase _ -> node s src = Sum
          | Field _ -> node s src = Product
          | Dead _ -> true))
  true s.edges

let is_root n = n = 0
let succ_e s n = List.filter (fun e -> e.src = n) s.edges
let pred_e s n = List.filter (fun e -> e.dst = n) s.edges
let (@*) = InfixOperator.(@*)

let edge_num e = match e with SumCase i | Field (_,i) | Dead i -> i | _ -> assert false

exception Error

let import_schema s =
  let r_int params lbl = 
    match List.assoc_opt lbl params with Some (G.Int i) -> i | _ -> raise Error in
  let r_string params lbl = 
    match List.assoc_opt lbl params with Some (G.String s) -> s | _ -> raise Error in
  let _pos, gml = G.parse_gml_parser_gml s in
  let sch = { nodes = StringMap.empty; edges = [] }
  in
  (* if n < String.length s then raise Error; *)
  match gml with
    | G.List ["graph", G.List elts] -> 
        let sch, idmap = (* parse nodes *)
          List.fold_left
            (fun (sch, idmap) n -> match n with
               | "node", G.List params ->
                   let id = r_int params "id"
                   and nodeid = r_string params "nodeid"
                   and nlabel = match List.assoc_opt "nlabel" params with
                     | Some (G.String "Multi") -> Multi
                     | Some (G.String "Hidden") -> Hidden
                     | Some (G.String "Sum") -> Sum
                     | Some (G.String "Product") -> Product
                     | Some (G.List ["Leaf", G.String "int"]) -> Leaf Leaf_int
                     | Some (G.List ["Leaf", G.String "float"]) -> Leaf Leaf_float
                     | Some (G.List ["Leaf", G.String "text"]) -> Leaf Leaf_text
                     | Some (G.List ["Leaf", G.String "binary"]) -> Leaf Leaf_binary
                     | _ -> raise Error
                   in
                   { sch with nodes = StringMap.add nodeid nlabel sch.nodes },
                   IntMap.add id nodeid idmap
               | "edge", _ -> sch, idmap
               | _ -> raise Error)
            (sch, IntMap.empty)
            elts
        in
        let sch = (* parse edges *)
          List.fold_left
            (fun sch n -> match n with
               | "node", _ -> sch
               | "edge", G.List params ->
                   let source = IntMap.find (r_int params "source") idmap
                   and target = IntMap.find (r_int params "target") idmap
                   and ismain = 0 <> r_int params "ismain"
                   and elabel = match List.assoc_opt "elabel" params with
                     | Some (G.List ["Multiedge", mul]) ->
                         Multi_edge
                           (match mul with
                              | G.String "Kint" -> Kint
                              | G.String "Kstring" -> Kstring
                              | G.List ["Kfields", G.List ll] ->
                                  Kfields
                                    (List.map
                                       (function
                                          | _, G.List l ->
                                              List.map (function _,G.String f -> f
                                                               | _ -> raise Error) l
                                          | _ -> raise Error) ll)
                              | _ -> raise Error)
                     | Some (G.String "Hiddenedge") -> Hidden_edge
                     | Some (G.List ["SumCase", G.Int i]) -> SumCase i
                     | Some (G.List ["Field", G.List fld]) ->
                         Field (r_string fld "field", r_int fld "index")
                     | Some (G.List ["Dead", G.Int i]) -> Dead i
                     | _ -> raise Error
                   in
                   { sch with edges = { src = source;
                                        dst = target;
                                        primary = ismain;
                                        lbl = elabel } :: sch.edges }
               | _ -> raise Error)
            sch
            elts
        in
        assert (check sch);
        sch
    | xx ->
        let rec tostring = function
          | G.String s -> "String "^s
          | G.Int i -> string_of_int i
          | G.Float f -> string_of_float f
          | G.List l -> String.concat_map ~left:"[ " ~right:" ]" ";" (fun (str,x) -> str ^ " -> " ^ tostring x) l
        in
        Printf.eprintf "[33mRoot not found while parsing GML graph:\n%s[0m\n[34mParsed as\n%s[0m\n%!"
          s (tostring xx);
        raise Error

let export_schema s =
  let next_id = let x = ref 0 in fun () -> let i = !x in incr x; i in
  assert (check s);
  let b = FBuffer.make 1013 in
  let pr x b = FBuffer.add b x in
  let (@>) g f = fun x -> (f (g x)) in (* inverse composition of functions *)
  let list f lst =
    pr "[ "
    @> (fun b -> List.fold_left_i (fun b x i -> (pr (Printf.sprintf "x%d " i) @> f x @> pr " ") b) b lst)
    @> pr "]"
  in
  let b = pr "graph [\n" b in
  let b,idmap =
    StringMap.fold
      (fun nodeid node (b,idmap) ->
         let id = next_id() in
         (pr (Printf.sprintf "  node [ id %d nodeid \"%s\" nlabel " id nodeid) @>
          (match node with
           | Leaf lf ->
               pr "[ Leaf \"" @>
               pr (StringOf.leaf lf) @>
               pr "\" ]"
           | _ -> pr "\"" @> pr (StringOf.node node) @> pr "\"") @>
          pr " ]\n")
           b,
         StringMap.add nodeid id idmap)
      s.nodes
      (b,StringMap.empty)
  in
  let b =
    List.fold_left
      (fun b e ->
         (pr (Printf.sprintf "  edge [ source %d target %d ismain %d elabel "
                (StringMap.find e.src idmap) (StringMap.find e.dst idmap)
                (if e.primary then 1 else 0)) @>
          (match e.lbl with
             | Multi_edge mk ->
                 pr "[ Multiedge " @>
                 (match mk with
                    | Kfields flds ->
                        pr "[ Kfields "
                        @> list (list (fun f -> pr (Printf.sprintf "%S" f))) flds
                        @> pr " ]"
                    | _ -> pr "\"" @> pr (StringOf.multi_key mk) @> pr "\"") @>
                 pr " ]"
             | Hidden_edge -> pr "\"Hiddenedge\""
             | Field (s,i) ->
                 pr (Printf.sprintf "[ Field [ field %S index %d ] ]" s i)
             | SumCase i ->
                 pr (Printf.sprintf "[ SumCase %d ]" i)
             | Dead i ->
                 pr (Printf.sprintf "[ Dead %d ]" i)) @>
          pr " ]\n")
           b)
      b
      s.edges
  in
  let b = pr "]\n" b in
  FBuffer.contents b


(** Second representation of schemas
   used for treatments of schemas (eg. calculus of diff)
   cf. type schema for the other representation *)
(* note: this data structure is not well-suited for (un)serialization
   hence, functions [to_tree] and [from_tree] are used
   to import/export from/to the first representation *)

type tree = Tnode of string * node * (edge_label * tree) list | Tlink of string

let tnode_id = function Tnode (id,_,_) | Tlink id -> id

let filter_dead = List.filter (function (Dead _,_) -> false | _ -> true)

let rec to_tree ?(n="root") s =
  let edges =
    List.map
      (fun e ->
         if e.primary then e.lbl, to_tree ~n:e.dst s
         else e.lbl, Tlink e.dst)
      (List.sort (fun e1 e2 -> compare e1.lbl e2.lbl) (succ_e s n))
  in
  Tnode (n, node s n, edges)

let rec from_tree ?(acc={nodes=StringMap.empty; edges = []}) t = match t with
  | Tnode (id, n, el) ->
      List.fold_left
        (fun acc (e,n) -> match n with
           | Tnode (id',_,_) ->
               let acc = from_tree ~acc n in
               { acc with edges = { src=id; dst=id'; primary=true; lbl=e }::acc.edges }
           | Tlink id'->
               { acc with edges = { src=id; dst=id'; primary=false; lbl=e }::acc.edges })
        { acc with nodes = StringMap.add id n acc.nodes }
        el
  | Tlink _ -> assert false

(** Utility functions on schema-trees *)

let rec fold f acc = function
  | Tnode (_,_,el) as tn ->
      List.fold_left (fun acc (_e,t) -> fold f acc t) (f acc tn) (filter_dead el)
  | Tlink _ as tl -> f acc tl

let fold_edges f =
  fold 
    (fun acc -> function
       | Tnode (id,_,el) -> List.fold_left (fun acc (e,t) -> f acc (id,e,tnode_id t)) acc el
       | _ -> acc)

let rec all_ids ?(acc=[]) =
  fold (fun acc -> function Tnode (id,_,_) -> id::acc | _ -> acc) acc

let rec find_id_opt id0 = function
  | Tnode (id,_,_el) as t when id = id0 -> Some t
  | Tnode (_,_,el) -> List.fold_left (fun acc (_e,t) -> if Option.is_some acc then acc else find_id_opt id0 t) None el
  | _ -> None

let find_id id0 t = Option.get (find_id_opt id0 t)

let rec map_up f = function
  | Tnode (id,n,el) -> f (Tnode (id,n,List.map (fun (e,n) -> e, map_up f n) el))
  | Tlink id -> f (Tlink id)

let is_parent t n n' =
  let rec aux = function
    | Tnode (id,_,_) when id = n' -> Some false
    | Tnode (id,_,_) as t when id = n -> Some (Option.is_some (find_id_opt n' t))
    | Tnode (_,_,el) -> List.fold_left (fun acc (_,t) -> match acc with None -> aux t | _ -> acc) None el
    | _ -> None
  in Option.get (aux t)

let out_edges = function
  | Tnode (_,_,el) -> el
  | _ -> []

let nice_print_path t id0 =
  let rec edge = function
    | Multi_edge _ -> "[]"
    | Hidden_edge -> ""
    | SumCase _ -> ""
    | Field (s,_) -> "/" ^ s
    | Dead _ -> "/<removed>" in
  let rec aux ?parent = function
    | Tnode (id,_n,el) when id = id0 ->
        if parent = Some Sum then
          Some (String.concat_map ~left:"/{" ~right:"}" "; "
                  (function (Field (s,_),_) -> s | _ -> assert false)
                  el)
        else if parent = None then Some "/" (* print the root *)
        else Some ""
    | Tnode (_,parent,el) ->
        List.fold_left
          (fun res (e,n) -> match res with None -> Option.map ((^) (edge e)) (aux ~parent n) | _ -> res)
          None el
    | _ -> None
  in Option.get (aux t)


(** Debug functions *)
let print_tree ?(color=false) t =
  (* ported from qmltoptest/dbc.qml *)
  let fstxlst f0 f1 f2 acc l =
    let rec aux acc l = match l with
      | [] -> acc
      | [hd] -> f2 acc hd
      | hd::tl -> aux (f1 acc hd) tl in
    match l with
      | [] -> acc
      | [hd] -> f2 acc hd
      | hd::tl -> aux (f0 acc hd) tl in
  let mkspace s = String.init (String.length s) (fun _ -> ' ') in
  let predge = function
    | Multi_edge Kint -> "[int]"
    | Multi_edge Kstring -> "[string]"
    | Multi_edge _ -> "[*]"
    | Hidden_edge -> "[*]"
    | SumCase i -> Printf.sprintf "[%d]" i
    | Field (s,i) -> Printf.sprintf "[%d-%s]" i s
    | Dead i -> Printf.sprintf "[%d]-DEAD" i in
  let prnode = function
    | Multi -> "SET"
    | Hidden -> "RECURSIVE"
    | Sum -> "SUM"
    | Product -> "RECORD"
    | Leaf lf -> StringOf.leaf lf in
  let rec aux pfx el =
    let el = match el with e,Tnode (id,n,el) -> e,Tnode (id,n,filter_dead el) | _ -> el in
    match el with
    | Dead _ as edge, _ -> predge edge
    | edge, Tnode (id,n,[]) ->
        Printf.sprintf "%s-(%s-%s)" (predge edge) id (prnode n)
    | edge, Tlink id ->
        Printf.sprintf "%s-->{%s}" (predge edge) id
    | edge, Tnode (id,n,[chld]) ->
        let v = Printf.sprintf "%s-(%s-%s)-" (predge edge) id (prnode n) in
        v ^ aux (pfx^(mkspace v)) chld
    | edge, Tnode (id,n,l) ->
        let v = Printf.sprintf "%s-(%s-%s)" (predge edge) id (prnode n) in
        let s = mkspace v in
        fstxlst
          (fun acc n -> acc ^ v ^ "-+-" ^ (aux (pfx^s^" | ") n) ^ "\n" ^ pfx)
          (fun acc n -> acc ^ s ^ " |-" ^ (aux (pfx^s^" | ") n) ^ "\n" ^ pfx)
          (fun acc n -> acc ^ s ^ " `-" ^ (aux (pfx^s^"   ") n))
          "" l
  in
  let stree = match t with
    | Tnode (_,_,[n]) -> "+-" ^ (aux "  " n)
    | Tnode (_,_,l) -> fstxlst
        (fun acc n -> acc ^ "+-" ^ (aux "| " n) ^ "\n")
        (fun acc n -> acc ^ "|-" ^ (aux "| " n) ^ "\n")
        (fun acc n -> acc ^ "`-" ^ (aux "  " n))
        "" l
    | _ -> assert false
  in
  if color then
    let replace a b s = String.replace s a b in
    ((replace "(" "[34m(" @* replace ")" ")[0m" @*
        replace "{" "[33m{" @* replace "}" "}[0m" @*
        replace "]" "][0m" @* replace "[" "[32m[" (* '[' should be done first *))
       stree)
  else stree
