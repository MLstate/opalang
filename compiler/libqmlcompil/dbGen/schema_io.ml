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

(* depends *)
module Format = Base.Format
module List = BaseList
module String = BaseString

(* alias *)
module SchemaGraph = SchemaGraphLib.SchemaGraph
module SchemaGraph0 = SchemaGraph.SchemaGraph0
module Gml = Graph.Gml

(* shorthands *)
module C = DbGen_common
module Q = QmlAst

(*----------------- DOT output -----------------*)
module SchemaDot = struct
  type t = SchemaGraph0.t
  module E = SchemaGraph.E
  module V = SchemaGraph.V
  let iter_edges_e = SchemaGraph0.iter_edges_e
  let iter_vertex = SchemaGraph0.iter_vertex


  let edge_attributes e =
    let x = E.label e in
      (if x.C.is_main then [`Style `Solid; `Weight 100] else [`Style `Dashed; `Weight 0])@
        (match x.C.label with
           | C.Field (str,i) -> [`Label (string_of_int i ^ "-" ^ str)]
           | C.SumCase i -> [`Label (string_of_int i)]
           | C.Multi_edge C.Kint -> [`Label "`int"]
           | C.Multi_edge C.Kstring -> [`Label "`string"]
           | C.Multi_edge (C.Kfields flds)-> [`Label (String.concat "; " (List.map (String.concat " ") flds))]
           | C.Hidden_edge -> [`Label "."])

  let default_edge_attributes _ = [`Dir `Forward]

  let vertex_attributes n =
    let x = V.label n in
    let id = x.C.nodeid in
    `Style (if x.C.plain then `Bold else `Solid) ::
      `Shape (if SchemaGraphLib.is_root n then `Doublecircle else match (V.label n).C.nlabel with C.Leaf _ -> `Box | _ -> `Circle) ::
        (* (`Style (if SchemaGraphLib.is_node_abstract n then `Dotted else `Solid)):: *)
        (`Comment (String.escaped (Format.to_string QmlPrint.pp#ty x.C.ty)))::
        (match (V.label n).C.nlabel with
           | C.Multi -> [`Label (id ^ ": *")]
           | C.Hidden -> [`Label (id ^ ": _")]
           | C.Sum -> [`Label (id ^ ": +")]
           | C.Product -> [`Label (id ^ ": ×")]
           | C.Leaf C.Leaf_int -> [`Label (id ^ ": int")]
           | C.Leaf C.Leaf_float -> [`Label (id ^ ": float")]
           | C.Leaf C.Leaf_text -> [`Label (id ^ ": text")]
           | C.Leaf C.Leaf_binary -> [`Label (id ^ ": binary")])

  let default_vertex_attributes _v = []

  let vertex_name n = "n"^((V.label n).C.nodeid)

  let get_subgraph _n = None

  let graph_attributes _t = [`Bgcolor 0xffffff(*; `OrderingOut*)]

  let merge_graphs t t' =
    let t' = SchemaGraph0.fold_vertex (fun v t -> SchemaGraph.replace_node t v (V.create (V.label v))) t' t' in
    let t'' = SchemaGraph0.fold_vertex (fun v t -> SchemaGraph0.add_vertex t v) t' t in
      SchemaGraph0.fold_edges_e (fun e t -> SchemaGraph0.add_edge_e t e) t' t''
end

module DotEngine = Graph.Graphviz.Dot(SchemaDot)

let to_dot t ch = DotEngine.output_graph ch t


(* ----------------- GML intput / output ----------------- *)

(* Removes all non strictly necessary type names, ie all non-recursive ones *)
(* todo: re-infer recursive types as well and get back definitions *)
let detype t =
  Schema_private.map_vertex
    (fun n -> if SchemaGraph0.in_degree t n > 1 then n
     else Schema_private.V.create { (Schema_private.V.label n) with C.ty = Q.typeNull })
    t
(* Re-infers types from leaves to get back a valid tree from a detyped tree *)
let retype t =
  let rec aux t n =
    let t, chld_types = List.fold_left_map
      (fun t e ->
         let t, chld =
           if (Schema_private.E.label e).C.is_main
           then aux t (Schema_private.E.dst e)
           else t, Schema_private.E.dst e in
         t, ((Schema_private.E.label e).C.label, (Schema_private.V.label chld).C.ty))
      t
      (SchemaGraph0.succ_e t n)
    in
    if (Schema_private.V.label n).C.ty <> Q.typeNull then t, n
    else match (Schema_private.V.label n).C.nlabel, chld_types with
      | C.Multi, [C.Multi_edge C.Kint, chld_type] ->
          SchemaGraphLib.set_node_type t n (Q.TypeName ([Q.TypeConst Q.TyInt; chld_type],
                                              Q.TypeIdent.of_string Opacapi.Types.map))
      | C.Multi, [C.Multi_edge C.Kstring, chld_type] ->
          SchemaGraphLib.set_node_type t n (Q.TypeName ([Q.TypeConst Q.TyString; chld_type],
                                              Q.TypeIdent.of_string Opacapi.Types.map))
      | C.Multi, [C.Multi_edge (C.Kfields _), chld_type] ->
          SchemaGraphLib.set_node_type t n (Q.TypeName ([chld_type], Q.TypeIdent.of_string Opacapi.Types.list))
      | C.Hidden, [C.Hidden_edge, chld_type] ->
          SchemaGraphLib.set_node_type t n chld_type
      | C.Sum, chld_types ->
          let chld_types =
            List.map
              (function (C.SumCase _, ty) -> ty | _ -> Schema_private.internal_error "Inconsistent database graph")
              chld_types in
          SchemaGraphLib.set_node_type t n (Q.TypeSumSugar chld_types)
      | C.Product, chld_types ->
          let chld_types =
            List.map
              (function (C.Field (s,_), ty) -> s, ty | _ -> Schema_private.internal_error "Inconsistent database graph")
              chld_types in
          let ty = Q.TypeRecord (QmlAstCons.Type.Row.make ~extend:false chld_types) in
          SchemaGraphLib.set_node_type t n ty
      | C.Leaf leaf, [] ->
          SchemaGraphLib.set_node_type t n (SchemaGraphLib.type_of_leaf leaf)
      | _ -> Schema_private.internal_error "Inconsistent database graph"
  in
  let root = SchemaGraphLib.get_root t in
  let t, _ = aux t root in
  t

module Output = Graph.Gml.Print (SchemaGraph0)
  (struct

     let node nl =
(*
       let opt f v = match v with
         | None -> Gml.List ["None", Gml.Int 0]
         | Some a -> Gml.List ["Some", f a] in
*)
       let label = match nl.C.nlabel with
         | C.Multi -> Gml.String "Multi"
         | C.Hidden -> Gml.String "Hidden"
         | C.Sum -> Gml.String "Sum"
         | C.Product -> Gml.String "Product"
         | C.Leaf lf ->
             Gml.List ["Leaf", Gml.String (match lf with
                                     | C.Leaf_int -> "int"
                                     | C.Leaf_float -> "float"
                                     | C.Leaf_text -> "text"
                                     | C.Leaf_binary -> "binary")]
       in
         ["nodeid", Gml.String nl.C.nodeid;
          "nlabel", label ] @
           if nl.C.ty = Q.typeNull then []
           else ["ty", Gml.String (String.to_hex (Format.to_string QmlPrint.pp#ty nl.C.ty))]

     let edge el =
       let list f l = Gml.List (List.mapi (fun i x -> Printf.sprintf "x%d" i, f x) l) in
       let label = match el.C.label with
         | C.Multi_edge k ->
             Gml.List [ "Multiedge",
                    (match k with
                       | C.Kint -> Gml.String "Kint"
                       | C.Kstring -> Gml.String "Kstring"
                       | C.Kfields flds -> Gml.List ["Kfields", list (list (fun x -> Gml.String x)) flds]) ]
         | C.Hidden_edge -> Gml.String "Hiddenedge"
         | C.SumCase i -> Gml.List ["SumCase", Gml.Int i]
         | C.Field (s,i) -> Gml.List ["Field", Gml.List ["field", Gml.String s; "index", Gml.Int i]]
       in
         ["elabel",label;
          "ismain",Gml.Int (if el.C.is_main then 1 else 0)]
   end)

module My_param_module_that_needs_to_be_rebound_and_named = struct
  module G = SchemaGraph0
  let copy g = g
  let empty () = G.empty
  let add_edge_e = G.add_edge_e
  let add_edge = G.add_edge
  let add_vertex = G.add_vertex
end

module Input = Graph.Gml.Parse
  (My_param_module_that_needs_to_be_rebound_and_named)
  (struct
     let err () = OManager.i_error "Database graph input: parse error"

     let node vl =
(*
       let opt parse = function
         | Gml.List ["None", Gml.Int 0] -> None
         | Gml.List ["Some", a] -> (try Some (parse a) with Match_failure _ -> err())
         | _ -> err () in
*)
       let nlabel = match List.assoc "nlabel" vl with
         | Gml.String "Multi" -> C.Multi
         | Gml.String "Hidden" -> C.Hidden
         | Gml.String "Sum" -> C.Sum
         | Gml.String "Product" -> C.Product
         | Gml.List ["Leaf", Gml.String lf] ->
             C.Leaf (match lf with
                     | "int" -> C.Leaf_int
                     | "float" -> C.Leaf_float
                     | "text" -> C.Leaf_text
                     | "binary" -> C.Leaf_binary
                     | _ -> err())
         | _ -> err ()
       in
         { C.from_package = "";
           C.nodeid = (match List.assoc "nodeid" vl with Gml.String id -> id | _ -> err());
           C.nlabel = nlabel;
           C.ty = (match List.assoc_opt "ty" vl with
                   | Some (Gml.String _) -> Q.typeNull (* TODO: change dependencies, and do : OpaParser.ty (String.from_hex a) *)
                   | _ -> Q.typeNull);
           C.default = None;
           C.constraints = [];
           C.context = QmlError.Context.pos (FilePos.nopos "input");
           C.plain = false (*TODO*)
         }

     let edge vl =
       let list f l = match l with
         | Gml.List l -> List.map (fun (_i,x) -> f x) l
         | _ -> err() in
       let label = match List.assoc "elabel" vl with
         | Gml.List [ "Multiedge", m ] ->
             C.Multi_edge (match m with
                           | Gml.String "Kint" -> C.Kint
                           | Gml.String "Kstring" -> C.Kstring
                           | Gml.List ["Kfields", fl] ->
                               C.Kfields (list (list (function Gml.String f -> f | _ -> err())) fl)
                           | _ -> err())
         | Gml.String "Hiddenedge" -> C.Hidden_edge
         | Gml.List ["SumCase", Gml.Int i] -> C.SumCase i
         | Gml.List ["Field", Gml.List ["field", Gml.String s; "index", Gml.Int i]] -> C.Field (s,i)
         | Gml.List ["Dead", Gml.Int _] -> C.Hidden_edge
         | _ -> err()
       in
         { C.label=label;
           C.is_main = match List.assoc "ismain" vl with Gml.Int 1 -> true | Gml.Int 0 -> false | _ -> err() }
   end)

let from_gml_string s =
  let file,ch = Filename.open_temp_file "DBschema" ".gml" in
  output_string ch s;
  close_out ch;
  let sch = Input.parse file in
  let sch = ((Obj.magic (sch: My_param_module_that_needs_to_be_rebound_and_named.G.t)) : Schema_private.t) in
    (* The trivial equality of these two types is beyond the sight of OCaml *)
  let sch = SchemaGraph0.remove_vertex sch
    ( { C.from_package = "";
        C.nodeid = "dead";
        C.nlabel = C.Hidden;
        C.ty = Q.typeNull;
        C.default = None;
        C.constraints = [];
        C.context = QmlError.Context.pos (FilePos.nopos "");
        C.plain = false;
      }) in
    (* clear dead edges and the dummy hidden node they point to *)
  let sch = retype sch in
  Sys.remove file;
  sch

let debugdot f sch = let ch = open_out (f^".dot") in to_dot sch ch; close_out ch

let to_gml_string (sch: Schema_private.t) =
  let sch = detype sch in
  Output.print Format.str_formatter sch;
  let s = Format.flush_str_formatter() in
  s

let to_gml (sch: Schema_private.t) chan =
  let sch = detype sch in
  let fmt = Format.formatter_of_out_channel chan in
  Output.print fmt sch;
  Format.pp_print_flush fmt ()
