(*
    Copyright Â© 2011 MLstate

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

#<Debugvar:DEBUG_XML>

module List = BaseList

(* -- *)

let debug fmt =
  #<If> Printf.eprintf ("[Xml]"^^fmt)
  #<Else> Printf.ifprintf stdout fmt
  #<End>


(* FIXME: unifier avec Qml *)
type value =
  | Value of string
(* FIXME: remettre ???  | Func of string * value list *)
    (* string * string list (* parameters *) *)
;;

type parameters = value StringMap.t (* name *) (* FIXME: stringmap (* ns *) *)
;;
type nid = int
;;
type node =
    { nns : string option
    ; nname : string
    ; npars : parameters
    ; ncontent : content list
    ; nparent : int
    }
and content =
  | Text of string
  | Node of nid
;;

type xml =
    { header : (string * value) list (* parameters *)
    ; count : int
    ; main : int list
    ; nodes : node IntMap.t (* nidmap *) }
;;

let empty_xml =
  { header = []
  ; count = 0
  ; main = []
  ; nodes = IntMap.empty
  }
;;

let add_node node elt = { node with ncontent = (elt :: node.ncontent) }

let new_node n o =
  { nns = None
  ; nname = n
  ; npars = StringMap.from_list o
  ; ncontent = []
  ; nparent = -1
  }

let rewrite l =
  let rec aux ok stack xml = function
    | `one ((n, _) as t) :: tl -> aux ok stack xml (`start t :: `stop n :: tl)
    | `start (n, o) :: tl ->
        debug "start: %s" n;
        let nid = xml.count
        and node = new_node n o in
        let stack, is_main = match stack with
        | (snid, snode) :: stl ->
            (nid, { node with nparent = snid }) ::
              (snid, add_node snode (Node nid)) :: stl, false
        | _ ->
            [nid, node], true
        in
        let xml = if is_main then { xml with main = nid :: xml.main } else xml in
        aux ok stack { xml with count = succ nid } tl
    | `stop n :: tl ->
        debug "stop: %s" n;
        begin match stack with
        | (nid, node) :: stl ->
            if node.nname = n then
              let node = { node with ncontent = List.rev node.ncontent } in
              aux ok stl { xml with nodes = IntMap.add nid node xml.nodes } tl
            else (
              debug "error in rewrite: closing tag %s which is not last open tag" n;
              aux false stack xml tl
            )
        | _ ->
            debug "error in rewrite: closing tag %s which is not open" n;
            aux false stack xml tl
        end
    | `text t :: tl ->
        debug "text: %s" t;
        begin match stack with
        | (nid, node) :: stl ->
            let node = add_node node (Text t) in
            aux ok ((nid, node)::stl) xml tl
        | _ ->
            debug "error in rewrite: text '%s' outside of tag (skipped)" t;
            aux false stack xml tl
        end
    | `space :: tl ->
        let l = if stack=[] then tl else (`text " " :: tl) in
        aux ok stack xml l
    | _ ->
        if stack=[] then { xml with main = List.rev xml.main }, ok
        else
          let id = fst (List.hd stack) in
          let name = (snd (List.hd stack)).nname in
          debug "error in rewrite: tag <%d:%s> is never closed" id name;
          aux false stack xml [`stop name]
  in
  aux true [] empty_xml l

let print_options m =
  StringMap.fold (
    fun x (Value y) acc ->
      acc ^ Printf.sprintf " %s=\"%s\"" x (String.escaped y)
  ) m ""

let print_header m =
  List.fold_left (
    fun acc (x, Value y) ->
      acc ^ Printf.sprintf " %s=\"%s\"" x (String.escaped y)
  ) "" m

(** print options and apply a function to each element
    -> utilisé par exemple pour remplacer
    action: par javascript: dans les lients *)
let print_options_f f m =
  StringMap.fold (
    fun x (Value y) acc ->
      acc ^ Printf.sprintf " %s=\"%s\"" x (String.escaped (f x y))
  ) m ""

let fold f1 f2 init xml =
  let rec aux ~path acc nid =
    let path = nid :: path in
    let node = IntMap.find nid xml.nodes in
    let acc = f1 acc node.nname node.npars in
    List.fold_left (
      fun acc x -> match x with
      | Text t -> f2 acc path t
      | Node n -> aux ~path acc n
    ) acc node.ncontent
  in
  List.fold_left (aux ~path:[]) init xml.main


(*       List.map ( *)
(*        fun x -> match x with *)
(*        | Text t -> f node.nname node.npars t *)
(*        | n -> n *)
(*       ) node.ncontent *)
let change_content f node =
  let rec aux acc_content acc_nodes = function
    | (Text t) :: tl ->
        let items, delete, insert =  f node.nname node.npars t in
        let acc_nodes = insert @ acc_nodes in
        if delete then None, acc_nodes
        else aux (items @ acc_content) acc_nodes (* FIXME: check rev *) tl
    | n :: tl -> aux (n :: acc_content) acc_nodes tl
    | _ -> Some (List.rev acc_content), acc_nodes
  in aux [] [] node.ncontent

let delete_node ?(replace=[]) xml parent nid =
  match parent with
  | None -> { xml with main = List.remove_all nid xml.main }
  | Some p ->
      let parent_node = IntMap.find p xml.nodes in
      let parent_content = List.replace (Node nid) (List.map (fun x -> Node x) replace) parent_node.ncontent in
      { xml with nodes = IntMap.add p { parent_node with ncontent = parent_content } xml.nodes }

let insert_node parent (xml, nid_list) (n, o, content) =
  debug "insert_node" ;
  let nid = xml.count
  and node = new_node n o in
  let node =
    { node with
        nparent = (match parent with Some p -> p | _ -> -1)
        ; ncontent = content } in
  { xml with nodes = IntMap.add nid node xml.nodes ; count = succ xml.count },
  nid :: nid_list

(** map sur le texte des noeuds Xml *)
(* FIXME: rename *)
let map f xml =
  let rec aux parent xml nid =
    let node = IntMap.find nid xml.nodes in

    match change_content f node with
    | Some ncontent, _insert ->
        (* nouveau contenu de la node *)
        let xml =
          { xml with nodes =
              IntMap.add nid { node with ncontent = ncontent } xml.nodes }
        (* liste des nid à modifier *)
        and next = List.fold_left (
          fun acc x -> match x with
          | Node n -> n :: acc
          | _ -> acc
        ) [] node.ncontent
        in
        List.fold_left (aux (Some nid)) xml next
    | _, insert ->
        let xml, nid_list = List.fold_left (insert_node parent) (xml, []) insert in
        delete_node xml parent nid ~replace:nid_list
  in
  List.fold_left (aux None) xml xml.main

let print ?(header=false) ?(pretty=false) xml =
  let nl = if pretty then "\n" else "" in
  let rec aux b nid =
    let node = IntMap.find nid xml.nodes in
    let b = FBuffer.add b (Printf.sprintf "<%s%s>%s" node.nname (print_options node.npars) nl) in
    let b = List.fold_left (
      fun acc x -> match x with
      | Text t -> FBuffer.add acc t
      | Node n -> aux acc n
    ) b node.ncontent in
    FBuffer.add b (Printf.sprintf "</%s>%s" node.nname nl)
  in
  let b = FBuffer.make ~name:"Xml.print" 256 in
  let b =
    if header then
      FBuffer.add b (Printf.sprintf "<?xml%s ?>%s" (print_header xml.header) nl)
    else b in
  FBuffer.contents (List.fold_left aux b xml.main)

(** construit un stringmap: id -> nid pour un xml
    retourne également un booléen FAUX si les ids sont correctes, VRAI si une id est redéfinie
*)
let build_ids ?(tag="id") xml =
  IntMap.fold (
    fun id node (acc, err) ->
      if StringMap.mem tag node.npars then
        let (Value value) = StringMap.find tag node.npars in
        StringMap.add value id acc,
      (StringMap.mem value acc)
      (* Base.warning_if_true (StringMap.mem value acc) *)
      (*   (Printf.sprintf "id %s is defined several times" value) *)
      or err
      else (acc, err)
  ) xml.nodes (StringMap.empty, false)

(** construit un stringmap: class -> nid list (classée par position décroissante) pour un xml *)
let build_classes ?(tag="class") xml =
  IntMap.fold (
    fun id node acc ->
      if StringMap.mem tag node.npars then
        let (Value value) = StringMap.find tag node.npars in
        let list = if StringMap.mem value acc then StringMap.find value acc else [] in
        StringMap.add value (id::list) acc
      else acc
  ) xml.nodes StringMap.empty

(** liste (id, tag)* des parents du noeud nid *)
let tag_path xml nid =
  let rec aux path id =
    if id = 0 then path
    else
      let node = IntMap.find id xml.nodes in
      aux ((node.nparent, node.nname)::path) node.nparent
  in
  aux [] nid
