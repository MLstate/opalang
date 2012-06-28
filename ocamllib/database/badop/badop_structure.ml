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
   @author Raja Boujbel
**)
(** This module describes the basic structures that form the skeleton of our
    graph database: keys and paths for accessing the structured data, path
    patterns and node configuration to define node-specific properties.

    Only contains types, sigs and signed modules: no mli
*)

#<Debugvar:BADOP_DEBUG>

module D = Dbgraph
module Array = BaseArray
module List = BaseList
module Hashtbl = BaseHashtbl

module Key: sig
  type t = Keys.t =
           | IntKey of int
           | StringKey of string
           | ListKey of Keys.t array
           | VariableKey of int (** Only valid within a transaction; resolved at commit to fresh keys *)
end = Keys (* from the impl/ subdirectory *)

module Path: sig
  type t = Path.t
  val root: t
  val add: t -> Keys.t -> t (** Adds at the end of path *)
  val pop_last: t -> (Keys.t * t) option (** The reverse of [add] *)
  val last: t -> Keys.t (** FIXME: remove ; this is unsafe *)
  val of_list: Keys.t list -> t
  val to_list: t -> Keys.t list
  val to_string: t -> string
end = Path (* from the impl/ subdirectory *)



module Node_property = struct
  type 'a key_pattern = Star | Equals of 'a

  type path_pattern =
    | IntKey of int key_pattern
    | StringKey of string key_pattern
    | ListKey of path_pattern array

  (** The type of what is stored at a given node *)
  type node_type =
    | Any
        (** (default) This node can store any data (but not a link) *)
    | Int | Text | Binary | Float
        (** Follows the implementations in DataImpl: check that only the given type is stored at that node *)
    | Unit
        (** This node doesn't contain value, abstract node (sum, mulit,...)*)
    | Link of path_pattern list
        (** This node must contain a link to the given pattern *)

  (** Tells the behaviour to adopt in case you commit a transaction that
      manipulates a node that changed in the meantime *)
  type merge_policy =
    | ConflictOnRead
        (** Conflicts if your transaction read the value, and it changed before your commit *)
    | ConflictOnWrite
        (** (default) Conflicts when two concurrent transaction wrote to the same node *)
    | TakeLast | TakeMin | TakeMax
        (** Never conflict, resp. last, minimum and maximum value wins. TakeLast means always overwrite *)
    | Add
        (** Never conflict, assume an int node where all changes are relative, and take the sum of changes *)

  type node_config = {
    node_type: node_type;
    merge_policy: merge_policy;
    property_searchable: bool; (** if true (default), build a search index for this node *)
  }

  let (default: node_config) = {
    node_type = Any;
    merge_policy = ConflictOnWrite;
    property_searchable = true;
  }

  type config = (path_pattern list * node_config) list

  (* Tree representation of node_config *)
  type tree = Node of node_config * ((path_pattern * tree) list)  | Leaf of node_config


  module StringOf = struct
    let intkey_pattern = function
      | Star -> ".*"
      | Equals i -> string_of_int i

    let stringkey_pattern = function
      | Star -> "*."
      | Equals s -> s

    let rec path_pattern = function
      | IntKey k -> intkey_pattern k
      | StringKey k -> stringkey_pattern k
      | ListKey a -> Array.print path_pattern a

    let node_type = function
      | Any -> "any"
      | Int -> "int"
      | Text -> "text"
      | Binary -> "binary"
      | Float -> "float"
      | Unit -> "unit"
      | Link p -> "link: "^(List.print path_pattern p)

    let merge_policy = function
      | ConflictOnRead -> "Conflict on read"
      | ConflictOnWrite -> "Conflict on write"
      | TakeLast -> "Take last"
      | TakeMin -> "Take min"
      | TakeMax -> "Take max"
      | Add -> "Add"

    let node_config nc =
      #<If$minlevel 5>
        Printf.sprintf "[32m{ [37mtype : [32m%s[37m; merge : [32m%s[37m; searchability : [32m%b }[0m"
        (node_type nc.node_type) (merge_policy nc.merge_policy) nc.property_searchable
      #<Else>
        Printf.sprintf "[32m{ [37mtype : [32m%s }[0m" (node_type nc.node_type)
      #<End>

    let config nodconf =
      List.to_string
        (fun (p, nc) ->
           Printf.sprintf "\n\t[34m%s [0m-> %s"
             (List.print path_pattern p) (node_config nc)) nodconf


    let tree tr =
      let rec aux prof tr =
        let pad = String.make prof ' ' in
        match tr with
        | Node (c, trl) ->
            Printf.sprintf "[37m%s[0m (%d)\n%s"
              (node_config c)
              (List.length trl)
              (List.to_string
                 (fun (path, tr) ->
                    Printf.sprintf "[34m%s%s- %s[0m :%s"
                      pad pad
                      (path_pattern path)
                      (aux (succ prof) tr)) trl)
        | Leaf c -> Printf.sprintf "[37m%s[0m\n" (node_config c)
      in
      aux 0 tr


  end


  let root_path_pattern = IntKey (Equals 1)
  let root_node_config = { node_type = Int ; merge_policy = ConflictOnWrite; property_searchable = true; }

  let debug ?(level=1) fmt =
    let _level = level + 10 in
    #<If$minlevel _level> Printf.fprintf stderr ("[37m[Properties][0m"^^fmt^^"\n%!")
    #<Else> Printf.ifprintf stderr ("[37m"^^fmt^^"[0m\n%!")
    #<End>


  (* according given schema, construct a config, where each node is associated to its own properties *)
  let construct schema =

    let get_path = function
      | D.Field (_, i)
      | D.SumCase i ->  (IntKey (Equals i))
      | D.Multi_edge D.Kint ->  (IntKey Star)
      | D.Multi_edge D.Kstring ->  (StringKey Star)
      | D.Multi_edge D.Kfields l ->
          (ListKey (Array.of_list (List.concat_map (fun x ->
                                                      ((List.map (fun _y -> StringKey (Star) ) x )) )l)))
      | _ ->  (IntKey Star) in

    let nodetyp = function
      | D.Sum | D.Leaf D.Leaf_int -> Int
      | D.Leaf D.Leaf_text -> Text
      | D.Leaf D.Leaf_float -> Float
      | D.Leaf D.Leaf_binary -> Binary
      | D.Product | D.Hidden | D.Multi
          -> Unit
    in


    let rec aux (confs : config) nodes prefix name s =
      let def ntype =
        { node_type = ntype;
          merge_policy = ConflictOnWrite;
          property_searchable = true; } in

      let edges = List.sort (fun e1 e2 -> compare e1.D.lbl e2.D.lbl) (List.filter (fun e -> e.D.src = name) s.D.edges) in
      let nodeconfig =
        match edges with
        | [] ->
            let node = StringMap.find name s.D.nodes in
            let conf = def (nodetyp node) in
            let path = (List.rev prefix) in
            (path, conf) :: confs
        | _ ->
            List.concat_map
              (fun e ->
                 let nodes = StringMap.add name prefix nodes in
                 let egl = e.D.lbl in
                 let prefixed_part = (get_path egl) :: prefix in
                 let path =  (List.rev prefix) in
                 let node = StringMap.find name s.D.nodes in
                 let nodeconf = def (nodetyp node) in
                 let confs = (path, nodeconf) :: confs in
                 if e.D.primary then
                   aux confs nodes prefixed_part e.D.dst s
                 else
                   (let linkpath =  (List.rev ((StringMap.find e.D.dst nodes) )) in
                    let path =  (List.rev prefixed_part) in
                    let nodeconf = def (Link linkpath) in
                    (path, nodeconf) :: confs)

              ) edges in
      nodeconfig in

    let schema = D.import_schema schema in
    let final_conf = aux [] StringMap.empty [root_path_pattern] "root" schema in
    List.uniq (List.sort compare final_conf)



  let to_tree_aux config =

    let extract_opt f lst =
      let rec aux acc l =
        match l with
        | [] -> None
        | x::y when f x -> Some (x, List.rev_append acc y)
        | x::y -> aux (x::acc) y
      in aux [] lst in

    let existsandadd tree conf path =
      let rec aux tr path =
        match tr,path with
        | Leaf c, a::b ->
            Node (c, [(a, aux (Leaf conf) b)])
        | Leaf c, [] -> if c = conf then tr else Leaf conf
        | Node (c, trl), (a::b) ->
            (match extract_opt (fun x -> (fst x) = a) trl with
             | Some ((_,trr), trl) -> Node (c, ((a, (aux trr b))::trl))
             | None -> Node (c, (a, aux (Leaf conf) b) :: trl))
        | Node (c, trl), [] ->
            if c = conf then tr else Node (conf, trl)
      in
      match path with
      | [_x] -> tree
      | _x::y -> aux tree y
      | [] -> assert false
    in

    let rec transform_config tr (paths:config) =
      match paths with
      | [] -> tr
      | (x,c)::y ->
          let newtr = existsandadd tr c x in
          transform_config newtr y
    in

    let paths = config in
    let defnode = Node (root_node_config, []) in
    transform_config defnode paths

  (* Return the node_config in a tree form.
   * Memoize the result, for futur request (we "reconstruct" each write) *)
  let to_tree =
    let tbl = Hashtbl.create 5 in
    fun config ->
      let tree =
        match Hashtbl.find_opt tbl config with
        | Some t -> t
        | None ->
          (let t = to_tree_aux config in
           Hashtbl.add tbl config t;
           t) in
  debug ~level:2 "Tree :\n%s\n" (StringOf.tree tree);
  tree


  (* Return the [node_config] of given tree *)
  let of_tree tree =
    let rec aux path tr acc =
      match tr with
      | Node (conf, subtreelst) ->
          let acc = (path, conf) :: acc in
          List.fold_left (fun ac (ppat, tr) -> aux (path @ [ppat]) tr ac) acc subtreelst
      | Leaf conf ->
          (path, conf) :: acc
    in
    aux [root_path_pattern] tree []


  (* According a path (from a link node), return the configuration of the linked node *)
  let follow_link (lnk : path_pattern list) (config:config) =
    let lst =
      List.filter
        (fun path ->
           if List.length lnk <> List.length path then false else
             fst (List.fold_left_map2
                    (fun acc x x' ->
                       let xx =
                         match x, x' with
                         | (StringKey Star | IntKey Star), _
                         | _, (StringKey Star | IntKey Star) -> true
                         | IntKey (Equals i), IntKey (Equals i') -> i= i'
                         | StringKey (Equals s), StringKey (Equals s') -> s= s'
                         | ListKey a, ListKey a' -> Array.length a = Array.length a'
                         | _ -> false in
                       (xx && acc), () ) true lnk path)) (fst (List.tail_split config)) in
    List.assoc (List.hd lst) config



  (* utils *)
  let equals_pattern e1 p2 =
    match e1, p2 with
    | _ , Star -> true
    | e, Equals e' -> e = e'

  let rec equals_path_pattern pp1 pp2 =
    match pp1, pp2 with
    | Keys.IntKey i, IntKey i' -> equals_pattern i i'
    | Keys.StringKey s, StringKey s' -> equals_pattern s s'
    | Keys.StringKey _, IntKey Star
    | Keys.IntKey _, StringKey Star -> true
    | Keys.ListKey a, ListKey a' when Array.length a = Array.length a' ->
        let l = Array.to_list a and l' = Array.to_list a' in
        let res, _ =
          List.fold_left_map2
            (fun  acc x x' -> ((equals_path_pattern x x') && acc), ()) true l l' in
        res
    | _ -> (debug "don't match : %s - %s" (Keys.to_string pp1) (StringOf.path_pattern pp2); false)



  (* Return the [node_config] of given path (from given config).
   * If a link is found, it is followed.
   * Configuration of invisible path (as schema & co) are hardcoded.
   * *)
  let get_node_config path config =
    if config = [] then raise (Invalid_argument "get_node_config: Empty config");
    debug "Looking for a pattern matching %s" (Path.to_string path);
    let path = Path.to_list path in
    match path with
    | Keys.IntKey 2 :: Keys.IntKey (-1) :: [] ->
        { node_type = Int; merge_policy = ConflictOnWrite; property_searchable = true }
    | Keys.IntKey 2 :: Keys.IntKey 0 :: [] ->
        { node_type = Binary; merge_policy = ConflictOnWrite; property_searchable = true }
    | Keys.IntKey 1 :: [] ->
        { node_type = Int; merge_policy = ConflictOnWrite; property_searchable = true }
    | _ ->
        (let conftree = to_tree config in

         let rec pp2kl = function
           | IntKey (Equals i) -> Keys.IntKey i
           | IntKey Star -> Keys.IntKey 0
           | StringKey (Equals s) -> Keys.StringKey s
           | StringKey Star -> Keys.StringKey ""
           | ListKey a -> Keys.ListKey (Array.map pp2kl a)
         in

         let rec aux path tree =
           match tree, path with
           | Node (c, _), []
           | Leaf c , [] -> relance c []
           | Node (_, ptl), (p::pl) ->
               debug ~level:3 "lookign for %s in %s" (Keys.to_string p) (List.print (fun x -> StringOf.path_pattern (fst x)) ptl);
               debug ~level:4 "subtree : %s" (StringOf.tree tree);
               (match List.find_opt (fun x -> equals_path_pattern p (fst x)) ptl with
                | Some (_, subtree) -> aux pl subtree
                | None -> assert false)
           | Leaf c, (_::_pl) -> relance c path

         and relance nodeconf restpath =
           match nodeconf.node_type with
           | Link p ->
             (let new_path = List.append (List.map pp2kl (List.tl p)) restpath in
              debug "link  : relance for path : %s" (List.print Keys.to_string new_path);
              aux new_path conftree)
           | _ -> nodeconf
         in
         aux (List.tl path) conftree)


end
