(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)
(* --------------------------------------------------------------- *)
(* the following work on the graph converted to tree, from Dbgraph *)
(* --------------------------------------------------------------- *)

module D = Dbgraph

(** Distance between graphs *)
type dist =
  | Zero (* The graphs are equivalent (up to renumbering) *)
  | VeryClose of int (* The graphs are very close (just some renaming) *)
  | Close of int (* The graphs are quite close (some add/remove) *)
  | VeryFar (* The graphs are very different (and we don't know how to migrate) *)

let (++) d1 d2 = match d1,d2 with
  | Zero, d | d, Zero -> d
  | VeryFar, _ | _, VeryFar -> VeryFar
  | Close d1, Close d2 -> Close (d1+d2)
  | Close d, _ | _, Close d -> Close d
  | VeryClose d1, VeryClose d2 -> VeryClose (d1+d2)

let (<+) d1 d2 = match d1,d2 with
  | Zero, _ -> true
  | _, Zero -> false
  | VeryClose d1, VeryClose d2 -> d1 <= d2
  | VeryClose _, _ -> true
  | _, VeryClose _ -> false
  | Close d1, Close d2 -> d1 <= d2
  | Close _, _ -> true
  | _, Close _ -> false
  | VeryFar, VeryFar -> true

let dist_compare d1 d2 =
  if d1 = d2 then 0 else if d1 <+ d2 then -1 else 1

let dmin d1 d2 = match d1,d2 with
  | Zero, _ | _, Zero -> Zero
  | VeryFar, d | d, VeryFar -> d
  | VeryClose d1, VeryClose d2 -> VeryClose (min d1 d2)
  | VeryClose d, _ | _, VeryClose d -> VeryClose d
  | Close d1, Close d2 -> Close (min d1 d2)

let dist_to_string = function
  | Zero -> "Zero"
  | VeryClose i -> "VeryClose "^string_of_int i
  | Close i -> "Close "^string_of_int i
  | VeryFar -> "VeryFar"

let dist_edges e1 e2 =
  if e1 = e2 then Zero else match e1,e2 with
    | D.SumCase _, D.SumCase _ -> Zero
    | D.Field (fld1,_), D.Field (fld2,_) ->
        if fld1 = fld2 then Zero
        else VeryClose (int_of_float (Sed.simple_sed fld1 fld2)) (* fixme: ?? *)
    | _, _ -> VeryFar

let close n = if n = 0 then Zero else Close n
let veryclose n = if n = 0 then Zero else VeryClose n

(** Some memoisation for the computation of distances *)
module MemoDist = Weak.Make
  (struct
     type t = D.tree *  D.tree * dist
     let equal (t1,t2,_) (t'1,t'2,_) = t1 == t'1 && t2 == t'2
     let hash (t1,t2,_) = Hashtbl.hash (t1,t2)
   end)

let distance, best_match =
  let memo =
    let tbl = MemoDist.create 53 in
    fun t1 t2 f ->
      try
        let _,_,dist = MemoDist.find tbl (t1,t2,Zero) in dist
      with Not_found ->
        let dist = f t1 t2 in
        MemoDist.add tbl (t1, t2, dist); dist
  in

  (** Some notion of distance between trees. Useful for finding matching fields.
      Todo: formalise the formula and justify *)
  let rec distance t1 t2 =
    memo t1 t2
      (fun t1 t2 ->
         if t1 = t2 then Zero else
         match t1, t2 with
           | D.Tnode (_, D.Multi, [e1, t1']), D.Tnode (_, D.Multi, [e2, t2']) ->
               dist_edges e1 e2 ++ distance t1' t2'
           | D.Tnode (_, D.Hidden, [D.Hidden_edge, t1']), D.Tnode (_, D.Hidden, [D.Hidden_edge, t2']) ->
               distance t1' t2'
           | D.Tnode (_, D.Sum, chld1), D.Tnode (_, D.Sum, chld2) ->
               let chld1 =  D.filter_dead chld1 in
               let matchlist = best_match chld1 chld2 in
               let matchlen = List.length matchlist in
               let len1,len2 = List.length chld1, List.length chld2 in
               if matchlen < len1 && matchlen < len2
               then VeryFar (* adding and removing cases at the same time is dangerous, we forbid for now *)
               else
                 close (abs (len1 - matchlen) + abs (len2 - matchlen)) ++
                   List.fold_left (fun acc (_,_,dist) -> acc ++ dist) Zero matchlist
           | D.Tnode (_, D.Product, chld1), D.Tnode (_, D.Product, chld2) ->
               let chld1 =  D.filter_dead chld1 in
               let matchlist = best_match chld1 chld2 in
               let lost = List.filter (fun c -> not (List.exists (fun (c1,_,_) -> c1 = c) matchlist)) chld1 in
               let added = List.filter (fun c -> not (List.exists (fun (_,c2,_) -> c2 = c) matchlist)) chld2 in
               let getfield = function (D.Field (fld,_),_) -> fld  | _ -> assert false in
               if
                 (#<If:DBGEN_BUTCHER> false #<Else> true #<End>) &&
                 (List.exists (fun el -> List.exists (fun el' -> getfield el = getfield el') lost) added)
               then (* some edges don't match but have the same name: that's bad *)
                 VeryFar
               else
                 close (List.length lost + List.length added) ++
                   List.fold_left (fun acc (_,_,dist) -> acc ++ dist) Zero matchlist
           | D.Tnode (_, D.Leaf l1, []), D.Tnode (_, D.Leaf l2, []) ->
               if l1=l2 then Zero else VeryFar
           | D.Tlink (_id1), D.Tlink (_id2) -> Zero (* FIXME: links not handled yet. *)
               (* Since they are only for simply recursive types for now, we'll bet on zero... *)
           | _ ->
               (* add some cross-cases: insertion of nodes on top of existing ones, ... *)
               VeryFar)

  (** Finds the (edge,node) closest to en in enl *)
  and min_distance (e,n) enl = match enl with
    | (e0,n0)::en0r ->
        List.fold_left
          (fun (enmin,distmin) (e', n') ->
             if distmin = Zero then (enmin, distmin) else
               let dist = dist_edges e e' ++ distance n n' in
               if distmin <+ dist then (enmin, distmin) else ((e',n'), dist))
          ((e0,n0), dist_edges e e0 ++ distance n n0) en0r
    | [] -> raise Not_found

  (** finds the best matching between two lists of (edge,node). May be partial if
      the sizes differ.
      @return a list of (edge1,node1),(edge2,node2),distance *)
  and best_match enl1 enl2 = (* todo: less naive algorithm ? *)
    if enl2 = [] then [] else
      (* compute all min distances, filter out non-matches *)
      let mins = List.map (fun en -> let en2, dist =  min_distance en enl2 in en,en2,dist) enl1 in
      let mins = List.filter (fun (_,_,dist) -> dist <> VeryFar) mins in
      (* sort by distance *)
      let mins = List.sort (fun (_,_,dist) (_,_,dist') -> dist_compare dist dist') mins in
      (* deal with duplicates (by recursion) *)
      let rec aux (avail1,avail2,acc) mins = match mins with
        | ((en1, en2, _dist) as min) :: r ->
            if List.exists (fun (e,_) -> e = fst en1) avail1
              &&  List.exists (fun (e,_) -> e = fst en2) avail2
            then
              aux (Base.List.remove_first en1 avail1, Base.List.remove_first en2 avail2, min::acc) r
            else
              acc @ best_match avail1 avail2
        | [] -> acc
      in aux (enl1, enl2, []) mins
  in
  distance, best_match

let rec idmap ?(acc=[]) t1 t2 = match t1,t2 with
  | D.Tnode (id1, n, el1), D.Tnode (id2, n', el2) ->
      assert (n=n');
      let el1 =  D.filter_dead el1 in
      let edges_match = best_match el1 el2 in
      let edge_mapping = match n with
        | D.Multi | D.Hidden | D.Leaf _ -> []
        | D.Sum | D.Product -> List.map (fun ((e1,_),(e2,_),_) ->  D.edge_num e1,  D.edge_num e2)  edges_match in
      List.fold_left
        (fun acc ((_,n1),(_,n2),_dist) -> idmap ~acc n1 n2)
        (((id1,id2), edge_mapping)::acc)
        edges_match
  | D.Tlink (_id1), D.Tlink (_id2) -> acc
  | _, _ -> acc

##extern-type [normalize] diff = { t1:  D.tree; t2:  D.tree; idmap: ((string * string) * (int * int) list) list; removed: string list; added: string list }
(* the idmap is of the form [((old_nodeid, new_nodeid),[(old_edgeid,new_edgeid); ...]); ...], ie
   (node mapping,list_of_outgoing_edges_mappings) *)

let rev_image df id =
  Option.map (fun ((x,_),_) -> x) (Base.List.find_opt (fun ((_x,y),_) -> y = id) df.idmap)

##register [opacapi] empty_diff : diff
let empty_diff =
  let dummy = D.Tnode ("root", D.Product, []) in
  { t1=dummy; t2=dummy; idmap=[]; removed=[]; added=[] }

##register [opacapi] diff : string, string -> diff
let diff s1 s2 =
  let s1 = D.import_schema s1
  and s2 = D.import_schema s2 in
  let t1 = D.to_tree s1
  and t2 = D.to_tree s2 in
  #<If:NO_DATABASE_UPGRADE>
    if t1 <> t2 then
      (#<If:NO_DATABASE_UPGRADE$minlevel 2>
         prerr_endline "WARNING: your datatabase schema has changed, and MLSTATE_NO_DATABASE_UPGRADE is set! This is likely to screw up."
       #<Else>
         prerr_endline "ERROR: your datatabase schema has changed, and MLSTATE_NO_DATABASE_UPGRADE is set! I am too scared to continue";
         exit 1
       #<End>);
    { t1=t2; t2=t2; idmap=[]; removed=[]; added=[] }
  #<Else>
  let idmap = idmap t1 t2 in
  let removed, added =
    let x1, x2 = List.split (List.map fst idmap) in
    List.filter (fun id -> not (List.mem id x1)) (D.all_ids t1),
    List.filter (fun id -> not (List.mem id x2)) (D.all_ids t2)
  in
  { t1=t1; t2=t2; idmap=idmap; removed=removed; added=added }
  #<End>

##register str_distance : diff -> string
let str_distance df =
  let dist = distance df.t1 df.t2 in
  dist_to_string dist

(** from an edge id in the new schema and its source node, try to find the
    matching edge number in the original schema *)
let find_previous_edge df nodeid2 edgeid2 =
  try
    let (_nodeid1,_), edgemap = List.find (fun ((_,nid2), _) -> nid2 = nodeid2) df.idmap in
    let (edgeid1,_) = List.find (fun (_,eid2) -> eid2 = edgeid2) edgemap in
    Some edgeid1
  with Not_found -> None

(** get the matching edge in the original database schema, returning
    a safe fresh id if it can't be found *)
##register [opacapi] matching_edge : diff, string, int -> int
let matching_edge df nodeid2 edgeid2 =
  if df.t1 == df.t2
  then
    (* happens only when the database have just been created (cf. empty_diff) *)
    edgeid2
  else
  match find_previous_edge df nodeid2 edgeid2 with
    | Some e -> e
    | None -> match rev_image df nodeid2 with
        | Some nodeid1 -> (* The edge has been added to an existing node: get a fresh id *)
            assert (List.mem
                      (D.tnode_id (snd (List.find (fun (e,_) -> D.edge_num e = edgeid2) (D.out_edges (D.find_id nodeid2 df.t2)))))
                      df.added);
            let eids = List.map (fun (e,_) -> D.edge_num e) (D.out_edges (D.find_id nodeid1 df.t1)) in
            (List.fold_left max 0 eids) + edgeid2 + 1
        | None -> (* No previous edge, use the number as is *)
            edgeid2

(* Return codes used in dbGen_private, ensure consistency *)
##register [opacapi] diff_status : diff -> int
let diff_status df =
  match distance df.t1 df.t2 with
    | Zero -> 0
    | VeryClose i -> min 1023 i
    | Close i -> 1024 + min 1023 i
    | VeryFar -> 2048

##register [opacapi] diff_message : string, diff -> string
let diff_message msg df =
  let add_removes pfx =
    (Base.String.concat_map ~left:pfx ~nil:"" pfx
       (fun id -> Printf.sprintf "the node at %s was REMOVED" (D.nice_print_path df.t1 id))
       (List.fold_left
          (fun acc n -> List.filter (fun n' -> not (D.is_parent df.t1 n n')) acc)
          df.removed df.removed)) ^
      (Base.String.concat_map ~left:pfx ~nil:"" pfx
         (fun id -> Printf.sprintf "a new node was added at %s" (D.nice_print_path df.t2 id))
         (List.fold_left
            (fun acc n -> List.filter (fun n' -> not (D.is_parent df.t2 n n')) acc)
            df.added df.added))
  in
  let renamings pfx =
    D.fold_edges
      (fun acc (id1,e,_id2) -> match e with
         | D.Field (fld,edgeid) ->
             (match find_previous_edge df id1 edgeid with
                | Some old_edgeid ->
                    let old_id1 = Option.get (rev_image df id1) in
                    let old_e,_ =
                      List.find (fun (e,_n) -> D.edge_num e = old_edgeid) (D.out_edges (D.find_id old_id1 df.t1)) in
                    (match old_e with
                       | D.Field (old_fld,_) when old_fld <> fld ->
                           Printf.sprintf "%s%sfield '%s' was renamed to '%s' at %s" acc pfx
                             old_fld fld (D.nice_print_path df.t2 id1)
                       | _ -> acc)
                | _ -> acc)
         | _ -> acc)
      "" df.t2
  in
  let pfx = "\n    - " in
  msg ^
  match distance df.t1 df.t2 with
    | Zero -> "no differences"
    | VeryClose i ->
        Printf.sprintf "some light alteration%s:%s" (if i>1 then "s" else "")
          (renamings pfx)
    | Close i ->
        Printf.sprintf "%d significant alteration%s:%s%s" i (if i>1 then "s" else"")
          (add_removes pfx)
          (match renamings pfx with "" -> "" | s -> "\nand some lighter alterations: "^s)
    | VeryFar -> "big alterations, migration is not possible"

let diffed_tree df =
  D.map_up
    (function
       | D.Tnode (id,n,el) ->
           let el = List.map
             (fun (e,n) ->
                (match e with
                   | D.SumCase i -> D.SumCase (matching_edge df id i)
                   | D.Field (s,i) -> D.Field (s, matching_edge df id i)
                   | e -> e),n)
             el
           in D.Tnode (id, n, el)
       | D.Tlink id -> D.Tlink id)
    df.t2

(* Apply on a diffed tree only *)
let rec propagate_dead_edges ?(survive=false) df t =
  D.map_up
    (function
       | D.Tnode (id,n,el) ->
           let dead_edges =
             match rev_image df id with
               | Some id1 ->
                   Base.List.filter_map
                     (function
                        | D.Dead i, _ ->
                            Some (D.Dead i, D.Tnode("dead",D.Hidden,[]))
                        | ((D.Field (_,i) | D.SumCase i), _) as old_e when not (List.exists (fun (e,_) -> D.edge_num e = i) el) ->
                            if survive then Some old_e else
                            Some (D.Dead i, D.Tnode("dead",D.Hidden,[]))
                        | _, _ -> None)
                     (D.out_edges (D.find_id id1 df.t1))
               | None -> []
           in
           D.Tnode (id, n, el @ dead_edges)
       | D.Tlink id -> D.Tlink id)
    t

##register [opacapi] get_diffed_schema : diff -> string
let get_diffed_schema df =
  let t = diffed_tree df in
  let t = propagate_dead_edges ~survive:(Base.debug_getenv_toggle "DATA_SURVIVAL_MODE") df t in
  let s = D.from_tree t in
  D.export_schema s

(* For debug *)
##register [opacapi] print_tree \ print_serialised_tree : string -> string
let print_serialised_tree sch =
  let s = D.import_schema sch in
  let t = D.to_tree s in
  D.print_tree ~color:(Unix.isatty (Unix.descr_of_out_channel stdout)) t
