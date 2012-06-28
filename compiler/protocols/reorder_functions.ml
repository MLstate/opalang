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
module B = Base
let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))
module O = Ocaml
module G = Grammar
module T = Tools
module L = B.List

module V : Graph.Sig.COMPARABLE with type t = string =
struct
  type t = string
  let equal u v = (u = v)
  let hash = B.Hashtbl.hash
  let compare = Pervasives.compare
end

module Gr = Graph.Imperative.Digraph.ConcreteBidirectional (V)
module SCC = Graph.Components.Make (Gr)

type functions =
  | Normal of string * G.expr list * G.expr list
  | Rec of (string * G.expr list * G.expr list) list

(* DFS to list all the dependicies of a function *)
let rec dfs = function
  | [] -> []
  | G.Errcont name ::tail -> name::dfs tail
  | G.Call (name,_) :: tail -> name::dfs tail
  | G.Upto (_, lst, elst, tout) :: tail
  | G.Fixed (_, lst, elst, tout) :: tail
  | G.Content (_, lst, elst, tout) :: tail
  | G.ReadRaw (lst, elst, tout) :: tail
  | G.Receive (_, _, lst, elst, tout) :: tail ->
      let get_branch = function G.Case (_,x) -> x | _ -> assert false in
        L.concat [
          (L.concat <| L.map (dfs @* get_branch) lst);
          (L.concat <| L.map (dfs @* get_branch) elst);
          (match tout with Some (G.Timeout (_, elst)) -> dfs elst | _ -> [] ) ;
          (dfs tail)
        ]
  | G.Listen (_,_,fn) :: tail -> (L.hd fn)::dfs tail
  | G.Connect (_,fn) :: tail -> (L.hd fn)::dfs tail
  | G.Send _ :: tail -> dfs tail
  | G.Block sub :: tail -> L.append (dfs sub) (dfs tail)
  | G.GMatch (_, _, _, pel) :: tail -> (dfs (L.map (fun (p,e) -> e) pel))@(dfs tail)
  | G.If (_, G.Block a, G.Block b) :: tail -> L.concat [dfs a; dfs b; dfs tail]
  | _ :: tail -> dfs tail

(* Inspired from opalang/opa/reordering.ml : lists the strongly
 * connected components of the graph *)
let reorder depslist =
  let graph = Gr.create () in
    L.iter (fun (v0, _) -> Gr.add_vertex graph v0) depslist;
    L.iter (fun (v0, deps) ->
                      StringSet.iter (fun v1->Gr.add_edge graph v0 v1) deps
                   ) depslist;
    SCC.scc_list graph

(* Checks if a function is recursive (or not). *)
let is_recursive name body = L.mem name <| dfs body

let init_table ht lst =
  L.iter (function
             | G.Startfun (n,p,b) -> B.Hashtbl.add ht n (p,b)
             | G.Fun (n,p,b) -> B.Hashtbl.add ht n (p,b)
             | _ -> assert false) lst

(* Given a list of functions, produce a list of function names with their
 * respective dependencies *)
let get_dep_list funs lst =
  let get_name = function
    | G.Startfun (n,_,_) | G.Fun (n,_,_) -> n
    | _ -> assert false
  in L.fold_left (fun l f ->
      let v = get_name f in
      let deps =
        if B.Hashtbl.mem funs v then let _,b = B.Hashtbl.find funs v in Some (dfs b)
        else None
      in Option.default_map l (fun d -> (v, StringSet.from_list d)::l) deps
    ) [] lst

let flag_as_rec funs l =
  let get_tuple x = let p,b = B.Hashtbl.find funs x in x,p,b in
  match l with
  | [x] -> (
      try
        let (x,p,b) as t = get_tuple x in
          if is_recursive x b then Rec [t]
          else Normal (x,p,b)
      with Not_found -> failwith <| "Undefined state : " ^ x
    )
  | lst -> Rec (
      L.map (fun x -> try get_tuple x
                with Not_found -> failwith <| "Undefined state : " ^ x)
        lst
    )

(* Reorder functions and resolve cycles (by inserting recursion where needed) *)
let do_it lst =
  let funs = B.Hashtbl.create <| L.length lst in
  let () = init_table funs lst in
  let deps_list = get_dep_list funs lst in
  let ccl = reorder deps_list in
    (* Tag functions as normal or [mutually-]recursive *)
    try
      L.map (flag_as_rec funs) ccl
    with Failure e ->
      let lst =
        B.Hashtbl.fold (fun name _ acc -> acc ^ "\n* " ^ name) funs
          "Here is a list of all defined states :"
      in failwith <| e ^ "\n" ^ lst
