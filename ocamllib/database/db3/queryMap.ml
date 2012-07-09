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
(* depends *)
module List = BaseList

(* debug *)
#<Debugvar:DEBUG_DB$minlevel 1>

(* -- *)

type query =
  | Set of Datas.t
  | Remove of Keys.t

(* This type represents a map from nonempty paths to queries.
   TODO: for speed and safety, represent multiple queries
   not as lists, but an option of a set query and a list
   of remove queries. Or only have a single recursive removal query. *)
type t = query list KeyRecMap.map


let path_to_nonempty_list l =
  let l = Path.to_list l in
  (match l with
   | [] -> Logger.warning "No transaction on root path";
   | _ -> ());
  l



(* Print *)

let print_query = function
  | Set d -> Printf.sprintf "set %s" (Datas.to_string d)
  | Remove k -> Printf.sprintf "remove %s" (Keys.to_string k)

let print_query_list list =
  List.to_string (fun q -> Printf.sprintf "%s " (print_query q)) list

let print_query_map map =
  let rec aux map prof =
    let indent = String.make (prof*8) '\032' in
    if KeyRecMap.is_empty map then (Printf.sprintf "%sempty\n" indent)
    else
      KeyRecMap.fold (
        fun key (qlist, new_map) acc ->
          let query_string = print_query_list qlist in
          Printf.sprintf "%s%s%s -> [%s],\n%s"
            acc indent (Keys.to_string key)
            query_string (aux new_map (succ prof))
      ) map ""
  in aux map 0


(* Merge *)

let find_map_from_path map path =
  let rec aux path map =
    match path with
    | [] -> raise Not_found
    | [k] -> KeyRecMap.find k map
    | hd :: tl ->
        let _, new_map = KeyRecMap.find hd map in
        aux tl new_map
  in
  aux (path_to_nonempty_list path) map

let rec check_no_set = function
  | [] -> true
  | Set _ :: _ -> false
  | _ :: tl -> check_no_set tl

(* TODO: this seems broken.
   The exception should be raised iff both transactions modify the same path
   in any way. This is not the case now. It's not even symmetric.
   TODO, including the next function, merge_query_list.
*)
let check_list l old_list old_map =
  List.iter (
    fun query ->
      match query with
      | Set (Datas.Data _) ->
          (* TODO: this is too weak. What if old_list level above contains
             remove of this node? the condition on Remove is not symmetric!*)
          if not (check_no_set old_list) then begin
            #<If>
              Logger.log ~color:`red "Merge conflict at Data!"
            #<End>;
            raise Hldb.Merge
          end
      | Set (Datas.Link _) ->
          if not (KeyRecMap.is_empty old_map && check_no_set old_list) then begin
            #<If>
              Logger.log ~color:`red "Merge conflict at Link!"
            #<End>;
            raise Hldb.Merge
          end
      | Set (Datas.Copy (_, _)) ->
          if not (check_no_set old_list) then begin
            #<If>
              Logger.log ~color:`red "Merge conflict at Copy!"
            #<End>;
            raise Hldb.Merge
          end
      | Set Datas.UnsetData -> assert false
      | Remove k ->
          (* TODO: isn't this too strong? what if old_list also removes it?
             Louis says it can stay so for now, but has to be symmetric *)
          if KeyRecMap.mem k old_map then begin
            #<If>
              Logger.log ~color:`red "Merge conflict at Remove!"
            #<End>;
            raise Hldb.Merge
          end
  ) l

(* TODO: simplify *)
let mergeable_query_list ql1 m1 ql2 m2 =
  match ql1 with
  | [] ->
      begin match ql2 with
      | [] -> ()
      | l -> check_list l [] m1
      end
  | l -> check_list l ql2 m2

(** check the mergeability of m1 and m2; raise Merge, if failure *)
let mergeable_query_maps m1 m2 =
  #<If>
    Logger.log ~color:`yellow "checking the mergeability of query maps %s%s"
      (print_query_map m1) (print_query_map m2)
  #<End>;
  if KeyRecMap.is_empty m1 || KeyRecMap.is_empty m2 then ()
  else
    let rec aux m1 m2 =
      KeyRecMap.iter (
        fun key (query_list, new_map) ->
          (*
            Mathieu Wed Mar 16 16:43:04 CET 2011
            AIE AIE AIE (mem + find)
          *)
          if KeyRecMap.mem key m1 then
            let old_query_list, old_map = KeyRecMap.find key m1 in
            mergeable_query_list query_list new_map old_query_list old_map;
            aux new_map old_map
      ) m2
    in
    aux m1 m2

(** merge m1 and m2 *)
let merge_query_map m1 m2 =
  #<If>
    Logger.log ~color:`yellow "merging query maps %s%s"
       (print_query_map m1) (print_query_map m2)
  #<End>;
  let rec aux m1 m2 =
    KeyRecMap.fold (
      fun key (query_list, new_map) acc ->
        if KeyRecMap.mem key acc then
          let old_query_list, old_map = KeyRecMap.find key acc in
          let new_query_list =
            mergeable_query_list query_list new_map old_query_list old_map;
            query_list @ old_query_list
          in
          let new_map = aux new_map old_map in
          KeyRecMap.add key (new_query_list, new_map) acc
        else
          KeyRecMap.add key (query_list, new_map) acc
    ) m2 m1
  in
  let res = aux m1 m2 in
  #<If>
    Logger.log ~color:`yellow "result = \n%s" (print_query_map res)
  #<End>;
  res


(* Add and remove *)

let add_to_query_map map path query =
  let rec aux map path =
    match path with
    | [] -> map
    | key :: rest ->
        let new_map, old_query_list =
          match KeyRecMap.find_opt key map with
          | Some (ql, m) -> aux m rest, ql
          | _ -> aux KeyRecMap.empty rest, []
        in
        let new_query_list =
          if rest <> [] then old_query_list (*TODO: perhaps remove Removes*)
          else
            let is_remove query =
              match query with
              | Remove _ -> true
              | Set _ -> false
            in
            if is_remove query then
              (* Multiple [Remove] are OK, as long as they remove
                 different children. *)
              if List.mem query old_query_list then old_query_list
              else query :: old_query_list
            else
              (* Multiple [Set] overwrite each other. *)
              let pruned_list = List.filter is_remove old_query_list in
              query :: pruned_list
        in
        KeyRecMap.add key (new_query_list, new_map) map
  in
  aux map (path_to_nonempty_list path)

let overwrite_in_query_map map path new_map =
  let rec aux map path =
    match path with
    | [] -> new_map
    | key :: rest ->
        let new_map, old_query_list =
          match KeyRecMap.find_opt key map with
          | Some (ql, m) -> aux m rest, ql
          | _ -> aux KeyRecMap.empty rest, []
        in
        KeyRecMap.add key (old_query_list, new_map) map
  in
  aux map (path_to_nonempty_list path)

let remove_from_query_map map path =
  let rec aux map path =
    match path with
    | [] -> assert false
    | [x] -> KeyRecMap.remove x map
    | hd :: tl ->
        let old_data, old_map = KeyRecMap.find hd map in
        let new_map = aux old_map tl in
        KeyRecMap.add hd (old_data, new_map) map
  in
  try aux map (path_to_nonempty_list path)
  with Not_found -> map

let rec find_set_in_query_list = function
  | [] -> None
  | Set d :: _ -> Some d
  | _ :: tl -> find_set_in_query_list tl
