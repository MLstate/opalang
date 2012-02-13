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
(* depends *)
module List = BaseList

(* -- *)

(* Current implementation is just a reversed list of keys (the last element of
   the list refers to the root of the db) *)

  type t = Keys.t list

  let root = []

  let add p k = k :: p

  let pop_last p =
    match p with
    | [] -> None
    | hd :: tl -> Some (hd, tl)

  let last p = fst (Option.get (pop_last p))

  let fold f init p = List.fold_left f init (List.rev p)

  let rec size p = match p with
  | [] -> 0
  | _ :: tl -> 1 + (size tl)

  let write p = p
  let read = write

  let to_string path =
    match path with
    | [] -> "[]"
    | k :: p ->
        Printf.sprintf "[%s%s]"
          (Base.List.to_string (
             fun k ->
               if k = Keys.newkey then ".new; "
               else Printf.sprintf "%s; " (Keys.to_string k)
           ) (List.rev p))
          (Keys.to_string k)

  let compare p1 p2 = Pervasives.compare p1 p2

  let remaining p1 p2 =
    let rec aux small big =
      match small with
      | [] -> Some big
      | hd :: tl -> match big with
        | [] -> assert false
        | hd2 :: tl2 ->
            if Keys.equal hd hd2
            then aux tl tl2
            else None
    in match compare (size p1) (size p2) with
    | x when x < 0 -> aux (List.rev p1) (List.rev p2)
    | _ -> aux (List.rev p2) (List.rev p1)

  let remaining_prefix p1 p2 =
    let rec aux small big =
      match small with
      | [] -> Some big
      | hd :: tl -> match big with
        | [] -> assert false
        | hd2 :: tl2 ->
            if Keys.equal hd hd2
            then aux tl tl2
            else None
    in match compare (size p1) (size p2) with
    | x when x < 0 -> aux p1 p2
    | _ -> aux p2 p1

  let is_prefix p1 p2 =
    let rec aux = function
      | (k1::r1,k2::r2) -> if Keys.equal k1 k2 then aux (r1,r2) else false
      | ([],_) -> true
      | _ -> false
    in
    aux (List.rev p1, List.rev p2)

  let concat p1 p2 = List.append p2 p1

  let to_list p = List.rev p

  let of_list p = List.rev p

module HashCons =
struct
  type ht = (Keys.t, (ht * Keys.t list)) Hashtbl.t
  let create () =
    Hashtbl.create 11
  let clear = Hashtbl.clear
  let find = Hashtbl.find
  let add = Hashtbl.add
end
