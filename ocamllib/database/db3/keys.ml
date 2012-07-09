(*
    Copyright © 2011 MLstate

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
module Array = BaseArray
module List = BaseList

(* -- *)

exception Unqualified of string

type t =
  | IntKey of int
  | StringKey of string
  | ListKey of t array
  | VariableKey of int

type real = t

let newkey = VariableKey 0 (* TODO: refresh, when needed; review the usage *)

let rec to_string = function
  | IntKey i -> Printf.sprintf "%d" i
  | StringKey s -> Printf.sprintf "\"%s\"" s
  | ListKey r ->
      (* TODO: or we may use this (and merge with the version in 01-unit.ml):
      String.concat_map ~left:"{ " "; " ~right:" }" key_to_string (Array.to_list l)
      *)
      let res = Array.fold_left (
        fun acc k -> Printf.sprintf "%s%s; " acc (to_string k)
      ) "{" r
      in Printf.sprintf "%s}" (String.sub res 0 (max 1 (String.length res - 2)))
  | _ -> "newkey"

let rec is_qualified k = match k with
  | ListKey r -> Array.fold_left (fun acc k -> acc && not (k = VariableKey 0)) true r
  | VariableKey 0 -> false
  | _ -> true

let unsafe_make k =
  if is_qualified k then k
  else raise (Unqualified (to_string k))

let max k1 k2 = match (k1, k2) with
  | (IntKey x, IntKey y) -> IntKey (max x y)
  | (StringKey s1, StringKey s2) -> StringKey (max s1 s2)
  | (ListKey r1, ListKey r2) -> ListKey (max r1 r2)
  | (VariableKey _, k) -> k
  | (k, VariableKey _) -> k
  | (IntKey _, _) -> k1
  | (_, IntKey _) -> k2
  | (StringKey _, _) -> k1
  | (_, StringKey _) -> k2

let min k1 k2 = match (k1, k2) with
  | (IntKey x, IntKey y) -> IntKey (min x y)
  | (StringKey s1, StringKey s2) -> StringKey (min s1 s2)
  | (ListKey r1, ListKey r2) -> ListKey (min r1 r2)
  | (VariableKey _, _) -> VariableKey 0
  | (_, VariableKey _) -> VariableKey 0
  | (IntKey _, _) -> k2
  | (_, IntKey _) -> k1
  | (StringKey _, _) -> k2
  | (_, StringKey _) -> k1

let succ k = match k with
  | IntKey i ->
      if i = max_int then None
      else  Some (IntKey (succ i))
  | _ -> None

let pred k = match k with
  | IntKey i ->
      if i = 0 then None
      else Some (IntKey (pred i))
  | _ -> None

let equal k1 k2 = match (k1, k2) with
  | (IntKey i1, IntKey i2) -> (i1 = i2)
  | (StringKey s1, StringKey s2) -> (s1 = s2)
  | (ListKey r1, ListKey r2) -> (r1 = r2)
  | (VariableKey 0, VariableKey 0) -> true
  | _ -> false

let rec compare k1 k2 =
  match (k1, k2) with
  | (VariableKey n1, VariableKey n2) -> Pervasives.compare n1 n2
  | (VariableKey _, _) -> -1
  | (_, VariableKey _) -> 1
  | (IntKey i, IntKey j) -> Pervasives.compare i j
  | (IntKey _, _) -> -1
  | (_, IntKey _) -> 1
  | (StringKey s1, StringKey s2) -> Pervasives.compare s1 s2
  | (StringKey _, _) -> -1
  | (_, StringKey _) -> 1
  | (ListKey r1, ListKey r2) -> Array.compare compare r1 r2

let value = function
  | IntKey i -> IntKey i
  | StringKey s -> StringKey s
  | ListKey r -> ListKey r
  | VariableKey _ -> raise (Unqualified "unknown type of key")

let _int i = unsafe_make (IntKey i)
let _string s = unsafe_make (StringKey s)
let _rec t = unsafe_make (ListKey t)
