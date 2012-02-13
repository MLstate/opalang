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
(**)

module type SafeOrderedType =
sig
  include Set.OrderedType
  val to_string : t -> string
end

module StringOrder : (SafeOrderedType with type t = string) = 
struct type t = string let compare = String.compare let to_string x = x end
module StringMap = Map.Make (StringOrder)
type 'a stringmap = 'a StringMap.t

module IntOrder : (SafeOrderedType with type t = int) = 
struct type t = int let compare = (-) let to_string x = string_of_int x end
module IntMap = Map.Make (IntOrder)
type 'a intmap = 'a IntMap.t

module type SafeSet =
sig
  type elt
  type t
  val empty : unit -> t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val map  : (elt -> elt) -> t -> t
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val max_elt : t -> elt
  val choose : t -> elt
  val split : elt -> t -> t * bool * t

  val to_string : t -> string
end

module SafeSet(Ord : SafeOrderedType) : (SafeSet with type elt = Ord.t) =
struct
  module Set = Set.Make(Ord)
  include Set

  let empty () = empty

  let map f s = 
    fold (fun x acc -> add (f x) acc) s (empty())

  let to_string set =
    let separator = ", " in
    let content = fold
      (fun elt acc ->
	 Format.sprintf "%s%s%s" acc (Ord.to_string elt) separator) set "" in
    let newsize = max 0 ((String.length content) - (String.length separator))
    in
    Format.sprintf "{%s}" (String.sub content 0 newsize)
end

module StringSet = SafeSet(StringOrder)
type stringset = StringSet.t
