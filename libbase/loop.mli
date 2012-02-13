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

(**
   Loop module
*)

module For :
sig

  (**
     [range a b acc fct]
     Bounds:
     start bound is included, end bound is Excluded.
     {[
     For.range 0 10 acc (
       fun i acc ->
         (* do something with i and acc, return an acc *)
     )
     ]}

     The accumulator is returned without changes if a >= b
  *)
  val range : int -> int -> 'a -> (int -> 'a -> 'a) -> 'a

  (*
    if needed
  *)
  (* val down_range *)

end

module InitAcc :
  sig
 (** init the list with [ fst( f i _) : i C {0..n-1} ] and return the folded accumulator  *)
  val list : int * 'a -> (int -> 'a -> 'b * 'a) -> 'b list * 'a end
module FilterMap :
  sig
    val array :
      'a array -> (int -> 'a -> bool) -> (int -> 'a -> 'b) -> 'b array
  end
module Map :
  sig
    val list : 'a list -> ('a -> 'b) -> 'b list
    val array : 'a array -> (int -> 'a -> 'b) -> 'b array
    val option : 'a option -> ('a -> 'b) -> 'b option
  end
module FoldMap :
  sig
    val list : 'a list * 'b -> ('a -> 'b -> 'c * 'b) -> 'c list * 'b
    val array :
      'a array * 'b -> (int -> 'a -> 'b -> 'c * 'b) -> 'c array * 'b
  end
module Iter :
  sig
    val list : 'a list -> ('a -> unit) -> unit
    val array : 'a array -> (int -> 'a -> unit) -> unit
  end
module Deprecated :
  sig
    val l_fold : 'a list * 'b -> ('a -> 'b -> 'b) -> 'b
    val l_map : 'a list -> ('a -> 'b) -> 'b list
    val l_filter : 'a list -> ('a -> bool) -> 'a list
    val l_filter_map : 'a list -> ('a -> 'b option) -> 'b list
    val l_for_all : 'a list -> ('a -> bool) -> bool
    val l_map_flat : 'a list -> ('a -> 'b list) -> 'b list
    val l_map_sort : 'a list -> ('a -> 'b) -> 'b list
    val l_iter : 'a list -> ('a -> unit) -> unit
  end
