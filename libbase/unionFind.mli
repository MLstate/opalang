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
   UnionFind.

   This module offers an imperative implementation for the union find algorithm
   based on linked pointer and collapsing rules.

   A support is given for hooking values to keys.

   @author Henri Binsztok
   @author Mikolaj Konarski
   @author Mathieu Barbin
*)

(** {6 Union Find with key/value } *)

(**
   the type of an element. an element can be seen as a set, or an equivalence class,
   represented by a elt of type 'a. 
   
   <!> values of type [('a, 'b) t] are mutable.

   performing an [union] or [replace] lead to change the returned key/values of a elt.
 *)
type ('a, 'b) t

(** build a new set *)
val make : 'a -> 'b -> ('a, 'b) t

(** merging 2 elts. 
    <!> The key/value resulting in the merged elt is in unspecified.
    @see 'replace' if you need to specify what key/value should be keeped *)
val union : ('a, 'b) t -> ('a, 'b) t -> unit

(**
   this is like an [union] but assure that the [key,value] of the [keeped] element
   are keeped. Essentially, this function is there because in any other implementation,
   the behavior of the function [union] may be unspecified.
*)
val replace : replaced:('a, 'b) t -> keeped:('a, 'b) t -> unit 

(** find pointed key/value by an elt
    @see 'key' for getting only the key
    @see 'value' for getting only the value *)
val find : ('a, 'b) t -> 'a * 'b

(** like [key t] is like [fst (find t)] but slighty optimized 
    @see 'find' for getting key * value *)
val key : ('a, 'b) t -> 'a

(** like [value t] is like [snd (find t)] but slighty optimized
    @see 'find' for getting key * value *)
val value : ('a, 'b) t -> 'b

(** changing the hooked by this class of key *)
val changeval : ('a, 'b) t -> 'b -> unit
