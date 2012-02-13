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
   Management of imperative scope
   @author Mathieu Barbin
   @author Vincent Benayoun
*)


module type IMPERATIVE_SCOPE =
sig
  (**
     The mutable type of the instance
  *)
  type 'a t
    
  type elt

  (** create a new structure given a size for blocks *)
  val create : int -> 'a t

  (** reset the structure to its initial state *)
  val reset : 'a t -> unit
    
  (** push a new block *)
  val push : 'a t -> unit
    
  (** pop the top block *)
  val pop : 'a t -> unit

  (** fold the top block *)
  val fold : (elt -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
   
  (** bind the elt to 'a in the top block *)
  val bind : 'a t -> elt -> 'a -> unit

  (** unbind the elt from the top block *)
  val unbind : 'a t -> elt -> unit

  (** find the first binded data to the elt (top down) *)
  val find_opt : 'a t -> elt -> 'a option
 
end


module type ARG =
sig
  type elt
  type 'a block

  val create : int -> 'a block
  val fold   : (elt -> 'a -> 'acc -> 'acc) -> 'a block -> 'acc -> 'acc
  val bind   : 'a block -> elt -> 'a -> unit
  val unbind : 'a block -> elt -> unit
  val find_opt   : 'a block -> elt -> 'a option
end


module Make ( Arg : ARG ) : IMPERATIVE_SCOPE with type elt = Arg.elt

module Default (Arg : sig type elt end) : IMPERATIVE_SCOPE with type elt = Arg.elt
