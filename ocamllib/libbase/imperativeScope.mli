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
