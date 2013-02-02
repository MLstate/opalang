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
   the original module from the stdlib
*)
type ('a, 'b) t = ('a,'b) Hashtbl.t
val create : int -> ('a, 'b) t
val clear : ('a, 'b) t -> unit
val add : ('a, 'b) t -> 'a -> 'b -> unit
val copy : ('a, 'b) t -> ('a, 'b) t
val find : ('a, 'b) t -> 'a -> 'b
val find_all : ('a, 'b) t -> 'a -> 'b list
val mem : ('a, 'b) t -> 'a -> bool
val remove : ('a, 'b) t -> 'a -> unit
val replace : ('a, 'b) t -> 'a -> 'b -> unit
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val length : ('a, 'b) t -> int

module type HashedType = Hashtbl.HashedType
module type S = sig
  include Hashtbl.S
  val replace_content :'a t -> 'a t -> unit
  val find_opt : 'a t -> key -> 'a option
end
module Make (H : HashedType) : S with type key = H.t

val hash : 'a -> int
external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

(**
   additional functions
*)

val find_opt : ('a,'b) Hashtbl.t -> 'a -> 'b option

val replace_content : ('a,'b) Hashtbl.t -> ('a,'b) Hashtbl.t -> unit
  (**
     [replace_content h1 h2] behaves as [h1 <- h2] if this operation was possible
     It is unspecified whether modifying the [h2] affects [h1].
     If you want the hashtbl to "diverge", use [replace_content h1 (copy h2)].
     If you want the hashtbl to share their changes, well, too bad for you,
     that's not possible. Use references over your hashtbl instead.
  *)

(**
   Magic combinator for hashes.
   Stolen from ocaml sources
*)
val combine : int -> int -> int

(** {6 Extension} *)

(**
   Pick an unspecified element of a [Hashtbl], and remove it.
   Nothing happens if the table is empty.
*)
val pick_remove : ('a, 'b) t -> unit
