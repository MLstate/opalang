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

module type STUFF =
  sig
    type t
    val get : t -> int -> char
    val set : t -> int -> char -> unit
  end

module StuffF :
  functor (S : STUFF) ->
    sig
      val lei32 : S.t -> int -> int -> unit
      val bei32 : S.t -> int -> int -> unit
      (*val lei64 : S.t -> int -> int -> unit
      val bei64 : S.t -> int -> int -> unit*)
      val led : S.t -> int -> float -> unit
      val bed : S.t -> int -> float -> unit
      val lei32l : S.t -> int -> int32 -> unit
      val bei32l : S.t -> int -> int32 -> unit
      val lei64L : S.t -> int -> int64 -> unit
      val bei64L : S.t -> int -> int64 -> unit
      val ldi32 : S.t -> int -> int
      val bdi32 : S.t -> int -> int
      (*val ldi64 : S.t -> int -> int
      val bdi64 : S.t -> int -> int*)
      val ldi64L : S.t -> int -> int64
      val bdi64L : S.t -> int -> int64
      val ldd : S.t -> int -> float
      val bdd : S.t -> int -> float
    end

module StuffString :
  sig
    val lei32 : string -> int -> int -> unit
    val bei32 : string -> int -> int -> unit
    (*val lei64 : string -> int -> int -> unit
    val bei64 : string -> int -> int -> unit*)
    val led : string -> int -> float -> unit
    val bed : string -> int -> float -> unit
    val lei32l : string -> int -> int32 -> unit
    val bei32l : string -> int -> int32 -> unit
    val lei64L : string -> int -> int64 -> unit
    val bei64L : string -> int -> int64 -> unit
    val ldi32 : string -> int -> int
    val bdi32 : string -> int -> int
    (*val ldi64 : string -> int -> int
    val bdi64 : string -> int -> int*)
    val ldi64L : string -> int -> int64
    val bdi64L : string -> int -> int64
    val ldd : string -> int -> float
    val bdd : string -> int -> float
  end

val add_le_int32 : Buf.buf -> int -> unit
val add_be_int32 : Buf.buf -> int -> unit
(*val add_le_int64 : Buf.buf -> int -> unit
val add_be_int64 : Buf.buf -> int -> unit*)
val add_le_d : Buf.buf -> float -> unit
val add_be_d : Buf.buf -> float -> unit
val add_le_int32l : Buf.buf -> int32 -> unit
val add_be_int32l : Buf.buf -> int32 -> unit
val add_le_int64L : Buf.buf -> int64 -> unit
val add_be_int64L : Buf.buf -> int64 -> unit

