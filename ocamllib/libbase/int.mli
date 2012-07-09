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
 *   This module provides 64-bit signed integers. It should be used in place
 * of [int] when this extra bit is required (int's are 63-bit) *and* to make
 * sure everything works on 32-bit platforms (where this type will still provide
 * 64-bits).
 *   In short this should be used in place of int's when dealing with values
 * that may exceed 31 bits.
 *   Performance notice: value of type Int.t will occupy more space and
 * operations on them will generally be slower than over the int type.
 *)

 (** A 64-bit integer (also on 32-bit platforms). *)
type t

 (** Addition *)
val ( + ): t -> t -> t

 (** Subtraction *)
val ( - ): t -> t -> t

 (** Multiplication *)
val ( * ): t -> t -> t

 (** Integer division *)
val ( / ): t -> t -> t

 (** Conversion from native integer *)
val of_int: int -> t

exception Overflow
 (** Conversion to native integer; throws Overflow if the value does not fit *)
val to_int: t -> int
