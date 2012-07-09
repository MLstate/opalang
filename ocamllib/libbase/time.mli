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

type t
(* FIXME, abstract away the type? *)
type tm = Unix.tm

val now : unit -> t
val infinity : t

val process_utime : unit -> t
val process_stime : unit -> t
val process_cutime : unit -> t
val process_cstime : unit -> t

val gmtime : t -> tm
val localtime : t -> tm

val gmt_msec : t -> int
val gmt_sec : t -> int
val gmt_min : t -> int
val gmt_hour : t -> int
val gmt_mday : t -> int
val gmt_mon : t -> int
val gmt_year : t -> int
val gmt_wday : t -> int
val gmt_yday : t -> int
val gmt_isdst : t -> bool

val local_msec : t -> int
val local_sec : t -> int
val local_min : t -> int
val local_hour : t -> int
val local_mday : t -> int
val local_mon : t -> int
val local_year : t -> int
val local_wday : t -> int
val local_yday : t -> int
val local_isdst : t -> bool
val local_timezone_offset : unit -> int

val mktime : year:int -> month:int -> day:int -> h:int -> min:int -> sec:int -> ms:int -> t

(*
let bound = Chrono.bound
*)

val zero : t

val is_infinite : t -> bool
val is_positive : t -> bool

val add : t -> t -> t
val difference : t -> t -> t
val max : t -> t -> t
val min : t -> t -> t

val is_after : t -> t -> bool
val is_before : t -> t -> bool

val seconds_float : float -> t
val seconds : int -> t
val minutes : int -> t
val hours : int -> t
val days : int -> t
val milliseconds : int -> t

val in_seconds : t -> float
val in_milliseconds : t -> int

val of_unix_time : float -> t
val of_unix_time_inf : float -> t (* Will not wrap past max_int *)
val to_unix_time : t -> float

val round_to_sec : t -> t


(** This function returns a time measured with better than millisecond precision.

    PLEASE BE AWARE, that the official type of date/time at MLstate is the type
    [t] above. You should only use this function when you absolutely need
    sub-millisecond precision AND when you know what you are doing. *)
val get_accurate_time : unit -> float
