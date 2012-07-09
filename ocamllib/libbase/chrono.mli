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
   Manipulation of Chronometers.
   @author Mathieu Barbin
*)

type t = {
  start : unit -> unit;
  (**
     start the chrono, does nothing if it already runs
  *)

  restart : unit -> unit;
  (**
     resets the cumulated time of the chrono, and starts it
  *)

  stop : unit -> unit;
  (**
     stops the chrono, but keeps the cumulated time
  *)

  read : unit -> float;
  (**
     reads the cumulated time
  *)

  reset : unit -> unit;
  (**
     stops the chrono and reset the cumulated time
  *)
}

val make : unit -> t
val start : t -> unit
val restart : t -> unit
val stop : t -> unit
val read : t -> float
val reset : t -> unit
val print : t -> string -> unit

(** {b Descr}: {b measure f g} Mesures the execution time of the
    function {b f} and returns the application of the function {b g} on this
    elapsed time amount. *)
val measure : (unit -> 'a) -> (float -> unit) -> 'a

(**
   Measure the execution time of the given thunk
   and display it on stderr, prefixed by the given string
*)
val measure_and_show : string -> (unit -> 'a) -> 'a

(** Bound the execution time of a function by a given number of seconds **)
val bound : int -> (unit -> 'a) -> (unit -> 'a) -> 'a
