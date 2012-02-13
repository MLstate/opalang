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
