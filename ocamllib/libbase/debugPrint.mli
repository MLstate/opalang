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
   This module gives you a universal printer
   that works by runtime introspection of values
*)

type printer = { f : 'a. 'a -> string option }
(**
   A printer is a function that takes anything, checks the runtime
   representation to see it is knows how to print it and if so,
   returns the string representation (and it should call DebugPrint
   to print expressions strictly deeper in the expression)
   If representation is no the one that the particuliar printer can print
   then it should return None
*)

val register : printer -> unit
(**
   Define a new printer.
   Do not give very generic function or else you will screw
   the printing for everybody
*)

val print : ?depth:int -> 'a -> string
(**
   The main function of this module. Tries to print the value with each
   of the registered printer, or a default printer if no given printer
   is suitable
*)

val pp : ?depth:int -> Format.formatter -> 'a -> unit
(**
   Same than [print] but with a format interface
*)

val simple_print : ?depth:int -> 'a -> string
(**
   A variant of [print] attempting to be easier to read on simple values,
   although possibly harder on complex values.
*)

(** {6 Helpers for defining printing functions} *)
val true_ : Obj.t -> bool
val false_ : Obj.t -> bool
val bool : Obj.t -> bool
val string : Obj.t -> bool
val option : ?a:(Obj.t -> bool) -> Obj.t -> bool

(**
   BEWARE: doesn't work for float arrays
   The optional tag argument is set to 0 (the array tag).
   It is possible to customize array tags.
*)
val array : ?tag:int -> ?a:(Obj.t -> bool) -> Obj.t -> bool
val unit : Obj.t -> bool
val int : ?plus:(int -> bool) -> Obj.t -> bool
val tuple0 : Obj.t -> bool
val tuple1 : ?f1:(Obj.t -> bool) -> Obj.t -> bool
val tuple2 : ?f1:(Obj.t -> bool) -> ?f2:(Obj.t -> bool) -> Obj.t -> bool
val tuple3 : ?f1:(Obj.t -> bool) -> ?f2:(Obj.t -> bool) -> ?f3:(Obj.t -> bool) -> Obj.t -> bool
val tuple4 : ?f1:(Obj.t -> bool) -> ?f2:(Obj.t -> bool) -> ?f3:(Obj.t -> bool) -> ?f4:(Obj.t -> bool) -> Obj.t -> bool
val tuple_n : (Obj.t -> bool) list -> Obj.t -> bool
