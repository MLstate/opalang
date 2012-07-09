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
   OpaTop global properties.
   @author Mathieu Barbin
*)

(**
   Global properties are a collection of imperative statefull variables
   of toggleling the behavior of the interpreter.

   It is used essentially for debug.
*)

(**
   GUIDELINES: keep alphabetic order of properties.
   For toggle variables (modes) names are [property_set, property_get].

   If it makes sense, add a corresponding directive for a dynamic switch
   in OpaTopDirectives, and add a corresponding option in the command line
   tool.
*)

(**
   Assertion mode: compile or ignore assertions.
   Default is on.
*)

val assert_set : bool -> unit
val assert_get : unit -> bool

(* (\** *)
(*    DbGen mode: with or without dbgen. *)
(*    Default is on. *)
(* *\) *)

(* val dbgen_set : bool -> unit *)
(* val dbgen_get : unit -> bool *)

(**
   DbGen Debug mode: with or without dumping dbgen process.
   Default is off.
*)

val dddbgen_set : bool -> unit
val dddbgen_get : unit -> bool


(**
   Dump infered types and evaluated values.
*)
val dump_set : bool -> unit
val dump_get : unit -> bool

(**
   Greedy mode: kill or continue in case of errors.
   Default is off during stdlib and user files, and on during stdin.
*)

val greedy_set : bool -> unit
val greedy_get : unit -> bool


(**
   Skip-evaluation mode: with or without evaluation.
   Default is off.
*)

val noeval_set : bool -> unit
val noeval_get : unit -> bool


(**
   Say if the prompt should be printed.
   In practice, is set to true whenever we parse the stdin.
   {[
   # g(x) = x ;;
   g : 'a -> 'a : <fun>
   #
   ]}
*)
val prompt_set : bool -> unit
val prompt_get : unit -> bool

(**
   Where goes the standard output of the interpreter.
   Default is stdout.
*)
val stdout : Format.formatter ref

(**
   Restricted bypass mode: with or without checking.
   Default is on.
*)

val restricted_bypass_set : bool -> unit
val restricted_bypass_get : unit -> bool

(**
   Value restriction mode: set the level of value restriction.
   Default is 0 (desactivated), 1 is normal, 2 is strict.
*)

val value_restriction_set : [`disabled|`normal|`strict] -> unit
val value_restriction_get : unit -> [`disabled|`normal|`strict]

(* (\** *)
(*    Typer mode: with or without typing. *)
(*    Default is on. *)
(* *\) *)

(* val typer_set : bool -> unit *)
(* val typer_get : unit -> bool *)

val switch_typechecker: string -> bool
