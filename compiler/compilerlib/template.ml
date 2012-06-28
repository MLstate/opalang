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

(* Guidelines for module implementation *)
(*
   - You MUST provide the interface of your implementation in a mli
     except if your implementation provides essentially type definitions :
     in this case your module should be named myModuleInterface.ml.

   - You must put the license at the beginning of the code

   - Don't use ocamldoc comments in the implementation of your module,
     use simple comments.
*)

(* Dependencies *)
(* module Bar = Foo.Bar
   ...
*)

(* Shorthands *)
(* module B = Baz
   ...
*)

(* Type aliases *)
(*
  In this part of the module, you can define type aliases, for a better readability.
*)

type filename = string
type line_number = int


(* Error report *)
(*
  In case of an error you would like to report but should not be caught by
  anyone, use errors from OManager module
  In case of an error you would like to put in an exception which can be
  caught later, act this way :
  - Define a type error
  - Define an exception with argument of type error
  - Define a printer to print your error

  If the error cases are very simple, you may use type string for error,
  if not, the guideline is to use a sum type.
*)

type error = string
exception Exception of error
let pp_error fmt err = Format.fprintf fmt "Error : %s" err

(* Shorthands for warnings *)
(*
  You can define operators to raise warning. Like (!!) (??) (!?) (?!) ...
  which will call OManager.warning, with a coherent and documented warning class.
*)

(* Debug *)
(* Define here ppdebug messages *)

let debug fmt =
  OManager.printf ("@{<cyan>[MyModule]@}@ @[<2>"^^fmt^^"@]@.")

let _ =
  #<If:CHECK_VARS $minlevel 1>
    debug "print some debug of level %d@\n" 1
  #<End>


(* Comment documenting function 1 *)
let function1 () = ()

(* Comment documenting function 2 *)
let function2 () = ()
