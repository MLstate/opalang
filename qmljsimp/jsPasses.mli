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
(**
   This module contains some passes for the javascript AST.
   @author Maxime Audouin
*)

type jsp = JsAst.code -> JsAst.code

(**
   Transform recursive code element on element code using [tailrec_machine].
   Replace call to function by call using the [t] and [tco] function prototype

   cf ["qmlJsfunClientLib.js"]
   for the implementation of prototype [t] and [tso].

   This transformation is made in 2 kind of location for function applications:

   1) in terminal call position, instead of beeing recursive, and returning a call to itself,
   it returns potentially a [TailCall] object, containing the rest of the execution to
   process, using the prototype [tco] of the [Function] class.

   2) in other call positions, instead of calling the function direclty (it would not type
   because the function potentially has previously returned a [TailCall] object instead of
   the restul) it used the prototype [t] of the [Function] class, which loops until the
   final result is returned.
*)
val mktl : jsp

(**
   This function makes an alpha renaming on each local value:
   -function parameters
   -local function variables
   -local letin

   using a short ident generator, using a reset for each toplevel declaration,
   for keeping identifier short, even on a huge code.
*)
val local_alpha_stm : JsAst.statement -> JsAst.statement
val local_alpha : jsp

(**
   Split too deep expressions.
*)
val split : jsp
