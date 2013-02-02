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
   Error management constructor for a Language treated by a compiler.

   @author Mathieu Barbin
   @author Quentin Bourgerie
*)

(* This module has no mli because it would duplicate the module type sig LangContext *)

(** {6 Design note} *)

(**
   This module is meant to factorize code to create located error messages, for a
   public or an internal audience for any language Lang following the design.

   This module uses internally the [passlib] which offers the opportunity to passes
   and checkers to be more verbose and outputting a detailed context in the
   track system, which can be analyzed after the compilation (or after the last error),
   using [opatrack]
*)

module type LangContext =
sig
  (** {6 Context} *)

  (**
     The type of the context of the error.

     The type context will not be private after the functor application,
     we use a context constructor
  *)
  type context

  (**
     The printer of your context. Print everything possible from the context.
     Example: the expr, the toplevel expr in which the expr is, etc...
  *)
  val full : context PassHandler.printer

  (**
     The reduced printer for the console. Example: only filename, line
  *)
  val console : context PassHandler.printer
end


module LangError (C : LangContext) =
struct

  (**
     The documentation of theses function is in PassHandler
     They have the same names, there are just partially applied
     to the context printer.
  *)

  (** Checkers *)
  (** *)
  let check_fail c =
    PassHandler.check_fail ~full:C.full ~console:C.console c
  let scheck_fail c =
    PassHandler.scheck_fail ~full:C.full ~console:C.console c

  (* TODO: finish and clarify the role of full/console in public errors *)
  (* Make sure every errors in qml/opa use the corresponding LangError *)

  (** Passes *)
  (** Internal errors *)
  (** *)
  (* overlaying for passes. eta-expanded for ocaml type generalization *)
  let scond_violation c = PassHandler.scond_violation C.full c
  let cond_violation c = PassHandler.cond_violation C.full c
  let i_error c = PassHandler.i_error C.full c
  let i_serror c = PassHandler.i_serror C.full c

  (** Public errors *)
  (** *)
  let error ?(msg="") c fmt = OManager.error ("%a%s"^^fmt) C.console c msg
  let serror c fmt = OManager.serror ("%a"^^fmt) C.console c
  let warning ~wclass c fmt = OManager.warning ~wclass ("%a"^^fmt) C.console c
end
