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
   Error management for Opa.

   @author Esther Baruk
   @author Mathieu Barbin
*)

(** {6 Design note} *)

(**
    This module is meant to factorize code to create located error messages, for a
    public or an internal audience.

    This module uses internally the [passlib] which offers the opportunity to passes
    and checkers to be more verbose and outputting a detailed context in the track
    system, which can be analyzed after the compilation (or after the last error),
    using [opatrack]
*)

(** {6 Context} *)

module Context :
sig
  type context

  (** Merging several context *)
  val merge2 : context -> context -> context
  val merge : context -> context list -> context

  (** As we fail on surface ast, we have still all posititions from the user code *)
  val pos : FilePos.pos -> context

  (** Temporary, until we do the refactoring of pos in the Surface Ast *)
  val label : 'a QmlLoc.label -> context
  val annot : QmlLoc.annot -> context

(** if needed, add some other context constructors, and merge,
    like [qmlError] *)
end

type context = Context.context

(**
   The documentation of these function is in [PassHandler] and [PassError]
*)

(** {6 Interaction with Checkers} *)

(** *)
val check_fail : PassHandler.cond_id -> context -> ('a, 'error) OManager.oformat -> 'a
val scheck_fail : PassHandler.cond_id -> context -> ('a, unit) OManager.oformat -> 'a

(** {6 Interaction with Passes} *)

(** Internal Errors *)
(** *)
val cond_violation : PassHandler.cond_id -> context -> ('a, 'error) OManager.oformat -> 'a
val scond_violation : PassHandler.cond_id -> context -> ('a, unit) OManager.oformat -> 'a
val i_error : PassHandler.cond_id option -> context -> ('a, 'error) OManager.oformat -> 'a
val i_serror : PassHandler.cond_id option -> context -> ('a, unit) OManager.oformat -> 'a

(** Public Errors *)
(** *)
val error : context -> ('params, 'exit) OManager.oformat -> 'params
val serror : context -> ('params, unit) OManager.oformat -> 'params
val warning : wclass:WarningClass.wclass -> context -> ('params, unit) OManager.oformat -> 'params
