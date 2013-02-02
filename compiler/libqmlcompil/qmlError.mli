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
   Error management for Qml.

   @author Mathieu Barbin
   @author Quentin Bourgerie
*)

(** {6 Design note} *)

(**
   This module is meant to factorize code to create located error messages, for a
   public or an internal audience.

   This module uses internally the [passlib] which offers the opportunity to passes
   and checkers to be more verbose and outputting a detailed context in the
   track system, which can be analyzed after the compilation (or after the last error),
   using [opatrack].
*)

(** {6 Context} *)

module Context :
sig

  (** The type of the context of the error. *)
  type context

  (** Merging several context *)
  val merge2 : context -> context -> context
  val merge : context -> context list -> context

  (** {6 Constructors} *)

  (**
     Should be enough in the majority of cases for outputting a public error message.
  *)

  val pos : FilePos.pos -> context
  val label : Annot.label -> context

  (**
     Currently, we are waiting for refactoring about positions,
     and we do not have them in many cases.
     So, waiting for this refactoring, you can try to give the annotmap
     and an expression, which should be enough in most of passes to get the
     position.
  *)

  val annoted_expr : QmlAst.annotmap -> QmlAst.expr -> context
  val annoted_pat : QmlAst.annotmap -> QmlAst.pat -> context

  val annotmap : QmlAst.annotmap -> context

  val code_elt : QmlAst.code_elt -> context

  val expr : QmlAst.expr -> context
  val exprs : QmlAst.expr -> QmlAst.expr list -> context

  val pat : QmlAst.pat -> context

  val ty : QmlAst.ty -> context

  val package : string -> context

  (**
     Sometimes, somebody is too lazy for refactoring code so
     that he can build a correct context.
     In this case, he should shame, and provide the absolute path
     to the function in the source code where is built the context.
     ["Module.SubModule.function:extra?"].
     What a shame !
  *)
  val shame_on_me_i_am_too_lazy : string -> context

  (** {b Descr}: Returns the source position recorded in the context. *)
  val get_pos : context -> FilePos.pos
end

(**
   The documentation of these function is in [PassHandler] and [PassError]
*)

(** {6 Interaction with Checkers} *)

type context = Context.context
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
val error : ?msg:string -> context -> ('params, 'exit) OManager.oformat -> 'params
val serror : context -> ('params, unit) OManager.oformat -> 'params
val warning : wclass:WarningClass.wclass -> context -> ('params, unit) OManager.oformat -> 'params
