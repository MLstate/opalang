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
   OpaTop expressions evaluation, kernel of the interpreter.
   @author Mathieu Barbin
*)

(** {6 Errors reporting} *)

(**
   This module uses [OManager].
   Currently, the positions are not available, so any runtime errors try to print
   some AST.
   This does no follows the guidelines, and should be fixed when the positions will be
   available in QmlAst, as position 0 of any constructor (this is a TODO in QmlAst).
*)

(** {6 Evalulation} *)

(**
   Note about bypasses:

   For building bypasses, we use the API provided by OpaTopBsl.
   In particular, this can work only if the corresponding plugins have arleady
   been loaded, and primitives registred. This is guaranty if you use the interpreter
   via the command line tool, but you have to be carrefully if you use the interpreter
   as a library.
*)

(**
   TODO(refactoring):
   type env = OpaTopValue.t IdentMap.t
*)

type env = OpaTopValue.t IdentMap.t

val eval : env -> QmlAst.expr -> OpaTopValue.t

(** {6 AnnotMap for Values} *)

(**
   For evaluating val rec, we use the unvalrec trick,
   these imperative maps are update by side-effect during
   each call to [eval].

   The map is never read by this module.
*)

val getValueOfAnnot : Annot.t -> OpaTopValue.t option
val resetAnnot : unit -> unit
