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
(*
    @author Sebastien Briais
*)

(** Lambda Lifting of typed QML code

    References:

    Lambda lifting in quadratic time
    Olivier Danvy, Ulrik Schultz

    ML-Style Typing, Lambda Lifting and Partial Evaluation
    Peter Thiemann
*)

(** prerequisite: identifiers are defined only once
    [in other words, name analysis has been done]

    no coercions

    no "let type ... in ..."

    @raise QmlTypes.Exception Check if this exception is really raised
    @raise QmlTyperException.Exception Check if this exception is really raised
*)

val process_code:
  early:bool ->
  side:[`client | `server] ->
  typed:bool ->
  (* typed mode preserves types but it also needs types everywhere in the annotmap
   * untyped mode works on a broken annotmap and breaks it even more *)
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  (QmlTypes.gamma * QmlAst.annotmap) * QmlAst.code

(* fun_action works in an untyped way, and 'lifts' only parts of the code in a very
 * specific way not to introduce partial applications *)
val process_code_fun_action:
  QmlAst.annotmap ->
  QmlAst.code ->
  QmlAst.annotmap * QmlAst.code
