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
   Resolution of coding directives.
   @author Mathieu Barbin
*)

(**
   Coding directives are directives used for maintenance, and or readability of the opa source code.
   These are directives put by developpers, for other developpers.
   Here is the list of handled directives, and their effect during this pass:

   \@deprecated
   A warning will be produced at all direct utilisation of a deprecated construction.
   The directive is skipped.

   \@todo
   A warning will be produced at all direct utilisation of an unimplemented construction.
   The directive is simply replaced by a \@fail directive.
   The purpose of this directive is to be used combined with the activation of the warn-error
   class coding.todo, for detecting every unimplemented constructions.
*)

(**
   {6 Warnings}
*)
val warning_set : WarningClass.Set.t

(**
   {6 Process code}
*)
val process_code :
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  QmlAst.annotmap * QmlAst.code
