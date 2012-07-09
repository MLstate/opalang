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
   Warning Coercion.
   @author Mathieu Barbin
*)

(**
   This pass is used after Typing for ensuring that some expressions have
   an expected type.
   Typically, we use this pass for cheking if an expression after a [do]
   has the type [void].
   In case the expression has not the given type, the pass_WarnCoerce raise
   a warning of the class typer.warncoerce. This class can be turned on --warn-error.

   The directives is keeped, it will be removed in the PurgeTyperDirective pass,
   with the other typer directives.

*)

val process_code:
  QmlTypes.gamma ->
  QmlAst.annotmap ->
  QmlAst.code ->
  unit
