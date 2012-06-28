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
   Typing Directives (language and compilation).
*)

(**
   This module can regroup utils working on directives.
   It is meant to be not far after QmlAst wrt the dependencies between modules.
   Possibly, the printer of string_of_directives can also be moved in that file.
*)

(**
   The type of the variant used for directives.
   Ideally in S4, the same variant groups should be shared between OPA and QML
*)
type directive = QmlAst.qml_directive


(** {6 Type of Directive} *)

(**
   A directive is used for extensible utilisation of the AST.
   It can have a som expressions in arguments, and some types as well.
   Currently, it is not possible in the syntax to write types arguments for a directives,
   but it is a TODO.
   See some examples :
   {[
   \@take_expr_and_ty("titi", "toto" ; float, (int -> int), (int, char -> bool))
   \@take_just_ty(; char)
   \@take_just_expr("titi")
   ]}

   The type of directive is known statically, and defined in this module.
   The typer, as well as [QmlAstCons.TypedExpr] use this module to type directive.
*)
val ty : directive -> QmlAst.expr list -> QmlAst.ty list -> QmlAst.ty

(** {6 Utils} *)

(**
   Get the arguments of the directive `create_lazy_record directives.
   The info is optional, the arguments are on the form :
   {[
   | [expr]
   | [expr ; info]
   ]}
   The returned tuple is the expr, and the optional info
*)
val create_lazy_record_arguments :
  QmlAst.expr list ->
  QmlAst.expr * QmlAst.expr option

val create_lazy_record_exprs :
  QmlAst.expr -> QmlAst.expr option ->
  QmlAst.expr list

val to_string : directive -> string
