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
   Various utilities that can be used from every module of the backend
*)


val pp_esc : Format.formatter -> string -> unit
val pp_path : Format.formatter -> string list -> unit
val pp_fieldset : Format.formatter -> StringSet.t -> unit

(**
   Interface to the runtime lib
*)
module ClientLib :
sig
  val build_bool : bool -> JsAst.expr
  val build_false : JsAst.expr
  val build_true : JsAst.expr

  val dot_bool : bool -> JsAst.expr
  val dot_false : JsAst.expr
  val dot_true : JsAst.expr

  val env_apply_with_ty : JsAst.expr

  val error : JsAst.expr
  val extend_record : JsAst.expr

  val match_failure : FilePos.pos -> JsAst.expr

  val size : JsAst.expr

  val void : JsAst.expr

  val type_string : JsAst.expr
  val type_char : JsAst.expr
  val type_int : JsAst.expr
  val type_float : JsAst.expr
  val type_fun : JsAst.expr
  val type_fun_arity : JsAst.expr
  val type_var : JsAst.expr
  val type_option : JsAst.expr
  val type_void : JsAst.expr
  val type_native_option : JsAst.expr
  val type_native_void : JsAst.expr
  val type_bool : JsAst.expr
  val type_extern : JsAst.expr
  val type_opavalue : JsAst.expr
  val assert_length : JsAst.expr
end

(**
   General purpose analysis
*)

val does_side_effects : JsAst.expr -> bool

(**
   [maybe_js_false ty] tell if an inhabitant of the opa type ty,
   once represented in javascript could be evaluated to false in
   a condition test.
   This is used for optimizing condition mixed with affectation.
   If not any value of the type [ty] could be evaluated to [false],
   we can generate :
   {[
   if (x = a.some)
   ]}
   instead of
   {[
   if (x = a.some && x != undefined)
   ]}

   We should add a cache in named type in this function for performance.
*)
val maybe_js_false : QmlTypes.gamma -> QmlAst.ty -> bool

(**
   Transform a QML constant into a Javascript constant.
*)
val const : QmlAst.const_expr -> JsAst.expr
