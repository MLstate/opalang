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
   Utils on Javascript code
   @author Mathieu Barbin
   @author Valentin Gatien-Baron
*)

(**
   Native ident analysis.
   Replace local native ident to global native ident when they are not
   local to the considered code elt.
*)
val globalize_native_ident : JsAst.code_elt -> JsAst.code_elt

(**
   Prefix every global identifier by "global.". Converts declarations
   so that they still make sense, i.e.

     var foo = bar; -> global.foo = bar;

   Handle also corner cases such as replacing a real export

     exports.foo = foo; -> global.foo = foo;
*)
val export_to_global_namespace : JsAst.code -> JsAst.code
