(*
    Copyright © 2011, 2012 MLstate

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
   @author Quentin Bourgerie
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

(**
   Collect all variable declarations in code and export them,
   e.g.
{[
      var foo = { bar: 1 };

      function hello(name) {
          console.log("Hello", name);
      }

   becomes

      var foo = { bar: 1 };

      function hello(name) {
          console.log("Hello", name);
      }

      exports.foo = foo;
      exports.hello = hello;
]}
   Note that this currently doesn't work for exporting variables
   that can change, e.g. a variable that holds a number whose value
   changes during program execution. However, it does work with objects,
   like in the above example
*)
val export_global_declarations : JsAst.code -> JsAst.code

(**
   Generate a json value that can be used in a package.json file
*)
val basic_package_json : ?version:string -> string -> string -> string

(** {3 Comparisons} *)

val compare_ident : JsAst.ident -> JsAst.ident -> int

val compare_expr : JsAst.expr -> JsAst.expr -> int

val compare_statement : JsAst.statement -> JsAst.statement -> int

val compare_code : JsAst.code -> JsAst.code -> int

(** A very conservative approximation of which expressions do observable side
    effects *)
val does_side_effects : JsAst.expr -> bool


(** Get dependencies to Opa packages from a JavaScript code. *)
val get_package_deps : JsAst.code -> StringSet.t
