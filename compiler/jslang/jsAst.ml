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
   Javascript Complete Ast
   @author Mathieu Barbin, adapted for Opa from ocamljs/jslib
*)

(**
   location in source, and annotation
*)
type loc = Annot.label

(**
   label used in statement
*)
type label = string

(**
   Native identfiers:
   They are used for:
   -generated code
   -parsed code from external js files

   The parser build only {[Native (Local ident)]}, and code generator
   as well.
   There is a pass for transforming [Local] native to [Global] native,
   so that these identifiers can be serialized, and renamed
   since we clean the jsbsl.
   The boolean associated to `global ident indicates if the corresponding value
   is pure or not.
*)
type native_ident = [ `global of bool | `local ]

(**
   Javascript Identifiers.
   ExprIdent are classical identifier, from the compilation of qml code.
*)
type ident = JsIdent.t =
  | ExprIdent of Ident.t
  | Native of native_ident * string

(**
   Unary operators
*)
type unop =
  | Ju_delete      (** {[ delete ]} *)
  | Ju_void        (** {[ void ]} *)
  | Ju_typeof      (** {[ typeof ]} *)
  | Ju_add2_pre    (** {[ ++x ]} *)
  | Ju_sub2_pre    (** {[ --x ]} *)
  | Ju_add_pre     (** {[ +x ]} *)
  | Ju_sub_pre     (** {[ -x ]} *)
  | Ju_tilde       (** {[ ~x ]} *)
  | Ju_not         (** {[ !x ]} *)
  | Ju_add2_post   (** {[ x++ ]} *)
  | Ju_sub2_post   (** {[ x-- ]} *)

(**
   Binary operators
*)
type binop =
  | Jb_hashref     (** {[ x[y] ]} *)
  | Jb_mul         (** {[ * ]} *)
  | Jb_div         (** {[ / ]} *)
  | Jb_mod         (** {[ % ]} *)
  | Jb_add         (** {[ + ]} *)
  | Jb_sub         (** {[ - ]} *)
  | Jb_lt          (** {[ < ]} *)
  | Jb_gt          (** {[ > ]} *)
  | Jb_leq         (** {[ <= ]} *)
  | Jb_geq         (** {[ >= ]} *)
  | Jb_lsr         (** {[ >> ]} *)
  | Jb_lsl         (** {[ << ]} *)
  | Jb_asr         (** {[ >>> ]} *)
  | Jb_eq          (** {[ == ]} *)
  | Jb_neq         (** {[ != ]} *)
  | Jb_in          (** {[ in ]} *)
  | Jb_instanceof  (** {[ instanceof ]} *)
  | Jb_seq         (** {[ === ]} *)
  | Jb_sneq        (** {[ !== ]} *)
  | Jb_land        (** {[ && ]} *)
  | Jb_lor         (** {[ || ]} *)
  | Jb_and         (** {[ & ]} *)
  | Jb_xor         (** {[ ^ ]} *)
  | Jb_or          (** {[ | ]} *)
  | Jb_assign      (** {[ = ]} *)
  | Jb_mul_assign  (** {[ *= ]} *)
  | Jb_div_assign  (** {[ /= ]} *)
  | Jb_mod_assign  (** {[ %= ]} *)
  | Jb_add_assign  (** {[ += ]} *)
  | Jb_sub_assign  (** {[ -+ ]} *)
  | Jb_lsl_assign  (** {[ <<= ]} *)
  | Jb_lsr_assign  (** {[ >>= ]} *)
  | Jb_asr_assign  (** {[ >>>= ]} *)
  | Jb_and_assign  (** {[ &= ]} *)
  | Jb_xor_assign  (** {[ ^= ]} *)
  | Jb_or_assign   (** {[ |= ]} *)

(**
   Expressions

   GUIDELINES for matching expressions
   {[
   | J.Je_this _ ->
   | J.Je_ident (_, ident) ->
   | J.Je_array (_, list) ->
   | J.Je_comma (_, list, last) ->
   | J.Je_object (_, fields) ->
   | J.Je_string (_, string, double_quote) ->
   | J.Je_num (_, num) ->
   | J.Je_null _ ->
   | J.Je_undefined _ ->
   | J.Je_bool (_, bool) ->
   | J.Je_regexp (_, body, flags) ->
   | J.Je_function (_, ident, params, body) ->
   | J.Je_dot (_, expr, field) ->
   | J.Je_unop (_, op, expr) ->
   | J.Je_binop (_, op, expr1, expr2) ->
   | J.Je_cond (_, cond, then_, else_) ->
   | J.Je_call (_, fun_, args, _) ->
   | J.Je_new (_, obj, args) ->
   | J.Je_hole (_, qml) ->
   ]}

*)
type expr =
  | Je_this of       loc
  | Je_ident of      loc * ident
  | Je_array of      loc * expr list
  | Je_comma of      loc * expr list * expr
  | Je_object of     loc * (string * expr) list
  | Je_string of     loc * string * bool
      (**
         true if double-quoted,
         else simple-quoted
      *)

  | Je_num of        loc * string
  | Je_null of       loc
  | Je_undefined of  loc
      (**
         <!> Beware, in js, [undefined] is not a keyword,
         so, we assume it will not be redefined.
      *)

  | Je_bool of       loc * bool
  | Je_regexp of     loc * string * string
  | Je_function of   loc * ident option * ident list * statement list
  | Je_dot of        loc * expr * string
  | Je_unop of       loc * unop * expr
  | Je_binop of      loc * binop * expr * expr
  | Je_cond of       loc * expr * expr * expr
  | Je_call of       loc * expr * expr list * bool
      (**
         Function call.
         bool flag:
         -true: pure;
         -false: possible side effects
      *)

  | Je_new of        loc * expr * expr list
  | Je_hole of       loc * QmlAst.expr
      (**
         Qml Hole, DynamicExpr computed on the server side
         + cf hybrid values.
         Resolved at runtime (initialization of the server)
         Compilation of hybrid_value directive.
      *)

  | Je_runtime of    loc * JsAstRuntime.expr
      (**
         This node allows to build expression of the runtime
         ast directly
      *)

(**
   Statements.

   GUIDELINES for matching statements
   {[
   | J.Js_var (_, ident, expr) ->
   | J.Js_function (_, ident, params, body) ->
   | J.Js_return (_, expr) ->
   | J.Js_continue (_, label) ->
   | J.Js_break (_, label) ->
   | J.Js_switch (_, expr, cases, default) ->
   | J.Js_if (_, cond, then_, else_) ->
   | J.Js_throw (_, expr) ->
   | J.Js_expr (_, expr) ->
   | J.Js_trycatch (_, body, catches, finally) ->
   | J.Js_for (_, init, cond, incr, body) ->
   | J.Js_forin (_, lhs, rhs, body) ->
   | J.Js_dowhile (_, body, cond) ->
   | J.Js_while (_, cond, body) ->
   | J.Js_block (_, body) ->
   | J.Js_with (_, expr, body) ->
   | J.Js_label (_, label, stmt) ->
   | J.Js_comment (_, kind, string) ->
   ]}

*)
and statement =
  | Js_var of        loc * ident * expr option
      (**
         Please be careful because [var x = undefined]
         and [var x] is not the same
         Think about [x = 1; var x] and [x = 1; var x = undefined]

         The printer will regroup successive variable definitions,
         except in the toplevel.
         {[
         function foo() {
           var x = 5, y, z;
         }
         ]}
      *)

  | Js_function of   loc * ident * ident list * statement list
  | Js_return of     loc * expr option
  | Js_continue of   loc * label option
  | Js_break of      loc * label option
  | Js_switch of     loc * expr * (expr * statement) list * statement option
  | Js_if of         loc * expr * statement * statement option
  | Js_throw of      loc * expr
  | Js_expr of       loc * expr
  | Js_trycatch of   loc * statement
                         * (ident * expr option * statement) list
                         * statement option
  | Js_for of        loc * expr option * expr option * expr option * statement
  | Js_forin of      loc * expr * expr * statement
  | Js_dowhile of    loc * statement * expr
  | Js_while of      loc * expr * statement
  | Js_block of      loc * statement list
  | Js_with of       loc * expr * statement
  | Js_label of      loc * string * statement
  | Js_comment of    loc * [ `doc | `simple | `one_line ] * string

(**
   A code element is simply a statement
*)
type code_elt = statement

(**
   Code
*)
type code = code_elt list


let is_assignment_unop = function
  | Ju_add2_pre    (** {[ ++x ]} *)
  | Ju_sub2_pre    (** {[ --x ]} *)
  | Ju_add2_post   (** {[ x++ ]} *)
  | Ju_sub2_post   (** {[ x-- ]} *)
      -> true
  | Ju_delete      (** {[ delete ]} *)
  | Ju_void        (** {[ void ]} *)
  | Ju_typeof      (** {[ typeof ]} *)
  | Ju_add_pre     (** {[ +x ]} *)
  | Ju_sub_pre     (** {[ -x ]} *)
  | Ju_tilde       (** {[ ~x ]} *)
  | Ju_not         (** {[ !x ]} *)
      -> false

(**
   Tell if a unary operator makes a side effect
*)
let is_side_effect_unop = function
  | Ju_delete      (** {[ delete ]} *)
  | Ju_add2_pre    (** {[ ++x ]} *)
  | Ju_sub2_pre    (** {[ --x ]} *)
  | Ju_add2_post   (** {[ x++ ]} *)
  | Ju_sub2_post   (** {[ x-- ]} *)
      -> true

  | Ju_void        (** {[ void ]} *)
  | Ju_typeof      (** {[ typeof ]} *)
  | Ju_add_pre     (** {[ +x ]} *)
  | Ju_sub_pre     (** {[ -x ]} *)
  | Ju_tilde       (** {[ ~x ]} *)
  | Ju_not         (** {[ !x ]} *)
      -> false

let is_assignment_binop = function
  | Jb_hashref     (** {[ x[y] ]} *)
  | Jb_mul         (** {[ * ]} *)
  | Jb_div         (** {[ / ]} *)
  | Jb_mod         (** {[ % ]} *)
  | Jb_add         (** {[ + ]} *)
  | Jb_sub         (** {[ - ]} *)
  | Jb_lt          (** {[ < ]} *)
  | Jb_gt          (** {[ > ]} *)
  | Jb_leq         (** {[ <= ]} *)
  | Jb_geq         (** {[ >= ]} *)
  | Jb_lsr         (** {[ >> ]} *)
  | Jb_lsl         (** {[ << ]} *)
  | Jb_asr         (** {[ >>> ]} *)
  | Jb_eq          (** {[ == ]} *)
  | Jb_neq         (** {[ != ]} *)
  | Jb_in          (** {[ in ]} *)
  | Jb_instanceof  (** {[ instanceof ]} *)
  | Jb_seq         (** {[ === ]} *)
  | Jb_sneq        (** {[ !== ]} *)
  | Jb_land        (** {[ && ]} *)
  | Jb_lor         (** {[ || ]} *)
  | Jb_and         (** {[ & ]} *)
  | Jb_xor         (** {[ ^ ]} *)
  | Jb_or          (** {[ | ]} *)
      -> false
  | Jb_assign      (** {[ = ]} *)
  | Jb_mul_assign  (** {[ *= ]} *)
  | Jb_div_assign  (** {[ /= ]} *)
  | Jb_mod_assign  (** {[ %= ]} *)
  | Jb_add_assign  (** {[ += ]} *)
  | Jb_sub_assign  (** {[ -+ ]} *)
  | Jb_lsl_assign  (** {[ <<= ]} *)
  | Jb_lsr_assign  (** {[ >>= ]} *)
  | Jb_asr_assign  (** {[ >>>= ]} *)
  | Jb_and_assign  (** {[ &= ]} *)
  | Jb_xor_assign  (** {[ ^= ]} *)
  | Jb_or_assign   (** {[ |= ]} *)
      -> true

(**
   Tell if a binary operator makes a side effect
*)
let is_side_effect_binop = is_assignment_binop



(**
   Tell if a string is a reserved javascript keyword.
   list from http://www.georgehernandez.com/h/xComputers/Cs/ReservedKeywords.asp
*)
let is_keyword id =
    match id with
    | "abstract"
    | "boolean" | "break" | "byte"
    | "case" | "catch" | "char" | "class" | "const" | "continue"
    | "debugger" | "default" | "delete" | "do" | "double"
    | "else" | "enum" | "export" | "extends"
    | "false" | "final" | "finally" | "float" | "for" | "function"
    | "goto"
    | "if" | "implements" | "import" | "in" | "instanceof" | "int" | "interface"
    | "long"
    | "native" | "new" | "null"
    | "package" | "private" | "protected" | "public"
    | "return"
    | "short" | "static" | "super" | "switch" | "synchronized"
    | "this" | "throw" | "throws" | "transient" | "true" | "try" | "typeof"

    | "undefined"
        (*
          Although this is not a keyword, the printer use it,
          so this should not be redefined.
        *)

    | "var" | "void" | "volatile"
    | "while" | "with" -> true
    | _ -> false

let (|>) = InfixOperator.(|>)

type runtime_error =
  | Match_failure of string

module TTIdent =
struct
  type t = ident
  let compare = compare
end

module IdentMap = BaseMap.Make(TTIdent)
module IdentSet = BaseSet.Make(TTIdent)

module JLabel :
sig
  val stm : statement -> Annot.label
  val expr : expr -> Annot.label
end =
struct
  let stm = Annot.Magic.label
  let expr = Annot.Magic.label
end

module JPos :
sig
  val stm : statement -> FilePos.pos
  val expr : expr -> FilePos.pos
end =
struct
  let stm = Annot.Magic.pos
  let expr = Annot.Magic.pos
end

module JNewAnnot :
sig
  val stm : statement -> Annot.t -> statement
  val expr : expr -> Annot.t -> expr
end =
struct
  let stm = Annot.Magic.new_annot
  let expr = Annot.Magic.new_annot
end
