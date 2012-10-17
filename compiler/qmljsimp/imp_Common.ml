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
(* depends *)
module Format = Base.Format

(* alias *)
module FieldSet = StringSet

(* shorthand *)
module J = JsAst
module Q = QmlAst

(* -- *)

let pp_esc fmt s = Format.fprintf fmt "%S" s
let pp_path fmt list = Format.pp_list " ; " pp_esc fmt list
let pp_fieldset fmt set =
  let sep = ref false in
  FieldSet.iter (
    fun field ->
      (if !sep then Format.fprintf fmt " ; " ; sep := true) ;
      Format.pp_print_string fmt field
  ) set


(* contains all the calls to the runtime (except the bsl which is called with
 * bypasses) *)
module ClientLib =
struct
  let (!!) s = JsCons.Expr.native_global s

  let build_true = !! "build_true"
  let build_false = !! "build_false"
  let build_bool b = if b then build_true else build_false

  let dot_true = !! "dot_true"
  let dot_false = !! "dot_false"
  let dot_bool b = if b then dot_true else dot_false
  let dot = !! "dot"
  let udot = !! "udot"

  let env_apply_with_ty = !! "_env_apply_with_ty"

  let error = !! "error"
  let extend_record = !! "extend_record"

  let match_failure pos =
    let message = JsCons.Expr.string (Format.sprintf "%a: Match failure %d" FilePos.pp_pos pos (BaseRandom.int 10000000)) in
    JsCons.Expr.call ~pure:false error [ message ]

  let size = !! "size"

  let void = !! "js_void"
  let none = !! "js_none"
  let some = !! "js_some"

  let option2js = !! "option2js"
  let js2option = !! "js2option"

  let type_string = !! "type_string"
  let type_char = !! "type_char"
  let type_int = !! "type_int"
  let type_float = !! "type_float"
  let type_fun = !! "type_fun"
  let type_fun_arity = !! "type_fun_arity"
  let type_var = !! "type_var"
  let type_option = !! "type_option"
  let type_void = !! "type_void"
  let type_native_option = !! "type_native_option"
  let type_native_void = !! "type_native_void"
  let type_bool = !! "type_bool"
  let type_extern = !! "type_extern"
  let type_opavalue = !! "type_opavalue"
  let assert_length = !! "assert_length"

  let wrap_tc = JsCons.Expr.native_global ~pure:true "wrap_tc"
end

(* a very conservative approximation of which expressions do observable side
 * effects *)
let does_side_effects e =
  JsWalk.OnlyExpr.exists
    (function
     | J.Je_hole _
     | J.Je_new _
     | J.Je_call (_,_,_,false) -> true
     | J.Je_unop (_, ( J.Ju_delete
                     | J.Ju_add2_pre
                     | J.Ju_sub2_pre
                     | J.Ju_add2_post
                     | J.Ju_sub2_post), _) -> true
     | J.Je_binop (_, ( J.Jb_assign
                      | J.Jb_mul_assign
                      | J.Jb_div_assign
                      | J.Jb_mod_assign
                      | J.Jb_add_assign
                      | J.Jb_sub_assign
                      | J.Jb_lsl_assign
                      | J.Jb_lsr_assign
                      | J.Jb_asr_assign
                      | J.Jb_and_assign
                      | J.Jb_xor_assign
                      | J.Jb_or_assign ), _, _) -> true

     | J.Je_runtime (_, e) -> (
         match e with
         | JsAstRuntime.SetDistant _ -> true
         | JsAstRuntime.TaggedString _ -> false
       )
     | _ -> false
    ) e

(* ************************************************************************** *)
(** {b Descr}: Returns [true] if the type has some values that may be evaluated
    into false in JS. The following types are considered "dangerous" because
    the specified value is evaluated into false in JS:
      - int : 0
      - float : 0.0
      - string : ""
      - char : ''
      - bool : false
      - type variable since we do not know what it will finally be, perhaps
        especially one of the above "dangerous" type.
      - abstract type since we don't known what it is really.
      - named type that remain named type after expansion since, if this
        happens this means that we have no explicit representation of them
        in term of basic types combinations, hence this probably corresponds
        to an abstract type. Note that currently, with the way QML represents
        abstract types, I'm really not sure that this can happen.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let maybe_void gamma ty =
  let rec do_job ~already_expanded = function
    | Q.TypeRecord (Q.TyRow ([], _)) -> true
    | Q.TypeSum (Q.TyCol (list, colvar)) ->
        Option.is_some colvar
        || List.exists (function [] -> true | _ -> false) list
    | Q.TypeRecord _ | Q.TypeArrow _ | Q.TypeConst _ -> false
    | Q.TypeAbstract | Q.TypeVar _ -> true
    | Q.TypeSumSugar _ ->
        (* There should not remain such type at this point. *)
        assert false
    | Q.TypeForall (_, _, _, t) -> do_job ~already_expanded t
    | (Q.TypeName _) as t ->
        if already_expanded then true
        else (
          (* The type has not already been expanded, hence we are allowed to
             expand it. *)
          let t = QmlTypesUtils.Inspect.follow_alias_noopt_private gamma t in
          (* And now it has been expanded, we are not allowed to expand it
             again forever. *)
          do_job ~already_expanded: true t
        )
  in
  do_job ~already_expanded: false ty

let maybe_js_false gamma ty =
  (* Local function processing the type. We only expand the type if it appears
     to be a named type.
     This saves time in case the type is not a named one. The flag
     [~already_expanded] tells if the type has been expanded, hence must not
     be again. *)
  let rec do_job ~already_expanded = function

    (*
      Special case for boolean.
      Do not call Inspect.is_type_bool, because it would perform a expansion
      everytime, and is not exactly what we need there.
      We are caring about value that are potentially [false], which includes
      bool value, but not only (e.g. with an open col, or row variable)
    *)
    | Q.TypeRecord (Q.TyRow (["false", ty], _)) ->
        maybe_void gamma ty

    | Q.TypeRecord (Q.TyRow ([], Some _)) -> true

    | Q.TypeSum (Q.TyCol (cols, colvar)) ->
        let void = ref true in
        let exists case =
          match case with
          | [ "false", tyf ] ->
              void := maybe_void gamma tyf ;
              !void

          | _ -> false
        in
        List.exists exists cols
        || (Option.is_some colvar && !void)

    (*
      From there, sum and record may not be bool
    *)
    | Q.TypeRecord _ | Q.TypeArrow _ -> false
    | Q.TypeAbstract | Q.TypeVar _ -> true
    | Q.TypeConst ct -> (
        (* In fact, all basic types are "dangerous". *)
        match ct with
        | Q.TyFloat | Q.TyInt | Q.TyString -> true
        | Q.TyNull -> assert false
      )
    | Q.TypeSumSugar _ ->
        (* There should not remain such type at this point. *)
        assert false
    | Q.TypeForall (_, _, _, t) -> do_job ~already_expanded t
    | (Q.TypeName _) as t ->
        if already_expanded then true
        else (
          (* The type has not already been expanded, hence we are allowed to
             expand it. *)
          let t = QmlTypesUtils.Inspect.follow_alias_noopt_private gamma t in
          (* And now it has been expanded, we are not allowed to expand it
             again forever. *)
          do_job ~already_expanded: true t
        )
  in
  (* Now, really do the job. Effective body of the function [maybe_js_false]. *)
  do_job ~already_expanded: false ty



let const const =
  match const with
  | Q.Int i ->
      JsCons.Expr.bint i
  | Q.Float f ->
      JsCons.Expr.float f
  | Q.String s ->
      JsCons.Expr.string s
