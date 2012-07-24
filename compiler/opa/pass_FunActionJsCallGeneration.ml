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
(* CF mli *)

(* refactoring in progress *)

(* alias *)
module JsIdent = Qmljs_Serializer.JsIdent

(* shorthand *)
module Q = QmlAst
module QCT = QmlAstCons.TypedExpr
module TC = QmlTypesUtils.Basic

let field_expr = "expr"
let field_value = "value"

(* MERGE WITH THE ONE IN opa_InsertRemote *)
module TyIdent = struct

  let get name ~side  = fun annotmap gamma->
    try
      let ident = OpaMapToIdent.val_ ~side name in
      let (ty:Q.ty) = QmlTypes.Scheme.instantiate (QmlTypes.Env.Ident.find ident gamma) in
      QCT.ident annotmap ident ty
    with Not_found -> assert false (*QmlError.i_error (Some QmlCheck.Ident.mandatory_undefined_id) (QmlError.context ~annotmap posexpr) "Missing ident"*)

end

let bad_case e = Format.printf "\n%a\n" QmlPrint.pp#expr e;;

(**
   Opa utility functions names
*)
let _FunActionServer_serialize_call = TyIdent.get Opacapi.FunActionServer.serialize_call
let _FunActionServer_serialize_ty_arg = TyIdent.get Opacapi.FunActionServer.serialize_ty_argument

(**
    @funaction[Deserialize](arg1_) => arg1_
*)
let process_arg = function
  | Q.Directive (_, `fun_action (Some Q.Deserialize), [id], []) -> id
  | _ -> assert false

(**
   [@funaction[Deserialize](arg1_);...]_caml  => [arg1_, ...]_qml
*)
let process_args gamma annotmap (arg_el: Q.expr list) =
  QCT.list (annotmap,gamma) (match arg_el with
  | Q.Directive (_, `fun_action _,_,_) :: _ -> List.map process_arg arg_el
  | [_] -> []
  | _ -> assert false
  )

(**
    ty_arg1 => FunActionServer_serialize_ty_arg(ty_arg1)
*)
let process_ty_arg ~stdlib_gamma gamma annotmap ty_e =
  let annotmap,serialize_ty_arg  = _FunActionServer_serialize_ty_arg ~side:`server annotmap stdlib_gamma in
  QCT.apply gamma annotmap serialize_ty_arg [ty_e]

(**
    [ty_arg1]_caml => [FunActionServer_serialize_ty_arg(ty_arg1),...]_qml
*)
let process_ty_args ~stdlib_gamma gamma annotmap (ty_el: Q.expr list) =
  let annotmap,l = Base.List.fold_left_map (process_ty_arg ~stdlib_gamma gamma) annotmap ty_el in
  QCT.list (annotmap,gamma) l

(**

   id => @js_ident("~id")

   where ~id is the stringified name of id
*)
let process_funaction client_renaming annotmap e =
  match e with
  | Q.Ident (_, id) ->
      let id = QmlRenamingMap.original_from_new client_renaming id in
      let jsident = JsIdent.resolve id in
      QCT.ident annotmap jsident QCT.ty_string

(*
  FIXME: use QmlError for an internal error.
*)
  | _ -> bad_case e ; assert false

(** get the final expr of nested letin *)
let rec get_final_expr e =
  match e with
  | Q.LetIn (_, _, e ) -> get_final_expr e
  | _ -> e

(** change the final expr of nested letin given the old final expr and the new final expr
    fail if the old final expr cannot be found
    assume the type has not changed (if not regenerate annotmap with final expr type)
    TODO the assumption is erroneous in our case
*)
let rec change_final_expr e ~olde ~newe =
  match e with
  | Q.LetIn (label, l, e) -> Q.LetIn (label, l, change_final_expr e ~olde ~newe)
  | _ ->
      assert (olde == e);
      newe

(**
   { expr = @funaction(
            arg1_ = ...
            ...
            (@funaction[Client_id](f(tyarg1..)))(
                  @funaction[Deserialize](arg1_)
                  @funaction[Deserialize](arg2_)
            )
   )}

   ==>

   { string =
            arg1_ = ...
            ...
            FunActionServer_serialize_call(
                  @js_ident(f),
                  [ FunActionServer_serialize_ty_arg(tyarg1) , ...],
                  [ arg1_, arg2_, ...]
            )
   )}

*)
(**
   create the string record
*)
let generate_string_record stdlib_gamma gamma annotmap
    ~initial_record ~initial_letin ~oldcall
    ~funaction ~ty_args ~args =
  let annotmap,serialize_call  = _FunActionServer_serialize_call ~side:`server annotmap stdlib_gamma in
  let annotmap, string_call = QCT.apply gamma annotmap
    serialize_call[funaction ; ty_args ; args] in
  let letin_string = change_final_expr initial_letin
    ~olde:oldcall ~newe:string_call in
  annotmap, Q.Record (Q.Label.expr initial_record, [field_value,letin_string])


(**
   deconstruct the initial record and create the new one
*)
let process_fun_action_record client_renaming stdlib_gamma gamma annotmap e =
  match e with
  | Q.Record (_, [f_expr,dir]) when f_expr = field_expr ->
      begin match dir with
      | Q.Directive (_, `fun_action None, [initial_letin], _) ->
          let full_funaction_call = get_final_expr initial_letin in
          (* getting the processed funaction and eventuals ty_args, args *)
          (* 1) getting instantiated funaction and standard args *)
          let funaction_ty,(annotmap,args) =
            match full_funaction_call with
            | Q.Apply (_, funaction, args) ->
                funaction,process_args gamma annotmap args
            | _ -> bad_case full_funaction_call ; assert false
          in
          (* 2) getting non instantiated funaction and type args *)
          let funactionid,(annotmap,ty_args) =
            match funaction_ty with
            | Q.Directive (_, `fun_action (Some Q.Client_id), [funactionid], _) ->
                begin match funactionid with
                (* with explicit instantiation type parameter *)
                | Q.Apply (_, funactionid, ty_args) -> funactionid, process_ty_args ~stdlib_gamma gamma annotmap ty_args
                (* with explicit instantiation type parameter *)
                | Q.Ident _ -> funactionid, process_ty_args ~stdlib_gamma gamma annotmap []
                | _ -> bad_case funaction_ty ; assert false
                end
            | _ -> bad_case funaction_ty ; assert false
          in
          let annotmap, funaction =
            process_funaction client_renaming annotmap funactionid
          in
          (* putting everything together *)
          generate_string_record stdlib_gamma gamma annotmap
            ~initial_record:e ~initial_letin ~oldcall:full_funaction_call
            ~funaction ~ty_args ~args
      | _ -> annotmap, e
      end
  | _ ->
      annotmap, e


let process_server_code client_renaming stdlib_gamma gamma annotmap code =
  let annotmap, code = QmlAstWalk.CodeExpr.fold_map (QmlAstWalk.Expr.foldmap (process_fun_action_record client_renaming stdlib_gamma gamma)) annotmap code in
  (gamma, annotmap), code
