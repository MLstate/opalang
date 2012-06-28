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
(* CF mli *)

(* depends in base *)
module List = Base.List

(* refactoring in progress *)

(* alias *)
module Q = QmlAst
module QC = QmlAstCons

(*
  TODO:
  + errors manager (principally internals) instead of assert false
*)

let directive_client_id = `fun_action (Some Q.Client_id)
let directive_deserialize = `fun_action (Some Q.Deserialize)
let directive_funaction = `fun_action None

let serialize_argument = Opacapi.FunActionServer.serialize_argument
let val_ v = OpaMapToIdent.val_ ~side:`server v
let jsast_string_of_ident (i:Q.ident) = JsPrint.string_of_ident (JsAst.ExprIdent i)

let process_server_code ~stdlib_gamma gamma annotmap code =

  (* get inserted functions *)
  let id_serialize_argument = lazy (val_ serialize_argument) in
  let make_ty arg_ty = Q.TypeRecord (Q.TyRow (["arg", arg_ty; "serialized_arg", QmlTypesUtils.Basic.string ], None)) in

  (* Rewritting the code *)
  let foldmap_expr toplevel_var annotmap e =
    QmlAstWalk.Expr.foldmap_up
      (fun annotmap e -> match e with
       | Q.Directive (_, `fun_action None, [expr], []) -> (
           match expr with
           | Q.Apply (_, f, args) -> (

               let fresh_ids = List.map (fun _ -> Ident.next "arg") args in

               let annotmap, fresh_args = List.fold_left_map2 (
                 fun annotmap id arg ->
                   let ty = QmlAnnotMap.find_ty (Q.QAnnot.expr arg) annotmap in
                   QC.TypedExpr.ident annotmap id (make_ty ty)
               ) annotmap fresh_ids args
               in

               (*
                 @funaction[Deserialize](arg1_),
                 @funaction[Deserialize](arg2_),
                 ..
               *)
               let annotmap, params = List.fold_left_map2 (
                 fun annotmap fresh_arg orig_arg ->
                   (* In order not to duplicate annotation, we must copy there the args *)
                   let annotmap, arg = QC.TypedExpr.copy annotmap fresh_arg in
                   QC.TypedExpr.directive_ty annotmap
                     directive_deserialize [arg] []
                     (QmlAnnotMap.find_ty (Q.QAnnot.expr orig_arg) annotmap)
               ) annotmap fresh_args args
               in

               (*
                 @funaction[client_id](f))(params)
               *)
               let annotmap, body =
                 let annotmap, client_id_f =
                   QC.TypedExpr.directive_ty annotmap
                     directive_client_id [f] []
                     (QmlAnnotMap.find_ty (Q.QAnnot.expr f) annotmap) in
                 QC.TypedExpr.apply gamma annotmap client_id_f params
               in

               (*
                 arg1_ = FunActionServer_serialize_argument(toplevel_var,arg1)
                 arg2_ = FunActionServer_serialize_argument(toplevel_var,arg2)
               *)
               let insert_let_arg id arg (annotmap,body) =
                 let inner_ty = QmlAnnotMap.find_ty (Q.QAnnot.expr arg) annotmap in
                 let ty = Q.TypeArrow ([Q.TypeConst Q.TyString; inner_ty], make_ty inner_ty) in
                 let annotmap, ident = QC.TypedExpr.ident annotmap (Lazy.force id_serialize_argument) ty in
                 (* putting the typescheme for explicit instantiation *)
                 let annotmap =
                   QmlAnnotMap.add_tsc_inst
                     (Q.QAnnot.expr ident)
                     (QmlTypes.Env.Ident.find (Lazy.force id_serialize_argument) stdlib_gamma) annotmap in
                 let annotmap, toplevel_var = QC.TypedExpr.string annotmap (Pass_GenerateServerAst.ident_to_string toplevel_var) in
                 let annotmap, apply = QC.TypedExpr.apply gamma annotmap ident [toplevel_var; arg] in
                 QC.TypedExpr.letin annotmap [id,apply] body
               in
               let annotmap,funaction =
                 List.fold_right2 insert_let_arg fresh_ids args (annotmap,body)
               in
               QC.TypedExpr.directive_ty annotmap
                 directive_funaction [funaction] []
                 (QmlAnnotMap.find_ty (Q.QAnnot.expr e) annotmap)
             )
           | _ ->
               (* TODO: uses OpaError *)
               OManager.printf "Unexpected expr : %a@\n" QmlPrint.pp#expr e ;
               OManager.i_error "funactions"
         )
       | _ -> annotmap, e
      ) annotmap e
  in
  let annotmap,code =
    QmlAstWalk.CodeExpr.fold_map_name_expr
      (fun acc (i,e) ->
         let acc, e = foldmap_expr i acc e in
         acc, (i, e))
      annotmap code in

  (gamma, annotmap), code
