(*
    Copyright © 2011, 2013 MLstate

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
module List = Base.List

(* refactoring in progress *)

(* alias *)
module Q = QmlAst

(**************************************************************)
(* Before adding a function here please READ MLI INSTRUCTIONS *)
(**************************************************************)

type ('env, 'a) checker = ('env -> 'a)  -> 'env PassHandler.cond

(* utils for context *)
let context_code_elt_expr code_elt expr =
  let c1 = QmlError.Context.code_elt code_elt in
  let c2 = QmlError.Context.expr expr in
  QmlError.Context.merge2 c1 c2

let context_annotmap_expr annotmap expr =
  let c1 = QmlError.Context.annotmap annotmap in
  let c2 = QmlError.Context.expr expr in
  QmlError.Context.merge2 c1 c2

(* a *)

module Annot =
struct

  let cond_annot =
    let doc = "Annotations checks" in
    WarningClass.create ~parent:WarningClass.cond ~name:"annot" ~doc ~err:true ~enable:true ()
  let cond_annot_unicity =
    WarningClass.create ~parent:cond_annot ~name:"unicity" ~doc:"Unicity of annotations" ~err:false ~enable:true ()
  let cond_annot_find =
    WarningClass.create ~parent:cond_annot ~name:"find" ~doc:"Checking on annotmap" ~err:true ~enable:true ()

  let id = PassHandler.define_cond cond_annot

  (* TODO:located error message or this test is unusable !!!*)
  let annot_find annotmap code =
    if not (QmlAnnotCheckup.code annotmap code) then
      OManager.warning ~wclass:cond_annot_find
        "Annotmap is corrupted@\n"
    else ()

  let find_id = PassHandler.define_cond cond_annot_find

  let find extract =
    PassHandler.make_condition find_id
      (fun env ->
        let annotmap, code = extract env in
        annot_find annotmap code)

  let annot_unicity code =
    let check_expr (annset, ()) e =
      QmlAstWalk.ExprPatt.fold
        (fun (annset, ()) e ->
          let a = Q.QAnnot.expr e in
          if AnnotSet.mem a annset then (
            QmlError.scheck_fail find_id
              (QmlError.Context.expr e)
              "Annot already seen@\n";
            (annset, ()))
          else (AnnotSet.add a annset, ()))
        (fun (annset, ()) p ->
          let a = Q.QAnnot.pat p in
          if AnnotSet.mem a annset then (
            QmlError.scheck_fail find_id
              (QmlError.Context.pat p)
              "Annot already seen@\n";
            (annset, ()))
          else (AnnotSet.add a annset, ()))
        (annset, ()) e
    in
    snd (QmlAstWalk.CodeExpr.fold
      check_expr (AnnotSet.empty, ()) code)

  let unicity_id = PassHandler.define_cond cond_annot_unicity

  let unicity extract =
    PassHandler.make_condition unicity_id
      (fun env -> annot_unicity (extract env))
end

(* b *)

module Bypass =
struct

  let cond_bypass =
    let doc = "Bypasses checks" in
    WarningClass.create ~parent:WarningClass.cond ~name:"bypass" ~doc ~err:true ~enable:true ()
  let cond_bypass_applied =
    WarningClass.create ~parent:cond_bypass ~name:"applied"
      ~doc:"Total application of bypasses"
      ~err:true ~enable:true ()

  let id = PassHandler.define_cond cond_bypass

  let applied_id = PassHandler.define_cond cond_bypass_applied

  let bypass_applied bypass_typer code =
    let is_fully_applied code_elt tra expr =
      let (!!) x =
        let context = context_code_elt_expr code_elt expr in
        QmlError.scheck_fail applied_id context x in
      let rec aux_expr expr =
        match expr with
        | Q.Apply _ ->
            (match QmlAstUtils.App.to_list expr with
             | [] | [_] -> assert false
             | bypass :: app_args ->
                 let rec aux_bypass bypass =
                   match bypass with
                   | Q.Directive (_, `may_cps, [bypass], _) -> aux_bypass bypass
                   | Q.Directive (_, `restricted_bypass _, [ Q.Bypass (_, key) ], _)
                   | Q.Bypass (_, key) ->
                       let ty = bypass_typer key in
                       let bypass_arity =
                         match ty with
                         | Some (QmlAst.TypeArrow (args, ty)) -> QmlTypesUtils.TypeArrow.curryfied_arity args ty
                         | Some _ -> 0 (* should not happen *)
                         | None -> 0
                       in
                       let args_number = List.length app_args in
                       if bypass_arity <> args_number then
                         let skey = BslKey.to_string key in
                         let d = bypass_arity - args_number in
                         if d > 0 then
                           !! (
                             "@[<2>This bypass (%s) is partially applied.@\n"^^
                               "The arity of this bypass is %d@\n"^^
                               "and it is there applied to %d argument(s)@]"
                           )
                             skey bypass_arity args_number
                         else
                           !! (
                             "@[<2>This bypass (%s) is applied to too many arguments.@\n"^^
                               "The arity of this bypass is %d@\n"^^
                               "And it is there applied to %d argument(s).@]"
                           )
                             skey bypass_arity args_number
                   | _ -> ()
                 in aux_bypass bypass
            )
        | Q.Directive (_, `may_cps, [bypass], _) -> aux_expr bypass
        | Q.Directive (_, `restricted_bypass _, [ Q.Bypass (_, key) ], _)
        | Q.Bypass (_, key) ->
            let ty = bypass_typer key in
            let bypass_arity =
              match ty with
              | Some (QmlAst.TypeArrow (args, ty)) ->
                  QmlTypesUtils.TypeArrow.curryfied_arity args ty (* should not happen *)
              | Some _ -> 0
              | None -> 0
            in
            if bypass_arity <> 0 then
              let skey = BslKey.to_string key in
              !! "This bypass (%s) takes %d argument(s) but is not applied@\n" skey bypass_arity
        | _ -> tra expr
      in
      aux_expr expr
    in
    List.iter (fun code_elt ->
                 QmlAstWalk.Top.iter_expr (QmlAstWalk.Expr.traverse_iter (is_fully_applied code_elt)) code_elt)
      code

  let applied extract =
    PassHandler.make_condition applied_id
      (fun env ->
        let bypass_typer, code = extract env in
        bypass_applied bypass_typer code)
end

(* c *)

module Code =
struct

  let cond_code =
    let doc = "Code checks" in
    WarningClass.create ~parent:WarningClass.cond ~name:"code" ~doc ~err:true ~enable:true ()
  let cond_code_contents =
    let doc = "Code elts present in the code" in
    WarningClass.create ~parent:cond_code ~name:"contents" ~doc ~err:true ~enable:true ()
  let cond_code_valrec =
    let doc = "Validity of recursives values" in
    WarningClass.create ~parent:cond_code ~name:"valrec" ~doc ~err:true ~enable:true ()


  let id = PassHandler.define_cond cond_code

  type contents_code_elt = {
    c_Database : bool ;
    c_NewDbValue : bool ;
    c_NewType : bool ;
    c_NewVal : bool ;
    c_NewValRec : bool ;
  }
  let contents_all = {
    c_Database = true ;
    c_NewDbValue = true ;
    c_NewType = true ;
    c_NewVal = true ;
    c_NewValRec = true ;
  }
  let contents_id = PassHandler.define_cond cond_code_contents
  let contents_check cont (annotmap, code) =
    let iter = function
      | Q.Database _ when cont.c_Database -> ()
      | Q.NewDbValue _ when cont.c_NewDbValue -> ()
      | Q.NewType _ when cont.c_NewType -> ()
      | Q.NewVal _ when cont.c_NewVal -> ()
      | Q.NewValRec _ when cont.c_NewValRec -> ()
      | code_elt ->
          let context =
            let c1 = QmlError.Context.code_elt code_elt in
            let c2 = QmlError.Context.annotmap annotmap in
            QmlError.Context.merge2 c1 c2
          in
          QmlError.check_fail contents_id context
            "The code should not more content such code_elt as this point.@\n"
    in
    List.iter iter code
  let contents cont extract =
    PassHandler.make_condition contents_id
      (fun env -> contents_check cont (extract env))

  (* Recursives values **************************)
  let valrec_id = PassHandler.define_cond cond_code_valrec

  type ('a,'b) ignored_directives = [
  | `doctype of 'a
  | QmlAst.type_directive
  | QmlAst.slicer_directive
  | `lifted_lambda of 'b
  | `async
  | `workable
  ]

  let check_valrec ~undot check_fail (annotmap, code) =
    let error =
      if check_fail then QmlError.scheck_fail valrec_id
      else QmlError.serror
    in
    let get_name =
      if check_fail then Ident.to_string
      else Ident.original_name
    in
    let make_message to_string no_lambda rec_def =
      let a =
        fst
          (List.fold_left
             (fun (s, c) (ident, e0) ->
                (Printf.sprintf "%s%c %s %s" s c (to_string ident) (Format.to_string QmlPrint.pp#expr e0)), ',')
             ("The following idents are not lambda ", ':')
             no_lambda)
      in
      fst
        (List.fold_left
           (fun (s, c) (ident, _) ->
              (Printf.sprintf "%s%c %s" s c (to_string ident)), ',')
           (Printf.sprintf "%s\nOn the following recursive definition" a, ':')
           rec_def)
    in
    let rec check_lambda (name, e0) =
      match e0 with

      (* Directives: Particular cases *)
      | Q.Directive (_, `module_, [e], _) ->
          if undot then
            (match e with
             | Q.Record (_, lst) ->
                 (*
                   FIXME: remove (or document seriously) these dirty magic !
                 *)
                 check_list (Obj.magic (fun x -> x))
                   (Some (context_annotmap_expr annotmap e)) (Obj.magic lst)
             | _ -> assert false)
          else false
      | Q.Directive (_, `insert_server_value _, _, _) ->
          (* an other check can be done later, after client code injection *)
          true

      (* traversed directives *)
      | Q.Directive (_, #ignored_directives, [e], _) -> check_lambda (name, e)

      | Q.Coerce (_, e, _) -> check_lambda (name, e)
      | Q.Directive (_, `recval, _, _)
      | Q.Lambda _ -> true
      | _ -> false

    and check_list to_string context l =
      let no_lambda =
        List.fold_left
          (fun no_lambda cpl ->
             if not (check_lambda cpl) then
               cpl::no_lambda
             else no_lambda
          ) [] l in
      if not (List.is_empty no_lambda) then (
        let context =
          match context with
          | None ->
              (* FIXME: this is a hack for building a context *)
              context_annotmap_expr annotmap (snd (List.hd no_lambda))
          | Some context -> context
        in

        error context
          "Invalid recursive definition@\n%s@\n"
          (make_message to_string no_lambda l);
        false
      ) else true
    in
    let check_into (_, e) =
      let aux e = match e with
      | Q.LetRecIn (_, ieli, _) ->
          ignore(
            check_list get_name
              (Some (context_annotmap_expr annotmap e)) ieli)
      | _ -> ()
      in QmlAstWalk.Expr.iter_down aux e
    in
    let check lcode =
      List.iter
        ( function
            | Q.NewVal (_, li) -> List.iter check_into li
            | Q.NewValRec (_, li)  ->
                ignore (check_list get_name None li)
            | _ -> ()
        )
        lcode
    in
    check code

  let valrec extract =
    PassHandler.make_condition valrec_id
      (fun env ->
         (* FIXME - Undot should be given by the caller *)
         check_valrec ~undot:true true (extract env))

  let valrec_user ~undot extract =
    PassHandler.make_condition valrec_id (fun env -> check_valrec ~undot false (extract env))
end
