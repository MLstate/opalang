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

(*
  Please see the .mli for more public documentation about this module.
  There is some comments in the code to help the understanding of the pass. (private documentation)

  @author Sebastien Briais
  @author Esther Baruk
  @author Mathieu Barbin
*)

(* refactoring in progress *)

(* shorthands *)
module Q = QmlAst

(* Type of environment
   [bypass] is the map of already processed bypass, a bypass is bound with the ident of its top level definition.
   [defs] is the map of the new bypass definitions which will be added at end in the beginning of the code. *)
type env =
    {
      bypass: (Ident.t * (Q.ty,unit) QmlGenericScheme.tsc option) BslKeyMap.t ;
      defs : QmlAst.expr IdentMap.t ;
    }

let empty_env () =
  {
    bypass = BslKeyMap.empty ;
    defs = IdentMap.empty ;
  }

let add_defs env code =
  IdentMap.fold
    (fun x body code ->
       let label = Annot.nolabel "QmlBypassHoisting.add_defs" in
       ( Q.NewVal (label, [x, body]) ) ::code )
    env.defs
    code

let debug fmt =
  OManager.printf ("@{<cyan>Bypass Hoisting@} "^^fmt^^"@.")

(* We simplify bypass alias *)
module Alias :
sig
  type map = Ident.t IdentMap.t

  val empty : map

  (* substitute all ident of expr using map binding. pre condition : name analysis has been done before this pass *)
  val resolve : map -> QmlAst.expr -> QmlAst.expr

  (* add an alias in the map. assert : the [expr] should be an ident *)
  val add : Ident.t -> QmlAst.expr -> map -> map
end =
struct
  type map = Ident.t IdentMap.t
  let empty = IdentMap.empty
  let resolve alias expr =
    if IdentMap.is_empty alias then expr else
      let _ =
        #<If:BYPASS_HOISTING $minlevel 1>
          let print (i, j) = Printf.sprintf "[ %s <- %s ]" (Ident.to_string i) (Ident.to_string j) in
          debug "resolving alias : %s" (Base.String.concat_map " ; " print (IdentMap.to_list alias))
        #<End>
      in
      QmlAstWalk.Expr.map
        (fun expr ->
           match expr with
           | Q.Ident (label, ident) ->
               let ident = Option.default ident (IdentMap.find_opt ident alias) in
               Q.Ident (label,ident)
           | _ -> expr
        ) expr
  let assert_ident expr = match expr with Q.Ident (_, id) -> id | _ -> assert false
  let add id expr alias = IdentMap.add id (assert_ident expr) alias
end

(* a family of foldmap depending of the case *)
module FoldMap :
sig
  type acc = QmlTypes.gamma * QmlAst.annotmap * env
  type foldmaper = typed:bool -> acc -> QmlAst.expr -> acc * QmlAst.expr

  (* in just_expand, if a bypass is unknown, it is not protected. *)
  val just_expand  : QmlTypes.bypass_typer -> BslKey.t -> foldmaper
  val expand_hoist : QmlTypes.bypass_typer -> BslKey.t -> foldmaper
  val just_hoist   : QmlTypes.bypass_typer -> BslKey.t -> foldmaper
  val make_ident   : (Q.ty,unit) QmlGenericScheme.tsc option -> Ident.t -> foldmaper
end =
struct
  type acc = QmlTypes.gamma * QmlAst.annotmap * env
  type foldmaper = typed:bool -> acc -> QmlAst.expr -> acc * QmlAst.expr

  (* Add a bypass to gamma and register it to the environment *)
  let register_bypass tsc tsc_opt key ident expr gamma annotmap env =
    (* 1) Update the private env *)
    let bypass = BslKeyMap.add key (ident,tsc_opt) env.bypass in
    let annotmap = QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr expr) tsc_opt annotmap in
    let defs = IdentMap.add ident expr env.defs in
    let env = { bypass = bypass; defs = defs } in

    (* 2) Update gamma *)
    let gamma = QmlTypes.Env.Ident.add ident tsc gamma in

    gamma, annotmap, env

  let just_expand bypass_typer key ~typed ((gamma, annotmap, env) as acc) expr =
    match bypass_typer key with
    | Some ty ->
        let _ =
          #<If:BYPASS_HOISTING $minlevel 2>
            debug "eta-expand (skey:%a) : %a" BslKey.pp key QmlPrint.pp_annotation#expr expr
          #<End>
        in

        let ty =
          if typed then
            QmlAnnotMap.find_ty (Q.QAnnot.expr expr) annotmap
          else
            ty in

        let aux annotmap body ty =
          match ty with
          | Q.TypeArrow (intys, _) ->
              let (annotmap, typed_args), args = Base.List.fold_left_map
                (fun (annotmap, typed_args) inty ->
                   let id = Ident.next "by_arg" in
                   let annotmap, arg =
                     if typed then QmlAstCons.TypedExpr.ident annotmap id inty
                     else annotmap, QmlAstCons.UntypedExpr.ident id
                   in
                   (annotmap, (id, inty)::typed_args), arg
                ) (annotmap, []) intys in
              let annotmap, body =
                if typed then
                  QmlAstCons.TypedExpr.apply gamma annotmap body args
                else
                  annotmap, QmlAstCons.UntypedExpr.apply body args
              in
              if typed then QmlAstCons.TypedExpr.lambda annotmap (List.rev typed_args) body
              else annotmap, QmlAstCons.UntypedExpr.lambda (List.rev_map fst typed_args) body
          | _ ->
              (* this bypass is a value *)
              annotmap, body
        in
        let label = Q.Label.expr expr in
        let annotmap, expr = aux annotmap expr ty in

        (* Encapsulate the expression e in `expanded_bypass directive *)

        let annotmap, expr =
          if typed
          then
            let expr = QmlAstCons.UntypedExprWithLabel.directive ~label `expanded_bypass [expr] [] in
            let annotmap, expr = QmlAstCons.TypedExpr.shallow_copy annotmap expr in
            (* we will never want to apply type arguments to bypass anyway, so there is no need to
             * put typescheme on them
             * let annotmap = QmlAnnotMap.add_tsc_inst_opt_label label (QmlAnnotMap.find_tsc_opt_label label annotmap) annotmap in*)
            let annotmap = QmlAnnotMap.remove_tsc_label label annotmap in
            annotmap, expr
          else
            annotmap, QmlAstCons.UntypedExpr.directive `expanded_bypass [expr] []
        in

        let _ =
          #<If:BYPASS_HOISTING $minlevel 3>
            debug "eta-expand (skey:%a) : %a" BslKey.pp key QmlPrint.pp_annotation#expr expr
          #<End>
        in

        (gamma, annotmap, env), expr

    | None ->
        if typed then
          (OManager.printf "Error: bypass %a not properly typed (expr has type %a)"
             BslKey.pp key QmlPrint.pp_annotation#expr expr;
           assert false)
        else acc, expr
        (* Do not pretend that the bypass is expanded because its type is unknown *)

  (* This function will be called only in typed mode.
     [QmlAnnotMap.find_ty] raise AnnotNotFound if the type is not in the annot.
  *)
  let get_ty annotmap ann =
    let _ =
      #<If:BYPASS_HOISTING $minlevel 4>
        debug "get_ty § %s" (Annot.to_string ann)
      #<End>
    in
    QmlAnnotMap.find_ty ann annotmap

  let make_ident tsc_opt ident ~typed (gamma, annotmap, env) expr =
    if typed then (
      let label = Q.Label.expr expr in
      let a = QmlAnnotMap.find_label label annotmap in
      let label = Annot.refresh label in
      let annotmap = QmlAnnotMap.add_label label a annotmap in
      let annotmap = QmlAnnotMap.add_tsc_inst_opt_label label tsc_opt annotmap in
      (gamma, annotmap, env), Q.Ident (label,ident)
    ) else (
      (gamma, annotmap, env), QmlAstCons.UntypedExpr.ident ident
    )

  let hoist_factory ~expand bypass_typer key ~typed acc expr =
    let skey = BslKey.to_string key in
    let _ =
      #<If:BYPASS_HOISTING $minlevel 1>
        debug "hoisting (skey:%s) : %a" skey QmlPrint.pp_annotation#expr expr
      #<End>
    in
    let ident = Ident.next skey in
    let (gamma, annotmap, env), expr =
      if expand
      then just_expand bypass_typer key ~typed acc expr
      else acc, expr
    in

    (* create tsc for the gamma, and tsc_opt for ei *)
    let tsc =
      let ty = Option.get (bypass_typer key) in
      let ty =
        try QmlTypes.type_of_type gamma ty
        with QmlTyperException.Exception _ as exn ->
          OManager.i_error "%a@." (QmlTyperErrHandling.pp_report_from_typer_exception gamma annotmap) exn
      in
      QmlTypes.Scheme.generalize gamma ty in
    let tsc_opt =
      match QmlGenericScheme.full_arity tsc with
      | (0,0,0) -> None
      | _ -> Some tsc in

    let gamma, annotmap, env = register_bypass tsc tsc_opt key ident expr gamma annotmap env in
    make_ident tsc_opt ident ~typed (gamma, annotmap, env) expr

  let expand_hoist = hoist_factory ~expand:true
  let just_hoist = hoist_factory ~expand:false
end

let safe_unexpand context expanded =
  try
    QmlAstUtils.Bypass.unexpand expanded
  with
  | Invalid_argument "QmlAstUtils.Bypass.unexpand" ->
      QmlError.cond_violation QmlCheck.Bypass.well_formed_id context
        "Cannot unexpand this expression: %a\n" QmlPrint.pp#expr expanded

let fold_map_expr bypass_typer ~just_expand ~typed =
  QmlAstWalk.Expr.traverse_foldmap
    (fun tra acc (expr as original_expr) ->
       match expr with
       | Q.Directive (_, `may_cps, [subexpr], _)
       | subexpr -> (
       match subexpr with
         (* special case for letin : do not make an alias, but a subst in the code (remove alias) *)
       | Q.LetIn (_, bind, expr') ->
           let filter_map_bind (acc, alias) (id, expr) =
             match expr with
             | Q.Directive (_, `may_cps, [subexpr], _)
             | subexpr -> (
             match subexpr with
             | Q.Directive (_, `expanded_bypass, [expanded], _) ->
                 if just_expand then (acc, alias), Some (id, expr)
                 else
                   (* just hoist, do not redo expand *)
                   let key, _ =
                     let context = QmlError.Context.exprs original_expr [expr] in
                     safe_unexpand context expanded in
                   let acc, expr = FoldMap.just_hoist ~typed bypass_typer key acc expr in
                   let alias = Alias.add id expr alias in
                   (acc, alias), None

             | Q.Bypass (_, key)
             | Q.Directive (_, `restricted_bypass _, [Q.Bypass (_, key)], _) ->
                 if just_expand then
                   let acc, expr = FoldMap.just_expand ~typed bypass_typer key acc expr in
                   (acc, alias), Some (id, expr)
                 else
                   (* expand, hoist, add to unalias request, and remove the node *)
                   let acc, expr = FoldMap.expand_hoist ~typed bypass_typer key acc expr in
                   let alias = Alias.add id expr alias in
                   (acc, alias), None

             (* Other cases :
                typed     : if there are typing directive, they will protect an alias.
                not typed : a purge can help if there are still type directives *)
             | _ -> (acc, alias), Some (id, expr)
               )
           in
           let (acc, alias), bind = Base.List.fold_left_filter_map filter_map_bind (acc, Alias.empty) bind in
           let expr' = Alias.resolve alias expr' in
           let expr = match bind with [] -> expr' | _ -> Q.LetIn (Q.Label.expr expr, bind, expr') in
           tra acc expr

       | Q.Directive (_, `expanded_bypass, [expanded], _) ->
           if just_expand then acc, expr
           else
             let key, _ =
               let context = QmlError.Context.exprs original_expr [expr] in
               safe_unexpand context expanded in
             let _, _, env = acc in
             ( match BslKeyMap.find_opt key env.bypass with
               | Some (ident,tsc_opt) ->
                   (* the key is already hoisted, put the ident instead *)
                   FoldMap.make_ident tsc_opt ident ~typed acc expr
               | None ->
                   (* else, this is our expanded bypass but it should be hoisted *)
                   FoldMap.just_hoist ~typed bypass_typer key acc expr
             )

       | Q.Bypass (_, key)
       | Q.Directive (_, `restricted_bypass _, [Q.Bypass (_, key)], _) ->
           if just_expand then FoldMap.just_expand ~typed bypass_typer key acc expr
           else
             let _, _, env = acc in
             ( match BslKeyMap.find_opt key env.bypass with
               | Some (ident,tsc_opt) ->
                   (* the key is already hoisted, put the ident instead *)
                   FoldMap.make_ident tsc_opt ident ~typed acc expr
               | None ->
                   (* else, it must be expanded and hoisted *)
                   FoldMap.expand_hoist ~typed bypass_typer key acc expr
             )
       | _ -> tra acc expr
    ))

let rec remove_slicer_annotations acc annotmap = function
  | Q.Directive (label, (#Q.slicer_directive as d), [e], tyl) ->
      let tsc_opt = QmlAnnotMap.find_tsc_opt_label label annotmap in
      let annotmap = QmlAnnotMap.add_tsc_opt (Q.QAnnot.expr e) tsc_opt annotmap in
      remove_slicer_annotations ((label, d, tyl) :: acc) annotmap e
  | e -> acc, annotmap, e
let rec put_back_slicer_annotations acc annotmap e =
  match acc with
  | [] -> annotmap, e
  | (label, d, tyl) :: acc ->
      let tsc_opt = QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr e) annotmap in
      let annotmap = QmlAnnotMap.add_tsc_opt_label label tsc_opt annotmap in
      put_back_slicer_annotations acc annotmap (Q.Directive (label, d, [e], tyl))

let process_code ~just_expand ~typed bypass_typer gamma annotmap code =
  let env = empty_env () in
  let acc = gamma, annotmap, env in
  let (gamma, annotmap, env), code =
    let fold_map_elt acc = function
      | (Q.NewVal (label, bind)) as code_elt ->
          (* This is not the function fold_map_expr because it is at toplevel a little bit different*)
          let fold_map_bind (gamma, annotmap, env) (id, expr) =
            let acc, annotmap, expr = remove_slicer_annotations [] annotmap expr in
            let (gamma, annotmap, env), (id, expr) =
              match expr with
              | Q.Directive (_, `may_cps, [subexpr], _)
              | subexpr -> (
                  match subexpr with
                  | Q.Directive (_, `expanded_bypass, [expanded], _) ->
                      let key, _ =
                        let context =
                          let c1 = QmlError.Context.code_elt code_elt in
                          let c2 = QmlError.Context.expr expr in
                          QmlError.Context.merge2 c1 c2
                        in
                        safe_unexpand context expanded in
                      ( match BslKeyMap.find_opt key env.bypass with
                        | Some (ident,tsc_opt) ->
                            (* the key is alread hoisted, make an alias *)
                            (* at toplevel, we does not remove this alias *)
                            let acc, expr = FoldMap.make_ident tsc_opt ident ~typed (gamma, annotmap, env) expr in
                            acc, (id, expr)

                        | None ->
                            (* else, this is our expanded bypass, do not change anything *)
                            (* inside, this would be hoisted but we are there alread at toplevel *)
                            let tsc_opt = QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr expr) annotmap in
                            let env = { env with bypass = BslKeyMap.add key (id,tsc_opt) env.bypass } in
                            (gamma, annotmap, env), (id, expr)
                      )
                  | Q.Bypass (_, key)
                  | Q.Directive (_, `restricted_bypass _, [Q.Bypass (_, key)], _) -> (
                      match BslKeyMap.find_opt key env.bypass with
                      | Some (ident,tsc_opt) ->
                          (* the key is already hoisted, make an alias *)
                          (* at toplevel, we do not remove this alias *)
                          let acc, expr = FoldMap.make_ident tsc_opt ident ~typed (gamma, annotmap, env) expr in
                          acc, (id, expr)

                      | None ->
                          (* else, it must be expanded, but not hoisted (we are already at toplevel) *)
                          let tsc_opt = QmlAnnotMap.find_tsc_opt (Q.QAnnot.expr expr) annotmap in
                          let (gamma, annotmap, env), expr = FoldMap.just_expand bypass_typer key ~typed (gamma, annotmap, env) expr in
                          let env = { env with bypass = BslKeyMap.add key (id,tsc_opt) env.bypass } in
                          (gamma, annotmap, env), (id, expr)
                    )

                  (* Other cases : regular transformation *)
                  | _ ->
                      let acc, expr = fold_map_expr bypass_typer ~just_expand ~typed (gamma, annotmap, env) expr in
                      acc, (id, expr)
                ) in

            let annotmap, expr = put_back_slicer_annotations acc annotmap expr in
            (gamma, annotmap, env), (id, expr)

          in
          let acc, bind = Base.List.fold_left_map fold_map_bind acc bind in
          acc, QmlAst.NewVal (label, bind)

      | elt ->
          QmlAstWalk.Top.fold_map_expr (fold_map_expr bypass_typer ~just_expand ~typed) acc elt
    in
    Base.List.fold_left_map fold_map_elt acc code in
  (gamma, annotmap), (add_defs env code)
