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
open QmlAst
open QmlAstCons


(* shorthands *)
module Q = QmlAst
module QC = QmlAstCons
module S = QmlSimpleSlicer
module SM = Pass_SimplifyMagic



let default_pos = QC.UntypedExpr.source


let get name = fun ?(posexpr = default_pos name) annotmap gamma ->
  try
    let ident = OpaMapToIdent.val_ name in
    let (ty : QmlAst.ty) =
      QmlTypes.Scheme.instantiate (QmlTypes.Env.Ident.find ident gamma) in
    let (annotmap, e) = TypedExpr.ident annotmap ident ty in
    let annotmap =
      QmlAnnotMap.add_tsc_inst (QmlAst.QAnnot.expr e)
        (QmlTypes.Env.Ident.find ident gamma) annotmap in
    annotmap, e
  with Not_found ->
    let context = QmlError.Context.annoted_expr annotmap posexpr in
    QmlError.cond_violation QmlAlphaConv.Check.unbound_id context
      "Missing ident"

(** Returns the magic add function. *)
let magic_add ~annotmap ~stdlib d =
  get
    (match d with
     | `stringifier -> Opacapi.OpaValue.add_to_string
     | `comparator -> Opacapi.OpaValue.add_compare
     | `serializer -> Opacapi.OpaValue.add_serializer
     | `xmlizer -> Opacapi.OpaValue.add_xmlizer
    )
    annotmap stdlib

let add_to_magic ~stdlib ~gamma ~annotmap n e d =
  let annotmap, add = magic_add ~annotmap ~stdlib d in
  let i = ExprIdent.next "magic_add" in
  let annotmap, e = QC.TypedExpr.apply gamma annotmap add [n; e] in
  let annotmap, e = QC.TypedExpr.directive annotmap (`side_annotation `both_implem) [e] [] in
  (annotmap, (i, e))

let add_to_specialize_env ~specialize_env ty expr d =
  match d with
  | `serializer | `xmlizer -> (*No specialize*) specialize_env
  | `comparator | `stringifier as d ->
      let specialized_ident = match d with
        | `comparator -> OpaMapToIdent.val_ "compare"
        | `stringifier -> OpaMapToIdent.val_ Opacapi.magicToString in
      IdentMap.update_default specialized_ident
        (fun l -> { l with SM.
                      specialize = (ty, expr) :: l.SM.specialize ;
                  })
        { SM.
            strict = false ;
            specialize = [(ty, expr)] ;
        } specialize_env

let process_expr ~specialize_env ~stdlib ~gamma ~annotmap ((i, e) as cpl) =
  let check expr =
    QmlAstWalk.Expr.iter_down
      (fun e -> match e with
       | Q.Directive (_, #Q.opavalue_directive, _, _) ->
           let context = QmlError.Context.annoted_expr annotmap e in
           QmlError.serror
             context "OpaValue directives are only allowed at toplevel"
       | _ -> ()) expr in
  let rec aux e =
    match e with
    | Q.Directive (label, (#Q.type_directive | #Q.slicer_directive as v), [expr], ty) ->
        let a, b, c, expr = aux expr in
        let e = Q.Directive (label, v, [expr], ty) in
        a, b, c, e
    | Q.Directive (_, (#Q.opavalue_directive as d), [expr], [Q.TypeName (args, name)]) ->
        check expr ;
        let ty = QmlAnnotMap.find_ty (QmlAst.QAnnot.expr expr) annotmap in
        let (annotmap, expri) = TypedExpr.ident annotmap i ty in
        let (annotmap, name) =
          TypedExpr.string annotmap (QmlAst.TypeIdent.to_string name) in
        let (annotmap, to_add) =
          add_to_magic ~stdlib ~gamma ~annotmap name expri d in
        let specialize_env =
          match args with
          | [] -> add_to_specialize_env ~specialize_env ty expri d
          | _ -> specialize_env (*specialize doesn't support polymorphic for moment *) in
        (specialize_env, annotmap, [to_add], expr)
    | Q.Directive (_, #Q.opavalue_directive, _, _) -> assert false
    | _ ->
        check e ;
        raise Exit in
  try
    let a, b, c, e = aux e in
    a, b, c, (i, e)
  with Exit ->
    (specialize_env, annotmap, [], cpl)

let process_list ~specialize_env ~stdlib ~gamma ~annotmap vlist =
  let specialize_env, annotmap, to_add, rvlist =
    List.fold_left
      (fun (specialize_env, annotmap, acc, rvlist) (i, expr) ->
         let specialize_env, annotmap, to_add, exprs =
           process_expr
             ~specialize_env ~stdlib ~gamma ~annotmap (i, expr) in
         (specialize_env, annotmap, (to_add @ acc), (exprs :: rvlist)))
      (specialize_env, annotmap, [], []) vlist in
  (specialize_env, annotmap, to_add, List.rev rvlist)

let process_code ~stdlib ~gamma ~annotmap code =
  let mklabel () = Annot.nolabel "enrich_magic" in
  let perform ~specialize_env ~annotmap construct (label, vlist) toplvl =
    let (specialize_env, annotmap, to_add, vlist) =
      process_list
        ~specialize_env ~stdlib ~gamma ~annotmap vlist in
    let elt = construct (label, vlist) in
    (match to_add with
     | [] -> (specialize_env, annotmap, (elt :: toplvl))
     | _ ->
         let eltadd = Q.NewVal (mklabel (), to_add) in
         (specialize_env, annotmap, (eltadd :: elt :: toplvl)))
  in
  let specialize_env, annotmap, toplvl =
    List.fold_left
      (fun (specialize_env, annotmap, toplvl) -> function
         | Q.NewValRec (l, v) ->
             perform
               ~specialize_env ~annotmap (fun (l, v) -> Q.NewValRec (l, v))
               (l,v) toplvl
         | Q.NewVal (l, v) ->
             perform
               ~specialize_env ~annotmap (fun (l, v) -> Q.NewVal (l, v))
               (l, v) toplvl
         | elt -> (specialize_env, annotmap, (elt :: toplvl)))
      (IdentMap.empty, annotmap, []) code in
  (specialize_env, annotmap, (List.rev toplvl))

let just_purge code =
  List.map
    (QmlAstWalk.Top.map_expr
       (function
          | Q.Directive (_, #Q.opavalue_directive, [expr], _) -> expr
          | Q.Directive (_, #Q.opavalue_directive, _, _) as e ->
              let context = QmlError.Context.expr e in
              QmlError.serror context "Unexpected form for an opavalue directive.";
              e
          | e -> e)
    ) code
