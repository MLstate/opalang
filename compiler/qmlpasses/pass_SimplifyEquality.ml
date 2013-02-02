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
module Q = QmlAst
module List = Base.List

type env = Q.pat IdentMap.t
type ignored_directive = [ Q.type_directive | Q.slicer_directive ]

module S =
struct
  type t = (QmlAst.annotmap * env)
  let pass = "pass_SimplifyEquality"
  let pp f (_,map) =
    IdentMap.iter
      (fun k v ->
         Format.fprintf f "@[<2>%s -> %a@]@\n" (Ident.to_string k) QmlPrint.pp#pat v
      ) map
end

module R =
struct
  include ObjectFiles.Make(S)
  let save ~loaded_env ~env ~annotmap =
    let diff = IdentMap.diff env loaded_env in
    let small_annotmap = QmlRefresh.restrict_annotmap_fold_pat
      (fun r acc map ->
         IdentMap.fold
           (fun _ pat acc ->
              r acc pat
           ) map acc
      ) annotmap diff in
    save (small_annotmap, diff)
  let load annotmap =
    fold_with_name
      (fun package (annotmap,env) (annotmap_old,map) ->
         let annotmap_old = QmlRefresh.refresh_annotmap package annotmap_old in
         IdentMap.fold
           (fun k pat (annotmap,env) ->
              let annotmap, pat = QmlRefresh.refresh_pat package ~annotmap_old annotmap pat in
              annotmap, IdentMap.add k pat env
           ) map (annotmap,env)
      ) (annotmap,IdentMap.empty)
end

(* FIXME: when the pattern ast is better, we won't need the annotmap here *)
let rec pattern_of_expr env annotmap = function
  | Q.Ident (_, i) -> (
      let previous =
        try
          IdentMap.find i env
        with
        | Not_found -> raise Exit
      in
      QmlAstCons.TypedPat.copy annotmap previous
    )
  | Q.Const (label, const_expr) ->
      (* not rewriting a == 0 into match a with 0 -> ...*)
      annotmap, Q.PatConst (label, const_expr)
  | Q.Coerce (_, e, _)
  | Q.Directive (_, #ignored_directive, [e], _) ->
      pattern_of_expr env annotmap e
  | Q.Record (_label, sel) ->
      let annotmap, spl = List.fold_left_map
        (fun annotmap (s, e) ->
           let annotmap, p = pattern_of_expr env annotmap e in
           annotmap, (s, p)) annotmap sel in
      QmlAstCons.TypedPat.record annotmap spl
  | _ -> raise Exit
let pattern_of_expr env annotmap e = pattern_of_expr env annotmap e

let rec is_patemptyrecord = function
  | Q.PatCoerce (_, e, _) -> is_patemptyrecord e
  | Q.PatRecord (_, [], _) -> true
  | _ -> false
let rec is_simple_patrecord = function
  | Q.PatCoerce (_, e, _) -> is_simple_patrecord e
  | Q.PatRecord (_, [field_name, field_value], `closed) ->
      if is_patemptyrecord field_value then Some field_name else None
  | _ -> None
let rec get_name = function
  | Q.Coerce (_, e, _)
  | Q.Directive (_, #ignored_directive, [e], _) -> get_name e
  | Q.Ident (_, i) -> Some i
  | _ -> None

let generate_match ?e1 ?e2 gamma annotmap kind expr pat =
  let annotmap, any = QmlAstCons.TypedPat.any annotmap in
  let annotmap, e1 =
    match e1 with
    | None -> QmlAstCons.TypedExpr._true (annotmap,gamma)
    | Some e1 -> annotmap, e1 in
  let annotmap, e2 =
    match e2 with
    | None -> QmlAstCons.TypedExpr._false (annotmap,gamma)
    | Some e2 -> annotmap, e2 in
  let e1, e2 =
    match kind with
    | `equality -> e1, e2
    | `inequality -> e2, e1 in
  QmlAstCons.TypedExpr.match_ annotmap expr [pat, e1; any, e2]

let rec match_equality_to_record env annotmap equality_ident inequality_ident = function
  | Q.Coerce (_, e, _)
  | Q.Directive (_, #Q.type_directive, [e], _) -> match_equality_to_record env annotmap equality_ident inequality_ident e
  | Q.Apply (_, Q.Ident (_, i), [e1;e2]) ->
      let ident =
        if Ident.equal i equality_ident then
          Some `equality
        else if Ident.equal i inequality_ident then
          Some `inequality
        else
          None in (
      match ident with
      | Some kind -> (
          try
            let annotmap, p = pattern_of_expr env annotmap e1 in
            Some (annotmap, kind, e2, p)
          with Exit ->
            try
              let annotmap, p = pattern_of_expr env annotmap e2 in
              Some (annotmap, kind, e1, p)
            with Exit ->
              None
        )
      | None -> None
      )
  | _ -> None

let update_env env annotmap iel =
  List.fold_left
    (fun (env, annotmap) (i, e) ->
       try
         let annotmap, p = pattern_of_expr env annotmap e in
         IdentMap.add i p env, annotmap
       with Exit ->
         match get_name e with
         | Some j -> (
             try (IdentMap.add i (IdentMap.find j env) env, annotmap)
             with Not_found -> (env, annotmap)
           )
         | None ->
             (env, annotmap)
    ) (env, annotmap) iel

let rewrite_equality_expr equality_ident inequality_ident gamma acc e =
  let aux self tra env acc e =
    match e with
    | Q.LetIn (_, iel, _)
    | Q.LetRecIn (_, iel, _) ->
         let env, acc = update_env env acc iel in
         tra env acc e
    | Q.Match (_, e0, [p1,e1; p2,e2]) ->
        (match is_simple_patrecord p1, is_simple_patrecord p2 with
         | Some "true", Some "false" ->
             (match match_equality_to_record env acc equality_ident inequality_ident e0 with
              | None -> tra env acc e
              | Some (acc, kind, e, p) ->
                  let acc, e = generate_match gamma ~e1 ~e2 acc kind e p in
                  self env acc e
             )
         | _ -> tra env acc e)
    | _ ->
        match match_equality_to_record env acc equality_ident inequality_ident e with
        | None -> tra env acc e
        | Some (acc, kind, e, p) ->
            let acc, e = generate_match gamma acc kind e p in
            self env acc e
  in
  QmlAstWalk.Expr.self_traverse_foldmap_context_down aux acc e

let rewrite_equality equality_ident inequality_ident env gamma annotmap code =
  List.fold_left_map
    (fun (env,acc) -> function
     | Q.NewVal (label,iel)
     | Q.NewValRec (label,iel) as c ->
         let env, acc = update_env env acc iel in
         let acc, iel =
           List.fold_left_map
             (fun acc (i,e) ->
                let acc, e = rewrite_equality_expr equality_ident inequality_ident gamma env acc e in
                acc, (i,e)) acc iel in
         (match c with
          | Q.NewVal _ -> (env, acc), Q.NewVal (label, iel)
          | Q.NewValRec _ -> (env, acc), Q.NewValRec (label, iel)
          | _ -> assert false)
     | c -> (env, acc), c) (env,annotmap) code

let process_code val_ gamma annotmap code =
  #<If:SIMPLIFYMAGIC_DISABLE>
    annotmap, code
  #<Else>
    try
      let annotmap, loaded_env = R.load annotmap in
      let equality_ident = val_ Opacapi.(==) in
      let inequality_ident = val_ Opacapi.(!=) in
      let (env, annotmap), code = rewrite_equality equality_ident inequality_ident loaded_env gamma annotmap code in
      R.save ~loaded_env ~env ~annotmap;
      annotmap, code
    with
    | Not_found (* no equality *)
    | QmlTyperException.Exception _ (* no boolean type *) ->
        R.save ~loaded_env:IdentMap.empty ~env:IdentMap.empty ~annotmap;
        annotmap, code
  #<End>
