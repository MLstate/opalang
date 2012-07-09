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
(* depends *)
module List = Base.List
let (|>) x f = f x

(* alias *)
module Q = QmlAst

(* refactoring in progress *)

(*
  Utils
*)
let wclass = WarningClass.pattern

let dead =
  let doc = "A branch of a match can never be matched" in
  WarningClass.create ~parent:wclass ~name:"dead" ~doc ~err:true ~enable:true ()

let obfuscation =
  let doc = "A variable matching an expr of a sum type is nammed like one of the sum case" in
  WarningClass.create ~parent:wclass ~name:"obfuscation" ~doc ~err:true ~enable:true ()

let warning_set =
  WarningClass.Set.create_from_list [
    wclass;
  ]

let is_label_in_simple_case searched_label annotkey annotmap gamma =
  match QmlAnnotMap.find_ty_opt annotkey annotmap with
  | None -> false
  | Some ty -> (
      (* We are only interrested in sum types. But in case of names type, we
         must first get its effective representation. *)
      match QmlTypesUtils.Inspect.follow_alias_noopt gamma ty with
      | QmlAst.TypeSum (QmlAst.TyCol (cases, _)) ->
          List.exists
            (fun case_fields ->
               (* For each case of the sum, i.e. each list of fields of this
                  case, we check if the searched label belongs to list of
                  fields of this case. We stop searching as soon as we find a
                  positive hit. *)
               List.exists
                 (fun (lbl, lbl_ty) ->
                    (* Found if the labels have the same name and the type of
                       this label is the empty record. Attention, we must
                       unwind named types before testing. *)
                    (lbl = searched_label) &&
                    (match QmlTypesUtils.Inspect.follow_alias_noopt gamma lbl_ty with
                     | QmlAst.TypeRecord (QmlAst.TyRow ([], None)) -> true
                     | _ -> false))
                 case_fields)
            cases
      | _ -> false
    )

(*
  Imperative simplification:
  [gamma] and [annotmap] are available as global variables.
  It is simplier to proced so, instead of passing [gamma] and [annotmap] around.
  The [free_gamma_annotmap] function is for relaxing the GC.
*)
let gamma = ref ( QmlTypes.Env.empty : QmlTypes.gamma )
let annotmap = ref ( QmlAnnotMap.empty : QmlAst.annotmap )
let set_gamma g = gamma := g
let set_annotmap a = annotmap := a
let free_gamma_annotmap () =
  gamma := QmlTypes.Env.empty ;
  annotmap := QmlAnnotMap.empty

(*
  This function tells if a pattern [p] hides a another pattern [p'].
  For example, in :
  {[
  match e with
  | _ -> "toto"
  | 6 -> "tutu"
  ]}
  The pattern [_] hids the pattern [6]

  The question answered by this function is :
  Does p kills (hides) p' ?

  <!> beware with rowvar,
  {[
  | { a }
  | { a ; ... }
  ]}
  The second case in general is not killed by the first case. (depending on the type).
  The check is syntactic.
*)
let rec is_killed_by p p' =
  match p, p' with
  | Q.PatCoerce (_, p, _), _ -> is_killed_by p p'
  | _, Q.PatCoerce(_, p', _) -> is_killed_by p p'

  | (Q.PatAny _ | Q.PatVar _), _ -> true

  | Q.PatRecord _, Q.PatRecord _ -> all_fields_killed_by p p'
  | Q.PatConst (_, c0), Q.PatConst (_, c1) -> c0 = c1

  | _ -> false

(* This function is called whith complexe patterns, i.e with fields *)
and all_fields_killed_by p p' =
  (*
    gather all field of a pattern, and return an extra bool,
    [true] if the pattern is strict (without  { ; ...} )
  *)
  let fields_list p =
    match p with
    | Q.PatRecord (_, fields, rowvar) -> fields, rowvar = `closed
    | _ ->
        let context = QmlError.Context.annoted_pat !annotmap p in
        QmlError.i_error None context (
          "This pattern is not well formed.@\n%a"
        )
          QmlPrint.pp#pat p
  in
  let l1, strict1 = fields_list p in
  let l2, strict2 = fields_list p' in

  if strict1
  then
    if strict2
    then
      (*
        All field of p' should be in p, and all should be killed.
        The two list should have the same length.
      *)
      let cmp (a, _) (b, _) = String.compare a b in
      let l1 = List.sort cmp l1 in
      let l2 = List.sort cmp l2 in
      Return.set_checkpoint (
        fun label ->
          let iter2 (a, p) (b, p') =
            if a <> b || not (is_killed_by p p')
            then Return.return label false
          in
          try
            List.iter2 iter2 l1 l2 ;
            true
          with
          | Invalid_argument _ -> false
      )
    else
      (*
        Syntactic check only.
        No matter what fields are in p', if p is strict, and p' not,
        the second pattern cover more cases than the first, so
        is not hidden.
      *)
      false
  else
    (*
      In this case, no matter the row variable of p', if all field p
      are also present in p', and killed by p, the pattern is killed.
      The fields present in p' but not in p would be matched inside
      the row var of p.
    *)
    List.for_all (
      fun (n, p)->
        match List.assoc_opt n l2 with
        | None -> false
        | Some p' -> is_killed_by p p'
    ) l1

(*
  Given a pattern [p] and a list of patterns [li] return the filtered list of [li],
  containing only the pattern hidden by [p]
*)
let killed_patterns p li =
  List.filter (fun (p', _) -> is_killed_by p p') li

(*
  Given an ordered list of patterns, will return the assoc list of type : [(pat * pat list) list]
  if [(p, li)] is in this list, that means that in the pattern matching, all pattern of [li]
  are hidden by [p]
*)
let collect_killed_patterns li =
  let rec aux acc = function
    | [] -> List.rev acc
    | (hd, _) :: tl ->
        let killed_patterns = killed_patterns hd tl in
        let acc =
          if killed_patterns <> [] then (hd, killed_patterns)::acc else acc
        in
        aux acc tl
  in
  aux [] li

(*
  Given an expression, check.
  This is meant to be used with Traverse functions,
  that's why the function is not recursive.
*)
let check_expr e =
  match e with
  | Q.Match (_, _, li) ->
      (* First check: dead patterns *)
      let iter (p, li) =
        let iter (p', _) =
          let c1 = QmlError.Context.annotmap !annotmap in
          let c2 = QmlError.Context.pat p in
          let c3 = QmlError.Context.pat p' in
          let context = QmlError.Context.merge c1 [c2 ; c3] in
          QmlError.warning ~wclass:dead context (
            "@[<2>This kind of pattern matching is not allowed@\n"^^
            "The first pattern hides the second one.@]"
          )
        in
        List.iter iter li
      in
      List.iter iter (collect_killed_patterns li)
      ;
      (* Second check: obfuscation *)
      let iter (p, _) =
        let iter p =
          match p with
          | Q.PatVar (_, ident) | Q.PatAs (_, _, ident) ->
              let label = Ident.original_name ident in
              if is_label_in_simple_case label (Q.QAnnot.pat p) !annotmap !gamma
              then
                let context = QmlError.Context.annoted_pat !annotmap p in
                QmlError.warning ~wclass:obfuscation context (
                  "You should not name this pattern variable @{<bright>%s@} because@\n"^^
                  "the type of the matched expression contain a sum case @{<bright>{ %s }@}."
                )
                  label label
          | _ -> ()
        in
        QmlAstWalk.Pattern.iter_down iter p
      in
      let doit = match li with | _::_::_ -> true | _ -> false in
      if doit then
        List.iter iter li
  | _ -> ()

(*
  The function returns unit.
  In case of illicit pattern, fail using QmlError with located error messages.
*)
let process_code gamma annotmap code =
  if WarningClass.is_warn wclass then (
    set_gamma gamma;
    set_annotmap annotmap;
    QmlAstWalk.CodeExpr.iter (QmlAstWalk.Expr.iter check_expr) code;
    free_gamma_annotmap ()
  )

(*
  New pass of analysis. Testing
*)
let process_code gamma annotmap code =
  process_code gamma annotmap code ;
  if WarningClass.is_warn wclass then (
  let foldmap annotmap = function
    | Q.Match (label, matched_expr, patterns) as expr ->
        let pattern_matching =
          QmlPatternAnalysis.conversion ~gamma ~annotmap ~label ~matched_expr ~patterns
        in
        let normalized = QmlPatternAnalysis.normalize pattern_matching in
        let () =
          if QmlPatternAnalysis.has_public_exceptions () then (
            let ctx = QmlError.Context.label label in
            QmlError.warning ~wclass:wclass ctx "%a"
              QmlPatternAnalysis.flush_exceptions_fmt ()
        ) in
        let acc =
          #<If:PATTERNS_NORMALIZE>
            QmlPatternAnalysis.generation normalized
          #<Else>
            ignore normalized ;
            annotmap, expr
          #<End>
        in
        acc
    | expr ->
        annotmap, expr
  in
  let norm_annotmap, norm_code =
    QmlAstWalk.CodeExpr.fold_map (QmlAstWalk.Expr.foldmap foldmap) annotmap code
  in
  let () = QmlPatternAnalysis.Env.reset () in
  #<If:PATTERNS_NORMALIZE>
    norm_annotmap, norm_code
  #<Else> (
    ignore norm_annotmap ;
    ignore norm_code ;
    annotmap, code
  )
  #<End>
  ) else
    annotmap, code
