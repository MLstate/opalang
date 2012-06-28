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
  TODO:
  We can take advantage of the analysis for adding new warnings
  about unused patterns.
  The problem to do this there, is that it will be only on the
  client side, and the same optimisation are possible on the server
  side. Maybe after a working prototype, we can try to extract
  the analysis part, to be able to use it on the server too.
  Possible solution:
  1) Functorization
  2) Parametrized libraries
*)

(* depends *)
module Format = Base.Format
module List = Base.List

(* alias *)
module Common = Imp_Common
module FieldSet = StringSet
module FieldMap = StringMap
module PatternAnalysis = Imp_PatternAnalysis
module SumAnalysis = Imp_SumCase.SumAnalysis
module SumCondition = Imp_SumCase.SumCondition
module SumEnv = Imp_SumCase.SumEnv

(* shorthands *)
module E = Imp_Env
module J = JsAst
module P = Imp_PatternAnalysis
module Q = QmlAst
module SC = SumCondition

(* -- *)

(* type alias *)

type env = Imp_Env.env
type penv = Imp_Env.private_env
type condition = Imp_SumCase.SumCondition.t
type decision = Imp_SumCase.SumCondition.decision
type check = Imp_SumCase.SumCondition.check
type path = Imp_SumCase.path
type rev_path = Imp_SumCase.rev_path
type guard = JsAst.expr
type rev_guards = guard list
type binding = JsAst.ident * JsAst.expr
type bindings = binding list
type pat = Imp_PatternAnalysis.pat
type 'rh rev_cases = (guard option * bindings * 'rh) list

module PathMap = BaseMap.Make ( Order.StringList )

type analysed_bool_pattern = [
  | `onlybool of bool
      (*
        The value is statically known as being a bool value, and every other
        matched expression is also necessary a bool at runtime.
        In that case, we compile the pattern as native javascript test.
        In case of an introduction, we always bind [js_void].
      *)

  | `partialbool of bool
      (*
        We know statically that the pattern matches a bool value, but we cannot
        assume that every matched value will be a bool at runtime, e.g open colvar.
        In this case, we cannot compile the test as a javascript native test, because
        every not null object will pass a native test:
        {[
        if (e) {
          // pass if e is a javascript object, even if (e !== [true])
        }
        ]}
        We compile the guard as a strict equality with the boolean [true].
        In case of an introduction, we always bind [js_void].
      *)

  | `maybebool of bool
      (*
        We do not know at compile time if the runtime value will be a boolean or not.
        If we end-up in this case, that means that either a pass as lost some type annotation,
        or the user has redefined some exotic types dealing with fields ["true"] or ["false"],
        like:
        {[
        type optionbool('a) = { false } / { true : 'a}
        ]}
        In this case, we will use special introspection accessors ClientLib.[dot_$bool].
        In case of an introduction, we bind the value returned by the introspection accessor.
      *)
]

let pp_analysed_bool fmt = function
  | `onlybool b -> Format.fprintf fmt "onlybool:%b" b
  | `partialbool b -> Format.fprintf fmt "partialbool:%b" b
  | `maybebool b -> Format.fprintf fmt "maybebool:%b" b

type special_bool_hack = analysed_bool_pattern


(*
  Priorities used for the patvar heuristic
*)
module Priority =
struct
  let shared_fields = 1
  let shared_var = 0
  let shared_const = 1
  let fields = 3
  let var = 2
  let const = 3
end

module Impl :
sig
  type 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : rev_path:FieldSet.elt list -> 'a -> 'a t -> 'a t
  val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
  val list : 'a t -> 'a list
end =
struct
  (*
    A type for aggregating implications.
    When 2 implications with diverging path are added to the structure,
    the structures becomes closed, and can never gets implications again.
    This is so because we do not handle negation of conjonction currently.
  *)
  type 'a t = (FieldSet.elt * 'a list) list option

  let empty = Some []

  let is_empty = function
    | Some pathmap -> List.is_empty pathmap
    | None -> true

  let add ~rev_path a = function
    | None -> None
    | Some map ->
        Return.set_checkpoint_none (fun label ->
          (*
            Not tail rec, but used with very small list (depth of pattern imbrication level)
          *)
          let rec aux path map =
            match path with
            | [] ->
                (*
                  Internal error, a dummy first element is always added to the path.
                *)
                assert false
            | [ hd ] -> (
                match map with
                | [] ->
                    [ hd, [ a ] ]
                | (hd2, list) :: tl ->
                    if FieldSet.compare_elt hd hd2 <> 0
                    then
                      Return.return label ()
                    else
                      (hd, a::list) :: tl
              )
            | hd :: tl -> (
                match map with
                | [] ->
                    (hd, []) :: aux tl map
                | ((hd2, _) as top) :: map ->
                    if FieldSet.compare_elt hd hd2 <> 0
                    then
                      Return.return label ()
                    else
                      top :: aux tl map
              )
          in
          aux (""::(List.rev rev_path)) map
        )

  let fold_right fold t acc =
    match t with
    | None -> acc
    | Some map ->
        List.fold_left (
          fun acc (_, list) ->
            List.fold_left (fun acc impl -> fold impl acc) acc list
        ) acc map

  let list = function
    | None -> []
    | Some map -> List.rev_concat_map snd map
end

type menv = {
  sum_env : JsAst.ident SumEnv.t ;
  (*
    The environment where already produced bindings are stored.
  *)

  sum : SumAnalysis.sum ;
  (*
    The abstract representation of the sum we are matching in this pattern matching.
  *)

  condition : SumCondition.t ;
  (*
     The union of all assertion we know verified statically, growing as long as
     we traverse patterns.
  *)

  shared_dot : int FieldMap.t PathMap.t ;

}

(* ========================================================================== *)

let aux_const expr const =
  let const = Common.const const in
  JsCons.Expr.strict_equality expr const

let aux_size expr size =
  let size = JsCons.Expr.int size in
  let object_size = JsCons.Expr.call ~pure:true Common.ClientLib.size [ expr ] in
  JsCons.Expr.strict_equality object_size size

let aux_build_guard ~env ~menv ~rev_path ~assign ~ident ~check ~special_bool_hack =
  match special_bool_hack with
  | Some (`onlybool present)->
      if present
      then
        assign
      else
        JsCons.Expr.not_ assign

  | Some (`partialbool present)->
      JsCons.Expr.strict_equality assign (JsCons.Expr.bool present)

  | _ ->
  let maybe_js_false =
    let gamma = env.E.gamma in
    let ty = SumAnalysis.ty gamma ~rev_path menv.sum in
    Common.maybe_js_false gamma ty
  in
  if maybe_js_false
  then
    let null = JsCons.Expr.null () in
    match ident with
    | Some ident ->
        let ident = JsCons.Expr.ident ident in
        let check =
          match check with
          | SC.Field present ->
              if present
              then
                JsCons.Expr.neq ident null
              else
                JsCons.Expr.equality ident null
          | SC.Const const ->
              aux_const ident const
          | SC.Size size ->
              aux_size ident size
        in
        JsCons.Expr.comma [ assign ] check
    | None -> (
        match check with
        | SC.Field present ->
            if present
            then
              JsCons.Expr.neq assign null
            else
              JsCons.Expr.equality assign null
        | SC.Const const ->
            aux_const assign const
        | SC.Size size ->
            aux_size assign size
      )
  else
    match check with
    | SC.Field present ->
        if present
        then
          assign
        else
          JsCons.Expr.not_ assign
    | SC.Const const ->
        aux_const assign const
    | SC.Size size ->
        aux_size assign size

(* ========================================================================== *)

let build_guard
    ~env ~penv ~menv ~minimal_condition ~rev_guards ~matched
    ~check ~special_bool_hack ~rev_path ~path =
  let rec aux ~penv ~menv ~minimal_condition ~rev_guards ~matched ~rev_comma ~rev_path ~path =
    match path with
    | [] ->
        let rev_guards =
          let ident = None in
          let assign = matched in
          let guard = aux_build_guard ~env ~menv ~rev_path ~assign ~ident ~check ~special_bool_hack in
          (*
            pending comma
          *)
          let guard = JsCons.Expr.comma (List.rev rev_comma) guard in
          guard :: rev_guards
        in
        (*
          We should update the conditions for everything which come after this dot
        *)
        let decision =
          match special_bool_hack with
          | Some (`onlybool bool) | Some (`partialbool bool) ->
              SC.Check (SC.Field true, (if bool then "true" else "false") :: rev_path)
          | _ ->
              SC.Check (check, rev_path)
        in
        let minimal_condition = SumCondition.add decision minimal_condition in
        let menv =
          (*
            With the maximal condition
          *)
          let condition = SumCondition.add decision menv.condition in
          { menv with
              condition ;
          }
        in
        penv, menv, minimal_condition, rev_guards

    | field :: path -> (
        (*
          Update the rev_path
        *)
        let rev_path = field :: rev_path in
        (*
          Perform the dot
        *)
        let penv, var = E.next penv field in
        let assign =
          let dot =
            match path, special_bool_hack with
            | [], Some (`maybebool bool)-> (
                match bool, field with
                | true, "true" ->
                    JsCons.Expr.call ~pure:true Common.ClientLib.dot_true [ matched ]
                | false, "false" ->
                    JsCons.Expr.call ~pure:true Common.ClientLib.dot_false [ matched ]
                | _ ->
                    (*
                      Internal error between path, maybebool
                    *)
                    OManager.printf "bool:%b -- field:%S@." bool field ;
                    assert false
              )
            | _ ->
                JsCons.Expr.dot matched field
          in
          JsCons.Expr.assign_ident var dot
        in
        (*
          add the dot in the sharing environment
        *)
        let sum_env = SumEnv.add_dot ~rev_path minimal_condition var menv.sum_env in
        (*
          This is different from last case, because all test about path are present checks,
          even in a absent final check. (for guessing if [x.a.b.c] is absent, we should be sure that
          [x.a.b] is present in the first place).
        *)
        let check, decision =
          if path = []
          then check, (
            match special_bool_hack with
            | Some (`onlybool bool) | Some (`partialbool bool) ->
                SC.Check (SC.Field true, (if bool then "true" else "false") :: rev_path)
            | _ ->
                SC.Check (check, rev_path)
          )
          else
            let check = SC.Field true in
            check, SC.Check (check, rev_path)
        in
        (*
          Enrich the comma, and guards
        *)
        let rev_comma, rev_guards =
          (*
            OPTIM: There this is possible that partial dot are implied by the maximial_condition,
            without available variables for avoiding the assignement.
            In this case, we should use a simple comma instead of a land
          *)
          let is_implied = SumCondition.implies_decision menv.condition decision in
          if is_implied
          then
            let rev_comma = assign :: rev_comma in
            rev_comma, rev_guards
          else
            let rev_guards =
              let ident = Some var in
              let guard = aux_build_guard ~env ~menv ~rev_path ~assign ~ident ~check ~special_bool_hack in
              (*
                pending comma
              *)
              let guard = JsCons.Expr.comma (List.rev rev_comma) guard in
              guard :: rev_guards in
              [], rev_guards
        in
        (*
          The next expr matched is now the just inserted var
        *)
        let matched = JsCons.Expr.ident var in
        (*
          We should update the conditions
        *)
        let minimal_condition = SumCondition.add decision minimal_condition in
        (*
          Enrich the sum_env
        *)
        let menv =
          (*
            With the maximal condition
          *)
          let condition = SumCondition.add decision menv.condition in
          { menv with
              condition ;
              sum_env ;
          }
        in
        (*
          recursive call
        *)
        if path = []
        then
          penv, menv, minimal_condition, rev_guards
        else
          aux ~penv ~menv ~minimal_condition ~rev_guards ~matched ~rev_comma ~rev_path ~path
      )
  in
  let rev_comma = [] in
  aux ~penv ~menv ~minimal_condition ~rev_guards ~matched ~rev_comma ~rev_path ~path

(* ========================================================================== *)

let aux_build_decision
    ~env ~penv ~menv ~minimal_condition ~rev_guards ~matched
    ~check ~special_bool_hack ~rev_path =
  (*
    order of call:, Example for x.a.b.c.d

    The starting rev_path is taken from the decision.
    If this rev_path is not found in the sum_env, We extract its [hd] and completing the path.
    Once we find a previous variable assigned to a rev_path, we call [build_guard]

         rev_path      |    path
    [ d ; c ; b ; a ]  |  []
    [ c ; b ; a ]      |  [ d ]
    [ b ; a ]          |  [ c ; d ]
    [ a ]              |  [ b ; c ; d ]
    [ ]                |  [ a ; b ; c ; d ]
    *)
  let rec find rev_path path =
    match SumEnv.find_dot ~rev_path menv.condition menv.sum_env with
    | Some ident ->
        (*
          The ident was found under the maximal_condition, which means that there is no special check
          to do to be sure about the definition of this field.
        *)
        let matched = JsCons.Expr.ident ident in
        build_guard
          ~env ~penv ~menv ~minimal_condition ~rev_guards
          ~matched ~check ~special_bool_hack ~rev_path ~path

    | None -> (
        match rev_path with
        | [] ->
            (*
              This is the end, not any partial dot was found,
              we should build it now. (nested_dot)
            *)
            build_guard
              ~env ~penv ~menv ~minimal_condition ~rev_guards
              ~matched ~check ~special_bool_hack ~rev_path ~path

        | hd :: rev_path ->
            (*
              This rev_path was not found in the sum_env, but there is a chance
              to find a partial one, so we retry with a shorter rev_path, and a longer path
            *)
            let path = hd :: path in
            find rev_path path
      )
  in
  find rev_path []

(* ========================================================================== *)

let build_decision ~env ~penv ~menv ~minimal_condition ~rev_guards ~matched ~decision =
  let special_bool_hack = None in
  let rec aux ((penv, menv, minimal_condition, rev_guards) as acc) decision =
    match decision with
    | SC.True ->
        acc

    | SC.Check (check, rev_path) ->
        aux_build_decision
          ~env ~penv ~menv ~minimal_condition ~rev_guards ~matched
          ~check ~special_bool_hack ~rev_path

    | SC.And list ->
        List.fold_left aux acc list
  in
  #<If:JS_MATCH_COMPILATION $contains "MatchGeneration.build_decision">
    let under_condition = menv.condition in
    let ((_, menv, minimal_condition, rev_guards) as acc) = aux (penv, menv, minimal_condition, rev_guards) decision in
    OManager.printf (
      "@[<2>@{<bright>build_decision@}@\n%a@]@\n@[<2>under condition:@\n%a@]@\n"^^
      "returns:@\n@[<2>min-cond:@\n%a@]@\n@[<2>and rev_guards:@\n%a@]@\n@[<2>and max-cond:@\n%a@]@\n@."
    )
      SumCondition.pp_decision decision
      SumCondition.pp under_condition
      SumCondition.pp minimal_condition
      (Format.pp_list " / " (JsPrint.pp#expr ~leading:false)) rev_guards
      SumCondition.pp menv.condition
    ;
    acc
  #<Else>
    aux (penv, menv, minimal_condition, rev_guards) decision
  #<End>
(* ========================================================================== *)

let build_dot ~menv ~matched ~rev_path ~maybebool =
  let dots =
    if maybebool
    then (
      fun matched path ->
        let rec aux acc = function
          | [] -> acc
          | [ last ] -> (
              match last with
              | "true" ->
                  JsCons.Expr.call ~pure:true Common.ClientLib.dot_true [ acc ]
              | "false" ->
                  JsCons.Expr.call ~pure:true Common.ClientLib.dot_false [ acc ]
              | _ ->
                  JsCons.Expr.dot acc last
            )
          | field :: tl ->
              let acc = JsCons.Expr.dot acc field in
              aux acc tl
        in
        aux matched path
    )
    else (
      fun matched path ->
        let dot acc field = JsCons.Expr.dot acc field in
        List.fold_left dot matched path
    )
  in
  (*
    This uses the same order of call than aux_build_decision:
    Example for x.a.b.c.d

         rev_path      |    path
    [ d ; c ; b ; a ]  |  []
    [ c ; b ; a ]      |  [ d ]
    [ b ; a ]          |  [ c ; d ]
    [ a ]              |  [ b ; c ; d ]
    [ ]                |  [ a ; b ; c ; d ]
    *)
  let rec find rev_path path =
    match SumEnv.find_dot ~rev_path menv.condition menv.sum_env with
    | Some ident ->
        let matched = JsCons.Expr.ident ident in
        dots matched path

    | None -> (
        match rev_path with
        | [] ->
            dots matched path

        | hd :: rev_path ->
            (*
              This rev_path was not found in the sum_env, but there is a chance
              to find a partial one, so we retry with a shorter rev_path, and a longer path
            *)
            let path = hd :: path in
            find rev_path path
      )
  in
  find rev_path []

(* ========================================================================== *)

(*
  Special dedicated function for the potential boolean case
*)

let aux_boolean
    ~env ~penv ~menv ~minimal_condition ~implications ~rev_guards ~rev_bindings ~rev_path
    ~matched ~sub_pat ~colvar ~rowvar ~bool ~unused_pattern =

  let fallback_condition = menv.condition in
  let fallback_minimal_condition = minimal_condition in
  let fallback_sum = menv.sum in
  let gamma = env.E.gamma in
  let rev_prefix_path = rev_path in
  let bool_value = bool = "true" in
  let ty_bool = SumAnalysis.ty gamma fallback_sum ~rev_path in
  let ty_void = SumAnalysis.ty gamma fallback_sum ~rev_path:(bool::rev_path) in
  let analysed_bool_pattern =
    if QmlTypesUtils.Inspect.is_type_bool gamma ty_bool
    then
      `onlybool bool_value
    else
      if QmlTypesUtils.Inspect.is_type_void gamma ty_void
        || (
          let rec aux sub_pat =
            match sub_pat with
            | P.Fields _ ->
                (*
                  Invariant: if we end-up there, the fields is necessary empty,
                  meaning that the global pattern is { true } or { false }
                *)
                true
            | P.As (_, pat, _) -> aux pat
            | _ -> false
          in
          aux sub_pat
        )
      then
        `partialbool bool_value
      else
        `maybebool bool_value
  in
  let () =
    #<If:JS_MATCH_COMPILATION $contains "MatchGeneration.boolean">
      OManager.printf (
        "@{<bright>aux_boolean@}@\n@[<2>where @{<bright>rev_path@} is: %a@]@\n"^^
        "@[<2>@{<bright>analysis@}: %a@]@\n@."
      )
      Common.pp_path rev_path
      pp_analysed_bool analysed_bool_pattern
      #<End>
  in
  let sum = SumAnalysis.from_sum gamma fallback_sum ~rev_path in
  let analysed_sum_case =
    let sum_case = StringSet.singleton bool in
    SumAnalysis.make sum ~rowvar ~colvar ~rev_prefix_path sum_case
  in
  let rev_path = match analysed_bool_pattern with `maybebool _ -> bool :: rev_path | _ -> rev_path in
  let penv, menv, minimal_condition, rev_guards =
    (*
      case validation
    *)
    let decisions =
      try
        SumAnalysis.decisions menv.condition analysed_sum_case
      with
      | SumCondition.Inconsistency _ ->
          unused_pattern ()
    in
    if Option.is_none decisions
    then
      penv, menv, minimal_condition, rev_guards
    else
      let penv, menv, minimal_condition, rev_guards =
        let check = SC.Field true in
        let special_bool_hack = Some analysed_bool_pattern in
        aux_build_decision
          ~env ~penv ~menv ~minimal_condition ~rev_guards
          ~matched ~check ~special_bool_hack ~rev_path
      in
      let acc = penv, menv, minimal_condition, rev_guards in
      let penv, menv, minimal_condition, rev_guards =
        (*
          Add a size guard only in the case maybebool with an open var, or with a closed var
          if the bool field appears in at least one other sum case.
        *)
        match analysed_bool_pattern with
        | `maybebool _ ->
            if (rowvar = `closed) && (
              (colvar = `open_) || (
                match SumAnalysis.cases sum with
                | None -> true
                | Some cases -> (
                    let count = Array.fold_left (
                      fun count set ->
                        if FieldSet.mem bool set then succ count else count
                    ) 0 cases in
                    count > 1
                  )
              ))
            then
              let check = SC.Size 1 in
              let special_bool_hack = None in
              aux_build_decision
                ~env ~penv ~menv ~minimal_condition ~rev_guards
                ~matched ~check ~special_bool_hack ~rev_path:rev_prefix_path
            else
              acc
        | _ ->
            acc
      in
      penv, menv, minimal_condition, rev_guards
  in
  (*
    subpat binding
  *)
  let penv, rev_bindings =
    (*
      Gathering all ident needed to be binded to the void or the contents of the record
    *)
    let idents =
      let rec aux acc = function
        | P.Var (_, ident) -> ident :: acc
        | P.As (_, pat, ident) -> aux (ident::acc) pat
        | _ ->
            (*
              There is nothing to do in any other cases.
              the possible cases there are :
              | P.Any -> really nothing to do
              | P.Fields [||] -> then, it means we are in the case onlybool, or partial bool,
              the test is already handled by the guard.
            *)
            acc
      in
      aux [] sub_pat
    in
    match idents with
    | [] ->
        penv, rev_bindings
    | idents -> (
        match analysed_bool_pattern with
        | `onlybool _ | `partialbool _ ->
            let bind = Common.ClientLib.void in
            let fold (penv, rev_bindings) ident =
              let penv, ident = E.next_exprident penv ident in
              let rev_bindings = (ident, bind) :: rev_bindings in
              penv, rev_bindings
            in
            List.fold_left fold (penv, rev_bindings) idents
        | `maybebool _ ->
            let fold (penv, rev_bindings) ident =
              (*
                We should reuse the same identifier used in the test.
              *)
              let penv, ident = E.next_exprident penv ident in
              let bind = build_dot ~menv ~matched ~rev_path ~maybebool:true in
              let rev_bindings = (ident, bind) :: rev_bindings in
              penv, rev_bindings
            in
            List.fold_left fold (penv, rev_bindings) idents
      )
  in
  (*
    Negation optimization
  *)
  let implications =
    let negation = SumAnalysis.negation fallback_condition analysed_sum_case in
    let implications =
      match negation with
      | None -> implications
      | Some decision ->
          let implication = fallback_minimal_condition, decision in
          Impl.add ~rev_path implication implications
    in
    implications
  in
  (*
    fallback to the condition before the pattern case validation.
  *)
  let menv =
    let condition = fallback_condition in
    { menv with
        condition ;
    }
  in
  penv, menv, minimal_condition, implications, rev_guards, rev_bindings

(* ========================================================================== *)

let aux_pattern ~env ~penv ~menv ~matched ~pat =
  let gamma = env.E.gamma in
  Return.set_checkpoint_none (fun label ->
  let rec aux ~penv ~menv ~minimal_condition ~implications ~rev_guards ~rev_bindings ~rev_path ~pat =
    match pat with
      (*
        SIMPLE CASES const, var, any
      *)

    | P.Const (_, const) ->
        let check = SC.Const const in
        let special_bool_hack = None in
        let penv, menv, minimal_condition, rev_guards =
          aux_build_decision
            ~env ~penv ~menv ~minimal_condition ~rev_guards
            ~matched ~check ~special_bool_hack ~rev_path in
        penv, menv, minimal_condition, implications, rev_guards, rev_bindings

    | P.Var (_, ident) ->
        let penv, ident = E.next_exprident penv ident in
        let bind = build_dot ~menv ~matched ~rev_path ~maybebool:false in
        (*
          This will potentially generate some alias, but they
          will be solved after that by the local renaming pass.
          In particular, all dots already done in the guard part will systematically
          generate there an alias.
        *)
        let rev_bindings = (ident, bind) :: rev_bindings in
        penv, menv, minimal_condition, implications, rev_guards, rev_bindings

    | P.As (_, pat, ident) ->
        let penv, ident = E.next_exprident penv ident in
        let bind = build_dot ~menv ~matched ~rev_path ~maybebool:false in
        let rev_bindings = (ident, bind) :: rev_bindings in
        aux ~penv ~menv ~minimal_condition ~implications ~rev_guards ~rev_bindings ~rev_path ~pat

    | P.Any _ ->
        penv, menv, minimal_condition, implications, rev_guards, rev_bindings

    (*
      RECORD CASE
    *)

    | P.Fields (annot_label, fields, rowvar, colvar) -> (
        let unused_pattern () =
          let context = QmlError.Context.pos (Annot.pos annot_label) in
          QmlError.warning ~wclass:WarningClass.pattern context (
            "This pattern is never matched"
          );
          Return.return label ()
        in
        let () =
          #<If:JS_MATCH_COMPILATION $contains "MatchGeneration.aux_pattern">
            OManager.printf (
              "@{<bright>compiling pat@}: | %a%a@\n@."
            )
            (Format.pp_list "." Format.pp_print_string) (List.rev (""::rev_path))
            PatternAnalysis.pp pat ;
            OManager.printf (
              "@{<bright>fallback_condition@} is:@\n%a@]@\n@."
            )
            SumCondition.pp menv.condition
          #<End>
        in
        (*
          detection: special case for potential booleans
        *)
        match fields with
        | [| ("true" |"false") as bool, sub_pat |] when (
            let rec aux sub_pat =
              match sub_pat with
              | P.Fields (_, fields, _, _) -> Array.length fields = 0
              | P.Const _ -> false
              | P.As (_, pat, _) -> aux pat
              | _ -> true
            in
            aux sub_pat
          ) ->
            aux_boolean
              ~env ~penv ~menv ~minimal_condition ~implications ~rev_guards ~rev_bindings ~rev_path
              ~matched ~sub_pat ~colvar ~rowvar ~bool ~unused_pattern
        | _ ->
        (*
          Store the folback informations, for compiling next patterns
        *)
        let fallback_rev_guards = rev_guards in
        let fallback_sum = menv.sum in
        let fallback_condition = menv.condition in
        let fallback_minimal_condition = minimal_condition in
        let rev_prefix_path = rev_path in
        (*
          What is the sum corresponding to this pattern ?
        *)
        let sum =
          SumAnalysis.from_sum gamma fallback_sum ~rev_path
        in
        (*
          sum case analysis
        *)
        let analysed_sum_case =
          let sum_case =
            Array.fold_left (fun acc p -> StringSet.add (fst p) acc) StringSet.empty fields
          in
          SumAnalysis.make sum ~rowvar ~colvar ~rev_prefix_path sum_case
        in
        let menv = {
          menv with
            sum ;
        } in
        (*
          toplevel validation
        *)
        let penv, menv, minimal_condition, rev_guards =
          let decisions =
            (*
              optimisation: taking informations from previous cases.
              static validation (or invalidation) of the pattern
            *)
            try
              SumAnalysis.decisions menv.condition analysed_sum_case
            with
              (*
                If the current condition is inconsistent with one of the decision, this means that
                the pattern is invalidated statically.
                This is a dead pattern, we simply skip it of the code.
              *)
            | SumCondition.Inconsistency _ ->
                (*
                  FIXME: what wclass should we use there ?
                  Probably a part of this analysis will be done by a common pass to the server and
                  client, so that the user may have more check on patterns. (TODO after prototyping)
                *)
                unused_pattern ()
          in
          match decisions with
          | None ->
              (*
                if one of the decisions validating the sum_case is implied by the
                accumulated condition state of the match_env, there is nothing to check at
                this level.
              *)
              penv, menv, minimal_condition, rev_guards
          | Some decisions -> (
              (*
                we should validate the sum_case, using one of the returned decisions
              *)
              (*
                collecting toplevel fields which will be doted anyway in the guard,
                this is used for applying the heuristic about decisions filtering
              *)
              let to_dot = Option.default FieldMap.empty
                (PathMap.find_opt rev_prefix_path menv.shared_dot)
              in
              let to_dot = Array.fold_left (
                fun to_dot (field, pat) ->
                  match pat with
                  | P.Var (_, ident) -> (
                      match Ident.renaming_should_warn_when ident with
                      | `unused | `never -> FieldMap.add field Priority.var to_dot
                      | `used -> to_dot
                    )
                  | P.Const _ ->
                      FieldMap.add field Priority.const to_dot
                  | P.Fields (_, fields, _, _) ->
                      if Array.length fields > 0
                      then
                        FieldMap.add field Priority.fields to_dot
                      else
                        to_dot
                  | _ -> to_dot
              ) to_dot fields
              in
              let () =
                #<If:JS_MATCH_COMPILATION $contains "SumAnalysis.Filter">
                  OManager.printf (
                    "@[<2>@{<bright>filtering decisions@}:@\nrev_path=[%a]@\nto_dot={%a}@]@\n@."
                  )
                  Common.pp_path rev_prefix_path
                  (FieldMap.pp " ; " (Format.pp_fmt "%s:%d")) to_dot
                  #<End>
              in
              (*
                use heuristics for choosing a final choice about what decision we want to use
              *)
              let decision =
                let sum = menv.sum in
                SumAnalysis.Filter.final_choice
                  ~rev_prefix_path
                  ~gamma
                  ~sum
                  ~to_dot
                  decisions
              in
              (*
                <!> Optimization there:
                It could be so that a part of the decision is already implied by the current condition,
                without beeing fully implied.
                FIXME: this is innefficient, a first filter has already been done in the List.exists
              *)
              let decision =
                if SumCondition.is_conjonction decision
                then
                  SumCondition.filter menv.condition decision
                else
                  decision
              in
              (*
                Transform the decision into a guard list
              *)
              build_decision ~env ~penv ~menv ~minimal_condition ~rev_guards ~matched ~decision
            )
        in
        (*
          From there, toplevel guard has been passed, meaning that the sum-case is validated,
          so we can enrich the condition of the menv, for possibly taking advantage of
          previous assigment when we will lookup into the SumEnv for computing nested guards,
          or nested dots in bindings.
        *)
        let menv =
          let condition = SumAnalysis.add analysed_sum_case menv.condition in
          { menv with
              condition ;
          }
        in
        (*
          nested validation and all bindings
        *)
        (*
          for computing internal sum cases and guessing type of sub-patterns, we need to keep
          in mind the sum of the pattern containing the sub-patterns, as well as the guards and
          the minimal_condition before the nested validation,
        *)
        let toplevel_rev_guards = rev_guards in
        let several_width_checks = ref 0 in
        let penv, menv, minimal_condition, implications, rev_guards, rev_bindings =
          let fold
              ((penv, menv, minimal_condition, implications, rev_guards, rev_bindings) as acc)
              (field, pat)
              =
            match pat with
            | P.Any _ ->
                (* just an optimization for avoiding tuple cons/decons *)
                acc
            | _ ->
                let rev_path = field :: rev_prefix_path in
                let (_, _, _, _, new_rev_guards, _) as acc =
                  aux ~penv ~menv ~minimal_condition ~implications ~rev_guards ~rev_bindings ~rev_path ~pat
                in
                if (new_rev_guards != rev_guards) then incr(several_width_checks);
                acc
          in
          let acc = penv, menv, minimal_condition, implications, rev_guards, rev_bindings in
          Array.fold_left fold acc fields
        in
        let () =
          #<If:JS_MATCH_COMPILATION $contains "MatchGeneration.negation">
            OManager.printf (
              "@{<bright>end-of-nested compilation for@}: | %a%a@\n@."
            )
            (Format.pp_list "." Format.pp_print_string) (List.rev (""::rev_path))
            PatternAnalysis.pp pat ;
            OManager.printf (
              "@[<2>@{<bright>fallback_condition@} is:@\n%a@]@\n@."
            )
            SumCondition.pp fallback_condition
          #<End>
        in
        (*
          Optimization: Gathering implications from cases negation
        *)
        let implications =
          if rev_guards == toplevel_rev_guards
            && toplevel_rev_guards != fallback_rev_guards
          then (
            (*
              optimization: giving informations for next cases
              if there is no nested_guard, we should add the negation of the analysed_sum_case
              in the current condition, for possibly optimizing the next case.
            *)
            let negation = SumAnalysis.negation fallback_condition analysed_sum_case in
            let implications =
              match negation with
              | None -> implications
              | Some decision ->
                  let implication = fallback_minimal_condition, decision in
                  Impl.add ~rev_path implication implications
            in
            implications
          )
          else
            (*
              There are some more check in this pattern, making so that the implications are not valid
            *)
            if !several_width_checks > 1
            then
              Impl.empty
            else
              implications
        in
        (*
          fallback to the condition before the pattern case validation,
          but keeping the sum_env, and the sharing dot.
          TODO: we can split in 2 the menv to avoid tuple allocation,
          using there the same object fallback_menv
        *)
        let menv =
          let condition = fallback_condition in
          let sum = fallback_sum in
          { menv with
              sum ;
              condition ;
          }
        in
        penv, menv, minimal_condition, implications, rev_guards, rev_bindings
      )
  in
  (*
    Initialization of variable for the toplevel pattern
  *)
  let minimal_condition =
    (*
      About why we reset the minimal condition at each new pattern:
      The patterns being in sequential order, each condition leading to
      pass through a previous pattern is necessary respected when we enter
      a new pattern.
    *)
    SumCondition.empty
  in
  let rev_guards = [] in
  let rev_bindings = [] in
  let rev_path = [] in
  let implications = Impl.empty in
  (*
    Complete Compilation of pattern, toplevel and nested
  *)
  let penv, menv, minimal_condition, implications, rev_guards, rev_bindings =
    aux ~penv ~menv ~minimal_condition ~implications ~rev_guards ~rev_bindings ~rev_path ~pat
  in
  (*
    Post processing of result for rebuilding the correct output of aux_pattern,
    for branching with [aux_compile]
  *)
  (*
    minimal_condition is reseted for each new pattern of the top-level
  *)
  let () = ignore minimal_condition in
  (*
    guard: this is the conjonction of all accumulated guards
  *)
  let guard =
    match List.rev rev_guards with
    | [] -> None
    | hd::tl ->
        let conjonction = List.fold_left JsCons.Expr.land_ hd tl in
        Some conjonction
  in
  let () =
    #<If:JS_MATCH_COMPILATION $contains "MatchGeneration.aux_pattern">
      OManager.printf (
        "@{<bright>guard for pattern@}: %a%a@\nis: %a@\n@."
      )
      (Format.pp_list "." Format.pp_print_string) (List.rev (""::rev_path))
      PatternAnalysis.pp pat
      (Option.pp_none (JsPrint.pp#expr ~leading:false)) guard
    #<End>
  in
  (*
    Only if we are at toplevel, we will add all pending implications in the condition
  *)
  let menv =
    if Option.is_none guard then menv else
      let condition =
        let condition = menv.condition in
        let () =
          #<If:JS_MATCH_COMPILATION $contains "MatchGeneration.negation">
            if not (Impl.is_empty implications)
            then
              OManager.printf (
                "@[<2>@{<bright>negation optimization@}: PREPRINT: adding implications to toplevel condition@\n"^^
                "@[<2>@{<bright>condition@}:@\n%a@]@\n"^^
                "@[<2>@{<bright>implications@}:@\n%a@]@]@\n"
              )
                SumCondition.pp condition
                (Format.pp_list "@\n" SumCondition.pp_implication) (Impl.list implications)
          #<End>
        in
        try
          Impl.fold_right SumCondition.add_implication implications condition
        with
        | SumCondition.Inconsistency _ ->
            (*
              This is an internal error.
              If we end-up there, that means that the condition is equivalent to [False],
              meaning that any further patterns would be dead code.
              That means that the pattern matching constituad with the patterns seen
              until now is already exhaustive.
              In that case, we should assert that the returned guard is [None].
            *)
            assert false
      in
      let () =
        #<If:JS_MATCH_COMPILATION $contains "MatchGeneration.negation">
          if not (Impl.is_empty implications)
          then
            OManager.printf (
              "@[<2>@{<bright>negation optimization@}: adding implications to toplevel condition@\n"^^
                "@[<2>@{<bright>condition@}:@\n%a@]@\n"^^
                "@[<2>@{<bright>implications@}:@\n%a@]@\n"^^
                "@[<2>@{<bright>result@}:@\n%a@]@]@\n@."
            )
              SumCondition.pp menv.condition
              (Format.pp_list "@\n" SumCondition.pp_implication) (Impl.list implications)
              SumCondition.pp condition
              #<End>
      in
      let menv =
        { menv with
            condition ;
        }
      in
      menv
  in
  (*
    bindings: this is just the accumulated bindings
  *)
  let bindings = List.rev rev_bindings in
  penv, menv, guard, bindings

) (* <--- this is the end of Return.set_checkpoint_none *)
(* ========================================================================== *)

let add_path_map path field priority map =
  let acc = Option.default FieldMap.empty (PathMap.find_opt path map) in
  let acc =
    match FieldMap.find_opt field acc with
    | None ->
        FieldMap.add field priority acc
    | Some p ->
        if p >= priority
        then
          acc
        else
          FieldMap.add field priority acc
  in
  PathMap.add path acc map

let aux_compile ~env ~penv ~matched ~ty ~patterns =
  (*
    start environmement
  *)
  let sum_env = SumEnv.empty in
  let sum = SumAnalysis.from_ty ~rev_path:[] env.E.gamma ty in
  (* FIXME: we should do this in the function aux_pattern *)
  let condition = SumCondition.add (SumAnalysis.implies sum) SumCondition.empty in
  let shared_dot =
    let rec fold rev_prefix_path acc (field, pat) =
      match pat with
      | P.Fields (_, fields, _, _) ->
          let acc =
            if Array.length fields = 0
            then
              acc
            else
              add_path_map rev_prefix_path field Priority.shared_fields acc
          in
          Array.fold_left (fold (field :: rev_prefix_path)) acc fields
      | P.Var _ ->
          add_path_map rev_prefix_path field Priority.shared_var acc
      | P.Const _ ->
          add_path_map rev_prefix_path field Priority.shared_const acc
      | P.As (_, pat, _) ->
          let acc = add_path_map rev_prefix_path field Priority.shared_var acc in
          fold rev_prefix_path acc (field, pat)
      | _ ->
          acc
    in
    let fold acc (pat, _) =
      match pat with
      | P.Fields (_, fields, _, _) ->
          Array.fold_left (fold []) acc fields
      | _ -> acc
    in
    List.fold_left fold PathMap.empty patterns
  in
  let () =
    #<If:JS_MATCH_COMPILATION $contains "SumAnalysis.Filter">
      OManager.printf (
        "@[<2>@{<bright>shared_dot@}:@\n"
      );
      PathMap.iter (
        fun rev_path map ->
          OManager.printf "rev_path:%a ==> map:%a@\n"
            Common.pp_path rev_path
            (FieldMap.pp " ; " (Format.pp_fmt "%s:%d")) map
      ) shared_dot;
      OManager.printf "@]@."
    #<End>
  in
  let menv = {
    sum_env ;
    sum ;
    condition ;
    shared_dot ;
  } in
  let rec filterfoldrevmap penv menv rev_cases = function
    | [] ->
        penv, rev_cases

    | (pat, jsexpr) :: tl ->
        let penv, menv, rev_cases, continue =
          match aux_pattern ~env ~penv ~menv ~matched ~pat with
          | Some (penv, menv, guard, bindings) ->
              let rev_cases = (guard, bindings, jsexpr) :: rev_cases in
              penv, menv, rev_cases, Option.is_some guard
          | None ->
              penv, menv, rev_cases, true
        in
        if continue
        then
          filterfoldrevmap penv menv rev_cases tl
        else (
          (*
            TODO there: we should warn for all pattern of tl, telling that they are unused.
            It is easy, just iter Warning.warning ...
            with the context from closed_pat.annot and env.annotmap
          *)
          penv, rev_cases
        )
  in
  filterfoldrevmap penv menv [] patterns

(* ========================================================================== *)

let compile_factory aux_compile ~env ~penv ~pos ~matched ~ty ~patterns =
  (*
    cases are cond * bindings * right-side
  *)
  let foldrev else_ (guard, bindings, right_side) =
    let guard = Option.get guard in
    let bindings = List.map (fun (id, expr) -> JsCons.Expr.assign_ident id expr) bindings in
    let then_ = JsCons.Expr.comma bindings right_side in
    JsCons.Expr.cond guard then_ else_
  in

  let penv, rev_cases = aux_compile ~env ~penv ~matched ~ty ~patterns in
  let matching =
    match rev_cases with
    | [] ->
        (*
          This is an internal error, empty match
        *)
        assert false

    | (guard, bindings, right_side) :: tl -> (
        if Option.is_none guard
        then
          (* the match is exhaustive, we should not add a match failure message *)
          let bindings = List.map (fun (id, expr) -> JsCons.Expr.assign_ident id expr) bindings in
          let last_node = JsCons.Expr.comma bindings right_side in
          List.fold_left foldrev last_node tl
        else
          (*
            The match may crash, we should add a runtime failure message.
            FIXME: add a warning at compile time
          *)
          let last_node =
            Common.ClientLib.match_failure pos
          in
          List.fold_left foldrev last_node rev_cases
      )
  in
  penv, matching


let compile ~env = compile_factory aux_compile ~env

(* ========================================================================== *)

module T =
struct
  module T = PatternAnalysis.T
  type pat = T.pat

  let rec aux_pat ~env ~penv ~bindings ~matched ~ty ~pat =
    match pat with
    | T.Const (_, const) ->
        let const = Common.const const in
        let cond = JsCons.Expr.equality matched const in
        let guard = Some cond in
        penv, guard, bindings

    | T.Var (_, ident) ->
        let penv, ident = E.next_exprident penv ident in
        let bindings = (ident, matched) :: bindings in
        penv, None, bindings

    | T.Any _ ->
        penv, None, []

    | T.As (_, pat, ident) ->
        let penv, ident = E.next_exprident penv ident in
        let bindings = (ident, matched) :: bindings in
        aux_pat ~env ~penv ~bindings ~matched ~ty ~pat

  let aux_compile ~env ~penv ~matched ~ty ~patterns =
    let rec filterfoldrevmap penv rev_cases = function
      | [] ->
          penv, rev_cases

      | (pat, right) :: tl ->
          let penv, rev_cases, continue =
            let penv, guard, bindings = aux_pat ~env ~penv ~bindings:[] ~matched ~ty ~pat in
            let case = guard, bindings, right in
            let rev_cases = case :: rev_cases in
            penv, rev_cases, Option.is_some guard
          in
          if continue
          then
            filterfoldrevmap penv rev_cases tl
          else (
            (*
              TODO there: we should warn for all pattern of tl, telling that they are unused.
              It is easy, just iter Warning.warning ...
              with the context from closed_pat.annot and env.annotmap
            *)
            penv, rev_cases
          )
    in
    filterfoldrevmap penv [] patterns

  let compile ~env = compile_factory aux_compile ~env

end

module AdHoc =
struct

  let rec patbool ~or_var_or_any bool = function
    | Q.PatRecord (_, [field, _], _) ->
        (field = "true") = bool
    | Q.PatCoerce (_, pat, _) -> patbool ~or_var_or_any bool pat
    | Q.PatVar _
    | Q.PatAny _ -> or_var_or_any
    | Q.PatAs (_, pat, _) -> patbool ~or_var_or_any bool pat
    | Q.PatRecord _
    | Q.PatConst _ -> assert false (* type error *)

  let compile ~env ~penv ~matched ~ty ~patterns =

    Return.set_checkpoint_opt (
      fun label ->

        (*
          1st & 2nd ad hoc compilation
          reforming && and ||
        *)
        if QmlTypesUtils.Inspect.is_type_bool env.E.gamma ty
        then (

          match patterns with

          (*
            &&
          *)
          | [
              pattrue, e1;
              patfalse, e2;
            ]
              when
                   patbool ~or_var_or_any:false true pattrue
                && patbool ~or_var_or_any:true false patfalse
            ->
              (
                match e2 with
                | J.Je_bool (_, false) ->
                    let res = JsCons.Expr.land_ matched e1 in
                    Return.return label (penv, res)
                | _ -> ()
              ) ;

              (
                match e1 with
                | J.Je_bool (_, true) ->
                    let res = JsCons.Expr.lor_ matched e2 in
                    Return.return label (penv, res)
                | _ -> ()
              ) ;

              ()

          | _ ->
              ()
        ) ;

        ()

    )

end
