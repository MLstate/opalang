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

(* depends *)
module List = BaseList

(* shorthands *)
module Q = QmlAst

let next =
  let r = ref 0 in
  fun () -> incr r; !r

module type S =
sig
  type effect
  val join_effect : effect -> effect -> effect
  val effect_of : [`bypass of BslKey.t] -> effect
  val no_effect : effect
  val all_effects : effect
  val to_string : effect -> string
end

module type E =
sig
  type effect
  type effects
  type typ
  val string_of_typ : typ -> string
  val flatten_effect : effects -> effect

  type env = (effects IdentMap.t * typ IdentMap.t)
  val infer_code : ?initial_env:env -> (BslKey.t -> Q.ty) -> Q.code -> env
end

module EffectAnalysis(S:S) : E with type effect = S.effect =
struct
  type level = int
  type effect = S.effect
  type var =
    | Fresh of level ref * int
    | Unified of typ
  and typ =
    | Var of var ref
    | Dontcare
    | Arrow of bool ref (* this boolean is really a hack
                         * because after lambda lifting we have
                         * poymorphic parameters and so we can't infer anymore
                         * this whole pass should really end up in the typer
                         *) * typ list * effects * typ
  and effects = effect * effect_var ref
  and effect_var =
    | EFresh of level ref * int
    | EUnified of effects

  let join_effects = S.join_effect

  let rec flatten_effect_aux (eff, v) =
    match !v with
    | EFresh (_,v) -> eff, v
    | EUnified (eff2, v2) -> flatten_effect_aux (join_effects eff eff2, v2)
  let flatten_effect e = fst (flatten_effect_aux e)

  let rec string_of_typ = function
    | Var v ->
        (match !v with
         | Fresh (lev,_) -> "_" ^ string_of_int !lev
         | Unified ty -> string_of_typ ty)
    | Dontcare ->
        "dontcare"
    | Arrow ({contents=true},_,_,_) ->
        string_of_typ Dontcare
    | Arrow ({contents=false},tyl,e,ty) ->
        let sl = String.concat " -> " (List.map string_of_typ tyl) in
        let s = string_of_typ ty in
        let eff, var = flatten_effect_aux e in
        "(" ^ sl ^ " " ^ string_of_int var ^ (S.to_string eff) ^ "->" ^ " " ^ s ^ ")"

  let rec traverse_normalize tra = function
    | Dontcare
    | Var {contents = Fresh _ } as ty -> ty
    | Var {contents = Unified ty} -> traverse_normalize tra ty
    | Arrow ({contents=true},_,_,_) -> traverse_normalize tra Dontcare
    | Arrow (ref_,typs,effects,typ) -> Arrow (ref_,List.map tra typs,traverse_normalize_eff effects,tra typ)
  and traverse_normalize_eff ((l,v) as p) =
    match !v with
    | EFresh _ -> p
    | EUnified (l2,v2) -> traverse_normalize_eff (join_effects l l2, v2)
  let shallow_normalize ty = traverse_normalize (fun x -> x) ty
  let rec normalize ty = traverse_normalize normalize ty

  let rec occur_check v = function
    | Dontcare -> ()
    | Var v' ->
      if v == v' then failwith "Cyclic unification"
      else
        (match !v' with
        | Fresh _ -> ()
        | Unified ty -> occur_check v ty)
    | Arrow ({contents=true},_,_,_) ->
        occur_check v Dontcare
    | Arrow ({contents=false},tyl,_,ty) ->
      List.iter (occur_check v) tyl;
      occur_check v ty

  let generic_level = -1

  let rec set_max_level max_level = function
    | Dontcare -> ()
    | Arrow ({contents=true},_,_,_) -> set_max_level max_level Dontcare
    | Arrow ({contents=false},tyl,_,ty) ->
        List.iter (set_max_level max_level) tyl;
        set_max_level max_level ty
    | Var v ->
        match !v with
        | Fresh (lev,_) ->
            if !lev <> generic_level then
              if !lev > max_level then
                lev := max_level
        | Unified ty -> set_max_level max_level ty

  let rec unify ty1 ty2 =
    if ty1 == ty2 then () else (
      let ty1 = shallow_normalize ty1 in
      let ty2 = shallow_normalize ty2 in
      if ty1 == ty2 then () else (
        match ty1, ty2 with
        | Var v1, Var v2 ->
          let lev1 = (match !v1 with Fresh (lev1, _) -> lev1 | _ -> assert false) in
          let lev2 = (match !v2 with Fresh (lev2, _) -> lev2 | _ -> assert false) in
          v2 := Unified ty1;
          lev1 := min !lev1 !lev2
        | Var v, ty
        | ty, Var v ->
          occur_check v ty;
          set_max_level (match !v with Fresh (lev, _) -> !lev | _ -> assert false) ty;
          v := Unified ty
        | Arrow (ref1,tyl1,(l1, r1),ret1), Arrow (ref2,tyl2,(l2, r2),ret2) ->
          assert (not !ref1 && not !ref2);
          if List.length tyl1 = List.length tyl2 then (
            let lev1 = (match !r1 with EFresh (r,_) -> !r | _ -> assert false) in
            let lev2 = (match !r2 with EFresh (r,_) -> !r | _ -> assert false) in
            let r3 = ref (EFresh (ref (min lev1 lev2), next())) in
            r1 := EUnified (l2, r3);
            r2 := EUnified (l1, r3);
            List.iter2 unify tyl1 tyl2;
            unify ret1 ret2
          ) else (
            (* see the comment about the meaning of the ref *)
            ref1 := true;
            ref2 := true;
          )
        | Dontcare, Dontcare ->
          ()
        | Dontcare, Arrow ({contents=false},tyl,(_,r),ty)
        | Arrow ({contents=false},tyl,(_,r),ty), Dontcare ->
            (*Printf.printf "Loss of precision: unifying %s and %s\n%!"
            (string_of_typ ty1) (string_of_typ ty2);*)
            (match !r with
             | EFresh (lev,_) ->
                 let r2 = ref (EFresh (lev, next())) in
                 r := EUnified (S.all_effects, r2);
                 List.iter (unify Dontcare) tyl;
                 unify Dontcare ty
             | _ -> assert false)
        | _, Arrow ({contents=true},_,_,_)
        | Arrow ({contents=true},_,_,_), _ ->
            assert false
      )
    )

  let rec instantiate level ((varmap,effmap) as map) = function
    | Dontcare -> map, Dontcare
    | Var {contents = Fresh (this_level,i)} as ty ->
      (try map, IntMap.find i varmap
       with Not_found ->
         if !this_level = generic_level then
           let v = Var (ref (Fresh (ref level, next ()))) in
           (IntMap.add i v varmap,effmap), v
         else
           map, ty)
    | Var {contents = Unified ty} ->
      instantiate level map ty
    | Arrow ({contents=true},_,_,_) -> instantiate level map Dontcare
    | Arrow ({contents=false},tyl,effects,ty) ->
      let map, tyl = List.fold_left_map (instantiate level) map tyl in
      let (varmap, effmap), ty = instantiate level map ty in
      let effmap, effects = instantiate_eff level effmap effects in
      (varmap, effmap), Arrow (ref false, tyl, effects, ty)
  and instantiate_eff level effmap (l,v) =
    match !v with
    | EFresh (this_level,i) ->
      (try effmap, (l, IntMap.find i effmap)
       with Not_found ->
         if !this_level = generic_level then
           let v = ref (EFresh (ref level, next())) in
           IntMap.add i v effmap, (l, v)
         else
           effmap, (l, v))
    | EUnified (l2,v2) -> instantiate_eff level effmap (join_effects l l2, v2)
  let instantiate level ty =
    snd (instantiate level (IntMap.empty,IntMap.empty) ty)

  let rec generalize level = function
    | Var v ->
      (match !v with
      | Fresh (this_level,_) ->
        if !this_level <> generic_level && !this_level > level then
          this_level := generic_level
      | Unified ty -> generalize level ty)
    | Dontcare -> ()
    | Arrow ({contents=true},_,_,_) -> generalize level Dontcare
    | Arrow ({contents=false},tyl,effects,ty) ->
      List.iter (generalize level) tyl;
      generalize level ty;
      generalize_eff level effects
  and generalize_eff level (_,v) =
    match !v with
    | EFresh (this_level,_) ->
      if !this_level <> generic_level && !this_level > level then
        this_level := generic_level
    | EUnified eff -> generalize_eff level eff

  let next_var level = Var (ref (Fresh (ref level, next())))
  let next_eff_var level = ref (EFresh (ref level, next()))

  let infer_pattern env p level =
    QmlAstWalk.Pattern.fold_down
      (fun env -> function
       | Q.PatVar (_, i) | Q.PatAs (_, _, i) ->
           IdentMap.add i (next_var level) env
       | _ -> env) env p

  let rec convert_type varmap level = function
    | Q.TypeArrow (tyl,ty) ->
        Arrow (ref false,List.map (convert_type varmap level) tyl, (S.no_effect, next_eff_var level), convert_type varmap level ty)
    | Q.TypeVar v ->
        (try QmlTypeVars.TypeVarMap.find v !varmap
         with Not_found ->
           let v2 = next_var level in
           varmap := QmlTypeVars.TypeVarMap.add v v2 !varmap;
           v2)
    | _ ->
        Dontcare

  (* need to know whether we are in covariant or contravariant positions
   * but since no bypass ever returns a function, well ... *)
  let rewrite_arrow level effect ty =
    let varmap = ref QmlTypeVars.TypeVarMap.empty in
    match ty with
    | Q.TypeArrow (tyl,ty) ->
        Arrow (ref false,List.map (convert_type varmap level) tyl, (effect, next_eff_var level), convert_type varmap level ty)
    | ty -> convert_type varmap level ty

  let rec infer bp env effect level e =
    try
    let ty =
    match e with
    | Q.Const _ -> Dontcare
    | Q.Ident (_, i) ->
        (try instantiate level (IdentMap.find i env)
        with Not_found -> Printf.printf "Not found %s\n%!"
          (Ident.to_string i);
           assert false)
    | Q.LetIn (_, iel,e) ->
      let env =
        List.fold_left
          (fun new_env (i,e) ->
             let ty = infer bp env effect (level+1) e in
             generalize level ty;
             IdentMap.add i ty new_env) env iel in
      infer bp env effect (level+1) e
    | Q.LetRecIn (_, iel, e) ->
      let itys = List.map (fun (i,_) -> (i,next_var (level+1))) iel in
      let env = List.fold_left (fun env (i,ty) -> IdentMap.add i ty env) env itys in
      let tys' = List.map (fun (_,e) -> infer bp env effect (level+1) e) iel in
      List.iter2 (fun (_,ty) ty' -> unify ty ty') itys tys';
      List.iter (generalize level) tys';
      infer bp env effect (level+1) e
    | Q.Lambda (_, sl, e) ->
      let styl = List.map (fun s -> (s, next_var level)) sl in
      let env =
        List.fold_left
          (fun env (s,ty) -> IdentMap.add s ty env) env styl in
      let effect = next_eff_var level in
      let ty = infer bp env effect (level+1) e in
      Arrow (ref false,List.map snd styl, (S.no_effect,effect), ty)
    | Q.Directive (_, `partial_apply (info,_), e :: _, _) -> (
        let missing = Option.get info in
        match e with
        | Q.Apply (_, e, el) ->
            (* no change on the current effect, since it is a partial
             * application *)
            let arrow_ty = infer bp env effect level e in
            let tyl = List.map (infer bp env effect level) el in
            let missing_types = List.init missing (fun _ -> next_var level) in
            let ret_ty = next_var level in
            let new_effect = (S.no_effect,next_eff_var level) in
            unify (Arrow (ref false,tyl @ missing_types,new_effect,ret_ty)) arrow_ty;
            Arrow (ref false,missing_types,new_effect,ret_ty)
        | _ -> assert false
      )
    | Q.Apply (_, e, el) ->
      let arrow_ty = infer bp env effect (level+1) e in
      let tyl = List.map (infer bp env effect (level+1)) el in
      let ret_ty = next_var level in
      unify (Arrow (ref false,tyl,(S.no_effect,effect),ret_ty)) arrow_ty;
      ret_ty
    | Q.Match (_, e, pel) ->
      (* not sure about that node *)
      let ___TY = infer bp env effect (level+1) e in
      let infer_rule env (p,e) =
        let env = infer_pattern env p level in
        infer bp env effect (level+1) e in
      (match pel with
       | [] -> assert false
       | rule_ :: pel ->
           let ty = infer_rule env rule_ in
           List.iter
             (fun rule_ ->
                let ty' = infer_rule env rule_ in
                unify ty ty')
             pel;
           ty)
    | Q.Record (_, sel) ->
        List.iter (fun (_s,e) -> ignore (infer bp env effect (level+1) e)) sel;
        Dontcare
    | Q.Dot (_, e, _s) ->
        ignore (infer bp env effect (level+1) e);
        Dontcare (* not quite good, will have troubles with higher order *)
    | Q.ExtendRecord (_, _s, e1, e2) ->
        ignore (infer bp env effect (level+1) e1);
        ignore (infer bp env effect (level+1) e2);
        Dontcare
    | Q.Bypass (_, b) ->
        (* call a bypass typer, and add side effect to the arrow *)
        let qty = bp b in
        let its_effect = S.effect_of (`bypass b) in
        (*Format.printf "%s has type %a and effect %s@."
          (BslKey.to_string b) QmlPrint.pp#ty qty (S.to_string its_effect);*)
        rewrite_arrow level its_effect qty
    | Q.Coerce (_, e, _) ->
        infer bp env effect (level+1) e
    | Q.Path (_, el, _) ->
        List.iter (function
                     | Q.ExprKey e -> ignore (infer bp env effect (level+1) e)
                     | _ -> ()) el;
        Dontcare
    | Q.Directive (_, `fail, el, _) ->
        List.iter (fun e -> ignore (infer bp env effect level e)) el;
        next_var level

    | Q.Directive (_, ( `restricted_bypass _
                      | #Q.type_directive
                      | `recval
                      | #Q.slicer_directive
                      | `lifted_lambda _
                      | `full_apply _
                      | `assert_), l, _) -> (
        match l with
        | [e] -> infer bp env effect (level+1) e
        | _ -> assert false
      )
    | Q.Directive (_, _, el, _) ->
        (* there should be different categories here, we care about some directives
         * and most of the time, the type is 'a -> 'a so we don't want to lose it! *)
        List.iter (fun e -> ignore (infer bp env effect (level+1) e)) el;
        Dontcare in
    (*Format.printf "%a -> %s@." QmlPrint.pp#expr e (string_of_typ ty);*)
    ty
    with exn ->
      let context = QmlError.Context.expr e in
      QmlError.serror context "QmlEffect error@.";
      raise exn

  type env = (effects IdentMap.t * typ IdentMap.t)
  let infer_code ?(initial_env=(IdentMap.empty, IdentMap.empty)) bp code =
    List.fold_left
      (fun ((_,env) as full_env) ->
         function
         | Q.NewVal (_,iel) ->
             let level = 0 in
             List.fold_left
               (fun (env_effect,last_env) (i,e) ->
                  let effect = next_eff_var generic_level in
                  let ty = infer bp env effect (level+1) e in
                  generalize level ty;
                  let last_env = IdentMap.add i ty last_env in
                  let env_effect = IdentMap.add i (S.no_effect,effect) env_effect in
                  #<If:EFFECTS_SHOW> Printf.printf "%s has type %s with effect %s\n%!" (Ident.to_string i) (string_of_typ ty) (S.to_string (flatten_effect (S.no_effect,effect)))#<End>;
                  env_effect, last_env
               ) full_env iel
         | Q.NewValRec (_,iel) ->
             let level = 0 in
             let itys = List.map (fun (i,_) -> (i,next_var (level+1))) iel in
             let full_env = List.fold_left (fun (env_effect,env) (i,ty) -> (env_effect,IdentMap.add i ty env)) full_env itys in
             let full_env, tys' = List.fold_left_map
               (fun (env_effect,env) (i,e) ->
                  let effect = next_eff_var generic_level in
                  let ty = infer bp env effect (level+1) e in
                  let env = IdentMap.add i ty env in
                  let env_effect = IdentMap.add i (S.no_effect,effect) env_effect in
                  (env_effect, env), ty
               ) full_env iel in
             List.iter2 (fun (_i,ty) ty' -> unify ty ty') itys tys';
             List.iter (generalize level) tys';
             #<If:EFFECTS_SHOW> List.iter (fun (i,ty) -> Printf.printf "%s has type %s\n%!" (Ident.to_string i) (string_of_typ ty)) itys#<End>;
             full_env
         | _ -> assert false
      )
      initial_env
      code
end

let effect_of' = function
  | `bypass s ->
    (* FIXME: this reminds me the dark days when we have no bsl
       and huge list of bypass floating around
       it should be replaced by a bypass property
       it affects the way bypass interacts slicer
       two bypass with the exactly the same definition have different behaviour
       which is totally misleading *)
      match BslKey.to_string s with
      | "bslpervasives_int_neg"
      | "bslpervasives_int_add"
      | "bslpervasives_int_sub"
      | "bslpervasives_int_mul"
      | "bslpervasives_int_div"
      | "bslpervasives_float_neg"
      | "bslpervasives_float_add"
      | "bslpervasives_float_sub"
      | "bslpervasives_float_mul"
      | "bslpervasives_float_div"
      | "bslstring_concat"
      | "bslstring_of_int"
      | "bsltime_local_format"
      | "bslpervasives_compare_int"
      | "bslpervasives_compare_string"
      | "bslpervasives_compare_char"
      | "bslpervasives_compare_float"
      | "bslpervasives_int_cmp_eq"
      | "bslpervasives_int_cmp_neq"
      | "bslpervasives_int_cmp_leq"
      | "bslpervasives_int_cmp_lneq"
      | "bslpervasives_int_cmp_gneq"
      | "bslpervasives_int_cmp_geq"
      | "bslvalue_tsc_get"
      | "bslpervasives_magic_id"
      | "bslvalue_record_name_of_field"
      | "bslvalue_record_field_of_name"
      | "bslnumber_int_of_float"
      | "bslnumber_int_of_string"
      | "bslnumber_float_of_int"
      | "bslpervasives_string_of_char"
      | "bslnumber_float_to_string"
      | "bslpervasives_dump"
      | "bslvalue_record_fold_record"
      | "bslvalue_record_fold_2_record"
      | "bslvalue_record_empty_constructor"
      | "bslvalue_record_add_field"
      | "bslvalue_record_make_record"
      | "bslvalue_record_make_simple_record"
      | "bslnumber_math_abs_f"
      | "bslnumber_math_abs_i"
      | "bslnumber_math_acos"
      | "bslnumber_math_asin"
      | "bslnumber_math_atan"
      | "bslnumber_math_ceil"
      | "bslnumber_math_cos"
      | "bslnumber_math_exp"
      | "bslnumber_math_floor"
      | "bslnumber_math_isnan"
      | "bslnumber_math_is_infinite"
      | "bslnumber_math_is_normal"
      | "bslnumber_math_log"
      | "bslnumber_math_sin"
      | "bslnumber_math_sqrt_f"
      | "bslnumber_math_sqrt_i"
      | "bslnumber_math_tan"
      | "bslnumber_int_ordering"
      | "bslpervasives_webutils_server_side"
      | "bslpervasives_aresameobject"
      | "bslstring_check_match_literal"
      | "bslstring_get"
      | "bslpervasives_int_of_first_char"
      | "bslnumber_int_op_asr"
      | "bslnumber_int_op_lsr"
      | "bslnumber_int_op_lsl"
      | "bslnumber_int_op_lnot"
      | "bslnumber_int_op_lxor"
      | "bslnumber_int_op_lor"
      | "bslnumber_int_op_land"
      | "bslnumber_int_to_char"
      | "bslpervasives_int_mod"
      | "bslstring_sub"
      | "bslstring_init" (* THIS ONE IS FALSE, no side effect if the given function
                          * has no side effect either *)
      | "bslcactutf_cactutf_length"
      | "bslstring_length"
      | "sys_argv"
      | "sys_argc"
        ->
          `pure

      | "bsltime_now" ->
          `read

      | "bslpervasives_print_endline"
      | "bslpervasives_print_string"
      | "bslpervasives_prerr_string"
      | "bslpervasives_print_int"
      | "bslpervasives_jlog" ->
          `write

      | "bslreference_create" ->
          `alloc

      | "bslpervasives_error" -> (* accepting to clean errors *)
          `error

      | _ -> `impure

module SideEffectS =
struct
  type effect = bool
  let join_effect = (||)
  let no_effect = false
  let all_effects = true
  let effect_of x =
      match effect_of' x with
    | `pure | `alloc | `read | `error -> false
    | `impure | `write -> true
  let to_string = function
    | true -> "+"
    | false -> ""
end

module SlicerEffectS =
  struct
    include SideEffectS
    let effect_of x =
      match effect_of' x with
      | `pure | `write | `error -> false
      | `impure | `alloc | `read -> true
  end

module SideEffect = EffectAnalysis(SideEffectS)
module SlicerEffect = EffectAnalysis(SlicerEffectS)
