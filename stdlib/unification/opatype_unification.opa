/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 * Unification of OPA runtime types
 */
/*
 * The substitution is implemented as a var to type redirection map
 * A substitution can be composed of several redirection, these redirection are meant to be compressed on the fly
 *
 */

import stdlib.core.compare

type OpaType.Unification.push_elmt = (OpaType.ty,OpaType.ty)
type OpaType.Unification.cycle_detector = TortoiseAndHare.t(OpaType.Unification.push_elmt)
type OpaType.Unification.CycleDetector = CycleDetector(OpaType.Unification.cycle_detector,OpaType.Unification.push_elmt)

type OpaType.Unification.subst = { var : stringmap(OpaType.ty)
                                   col : stringmap(OpaType.ty)
                                   row : stringmap(OpaType.ty)
                                   cycle_detector :  OpaType.Unification.cycle_detector}

type OpaType.Unification.error_case = {generic : (OpaType.ty,OpaType.ty)} / {incompatible_arity} / {incompatible_quantification} / {incompatible_record}

type OpaType.Unification.error = list(OpaType.Unification.error_case)

type OpaType.Unification.result = outcome(OpaType.Unification.subst,OpaType.Unification.error)

/*
 * This module contains private routines relative to unification
 */
@private P = {{

  cmp_ty_ty(tys0,tys1) = Compare.equal_ty(tys0.f1,tys1.f1) && Compare.equal_ty(tys0.f2,tys1.f2)
  CycleDetector = TortoiseAndHare.create(cmp_ty_ty):OpaType.Unification.CycleDetector

  empty_subst = {var=StringMap.empty col=StringMap.empty row=StringMap.empty cycle_detector=CycleDetector.empty} : OpaType.Unification.subst

  /* substitute_var_no_opti(tv,ty,subst) get the global substitution type for tv if any or return ty (e.g. TyVar tv normally) */
  substitute_var_no_opti(tv,ty,subst):OpaType.ty =
    match StringMap.get(tv,subst.var)
    {none} -> ty
    {some=ty} -> substitute_no_opti(ty,subst)

  /* same as above */
  substitute_no_opti(ty,subst):OpaType.ty =
    match ty
    {TyVar=tv} -> substitute_var_no_opti(tv,ty,subst)
    _ -> ty

  /* same as above but also return a global substitution optimised (for tv only) */
  substitute_var(tv,ty,subst) =
    match StringMap.get(tv,subst.var)
    {none} -> (ty,subst)
    {some=ty} ->
      final_ty = substitute_no_opti(ty,subst)
      if final_ty==ty then (ty,subst)
      else (final_ty,unify_var_ty(tv,final_ty,subst))

  /* [ unify_var_ty(tv,ty,subst)] add a susbtituion tv => ty to the global substitution [subst] */
  unify_var_ty(tv,ty,subst) = {subst with var=StringMap.add(tv,ty,subst.var)}

  /* and on a outcome(substitunion) and a substituion transformaion function (subst->outcome(subst)) only applied if the former is {success=...} */
  `&&&`(out_subst,unif):OpaType.Unification.result=
    match out_subst:OpaType.Unification.result
      {failure=_} as f-> f
      {success=s} -> unif(s)

  /* type name expansion */
  expand(name,args):OpaType.ty= OpaTsc.instantiate(OpaTsc.instantiation_type_only(args), OpaTsc.get_unsafe(name))

  /* full type expansion until a structural construction is met, can loop on direct named recursion (i.e. type t=t or similar)
     the first element is the strutural type */
  full_expansion(ty,name,args):list(OpaType.ty) =
    rec full_ty_expansion(ty,acc) =
     acc = [ty|acc]
     match ty
      {TyName_ident=name TyName_args=args} -> full_ty_expansion(expand(name,args),acc)
      _ -> acc
     end
    full_ty_expansion(expand(name,args),[ty])

  /* return the final structural expansion of a type */
  structural_type(ty,name,args):OpaType.ty = List.head(full_expansion(ty,name,args)) // TODO make faster

  check_no_substitute(ty,subst) = substitute_no_opti(ty,subst)===ty

  /* unify_vars(v1,v2,t1,t2,subst) add a substitiution between v1 (<=>t1) and v2 (<=>t2), the substitution is */
  unify_vars(v1,v2,t1,t2,subst):OpaType.Unification.subst =
    if v1==v2 then subst
    else if v1 < v2 then unify_var_ty(v2,t1,subst)
    else unify_var_ty(v1,t2,subst)

  unifiable_list(diff_len_error, l1, l2, subst)(unifiable):OpaType.Unification.result =
    if List.length(l1) != List.length(l2) then {failure=diff_len_error}
    else Fold.list2(l1,l2,{success=subst})(v1,v2,subst->
      match subst
      {failure=_} -> subst
      {success=subst} -> unifiable(v1,v2,subst)
    )

  unifiable_function_parameters(l1, l2,subst) =
    unifiable_list([{incompatible_arity}],l1,l2,subst)(unifiable)

  unifiable_named_parameters(l1, l2,subst) =
    unifiable_list([{incompatible_arity}],l1,l2,subst)(unifiable)

  // try to unify two types expanded, first trying high level named type then structural type
  unifiable_sync_expansion(tyl1_tyl2,subst) =
    match tyl1_tyl2:(list(OpaType.ty),list(OpaType.ty))
    ([ty1|tyl1],[ty2|tyl2]) ->
      match (ty1,ty2)
      ({TyName_ident=n1; TyName_args=args1},{TyName_ident=n2; TyName_args=args2}) ->
        if n1==n2 then unifiable_named_parameters(args1,args2,subst)
        else unifiable_sync_expansion((tyl1,tyl2),subst)
      _ ->
        do @assert(tyl1==[])
        do @assert(tyl2==[])
        unifiable_with_cycle_cut(ty1,ty2,subst)
      end
    _ -> {failure=[]}
    end

  expand_and_sync(ty1,n1,args1,ty2,n2,args2) =
    // TODO something that don't expand uneeded name type (see the compiler)
    // 1 expansion could be stopped earlier if two names sync
    // 2 expansion could be guided by a depth property for named type
    tyl1 = full_expansion(ty1,n1,args1)
    tyl2 = full_expansion(ty2,n2,args2)
    depth1 = List.length(tyl1)
    depth2 = List.length(tyl2)
//    do println("EXPANSION\n{tyl1}\n{tyl2}\n")
    tyl1 = List.drop(max(0,depth1-depth2), List.rev(tyl1))
    tyl2 = List.drop(max(0,depth2-depth1), List.rev(tyl2))
//    do println("SYNC EXPANSION\n{tyl1}\n{tyl2}\n")
    (tyl1,tyl2)

  unifiable_named(ty1,n1,args1,ty2,n2,args2,subst):OpaType.Unification.result =
    if n1==n2 then unifiable_named_parameters(args1,args2,subst)
    else unifiable_sync_expansion(expand_and_sync(ty1,n1,args1,ty2,n2,args2),subst)

  unifiable_field(f1:OpaType.field, f2:OpaType.field,subst) =
    if f1.label!=f2.label then {failure=[]}
    else unifiable(f1.ty,f2.ty,subst)

  unifiable_fields(f1:OpaType.fields, f2:OpaType.fields,subst) =
    unifiable_list([{incompatible_record}],f1,f2,subst)(unifiable_field)

  unifiable_row(r1,r2,subst) =
    /* assumed sorted */
    unifiable_fields(r1, r2,subst)

  unifiable_list_with_vars(
   cmp:'elmt,'elmt->'cmp,
   unifiable_var:'var,'elmt,'subst-> OpaType.Unification.result,
   unifiable_elmt:'elmt,'elmt,'subst -> OpaType.Unification.result,
   v1:option('var),v2:option('var),l1:list('elmt),l2:list('elmt),subst:'subst
  ):OpaType.Unification.result =
   continue(l1,l2,subst) = unifiable_list_with_vars(cmp,unifiable_var,unifiable_elmt,v1,v2,l1,l2,subst)
   raw_unifiable_var_and_continue(v,e,nl1,nl2) =
     match v
       {some=v} -> unifiable_var(v,e,subst) &&& continue(nl1,nl2,_)
       {none} -> {failure=[]}
      end

   unifiable_var_and_continue(side,v1,v2,e1,e2,l1,l2,nl1,nl2) =
     if side then raw_unifiable_var_and_continue(v2,e1,nl1,l2)
             else raw_unifiable_var_and_continue(v1,e2,l1,nl2)

   match (l1,l2)
    ([e1|nl1],[e2|nl2]) ->
      match cmp(e1,e2)
      {eq} -> unifiable_elmt(e1,e2,subst) &&& continue(nl1,nl2,_)
      _ as neq -> unifiable_var_and_continue(neq=={lt},v1,v2,e1,e2,l1,l2,nl1,nl2)
      end
    ([],[]) -> {success=subst}
    ([e|nl1],[] as nl2)
    ([] as nl1,[e|nl2]) -> unifiable_var_and_continue(l2==[],v1,v2,e,e,l1,l2,nl1,nl2)
    end


  compare_field(f1:OpaType.field,f2:OpaType.field) = String.compare(f1.label,f2.label)

  unifiable_rowvar(_v,_f,_subst):OpaType.Unification.result =
    do @assert(false) // do something with v and f
    @fail("unifiable_rowvar")

  unifiable_row_with_vars(r1:OpaType.fields,r2:OpaType.fields,v1:option(OpaType.rowvar),v2:option(OpaType.rowvar),subst) =
    unifiable_list_with_vars(compare_field,unifiable_rowvar,unifiable_field,v1,v2,r1,r2,subst)

  compare_fields(l1:OpaType.fields,l2:OpaType.fields) =
   match l1
   [] -> match l2
     [] -> {eq}
      _  -> {lt}
     end
   [r1|l1] -> match l2
     [] -> {gt}
     [r2|l2] ->
       if r1.label == r2.label then compare_fields(l1,l2)
       else if r1 < r2 then {lt}
       else {gt}
     end
   end

  unifiable_colvar(_v,_r,_subst) =
    do @assert(false) // do something with v and r
   @fail("unifiable_colvar")

  unifiable_sums_with_vars(rl1,rl2,v1,v2,subst) = unifiable_list_with_vars(compare_fields,unifiable_colvar,unifiable_fields,v1,v2,rl1,rl2,subst)

  // must be called each time a structural expansion is used for being unified
  // if a unification requires itself than it is assumed safe
  unifiable_with_cycle_cut(ty1,ty2,subst)=
   old_cycle_detector = subst.cycle_detector
   cycle_detector = CycleDetector.push((ty1,ty2),old_cycle_detector)
   if cycle_detector.detected then
    do println("CUT {OpaType.to_pretty(ty1)} vs {OpaType.to_pretty(ty2)}")
    {success=subst}
   else match unifiable(ty1,ty2,{subst with ~cycle_detector})
     {success=subst} -> {success={subst with cycle_detector=old_cycle_detector}}
     r -> r

  unifiable(ty1,ty2,subst):OpaType.Unification.result =
     do println("UNIFIABLE {OpaType.to_pretty(ty1)} vs {OpaType.to_pretty(ty2)} | {StringMap.size(subst.var)} vars")
//    if ty1===ty2 then {success=subst} else
    r = match (ty1,ty2) with
    /* Named type */
    ({TyName_ident=n1; TyName_args=args1},{TyName_ident=n2; TyName_args=args2}) ->
      unifiable_named(ty1,n1,args1,ty2,n2,args2,subst)

    ({TyName_ident=n1; TyName_args=args1},_ ) -> unifiable_with_cycle_cut(structural_type(ty1,n1,args1),ty2,subst)
    (_, {TyName_ident=n2; TyName_args=args2}) -> unifiable_with_cycle_cut(ty1,structural_type(ty2,n2,args2),subst) //detection here is not mandatory because it is done on the reversed case

    /* Std type */
    ({TyConst=t1}, {TyConst=t2}) ->
      if opa_type_const_eq(t1, t2) then {success=subst} else {failure=[]}


    ({TyArrow_params=p1; TyArrow_res=r1}, {TyArrow_params=p2; TyArrow_res=r2}) ->
       unifiable(r1,r2,subst) &&& unifiable_function_parameters(p1,p2,_)

    /* Tvar */
    ({TyVar=v1}, {TyVar=v2}) ->
      // first call, something may not be completely substituted
      if v1 == v2 then {success=subst} else // terminal case
      (nty1,subst) = substitute_var(v1,ty1,subst) // substitute and optimize substitution
      (nty2,subst) = substitute_var(v2,ty2,subst) // substitute and optimize substitution
      if nty1===ty1 && nty2===ty2 then
          // unify var on core tvar basis (non substituable tvar)
         {success=unify_vars(v1,v2,ty1,ty2,subst)}
      else
         // process on substitued type
         unifiable(nty1,nty2,subst)

    ({TyVar=v1}, _) ->
      (nty1,subst) = substitute_var(v1,ty1,subst) // completely substituted
      unifiable(nty1,ty2,subst)

    (_, {TyVar=v2}) ->
      (nty2,subst) = substitute_var(v2,ty2,subst) // completely substituted
      unifiable(ty1,nty2,subst)


    /* Record */
    ({TyRecord_row=r1}, {TyRecord_row=r2}) -> unifiable_row(r1,r2,subst)
    /* RowVar */
    ({TyRecord_row=r1 TyRecord_rowvar=v1}, {TyRecord_row=r2 TyRecord_rowvar=v2}) -> unifiable_row_with_vars(r1,r2,some(v1),some(v2),subst)
    ({TyRecord_row=r1 TyRecord_rowvar=v1}, {TyRecord_row=r2}) -> unifiable_row_with_vars(r1,r2,some(v1),none,subst)
    ({TyRecord_row=r1}, {TyRecord_row=r2 TyRecord_rowvar=v2}) -> unifiable_row_with_vars(r1,r2,none,some(v2),subst)

    /* Sums */
    ({TySum_col=l1}, {TySum_col=l2}) -> unifiable_sums_with_vars(l1,l2,none,none,subst) // TODO version without vars
    /* ColVar */
    ({TySum_col=l1 TySum_colvar=c1}, {TySum_col=l2; TySum_colvar=c2}) -> unifiable_sums_with_vars(l1,l2,some(c1),some(c2),subst)
    ({TySum_col=l1 TySum_colvar=c1}, {TySum_col=l2}) -> unifiable_sums_with_vars(l1,l2,some(c1),none,subst)
    ({TySum_col=l1}, {TySum_col=l2 TySum_colvar=c2}) -> unifiable_sums_with_vars(l1,l2,none,some(c2),subst)

    /* For alls */
    ({TyForall_quant=q1 TyForall_body=b1}, {TyForall_quant=q2 TyForall_body=b2}) ->
      //instantiate and unify, and verify quantified vars are still distinct and non unified
      (iq1,ty1) = OpaTsc.instantiate_from_quantifier(q1,b1)
      (iq2,ty2) = OpaTsc.instantiate_from_quantifier(q2,b2)
      match unifiable(ty1,ty2,subst) // subst could be clearer from iq1 and iq2
      // in theory we should check now that subst defines a bijection between iq1 and iq2
      {success=subst} ->
       if bijection(iq1.types,iq2.types,proj_var,substitute_var_to_var(_,subst))
       //&& bijection(iq1.rows,iq2.rows,substitute_row_to_row)
       //&& bijection(iq1.cols,iq2.cols,substitute_col_to_col)
       then {success=subst}
       else {failure=[{incompatible_quantification}]}
      f -> f
      end

    //instantiate and unify
    ({TyForall_quant=q1 TyForall_body=b1}, _) ->
      (_iq1,ty1) = OpaTsc.instantiate_from_quantifier(q1,b1)
      unifiable(ty1,ty2,subst)
    (_, {TyForall_quant=q2; TyForall_body=b2}) ->
      (_iq2,ty2) = OpaTsc.instantiate_from_quantifier(q2,b2)
      unifiable(ty1,ty2,subst)

    /* external types, should have been unified on type name basis if unifiable */
    ({TyAbstract}, _)(_,{TyAbstract}) -> {failure=[]}


    ({TyConst=_},_) //checked
    ({TyArrow_params=_ TyArrow_res=_},_)
    ({TyRecord_row=_},_) //checked
    ({TyRecord_row=_ TyRecord_rowvar=_},_)
    ({TySum_col=_},_)
    ({TySum_col=_ TySum_colvar=_},_)
     -> error("")

    end
    match r
    {success=_} -> r
    {failure=l} -> {failure=[{generic=(ty1,ty2)}|l]}


   @private substitute_var_to_var(tv1,subst) =
    match substitute_var_no_opti(tv1,{TyVar=tv1},subst)
    {TyVar=tv2} ->
     if tv1 == tv2 then none
     else some(tv2)
    _ -> none

   @private proj_var =
   |{TyVar=v} -> v
   |_ -> @fail("proj_var")

   @private substitute_col_to_col(_v1,_subst) = @fail("substitute_col_to_col") // TODO after unification
   @private substitute_row_to_row(_v1,_subst) = @fail("substitute_row_to_row") // TODO after unification

   // we assume no subst exists between vars of l1 or vars of l2
   bijection(l1,l2,proj,get_var)=
    up(l)(acc) = Fold.list(l,acc)(v1,(set1,set2)->
      match get_var(v1):option
      {some=v2} ->
       if StringSet.mem(v1,set1) || StringSet.mem(v1,set2)
       && StringSet.mem(v2,set1) || StringSet.mem(v2,set2)
       then
        set1 = StringSet.remove(v1,set1)
        set2 = StringSet.remove(v1,set2)
        set1 = StringSet.remove(v2,set1)
        set2 = StringSet.remove(v2,set2)
        (set1,set2)
       else (set1,set2)
      _ ->  (set1,set2)
    )
    l1 = List.map(proj,l1)
    l2 = List.map(proj,l2)
    set1 = StringSet.From.list(l1)
    set2 = StringSet.From.list(l2)
    (set1,set2) = (set1,set2) |> up(l1) |> up(l2)
    StringSet.is_empty(set1) && StringSet.is_empty(set2)

}}

Unification = {{
  unifiable(ty1,ty2) = P.unifiable(ty1,ty2,P.empty_subst)
}}
