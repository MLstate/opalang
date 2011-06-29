/*
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
*/

/**
 * Comparison
 */

import stdlib.core.map

/**
 * {1 About this module}
 *
 * This module implements comparison using runtime type.
 *
 * The implementation separate matching type and value to provide efficient
 * comparison.
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

/*
 * {1 Types defined in this module}
 */

@private
type input = external


@private
type P.implementation('a) = P.postenv,'a,'a->Order.comparison

@private
type P.compare('a) = 'a,'a->Order.comparison

@private
type P.postenv = llarray(P.implementation(input))

@private
type P.preenv = {entries:list(P.entry(input)) next_index:int}

@private
type P.key ={
     name : string
     args : list(OpaType.ty)
}

/**
 * [ty : P.key]
 * The type for the entry
 *
 * [implementation : option(P.implementation('a))]
 * The implementation if already known (during type comparison calculation
 * implementation is not known)
 *
 * [postenv_i  : option(int)]
 * If the unknown implementation need to be, it is affected an index in the
 * env_postmatch_ty, e.g. recursion
 */
@private
type P.entry('a) = {
     ty             : P.key
     implementation : option(P.implementation('a))
     postenv_i      : option(int)
}

@private
type Record.patterns_indexes = external
@private
type Record.fields_indexes = external
@private
type Record.field_index = external

/**
 * {1 Interface}
 */

@both_implem // to duplicate caches
Compare = {{

@private
`@` = LowLevelArray.get

@private @expand
`&&&`(a,b) = if a then {eq} else b

@private @expand
`|||`(a,b) = a=a if a=={eq} then b else a

@private P = ComparePrivate
@private Record = ComparePrivate.Record


compare = @nonexpansive(
  @unsafe_cast(
   compare_front(@typeval('a)): input,input-> Order.comparison
  ): 'a,'a-> Order.comparison
 )

compare_ty = compare_prematch_ty(@typeval(OpaType.ty)):OpaType.ty,OpaType.ty->Order.comparison
order_ty  = Order.make(@unsafe_cast(compare_ty)):order(OpaType.ty,Order.default)

@private compare_char_postenv_(_,  a,b) = compare_char(  a,b)
@private compare_int_postenv_(_,   a,b) = compare_int(   a,b)
@private compare_float_postenv_(_, a,b) = compare_float( a,b)
@private compare_string_postenv_(_,a,b) = compare_string(a,b)


/** Workaround for bad definition of @unsafe_cast */
@private compare_char_postenv   = @nonexpansive(Magic.id(compare_char_postenv_) : P.implementation)
@private compare_int_postenv    = @nonexpansive(Magic.id(compare_int_postenv_)  : P.implementation)
@private compare_float_postenv  = @nonexpansive(Magic.id(compare_float_postenv_)  :P.implementation)
@private compare_string_postenv = @nonexpansive(Magic.id(compare_string_postenv_) :P.implementation)
@private compare_char   = @nonexpansive(Magic.id(@toplevel().compare_char) :'a,'a->Order.comparison)
@private compare_int    = @nonexpansive(Magic.id(@toplevel().compare_int) :'a,'a->Order.comparison)
@private compare_float  = @nonexpansive(Magic.id(@toplevel().compare_float) :'a,'a->Order.comparison)
@private compare_string = @nonexpansive(Magic.id(@toplevel().compare_string) :'a,'a->Order.comparison)

@private always_equal_postenv(_,_,_)   = {eq}




// TODO us something faster than a map
@private cache_ty = {{
  compare_lty(lty1,lty2) =
  match lty1
    [hd1|tl1] ->
      match lty2
      [hd2|tl2] -> compare_ty(hd1,hd2) ||| compare_lty(tl1,tl2) // TODO PATH
      [] -> {lt}
     end
    [] ->
      match lty2
      [] -> {eq}
      _  -> {gt}
    end

  compare_key(a:P.key,b:P.key) = compare_string(a.name,b.name) ||| compare_lty(a.args,b.args)
  order_key =  Order.make(@unsafe_cast(compare_key)):order(P.key,Order.default)
  TyMap = Map_make(order_key)
  cache = Mutable.make(TyMap.empty):Mutable.t(map(P.key,(P.compare(input))))
  get(ty) = TyMap.get(ty,cache.get())
  add(ty,cmp) = cache.set(TyMap.add(ty,cmp,cache.get()))
}}


/*
   Return the comparison function for constant type as fast as possible,
   Remove noop encapsulation of the type
   Cache top type for comparison (other deeper name are locally cached by the
   compare_prematch_ty
   The goal is that the cache contains only type that are compared by compare
   and nothing else (to minimize time of cache access)
*/
@private
compare_front(ty) =
 match ty
 {TyConst = c} -> match c
   {TyInt}    -> @unsafe_cast(compare_int)
   {TyFloat}  -> @unsafe_cast(compare_float)
   {TyString} -> @unsafe_cast(compare_string)
   {TyChar}   -> @unsafe_cast(compare_char)
   end

 {TyName_args = args; TyName_ident = ident} as ty ->
    key = P.key(ident,args)
    cache_ty.get(key)
    ?
      cmp = compare_prematch_ty(ty)
      do cache_ty.add(key,cmp)
      cmp

 {TyForall_quant = _; TyForall_body = impl}
 {TyPrivate_impl = impl; TyPrivate_ghost = _} -> compare_front(impl)
 _ -> compare_prematch_ty(ty)

 @private
 @expand
 field_ty_to_field(f)=OpaValue.Record.field_of_name(f.label)?error("")
 @private fields_ty_to_fields(fields) = LowLevelArray.mapi(fields)(_i,f->field_ty_to_field(f)):llarray(Record.field)


  @private
 compare_prematch_ty(original_ty) =
//    do debug(->"CMP PREMATCH")
    error_ty(ty) = error("Comparing {OpaType.to_pretty(original_ty)} is impossible.\n" ^
                         "Because contains a value of type {OpaType.to_pretty(ty)}.")


    /* For record and sum, take values and fields */
    rec record_gen(fields_ty,fields,fields_indexes,preenv) =
     match LowLevelArray.size(fields)
     0 -> (always_equal_postenv,preenv)
     /* the following optimization is important and should be extended */
     1 -> // sum case record with no information have already been compared physically
          // so it s not necessary to short cut with always_equal_postenv for void empty field
          (field_cmp,preenv) = aux((fields_ty@0).ty,preenv)
          field_index = Record.field_index(fields_indexes,fields@0)
          (Record.compare_field(field_index,field_cmp, _, _, _),preenv)
     2 -> (field_cmp0,preenv) = aux((fields_ty@0).ty,preenv)
          (field_cmp1,preenv) = aux((fields_ty@1).ty,preenv)
          field_index0 = Record.field_index(fields_indexes,fields@0)
          field_index1 = Record.field_index(fields_indexes,fields@1)
          ((postenv,a,b->
               Record.compare_field(field_index0, field_cmp0, postenv, a, b)
           ||| Record.compare_field(field_index1, field_cmp1, postenv, a, b) )
          ,preenv)
     /* feel free to add more optimized cases */
     n ->
      (fields_cmp,preenv) = LowLevelArray.fold_mapi(fields_ty,preenv)(_i,x,preenv->
        aux(x.ty,preenv)
      ) // passer aux arrays
      fields_index = LowLevelArray.mapi(fields)(_i,field-> Record.field_index(fields_indexes,field))
      cmp=Record.compare_fields(n,fields_index,fields_cmp)
      (cmp,preenv)

    and record(fields_ty,preenv) =
      fields = fields_ty_to_fields(fields_ty)
      fields_indexes = fields |> Record.fields_indexes
      record_gen(fields_ty,fields,fields_indexes,preenv)

    /* Perhaps here it's problematics : it depends of the order of sum */
    and sum(col,preenv)=
      col_a = LowLevelArray.of_list_mapi(col)(_i,l->LowLevelArray.of_list(l)) : llarray(llarray(OpaType.field))
      n = LowLevelArray.size(col_a)
      match n
      | 1 -> record(col_a@0,preenv)
      | _ ->
      fields = LowLevelArray.mapi(col_a)(_i,row->fields_ty_to_fields(row))
      fields_indexes = LowLevelArray.mapi(fields)(_i,fields-> Record.fields_indexes(fields))
      patterns_indexes = Record.patterns_indexes(fields_indexes) // O(1)
      (cmps,preenv) = LowLevelArray.fold_mapi(col_a,preenv)(i,fields_ty,preenv->record_gen(fields_ty,fields@i,fields_indexes@i,preenv))
      cmp(postenv,a,b)=
         if a === b then {eq} else
         match ComparePrivate.Record.compare_structure(patterns_indexes,a,b)
         -2 -> {lt}
         -1 -> {gt}
          i -> (cmps@i)(postenv,a,b)
         end
         (cmp,preenv)

    /** named type can be in tree state in preenv
        absent => need to generate something
        present but not known => need to call the futur implementation from postenv
        present and known => directly use implementation of preenv
    */
    and named(ident,args,preenv) =
      key = P.key(ident,args)
      match P.get_entry(key,preenv)
      /* no entry => need to calculate it
         and register it in preenv */
      {none} ->
        preenv = P.add_entry(key,preenv)
        ty_impl = OpaType.type_of_name(ident, args)
        (cmp,preenv) = aux(ty_impl,preenv)
        preenv=P.update_implementation(key,cmp,preenv)
        (cmp,preenv)
      {some=entry} ->
        match entry.implementation
        /* implementation is already being calculated (i.e. recursive type) */
        {none} ->
          (preenv,i) = P.need_env_postmatch(key,preenv)
          cmp=(postenv:P.postenv,a,b->
          (postenv@i)(postenv,a,b))
          (cmp,preenv)
        /* entry is available */
        {some=cmp} -> (cmp,preenv)
        end

     and custom(comparator,args,preenv) =
       nargs = List.length(args)
       #<Ifstatic:OPA_CLOSURE>
       match nargs with
       | 0 -> ((_postenv,a,b -> comparator(a,b)), preenv)
       | _ ->
         (args_cmp,preenv) = List.fold_map(aux,args,preenv)
         cmp(postenv,a,b)=
           clos_arg = OpaValue.Closure.Args.create(nargs+2)
           do List.iteri(
             (i, cmp -> OpaValue.Closure.Args.set(clos_arg, i, cmp(postenv,_,_))
           ), args_cmp)
           // FIXME: we shouldn't be applying an array where the two last
           // indexes are undefined!!!
           f = OpaValue.Closure.apply(@unsafe_cast(comparator), clos_arg)
           f(a,b)
         (cmp,preenv)
       #<Else>
       match nargs with
       | 0 -> ((_postenv,a,b -> comparator(a,b)), preenv)
       | _ -> @fail("Custom comparison is not implemented for cases where List.length(args) > 0 in non closure mode")
       #<End>

    and lazy_error(ty,preenv)=
      cmp(_,_,_) = error_ty(ty)
      (cmp,preenv)

    // TODO call immediate_error in current lazy_error case that are always accessed (not cover by a sum type)
    and _immediate_error(ty) = error_ty(ty)

    /* Main aux function ************************/
    and aux(ty,preenv) =
//      do debug(->"AUX TY={ty}")
      match ty with
      /* Basic case *****************************/
      /* ordered for speed */
      {TyConst = c} ->
       cmp = match c
         {TyInt}    -> @unsafe_cast(compare_int_postenv)
         {TyFloat}  -> @unsafe_cast(compare_float_postenv)
         {TyString} -> @unsafe_cast(compare_string_postenv)
         {TyChar}   -> @unsafe_cast(compare_char_postenv)
         end
       (cmp, preenv)

      /* Record case ****************************/
      {TyRecord_row = row} -> record(LowLevelArray.of_list(row),preenv)
      {TySum_col = col}
      {TySum_col = col; TySum_colvar = _} -> sum(col,preenv)

      /* Named case *****************************/
      {TyName_args = args; TyName_ident = ident} ->
        match (%%BslValue.MagicContainer.compare_get%%)(ident) with
        {none} -> named(ident,args,preenv)
        {some = comparator} -> custom(comparator,args,preenv)
        end

      /* Indirection cases ***********************/
      {TyForall_quant = _; TyForall_body = impl}
      {TyPrivate_impl = impl; TyPrivate_ghost = _} -> aux(impl,preenv)

      /* Maybe error case if called *************/
      {TyRecord_row = _; TyRecord_rowvar = _}
      {TyVar = _} -> lazy_error(ty,preenv)

      /* Unimplemented case *********************/
      {TyArrow_params = _; TyArrow_res = _} -> lazy_error(ty,preenv)

      /* Error case *****************************/
      {TyAbstract} -> lazy_error(ty,preenv)

  (cmp,preenv) = aux(original_ty,P.empty_preenv)
  postenv = P.postenv_i(preenv)
  @unsafe_cast(cmp(postenv,_,_)):'a,'a->Order.comparison

}}


@private ComparePrivate = {{
@private
`@` = LowLevelArray.get

@private @expand
`|||`(a,b) = if a=={eq} then b else a

equal_lty(l1,l2) = l1 === l2 || match List.for_all2(equal_ty,l1,l2) {result=b} -> b _ -> false
equal_field(f1:OpaType.field,f2:OpaType.field) = f1.label==f2.label && equal_ty(f1.ty,f2.ty)
equal_fields(l1,l2) = l1 === l2 ||  match List.for_all2(equal_field,l1,l2) {result=b} -> b _ -> false
equal_sum(s1,s2) = s1 == s2 || match List.for_all2(equal_fields,s1,s2) {result=b} -> b _ -> false

/** basic implementation to bootstrap comparison
    must be fast so it does not assume any optimisation of the backend */
equal_ty(ty1:OpaType.ty,ty2:OpaType.ty) =
  ty1 === ty2 ||
  match ty1
  /* Basic case *****************************/
  /* ordered for speed */
  {TyConst = c1 }    -> // constant sharing probably make the remaining useless
    match ty2
    {TyConst = c2 } ->
      c1 === c2 ||
      match c1
      {TyInt}    -> match c2 {TyInt}    -> true _ -> false end
      {TyString} -> match c2 {TyString} -> true _ -> false end
      {TyFloat}  -> match c2 {TyFloat}  -> true _ -> false end
      {TyChar}   -> match c2 {TyChar}   -> true _ -> false end
      end
    _ -> false
    end

  /* Named case *****************************/
  {TyName_args = args1; TyName_ident = ident1} -> match ty2 {TyName_args = args2; TyName_ident = ident2} -> ident1==ident2 && equal_lty(args1,args2) _ -> false end

  /* Record case ****************************/
  {TyRecord_row = row1}{TyRecord_row = row1; TyRecord_rowvar = _}->
    match ty2  {TyRecord_row = row2} {TyRecord_row = row2; TyRecord_rowvar = _} -> equal_fields(row1,row2) _ -> false end

  {TySum_col = col1}{TySum_col = col1; TySum_colvar = _}     ->
    match ty2  {TySum_col = col2}{TySum_col = col2; TySum_colvar = _}   -> equal_sum(col1,col2) _ -> false end

  /* Indirection cases ***********************/
  {TyForall_quant = _; TyForall_body = impl1} -> match ty2 {TyForall_quant = _; TyForall_body = impl2} -> equal_ty(impl1,impl2) _ -> false end
  {TyPrivate_impl = impl1; TyPrivate_ghost = _}-> match ty2 {TyPrivate_impl = impl2; TyPrivate_ghost = _}-> equal_ty(impl1,impl2) _ -> false end

  {TyVar = _} -> match ty2 {TyVar = _} -> true _ -> false end // ??

  {TyArrow_params = p1; TyArrow_res = r1} -> match ty2 {TyArrow_params = p2; TyArrow_res = r2} -> equal_ty(r1,r2) && equal_lty(p1,p2) _-> false end
  {TyAbstract} -> false
   _ -> false


empty_preenv = {next_index=0 entries=[]}

/* Key construction */
key(name,args)= ~{name args}:P.key

/* Entry construction */
entry(key) = {ty=key implementation=none postenv_i=none} :  P.entry

key_eq(k1:P.key,k2:P.key) = k1.name==k2.name &&  equal_lty(k1.args,k2.args)
// do something faster, '==' use slow comparison

entry_key_eq(k1:P.key,e2:P.entry) = key_eq(k1,e2.ty)

/** Change the implementation associated to a key */
update_implementation(key,implem:P.implementation(input),preenv:P.preenv) =
//  do debug(->"try update_implementation {key}")
  upif(v:P.entry) =
    if key_eq(v.ty,key) then
      match v.implementation
      {none} ->
//        do debug(->"update_implementation {key}")
        {v with implementation=some(@unsafe_cast(implem))}
      {some=_} ->
//        do debug(->"update_implementation twice")
        v
    else v
  entries=List.map(upif,preenv.entries)
  {preenv with ~entries}

open_entry(v:option(P.entry(input)))=@unsafe_cast(v):option(P.entry)

findl(key,list) =
  match list
  [hd|tl] -> if entry_key_eq(key,hd) then some(hd) else findl(key,tl)
  [] -> none

/** Get the entry associated to a key */
get_entry(key,preenv:P.preenv) = open_entry(findl(key,preenv.entries))

/** Set the env_postmatch flag of an entry */
need_env_postmatch(key,preenv:P.preenv) =
  match List.index_p(v -> entry_key_eq(key,v) && v.postenv_i!={none},preenv.entries)
  {some=i} -> (preenv,i)
  {none} ->
  i = preenv.next_index
  upif(v) =
    if entry_key_eq(key,v) then {v with postenv_i=some(i)}
                           else v
  entries=List.map(upif,preenv.entries)
  ({preenv with ~entries next_index=i+1},i)

/**
 * Generate the post env
 * It is an array of comparison implementations, indicated and indexed as
 * specified by the preenv
 */
postenv_i(preenv:P.preenv):P.postenv =
  n = List.fold(e,acc-> match e.postenv_i {none}->acc {some=_}-> acc+1,preenv.entries,0)
  postenv = LowLevelArray.create(n, @unsafe_cast(0)) // TODO
  set(e:P.entry(input)) =
    match e.postenv_i
    {none} -> void
    {some=i} ->
      implem = e.implementation?error("[compare.P.postenv_i] no implementation")
      LowLevelArray.set(postenv,i,implem)
    end
  do List.iter(set,preenv.entries)
  postenv

add_entry(key,preenv:P.preenv):P.preenv =
  entries = [entry(key)|preenv.entries]
  {preenv with ~entries}

preenv_to_string(preenv) =
  preenv.entries
  |> List.map(entry -> "ty = {OpaType.to_string({ TyName_args=entry.ty.args TyName_ident=entry.ty.name})} (in postenv = {entry.postenv_i})", _)
  |> ["PREENV="] ++ _
  |> String.concat("\n",_)

Record = {{
  field_index = %% bslValue.Record.field_index %% : Record.fields_indexes,Record.field -> Record.field_index
  fields_indexes = %% bslValue.Record.fields_indexes %% : llarray(Record.field) -> Record.fields_indexes
  dot_with_field_index =  %% bslValue.Record.dot_with_field_index %% : 'record, Record.field_index -> 'field_content
  patterns_indexes  = %% bslValue.Record.patterns_indexes  %% : llarray(Record.fields_indexes) -> Record.patterns_indexes

  /*
   * If the returned integer is -2 or -1 it is a comparison otherwise it is the
   * index
   */
  compare_structure = %% bslValue.Record.compare_structure %% : Record.patterns_indexes,'record,'record -> int

  /*
   * Comparing fields, should use something faster like llarray instead of list
   * so fold2 and fold should provide field index list((string,('preenv,'a,'a -> 'cmp)))
   */
  @expand
  compare_field(field_i:Record.field_index, field_cmp:P.implementation, postenv, a, b) =
    field_i = field_i
    fa = dot_with_field_index(a,field_i)
    fb = dot_with_field_index(b,field_i)
    field_cmp(postenv,fa,fb)

  compare_fields(n,fields_index, fields_cmp : llarray(P.implementation) )(postenv,a, b) =
    if a === b then {eq} else
    rec aux(nm1,i) =
       r = compare_field(fields_index@i, fields_cmp@i, postenv, a, b)
       if r!={eq} || i==nm1 then r
       else aux(nm1,i+1)
    aux(n-1,0)
}}

}}
