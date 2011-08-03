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
 * Manipulating types of OPA values
 */

/* **************************************************************** */
/* **************************************************************** */
/* **************************************************************** */
/**
 * {1 Module OpaTsc}
 *
 * This module allow to get a type scheme from a name of a type
 *
 * @category introspection
 * @destination ??
 * @author Quentin Bourgerie
 * @stability almost stable
 */

/**
 * {1 Types defined for OpaTsc}
 */

/**
 * Type of type scheme
 */
type OpaTsc.quantifier = {types:list(OpaType.typevar); rows:list(OpaType.rowvar); cols:list(OpaType.colvar)}
@opacapi
type OpaTsc.t = {quantifier : OpaTsc.quantifier; body : OpaType.ty}
type OpaTsc.instantiation = {types:list(OpaType.ty); rows:list(OpaType.row); cols:list(OpaType.col)}

OpaTsc = {{
  /**
   * {2 Get type schemes}
   */

  /**
   * Get a type scheme from a [name] if it exists, else return [none].
   */
  get : string -> option(OpaTsc.t) = %%BslValue.Tsc.get%%

  /**
   * Get a type scheme from a [name] if exists, else make an error.
   */
  get_unsafe(name:string) : OpaTsc.t =
    Option.lazy_default(-> error("OpaTsc.get_unsafe("^name^")"),
      get(name))

  instantiation_type_only(lt:list(OpaType.ty)) : OpaTsc.instantiation =
    {types = lt; rows = []; cols = []}

  /**
   * {2 Manipulation of type schemes}
   */
  @private
  find_assoc(var:string, default:'a, quant:list(string), inst:list('a)) : 'a =
    match (quant, inst) with
    | ([], []) -> default // can happen with foralls
    | ([qh | qt], [ih | it]) ->
      if String.equals(var, qh) then ih
      else find_assoc(var, default, qt, it)
    | _ ->
      error("Try to instantiate with a wrong list of type. Doesn't instantiate " ^ var)

  /**
   * Instantiate a type scheme with list of type.
   * Assumes that list of type has the right length.
   */
  instantiate(inst:OpaTsc.instantiation, tsc:OpaTsc.t) =
    rec aux(t) =
      match (t : OpaType.ty) with
      | {TyConst = _} -> t
      | {TyVar = var} -> find_assoc(var, t, tsc.quantifier.types, inst.types)
      | {~TyArrow_params ~TyArrow_res} ->
        {TyArrow_params = aux_list(TyArrow_params);
         TyArrow_res = aux(TyArrow_res)}
      | {~TyRecord_row} -> {TyRecord_row = aux_fields(TyRecord_row)}
      | {~TyRecord_row ~TyRecord_rowvar} ->
        fields = aux_fields(TyRecord_row)
        row = find_assoc(TyRecord_rowvar, {TyRecord_row=[]; ~TyRecord_rowvar}, tsc.quantifier.rows, inst.rows)
        OpaType.instantiate_row(fields,row)
      | {~TySum_col} -> {TySum_col = aux_fields_list(TySum_col)}
      | {~TySum_col ~TySum_colvar} ->
        fieldss = aux_fields_list(TySum_col)
        col = find_assoc(TySum_colvar, {TySum_col=[]; ~TySum_colvar}, tsc.quantifier.cols, inst.cols)
        OpaType.instantiate_col(fieldss,col)
      | {~TyName_args ~TyName_ident} ->
        {TyName_args = aux_list(TyName_args);
         TyName_ident = TyName_ident}
      | {TyAbstract} -> t
      | {~TyForall_quant ~TyForall_body} ->
        //TODO : Remove quantifier if all instantiate in body??
        {TyForall_quant = TyForall_quant;
         TyForall_body = aux(TyForall_body)}
    and aux_list(list) = List.map(aux, list)
    and aux_fields(fields : list(OpaType.field)) = List.map(field ->
      {label = field.label; ty = aux(field.ty)}, fields)
    and aux_fields_list(l) = List.map(fields -> aux_fields(fields), l)
    match inst with /* when there is no instantiation to be done, we save ourself
                     * the traversal and reallocation of the type */
    | {types=[]; rows=[]; cols=[]} -> tsc.body
    | _ -> aux(tsc.body)

  instantiate_type_only(inst:list(OpaType.ty), tsc:OpaTsc.t) =
    instantiate(instantiation_type_only(inst), tsc)

  /**
   * [implementation(lt, tsc)] returns the implementation of type
   * scheme [tsc] instantiated by the type list [lt].
   */
  implementation(lt : OpaTsc.instantiation, (tsc : OpaTsc.t)) =
    OpaType.implementation(OpaTsc.instantiate(lt, tsc))

}}

/* **************************************************************** */
/* **************************************************************** */
/* **************************************************************** */
/**
 * {1 Module OpaType}
 *
 * Module OpaType defines what is an OPA type and it offers some
 * utils for manipulate this type.
 *
 * @category introspection
 * @destination ??
 * @author Quentin Bourgerie
 * @stability almost stable
 */

/**
 * {1 Interface for module OpaType}
 */
/* disabled for S3:
type OpaType.interface = {{
  /* 1 - String representation */
  to_string : OpaType.ty -> string
  to_pretty : OpaType.ty -> string

  /* 2 - Utilities functions */
  type_of_field : OpaType.fields -> OpaType.Field.label -> option(OpaType.ty)
  type_of_field_unsafe : OpaType.fields -> OpaType.Field.label -> OpaType.ty

  instantiate : list(OpaType.ty) -> OpaTsc.t -> OpaType.ty

  is_void : OpaType.ty -> bool

  /* 3 - Module Field */
  Field : {{
    of_string : string -> OpaType.Field.label
    to_string : OpaType.Field.label -> string
  }}
}}
*/

/**
 * {1 Types defined for OpaType}
 */
/**
 * This type define the type of OPA value.
 *
 * Please note that, for the invariants on this datatype to be valid, they have to
 * be satisfied by Pass_ExplicitInstantiation (for the initial creation of type) and by
 * by every function in the stdlib (at type instantiation for instance).
 *
 * BEWARE: for now, the INVARIANT is probably not satisfied because serialization
 * and unseralization of a type probably shuffles the order.
 * Still, [unifiable] relies on the invariant for now
 */
@opacapi
type OpaType.ty =
  {TyConst : OpaType.ty_const} /
  {TyVar : OpaType.typevar} /
  {TyArrow_params : list(OpaType.ty); TyArrow_res : OpaType.ty} /
  {TyName_args : list(OpaType.ty); TyName_ident : OpaType.ty_ident} /
  {TyAbstract} /
  {TyForall_quant : OpaTsc.quantifier; TyForall_body : OpaType.ty} /
  {TyRecord_row : OpaType.fields} / /* INVARIANT: the lists are sorted by the field names lexixographically
                                     * (and the shorter record is the smaller) */
  {TyRecord_row : OpaType.fields; TyRecord_rowvar : OpaType.rowvar} / /* INVARIANT: same as above */
  {TySum_col : list(OpaType.fields)} / /* INVARIANT: the records are sorted lexicographically based on the
                                        * order on record defined above */
  {TySum_col : list(OpaType.fields); TySum_colvar : OpaType.colvar} /* INVARIANT: same as above */

/* FIXME: use this type to define OpaType.ty instead of copy pasting its definition
 * right now, when trying to do so, the typer stupidly complains that a part of the sum is not a record */
@opacapi
type OpaType.row =
  {TyRecord_row : OpaType.fields} / /* INVARIANT: the lists are sorted by the field names lexixographically
                                     * (and the shorter record is the smaller) */
  {TyRecord_row : OpaType.fields; TyRecord_rowvar : OpaType.rowvar} /* INVARIANT: same as above */

/* FIXME: use this type to define OpaType.ty instead of copy pasting its definition
 * right now, when trying to do so, the typer stupidly complains that a part of the sum is not a record */
@opacapi
type OpaType.col =
  {TySum_col : list(OpaType.fields)} / /* INVARIANT: the records are sorted lexicographically based on the
                                        * order on record defined above */
  {TySum_col : list(OpaType.fields); TySum_colvar : OpaType.colvar} /* INVARIANT: same as above */


/**
 * Type of simple value.
 */
type OpaType.ty_const =
  {TyInt} /
  {TyFloat} /
  {TyString}

opa_type_const_eq(a : OpaType.ty_const, b : OpaType.ty_const) =
  match (a, b) with
  | ({TyInt},{TyInt})
  | ({TyFloat},{TyFloat})
  | ({TyString}, {TyString}) -> true
  | _ -> false
/**
 * Below we have lot of sub types for OpaType.ty.
 *
 * Maybe we can simplify this? Are all these types useful?
 * -- perhaps more readable and we may have to change it later from string
 *    to something more complex, similar to QmlAst
 */
type OpaType.field = {label : OpaType.Field.label; ty : OpaType.ty}
type OpaType.fields = list(OpaType.field)
@opacapi
type OpaType.typevar = string
type OpaType.colvar = string
type OpaType.rowvar = string
type OpaType.ty_ident = string

/**
 * Type of a label of a field of a record.
 */
type OpaType.Field.label = string


OpaType = {{

  /**
   * {2 String representation}
   */

  /**
   * Returns a string representation of [ty].
   */
  to_string(ty) =
    rec aux_ty_const(ty_const : OpaType.ty_const) =
      match ty_const with
      | {TyInt} -> "TyInt"
      | {TyFloat} -> "TyFloat"
      | {TyString} -> "TyString"
    and aux_list(list) =
      List.fold(
        (ty, acc -> acc ^ "{aux(ty)}; "),
       list, "[") ^ "]"
    and aux_fields(fields) =
      List.fold(
        (field, acc -> acc ^ "{field.label} : {aux(field.ty)}; "), //sugar
       fields, "[") ^ "]"
    and aux_fields_list(lfields) =
      List.fold((item, acc -> acc ^ aux_fields(item) ^ "; "),
        lfields, "[") ^ "]"
    and aux(ty : OpaType.ty) =
      match ty with
      | {~TyConst} -> "\{TyConst = {aux_ty_const(TyConst)}}"
      | {~TyVar} -> "\{TyVar = {TyVar}}"
      | {~TyArrow_params ~TyArrow_res} -> "\{TyArrow_params = {aux_list(TyArrow_params)}; TyArrow_res = {aux(TyArrow_res)}}"
      | {~TyRecord_row} -> "\{TyRecord_row = {aux_fields(TyRecord_row)}}"
      | {~TyRecord_row ~TyRecord_rowvar} -> "\{TyRecord_row = {aux_fields(TyRecord_row)}; TyRecord_rowvar = {TyRecord_rowvar}}"
      | {~TySum_col} -> "\{TySum_col = {aux_fields_list(TySum_col)}}"
      | {~TySum_col ~TySum_colvar} -> "\{TySum_col = {aux_fields_list(TySum_col)}; TySum_colvar = {TySum_colvar}}"
      | {~TyName_args ~TyName_ident} -> "\{TyName_args = {aux_list(TyName_args)}; TyName_ident = {TyName_ident}}"
      | {TyAbstract} -> "\{TyAbstract}"
      | {~TyForall_quant ~TyForall_body} ->

        "\{TyForall_quant = {aux_quant(TyForall_quant)}; TyForall_body = {aux(TyForall_body)}}"
    and aux_quant(~{types rows cols}:OpaTsc.quantifier) =
      "vars:" ^ List.to_string(types) ^ ",rows:" ^ List.to_string(rows) ^ ",cols:" ^ List.to_string(cols)
    aux(ty)

  to_pretty_fields(fields : list(OpaType.field)) =
    List.fold((field, (acc, pre) ->
      (acc ^ pre ^ Field.to_string(field.label) ^ " : " ^ to_pretty(field.ty), "; ")),
      fields, ("", "")).f1

  to_pretty_lfields(lfields) =
    List.fold((field, (acc, pre) ->
      (acc ^ pre ^ "\{" ^ to_pretty_fields(field) ^ "}", " / ")),
      lfields, ("", "")).f1

  /**
   * Returns a pretty string representation of [ty].
   * do not use magic_to_string and other magic functions, or we get a cycle
   */
  to_pretty(ty : OpaType.ty)=
    rec aux_ty_const(ty_const : OpaType.ty_const) =
      match ty_const with
      | {TyInt} -> "int"
      | {TyFloat} -> "float"
      | {TyString} -> "string"
    //TODO: review and simplify the 2 functions below
    and aux_list(list) =
      elems = List.fold((ty, (acc, pre) ->
        (acc ^ pre ^ aux(ty), " ,")),
        list, ("", "")).f1
      if List.is_empty(list) then "" else "(" ^ elems ^ ")"
    and aux(ty : OpaType.ty) =
      match ty with
      | {TyConst = x} -> aux_ty_const(x)
      | {TyVar = var } -> var
      | {~TyArrow_params ~TyArrow_res} ->
        aux_list(TyArrow_params) ^ " -> " ^ aux(TyArrow_res)
      | {~TyRecord_row} -> "\{" ^ to_pretty_fields(TyRecord_row) ^ "}"
      | {~TyRecord_row TyRecord_rowvar = _} ->
        "\{" ^ to_pretty_fields(TyRecord_row) ^ "; ...}"
      | {~TySum_col} -> to_pretty_lfields(TySum_col)
      | {~TySum_col TySum_colvar = _} -> to_pretty_lfields(TySum_col) ^ " / __"
      | {~TyName_args ~TyName_ident} -> TyName_ident ^ aux_list(TyName_args)
      | {TyAbstract} -> "abstract"
      | {~TyForall_quant ~TyForall_body} ->
        "forall(" ^ aux_quant(TyForall_quant) ^ ")." ^ aux(TyForall_body)
    and aux_quant(~{types rows cols}:OpaTsc.quantifier) =
      match (rows, cols) with
      | ([], []) -> List.to_string(types)
      | _ -> List.to_string(types) ^ ",rows:" ^ List.to_string(rows) ^ ",cols:" ^ List.to_string(cols)
      end
    aux(ty)

  /**
   * {2 Utilities functions}
   */

  /**
   * Get the type of field [name] in fields.
   * If field [name] doesn't exists returns [none], else [some] with
   * a type.
   */
  type_of_field(fields:OpaType.fields, field_label:OpaType.Field.label) =
      Option.map(c -> c.ty,
        List.find((field2 ->
                     String.equals(field_label, field2.label)), fields))


  /**
   * Unsafe type_of_field. Make an error if field [name] doesn't
   * exists else return type of this field.
   */
  type_of_field_unsafe(fields, name) =
    Option.lazy_default(-> error("[type_of_field_unsafe] type \{" ^ String.of_list(x -> x.label, " ", fields) ^ "} doesn't offer any field named " ^ name),
      type_of_field(fields, name))

  /**
   * Returns encapsulated type of a named type
   */
  type_of_name(ident, (args : list(OpaType.ty))) =
    OpaTsc.instantiate_type_only(args, OpaTsc.get_unsafe(ident))

  /**
   * Get implementation of the given type. i.e. traverse [TyName].
   *
   * Note : If that should be returns {TyAbstract} instead returns the
   * named type that encapsulate {TyAbstract} (if it possible...)
   */
  implementation(ty : OpaType.ty) =
    rec aux(ty : OpaType.ty) =
      match ty with
      | {TyName_ident = ident; TyName_args = args} ->
        match type_of_name(ident, args) with
        | {TyAbstract} -> ty
        | ty -> aux(ty)
        end
      | _ -> ty
    aux(ty)

  /**
   * Get the fields type from a [value] and a list of fields
   * [lfields]. [lfields] must be ordered else cause a runtime
   * error. Usefull for retrieve type of a record when [@typeof] return
   * a type sum.
   *
   * @return (fields corresponding to the value, position on [lfields])
   *
   */
  fields_of_fields_list(value, (lfields : list(OpaType.fields))) =
    /* Attribute a number to candidates*/
    (_, lfields, candidates) = List.fold(
        (fields, (i, lfields, candidates) ->
          (i+1, fields +> lfields, (fields, i) +> candidates)
        ), lfields, (0, [],[]))
    /* Filter candidates with fields names of record */
    (u, c) = OpaValue.Record.fold(
      (field, _, (unconstructed, candidates) ->
        name = OpaValue.Record.name_of_field_unsafe(field)
        rec aux((u : list), c, (uacc, cacc)) =
          match u with
          | [] -> (uacc, cacc)
          | [fields | tlfields] ->
            match fields with
            | [] -> aux(tlfields, List.tail(c), (uacc, cacc))
            | [hd | tl] ->
              if String.equals(hd.label, name) then
                aux(tlfields, List.tail(c), (tl +> uacc, List.head(c) +> cacc))
              else aux(tlfields, List.tail(c), (uacc, cacc))
        aux(unconstructed, candidates, ([], []))
      ), value, (lfields, candidates))
    /* Remove candidates that contains too mush fields */
    rec aux((u : list), (c : list), (acc : list)) =
      match u with
      | [] -> acc
      | [hd | tl] ->
        match hd with
        | [] -> aux(tl, List.tail(c), List.head(c) +> acc)
        | _ -> aux(tl, List.tail(c), acc)
    /* Check if we have only one candidates */
    match aux(u, c, []) with
    | [ res | []] -> res
    | _ -> error("[OpaType.fields_of_fields_list] Can't select a row for :\n" ^
                 "value (dump): "^Debug.dump(value) ^
                 "list fields : "^to_pretty_lfields(lfields))

  /**
   * Return [true] if the given type represents a [void] value.
   */
  is_void =
    | {TyName_ident = "void"; TyName_args = _}
    | {TyRecord_row = []}
    | {TyRecord_row = []; TyRecord_rowvar = _}
    | {TySum_col = [[]]}
    | {TySum_col = [[]]; TySum_colvar = _} -> true
    | {TyName_args = args; TyName_ident = ident} -> is_void(type_of_name(ident, args))
    | _ -> false

  /**
   * {2 Module OpaType.Field}
   *
   * This module provides some functions for manipulate value of
   * type [OpaType.Field.label]. This field in [OpaType] is not the
   * same that the field in [OpaValue].
   */
  Field = {{

    /** Get the field from a string. */
    of_string(str) = str

    /** Get the name of given field. */
    to_string(field) = field

 }}

  compare_field(h1:OpaType.field, h2:OpaType.field) : Order.comparison = String.compare(h1.label,h2.label)
  compare_record(l1:OpaType.fields,l2:OpaType.fields) = List.compare(compare_field,l1,l2)

  merge_fields(fields1:OpaType.fields, fields2:OpaType.fields) : OpaType.fields =
    List.merge_with_comparison(compare_field,fields1,fields2)
  merge_fieldss(fieldss1:list(OpaType.fields), fieldss2:list(OpaType.fields)) : list(OpaType.fields) =
    List.merge_with_comparison(compare_record,fieldss1,fieldss2)

  instantiate_row(fields1,tyrow) =
    match tyrow with
    | {TyRecord_row = fields2; TyRecord_rowvar = var} ->
      {TyRecord_row = merge_fields(fields1,fields2); TyRecord_rowvar = var}
    | {TyRecord_row = fields2} ->
      {TyRecord_row = merge_fields(fields1,fields2)}
  instantiate_col(fieldss1,tycol) =
    match tycol with
    | {TySum_col = fieldss2; TySum_colvar = var} ->
      {TySum_col = merge_fieldss(fieldss1,fieldss2); TySum_colvar = var}
    | {TySum_col = fieldss2} ->
      {TySum_col = merge_fieldss(fieldss1,fieldss2)}

}} /* disabled for S3: : OpaType.interface */

@opacapi OpaType_instantiate_row = OpaType.instantiate_row
@opacapi OpaType_instantiate_col = OpaType.instantiate_col
