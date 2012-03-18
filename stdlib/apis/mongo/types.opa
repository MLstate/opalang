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
package stdlib.apis.mongo

/**
 * MongoDB binding for OPA.
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * Module [MongoTypeSelect] is a low-level module providing support
 * for the [MongoSelect] and [MongoUpdate] modules although it has
 * uses elsewhere, for example in [MongoView].
 *
 * These routines are not documented because they are principally
 * for internal use within the Mongo drivers.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/* Type support */

MongoTypeSelect = {{

  @private ML = MongoLog

  /** Abbreviations for common types **/
  tempty = {TyRecord_row=[]}
  tvar(tv) = {TyRecord_row=[]; TyRecord_rowvar=tv}
  istvar(ty) = match ty with | {TyRecord_row=[]; TyRecord_rowvar=_} -> true | _ -> false
  tvoid = {TyName_args=[]; TyName_ident="void"}
  tint = {TyConst={TyInt={}}}
  tint32 = {TyName_args=[]; TyName_ident="int32"}
  tint64 = {TyName_args=[]; TyName_ident="int64"}
  tstring = {TyConst={TyString={}}}
  tfloat = {TyConst={TyFloat={}}}
  tbool = {TyName_args=[]; TyName_ident="bool"}
  tnumeric = {TyName_args=[]; TyName_ident="Bson.numeric"} // pseudo type
  tdate = {TyName_args=[]; TyName_ident="Date.date"}
  toid = {TyName_args=[]; TyName_ident="Bson.oid"}
  tbinary = {TyName_args=[]; TyName_ident="Bson.binary"}
  tregexp = {TyName_args=[]; TyName_ident="Bson.regexp"}
  tcode = {TyName_args=[]; TyName_ident="Bson.code"}
  tsymbol = {TyName_args=[]; TyName_ident="Bson.symbol"}
  tcodescope = {TyName_args=[]; TyName_ident="Bson.codescope"}
  ttimestamp = {TyName_args=[]; TyName_ident="Bson.timestamp"}
  tvalue = {TyName_args=[]; TyName_ident="Bson.value"}
  telement = {TyName_args=[]; TyName_ident="Bson.element"}
  tdoc = {TyName_args=[]; TyName_ident="Bson.document"}

  /** Constructor for more complex types **/
  ttup2(ty1:OpaType.ty,ty2:OpaType.ty):OpaType.ty = {TyName_args=[ty1, ty2]; TyName_ident="tuple_2"}
  tlist(ty:OpaType.ty):OpaType.ty = {TyName_args=[ty]; TyName_ident="list"}
  trec(label, ty) = {TyRecord_row=[~{label; ty}]}

  /** Sort a record by field name **/
  tsortrec(ty) =
    match ty with
    | {TyRecord_row=row; ...} -> {ty with TyRecord_row=List.sort_by((r -> r.label),row)}
    | ty -> ty

  /** Field sets (used in following analysis **/
  order_field(f1, f2): Order.ordering = String.ordering(f1.label,f2.label)
  FieldSet = Set_make(((Order.make(order_field):order(OpaType.field,Order.default))))

  /** Set difference, bizarrely missing from Set module. **/ 
  diff(s1,s2) = FieldSet.fold(FieldSet.remove,s2,s1)

  /** Overlay two types, matching and merging sub-types **/
  tmrgrecs(rec1, rec2) =
    if rec1 == rec2 || rec2 == tempty
    then rec1
    else if rec1 == tempty
    then rec2
    else
      match (rec1,rec2) with
      | ({TyRecord_row=row1},{TyRecord_row=row2}) ->
        s1 = FieldSet.From.list(row1)
        s2 = FieldSet.From.list(row2)
        i = FieldSet.intersection(s1,s2)
        if FieldSet.is_empty(i)
        then {TyRecord_row=List.sort_by((r -> r.label),List.flatten([row1,row2]))}
        else
          ii = FieldSet.fold((f, l ->
                                match (FieldSet.get(f,s1),FieldSet.get(f,s2)) with
                                | ({some=f1},{some=f2}) -> [{label=f1.label; ty=tmrgrecs(f1.ty,f2.ty)}|l]
                                | _ -> @fail/*Can't happen*/),i,[])
          d = FieldSet.To.list(FieldSet.union(diff(s1,s2),diff(s2,s1)))
          res = {TyRecord_row=List.sort_by((r -> r.label),List.flatten([ii,d]))}
          res
      | _ ->
        rec1str = OpaType.to_pretty(rec1)
        rec2str = OpaType.to_pretty(rec2)
        ML.fatal("TypeSelect.tmrgrecs","Attempt to merge non-record types {rec1str} and {rec2str}",-1)

  /** Add a row to a column **/
  taddcol(cty,row) =
    match (cty,row) with
    | ({TySum_col=cols},{TyRecord_row=row}) -> {TySum_col=[row|cols]}

  /** Predicate for field included in row **/
  in_row(label,row) = List.exists((f -> f.label == label),row)

  /** Find a named label in a column **/
  find_label_in_col(label,col) = List.find((crow -> in_row(label,crow)),col)

  /** Find a row in column (all row fields must be present and in order) **/
  find_row_in_col(row,col) =
    all_in_row(row1,row2) = List.for_all((f -> in_row(f.label,row2)),row1)
    List.find((crow -> all_in_row(row,crow)),col)

  /** Naive type compare.  No fancy caching but it works on broken types. **/
  rec naive_type_compare(ty1:OpaType.ty, ty2:OpaType.ty): bool =
    compare_consts(c1,c2) =
      (match (c1,c2) with
       | ({TyInt={}},{TyInt={}}) -> true
       | ({TyString={}},{TyString={}}) -> true
       | ({TyFloat={}},{TyFloat={}}) -> true
       | _ -> false)
    compare_rows(row1,row2) =
       (match List.for_all2((f1, f2 -> f1.label == f2.label && naive_type_compare(f1.ty,f2.ty)),row1,row2) with
        | {result=tf} -> tf
        | _ -> false)
    match (ty1,ty2) with
    | ({TyConst=const1},{TyConst=const2}) -> compare_consts(const1,const2)
    | ({TyName_args=[]; TyName_ident=i1},{TyName_args=[]; TyName_ident=i2}) -> i1 == i2
    | ({TyName_args=a1; TyName_ident=i1},_) -> naive_type_compare(OpaType.type_of_name(i1, a1),ty2)
    | (_,{TyName_args=a2; TyName_ident=i2}) -> naive_type_compare(ty1,OpaType.type_of_name(i2, a2))
    | ({TyRecord_row=row1 ...},{TyRecord_row=row2 ...}) -> compare_rows(row1,row2)
    | ({TySum_col=col1 ...},{TySum_col=col2 ...}) ->
       (match List.for_all2((r1, r2 -> compare_rows(r1,r2)),col1,col2) with | {result=tf} -> tf | _ -> false)
    | _ -> ML.fatal("TypeSelect.naive_type_compare","Can't compare {OpaType.to_pretty(ty1)} and {OpaType.to_pretty(ty2)}",-1)

  /** Extract the type from a named type (not recursively) **/
  name_type(ty:OpaType.ty): OpaType.ty =
    match ty with
    | {TyName_args=tys; TyName_ident=tyid} -> OpaType.type_of_name(tyid, tys)
    | ty -> ty

  /** Map a function over the types of the fields in all records **/
  rec map_field(ty, f) =
    map_row(row) = List.map((fld -> f({fld with ty=map_field(fld.ty, f)})),row)
    match ty with
    | {TyRecord_row=row}
    | {TyRecord_row=row; TyRecord_rowvar=_} -> {TyRecord_row=map_row(row)}
    | {TySum_col=col}
    | {TySum_col=col; TySum_colvar=_} -> {TySum_col=List.map(map_row,col)}
    | {TyName_args=tys; TyName_ident=tyid} -> map_field(OpaType.type_of_name(tyid, tys), f)
    | ty -> ty

  /** Filter the records in a type **/
  @private
  rec filter_field_(names:list(string), ty, f) =
    filter_row(row) =
      rec aux(row) =
        match row with
        | [fld|rest] ->
          flds = aux(rest)
          names = List.flatten([names,[fld.label]])
          (not_empty,ty) = filter_field_(names, fld.ty, f)
          if not_empty || f(names) then [{fld with ~ty}|flds] else flds
        | [] -> []
      aux(row)
    match ty with
    | {TyRecord_row=row}
    | {TyRecord_row=row; TyRecord_rowvar=_} ->
       frow = filter_row(row)
       (frow != [],{TyRecord_row=frow})
    | {TySum_col=col}
    | {TySum_col=col; TySum_colvar=_} -> 
       (match List.filter((r -> r != []),List.map(filter_row,col)) with
        | [] -> (true,{TyRecord_row=[]})
        | [r] -> (false,{TyRecord_row=r})
        | col -> (false,{TySum_col=col}))
    | {TyName_args=tys; TyName_ident=tyid} -> filter_field_(names, OpaType.type_of_name(tyid, tys), f)
    | ty -> (false,ty)

  filter_field(ty, f) = (filter_field_([], ty, f)).f2

  /** Expand all dot notation in a top-level record.
   *  Does not recurse through named types.
   **/
  explode_dot(ty:OpaType.ty): OpaType.ty =
    explode_row(row) =
         List.map((f -> 
                   match String.explode(".",f.label) with
                   | [] | [_] -> f
                   | [dot|dots] ->
                      rec aux(dots) =
                        match dots with
                        | [] -> @fail // can't happen
                        | [label] -> {TyRecord_row=[{~label; ty=(f.ty:OpaType.ty)}]}
                        | [label|dots] -> {TyRecord_row=[{~label; ty=aux(dots)}]}
                      {label=dot; ty=aux(dots)}),row)
    match ty with
    | {TyRecord_row=row} -> {TyRecord_row=explode_row(row)}
    | {TyRecord_row=row; TyRecord_rowvar=rowvar} -> {TyRecord_row=explode_row(row); TyRecord_rowvar=rowvar}
    | _ -> ty

  /**
   * Find a label in either a record or a sum type, with name expansion.
   **/
  find_label_in_row(ty, label) =
    match ty with
    | {TyRecord_row=row}
    | {TyRecord_row=row; TyRecord_rowvar=_} -> List.find((f -> f.label == label),row)
    | {TySum_col=col}
    | {TySum_col=col; TySum_colvar=_} ->
      List.fold((r, a ->
                 if Option.is_none(a)
                 then
                   match List.find((f -> f.label == label),r) with
                   | {some=l} -> {some=l}
                   | {none} -> a
                 else a),col,{none})
    | {TyName_args=tys; TyName_ident=tyid} -> find_label_in_row(OpaType.type_of_name(tyid, tys), label)
    | _ -> {none}

}} /* End of type support */

// End of file types.opa
