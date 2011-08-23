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
   Types describing one declaration api
*/

OpaApiFile = {{
     extension = "api" // add to opa filename
     from_api_file(fname)=
         JsonFile.open_api(fname):list(Api.entry)

     indent_api(api) =
       l = String.explode("\n", api)
       l = List.map(Xhtml.of_string, l)
       <>{ List.intersperse(<br />, l) }</>

  @private basename = %% BslFile.basename %% : string -> option(string)

  /**
   * Build the association from types to values hyperlink, and entries by path_name
  **/
  extract_by_path(type_table : Join.use_type(Entry.html), api_l) =
    aux((type_table, acc) as tuple_acc, entry : Api.entry) =
      if OpaDocUtils.is_private(entry) then tuple_acc else
      path_name = String.concat(".", entry.path)
      path_html = OpaDocUtils.sanitize_path("{entry.pkg}.{path_name}")
      entry_html = (path_name, (entry, path_html, basename(entry.fname) ? error("extract_by_path basename"))) : Entry.html
      type_table =
        match entry.code_elt with
        | { value = { ty = opatype ; ... } } ->
          extract_value_types(type_table, entry_html, opatype)
        | { type_def=_ } -> type_table
        end
      acc = entry_html +> acc
      (type_table, acc)
    List.fold_left(aux, (type_table, []), api_l)

       /**
        * Extract and add types to a types table
        */
       extract_value_types(table, val, ty:Api.ty) =
         associate_id_val(id, val, table) =
           match StringMap.get(id, table) with
           | { none } -> StringMap.add(id, [val], table)
           | { some=l } ->
             match List.find((v->v==val), l) with
             | {none} -> StringMap.add(id, [val | l], table)
             | {some=_} -> table
             end
           end
         rec aux_ty_const(table, ty_const) =
           match ty_const with
           | {TyInt} -> associate_id_val("int", val, table)
           | {TyFloat} -> associate_id_val("float", val, table)
           | {TyString} -> associate_id_val("string", val, table)
         and aux_list(table, list) =
           List.fold((ty, table ->
             aux(table, ty)), list, table)
         and aux_fields(table, fields) =
           List.fold((field, table ->
             aux(table, field.ty)), fields, table)
         and aux_fields_list(table, lfields) =
           List.fold((fields, table ->
             aux_fields(table, fields)), lfields, table)
         and aux(table, ty:OpaType.ty) =
           match ty with
           | {~TyConst} -> aux_ty_const(table, TyConst)
           | {TyVar=_} -> table
           | {~TyArrow_params ~TyArrow_res} ->
             table = aux_list(table, TyArrow_params)
             aux(table, TyArrow_res)
           | {~TyRecord_row} -> aux_fields(table, TyRecord_row)
           | {~TyRecord_row TyRecord_rowvar = _} -> aux_fields(table, TyRecord_row)
           | {~TySum_col} -> aux_fields_list(table, TySum_col)
           | {~TySum_col TySum_colvar = _} -> aux_fields_list(table, TySum_col)
           | {TyName_args=[] ~TyName_ident} -> associate_id_val(TyName_ident, val, table)
           | {~TyName_args ~TyName_ident} ->
             table = associate_id_val(TyName_ident, val, table)
             aux_list(table, TyName_args)
           | {TyAbstract} -> table
           | { TyForall_quant=_ TyForall_body=body } -> aux(table, body)
           end
         aux_sugar(table, ty:Api.ty) =
           match ty with
           | {~TySumSugar} -> aux_list(table, TySumSugar)
           | ty -> aux(table, Magic.id(ty):OpaType.ty)
           end
         aux_sugar(table, ty)

  }}
