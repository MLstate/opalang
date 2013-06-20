
PgSerialize = {{

  unserialize_basic(str, ty):outcome('a, string) =
    match Json.deserialize(str) with
    | {none} -> {failure="invalid json:{str}"}
    | {some=json} -> unserialize_json_basic(json, ty)

  unserialize_json_basic(unser, ty) =
    magic_success(v) = {success=Magic.id(v)}

    /* Function for record **********************/
    rec aux_rec(js_lst:list, fields:OpaType.fields) =
      match fields with
      | [{label=name; ty=ty}] ->
        if OpaType.is_void(ty) then
          /* build the optimized representation */
          match OpaValue.Record.field_of_name(name) with
          | {none} -> {failure="No backend field '{name}'"}
          | {some=field} -> {success=OpaValue.Record.make_simple_record(field)}
          end
        else
          aux_rec_unoptimized(js_lst,fields)
      | _ ->
        aux_rec_unoptimized(js_lst,fields)

    and aux_rec_unoptimized(js_lst, fields) =
      res = (List.foldr(_,js_lst, (OpaValue.Record.empty_constructor(), fields, {}))){
        (name, json), (acc, fields, err) ->
        match err with
        | {failure=_} -> (acc, [], err)
        | _ -> match fields with
          | [] -> (acc, [], {failure="Superfluous field '{name}' in json"})
          | [hd | tl] ->
            if not(@toplevel.String.equals(hd.label,name)) then
             (acc, [],
              if name < hd.label then
                {failure="Superfluous field '{name}' in json"}
              else
               {failure="Missing field '{name}' in json"})
            else match aux(json, hd.ty) with
            | {failure=_} as f -> (acc, [], f)
            | {success=value} ->
              match OpaValue.Record.field_of_name(name) with
              | {none} -> (acc, [], {failure="No backend field '{name}'"})
              | {some = field} -> (OpaValue.Record.add_field(acc, field, value), tl, err)
          }
        match res.f2 with
        | [_ | _] as fields ->
          {failure="Missing json fields '{OpaType.to_pretty_fields(fields)}'"}
        | _ ->
          match res.f3 with
          | {failure=_} as f -> f
          | _ -> {success=OpaValue.Record.make_record(res.f1)}

    /* For list, slow but tail-rec
       construct a couple (should be removed automatically)
       and an option (need to have a special folder)
     */
    and aux_list(l,ty_arg_opt)=
      match ty_arg_opt
      | {some=ty_arg}->
          l=List.foldr(elmt, l ->
            match l with
            | {failure=_} -> l
            | {success=l} -> match aux(elmt,ty_arg) with
              | {failure=_} as f -> f
              | {success=hd} -> {success=[hd|l]}
            , l, {success=[]})
          Magic.id(l)
      | {none} -> {failure="Can't find type argument of list"}

    /* Main aux function ************************/
    and aux(json:RPC.Json.json, ty:OpaType.ty) =
      match (json, ty) with
      /* Basic case *****************************/
      | ({Int = value}, {TyConst = {TyInt}})
      | ({Float = value}, {TyConst = {TyFloat}})
      | ({String = value}, {TyConst = {TyString}})
      | ({Bool = value}, {TyName_args = []; TyName_ident = "bool"}) -> magic_success(value)

      /* Degenerate float case ******************/
      | ({~String} , {TyConst = {TyFloat}}) ->
        match String
        | "Infinity"  -> magic_success(1.0 / 0.0) // temporary hack break Math dependencies
        | "-Infinity" -> magic_success(-1.0 / 0.0)
        | "NaN"       -> magic_success(0.0 / 0.0)
        | _ -> {failure="Float vs '{String}'"}
        end
      | ({Int = value}, {TyConst = {TyFloat}}) -> magic_success(Float.of_int(value))

      /* Record case ****************************/
      | ({Record = js_lst}, {TyRecord_row = row})
      | ({Record = js_lst}, {TyRecord_row = row; TyRecord_rowvar = _}) ->
        aux_rec(js_lst, row)
      | ({Record = js_lst}, {TySum_col = lfields})
      | ({Record = js_lst}, {TySum_col = lfields; TySum_colvar = _}) ->
        ltyfield =
          List.fold(((name, _), acc ->
            OpaType.Field.of_string(name) +> acc),
            js_lst, [])
        match OpaType.fields_of_fields_list2(ltyfield, lfields) with
        | {none} -> {failure="fields '{OpaType.to_pretty_lfields(lfields)}' are not found in type sum ({OpaType.to_pretty(ty)})"}
        | {some = fields} -> aux_rec(js_lst, fields)
        end

      /* List case ******************************/
      | ({List = []}, _ ) -> magic_success([]) // Unsafe !!

      | ({List = l}, {TyName_ident = "list"; TyName_args = [ty_arg]}) ->
        aux_list(l, some(ty_arg) )

      /* List case when deserialization case is not a record type */
      | ({List = l}, {TyRecord_row = row ...}) ->
        aux_list(l, OpaType.type_of_field(row, OpaType.Field.of_string("hd")) )
      | ({List = l}, {TySum_col = col ...}) ->
        match OpaType.fields_of_fields_list2(["hd","tl"], col) with
        | {none} ->
          {failure="fields hd and tl are not found in type sum ({OpaType.to_pretty(ty)})"}
        | {some = row} ->
          aux_list(l, OpaType.type_of_field(row,OpaType.Field.of_string("hd")) )
        end

      /* List case when serialization type is not list */
      | ({Record = _}, {TyName_ident = "list"; TyName_args = [ty_arg]}) ->
         rec record_to_list(r,acc)=
           match r:RPC.Json.json
           | {Record = js_lst} ->
             match List.assoc("hd",js_lst) with
             | {some=hd} ->
               match aux(hd,ty_arg) with
               | {success=hd} ->
                 match List.assoc("tl",js_lst) with
                 | {some=tl} -> record_to_list(tl , [hd|acc])
                 | {none} -> {failure="missing tl field"}
                 end
               | {failure=_} as f -> f
               end
             | {none} -> magic_success(List.rev(acc))
             end
           | _ -> {failure="mixed record/list case"}
           end
         record_to_list(json, [])

      /* Json *************************************/
      | (_, {TyName_ident = "OpaSerialize.unser"; TyName_args = _})
      | (_ ,{TyName_ident = "RPC.Json.json"; TyName_args = _}) ->
        magic_success(json)

      /* Encapsulated types ***********************/
      | (_, {TyName_args = args; TyName_ident = ident}) ->
        aux(json, OpaType.type_of_name(ident, args))
      | (_, {TyForall_quant = _; TyForall_body = body}) ->
        aux(json, body)

      /* Error case *****************************/
      | _ ->
        {failure="can't unserialize type {OpaType.to_pretty(ty)} with value {Json.to_string(json)}"}
    aux(unser, ty)

}}
