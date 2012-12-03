/*
    Copyright © 2011-2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/**
 * Postgres low-level driver
 *
 * This file provides the type conversion functions for the Postgres API.
 *
 * @category web
 * @author Norman Scaife, 2012
 * @destination public
 * @stability Work in progress
 */

type Postgres.type_id = int // for clarity

type Postgres.smallint = int
//type Postgres.bigint = int64 // ??? not really necessary
type Postgres.real = float

type Postgres.bytea = binary

type Postgres.money = {currency:option(string); amount:float}

type Postgres.numeric = {pre:string; post:string} // TODO: arbitrary precision numbers in Opa

type Postgres.date = Date.date
type Postgres.time = Date.date
type Postgres.timestamp = Date.date
type Postgres.timestamptz = Date.date

type Postgres.opatype =
   {Null}
 / {Int:int}
 / {Int16:int}
 / {Int64:int64}
 / {Bool:bool}
 / {String:string}
 / {Real:float}
 / {Float:float}
 / {Money:Postgres.money}
 / {Numeric:Postgres.numeric}
 / {Date:Date.date}
 / {Time:Postgres.time}
 / {Timestamp:Postgres.timestamp}
 / {Timestamptz:Postgres.timestamptz}
 / {Bytea:binary}
 / {IntArray1:list(int)}
 / {IntArray2:list(list(int))}
 / {IntArray3:list(list(list(int)))}
 / {Int16Array1:list(int)}
 / {Int16Array2:list(list(int))}
 / {Int16Array3:list(list(list(int)))}
 / {Int64Array1:list(int64)}
 / {Int64Array2:list(list(int64))}
 / {Int64Array3:list(list(list(int64)))}
 / {RealArray1:list(float)}
 / {RealArray2:list(list(float))}
 / {RealArray3:list(list(list(float)))}
 / {FloatArray1:list(float)}
 / {FloatArray2:list(list(float))}
 / {FloatArray3:list(list(list(float)))}
 / {StringArray1:list(string)}
 / {StringArray2:list(list(string))}
 / {StringArray3:list(list(list(string)))}
 / {Duration:Duration.duration}
 / {TypeId:(int,Postgres.data)}
 / {BadData:binary}
 / {BadText:string}
 / {BadCode:(int,binary)}
 / {BadDate:string}
 / {BadEnum:list(string)}

type Postgres.oparow = stringmap(Postgres.opatype)

type Postgres.oparowl = list((string,Postgres.opatype))

type Postgres.handler('a) = Postgres.type_id, OpaType.ty, Postgres.data -> option('a)

type Postgres.abstract_handler = Postgres.handler(void)

PostgresTypes = {{

  @private bindump = (%% BslPervasives.bindump %%: binary -> string)

  @private int_to_month(i:int) : Date.month =
    match i with
    | 1 -> {january}
    | 2 -> {february}
    | 3 -> {march}
    | 4 -> {april}
    | 5 -> {may}
    | 6 -> {june}
    | 7 -> {july}
    | 8 -> {august}
    | 9 -> {september}
    | 10 -> {october}
    | 11 -> {november}
    | 12 -> {december}
    | _ -> {january} //??
  @private ms = parser "." ms=Rule.natural -> ms
  @private tz = parser
    | "+" tz=Rule.fixed_length_natural(2) -> Duration.h(tz)
    | "-" tz=Rule.fixed_length_natural(2) -> Duration.h(-tz)
    //... others
  @private do_tz(date, tz) =
    match tz with
    | {some=tz} -> Date.advance(date, tz)
    | {none} -> date
    end
  @private date1p = parser
    | year=Rule.natural "-" month=Rule.natural "-" day=Rule.natural Rule.ws
      h=Rule.natural ":" min=Rule.natural ":" s=Rule.natural ~ms? ~tz?->
      month = int_to_month(month)
      ms = Option.default(0,ms)
      do_tz(Date.build(~{year;month;day;h;min;s;ms}),tz)
    | year=Rule.natural "-" month=Rule.natural "-" day=Rule.natural Rule.ws ->
      month = int_to_month(month)
      Date.build(~{year;month;day})
    | h=Rule.natural ":" min=Rule.natural ":" s=Rule.natural ~ms? ~tz? ->
      ms = Option.default(0,ms)
      do_tz(Date.build(~{year=1970;month={january};day=1;h;min;s;ms}),tz)

  getTextDate(type_id:int, s:string) : Postgres.opatype =
    match (type_id,Parser.try_parse(date1p, s)) with
    | (1082,{some=date}) -> {Date=date}
    | (1083,{some=time}) -> {Time=time}
    | (1114,{some=timestamp}) -> {Timestamp=timestamp}
    | (1184,{some=timestamptz}) -> {Timestamptz=timestamptz}
    | _ -> {BadDate=s}

  getTextByteA(s:string) : Postgres.opatype =
    if String.has_prefix("\\x",s)
    then {Bytea=Binary.of_hex(String.sub(2,String.length(s)-2,s))}
    else @fail("TODO: bytea escape format")

  @private comma = parser "," -> void
  @private lp(ep) = parser | "\{" l=Rule.parse_list_sep(false, ep, comma) "}" -> l

  @private dims(s) =
    len = String.length(s)
    rec aux(pos) =
      if pos >= len
      then {none}
      else
        if String.get(pos,s) == "\{"
        then aux(pos+1)
        else {some=pos}
    aux(0)

  @private getTextArray(s:string, elem, mk) : Postgres.opatype =
    match dims(s) with
    | {some=1} -> 
      match Parser.try_parse(lp(elem),s) with
      | {some=l} -> mk({Array1=l})
      | {none} -> {BadText=s}
      end
    | {some=2} -> 
      match Parser.try_parse(lp(lp(elem)),s) with
      | {some=ll} -> mk({Array2=ll})
      | {none} -> {BadText=s}
      end
    | {some=3} -> 
      match Parser.try_parse(lp(lp(lp(elem))),s) with
      | {some=ll} -> mk({Array3=ll})
      | {none} -> {BadText=s}
      end
    | {some=_} -> {BadText=s}
    | {none} -> {BadText=s}

  @private mkintarray(a) : Postgres.opatype =
    match a with
    | {~Array1} -> {IntArray1=Array1}
    | {~Array2} -> {IntArray2=Array2}
    | {~Array3} -> {IntArray3=Array3}

  getTextIntArray = getTextArray(_, Rule.integer, mkintarray)

  @private mkint16array(a) : Postgres.opatype =
    match a with
    | {~Array1} -> {Int16Array1=Array1}
    | {~Array2} -> {Int16Array2=Array2}
    | {~Array3} -> {Int16Array3=Array3}

  getTextInt16Array = getTextArray(_, Rule.integer, mkint16array) // TODO: check range

  @private mkint64array(a) : Postgres.opatype =
    match a with
    | {~Array1} -> {Int64Array1=Array1}
    | {~Array2} -> {Int64Array2=Array2}
    | {~Array3} -> {Int64Array3=Array3}

  getTextInt64Array = getTextArray(_, Rule.int64, mkint64array)

  @private mkrealarray(a) : Postgres.opatype =
    match a with
    | {~Array1} -> {RealArray1=Array1}
    | {~Array2} -> {RealArray2=Array2}
    | {~Array3} -> {RealArray3=Array3}

  getTextRealArray = getTextArray(_, Rule.float, mkrealarray)

  @private mkfloatarray(a) : Postgres.opatype =
    match a with
    | {~Array1} -> {FloatArray1=Array1}
    | {~Array2} -> {FloatArray2=Array2}
    | {~Array3} -> {FloatArray3=Array3}

  getTextFloatArray = getTextArray(_, Rule.float, mkfloatarray)

  @private mkstringarray(a) : Postgres.opatype =
    match a with
    | {~Array1} -> {StringArray1=Array1}
    | {~Array2} -> {StringArray2=Array2}
    | {~Array3} -> {StringArray3=Array3}

  @private stringchp = parser "\\\"" -> Text.cons("\\\"")
                            | ch=(!"\"" .) -> ch
  @private stringp = parser "\"" str=(stringchp)* "\"" -> Text.to_string(Text.ltconcat(str))
                          | str=(!"," !"}" .)* -> Text.to_string(Text.ltconcat(str))

  getTextStringArray = getTextArray(_, stringp, mkstringarray)

  @private yearp = parser ("Y"|"year") -> void
  @private monp = parser ("M"|"mon") -> void
  @private weekp = parser ("W"|"week") -> void
  @private dayp = parser ("D"|"day") -> void
  @private hourp = parser ("H"|"hour") -> void
  @private minp = parser ("M"|"min") -> void
  @private secp = parser ("S"|"sec") -> void
  @private intel(name) = parser num=Rule.integer Rule.ws name "s"? Rule.ws -> num
  @private pgintervaldatep = parser
    | year=intel(yearp)? month=intel(monp)? day=intel(dayp)? -> {year=year?0; month=month?0; day=day?0}
  @private pgintervaltimep = parser
    | h=Rule.integer ":" min=Rule.integer ":" s=Rule.integer Rule.ws -> ~{h;min;s}
    | h=intel(hourp)? min=intel(minp)? s=intel(secp)? -> {h=h?0;min=min?0;s=s?0}
  @private pgintervalp = parser
    | "@"? d=pgintervaldatep t=pgintervaltimep ago="ago"? ->
      Duration.of_human_readable(~{forward=Option.is_none(ago);
                                   year=d.year; month=d.month; day=d.day; h=t.h; min=t.min; s=t.s; ms=0})
    | "P" d=pgintervaldatep "T" t=pgintervaltimep ->
      Duration.of_human_readable(~{forward=false;
                                   year=d.year; month=d.month; day=d.day; h=t.h; min=t.min; s=t.s; ms=0})

  getTextInterval(s:string) : Postgres.opatype =
    match Parser.try_parse(pgintervalp,s) with
    | {some=duration} -> {Duration=duration}
    | {none} -> {BadText=s}

  @private pgcurrencyc = parser | "$" -> "$" | "£" -> "£" | "€" -> "€" | "¥" -> "¥"

  @private pgmoneyp = parser currency=pgcurrencyc? amount=Rule.float Rule.ws -> ~{currency; amount}

  getTextMoney(s:string) : Postgres.opatype =
    match Parser.try_parse(pgmoneyp,s) with
    | {some=money} -> {Money=money}
    | {none} -> {BadText=s}

  @private postdigits = parser "."? digits=(Rule.digit*) -> digits
  @private pgnumericp = parser pre=(Rule.digit*) post=postdigits Rule.ws -> {pre=Text.to_string(pre); post=Text.to_string(post)}

  getTextNumeric(s:string) : Postgres.opatype =
    match Parser.try_parse(pgnumericp,s) with
    | {some=numeric} -> {Numeric=numeric}
    | {none} -> {BadText=s}

  getElement(rowdesc:Postgres.rowdesc, data:binary, output:Postgres.oparow) : Postgres.oparow =
    add(v) = StringMap.add(rowdesc.name,v,output)
    //do jlog("rowdesc:{rowdesc}")
    //do jlog("{rowdesc.name}: data({Binary.length(data)})=\n{bindump(data)}") 
    if Binary.length(data) == 0
    then add({Null})
    else
      match rowdesc.format_code with
      | 0 ->
        text = string_of_binary(data)
        match rowdesc.type_id with
        | 16 -> add({Bool=(text=="t")})
        | 17 -> add(getTextByteA(text))
        | 20 -> add({Int64=Int64.of_string(text)})
        | 21 -> add({Int16=Int.of_string(text)})
        | 23 | 26 -> add({Int=Int.of_string(text)})
        | 25 -> add({String=text})
        | 790 -> add(getTextMoney(text))
        | 700 -> add({Real=Float.of_string(text)})
        | 701 -> add({Float=Float.of_string(text)})
        | 1042 | 1043 -> add({String=text})
        | 1082 | 1083 | 1114 | 1184 -> add(getTextDate(rowdesc.type_id,text))
        | 1005 -> add(getTextInt16Array(text))
        | 1007 | 1231 -> add(getTextIntArray(text))
        | 1016 -> add(getTextInt64Array(text))
        | 1009 | 1015 -> add(getTextStringArray(text))
        | 1021 -> add(getTextRealArray(text))
        | 1022 -> add(getTextFloatArray(text))
        | 1186 -> add(getTextInterval(text))
        | 1700 -> add(getTextNumeric(text))
        | id -> add({TypeId=(id,~{text})})
        end
      | 1 ->
        // TODO: we also need codes for binary data
        add({TypeId=(rowdesc.type_id,{binary=data})})
      | code ->
        add({BadCode=(code,data)})

  getRow(rowdescs:Postgres.rowdescs, row:Postgres.row) : Postgres.oparow =
    List.fold2(getElement,rowdescs,row,StringMap.empty)

  getCustom(oparow:Postgres.oparow, name:string, handler:Postgres.handler('a)) : option('a) =
    match StringMap.get(name, oparow) with
    | {some={TypeId=(type_id,data)}} -> handler(type_id,name_type(@typeval('a)),data)
    | _ -> {none}

  rec_to_postgres(conn:Postgres.connection, v:'a, fields:OpaType.fields): Postgres.oparowl =
    List.flatten(OpaValue.Record.fold_with_fields((field, tyfield, value, oparowl ->
                                                    name = OpaValue.Record.name_of_field_unsafe(field)
                                                    res = opa_to_pg(conn, name, value, tyfield)
                                                    [res | oparowl]), v, fields, []))

  error(msg) = do Log.error("Postgres.opa_to_postgres", msg) @fail(msg)

  enum_field_names(ty:OpaType.ty) : outcome(list(string),string) =
    match ty with
    | {TySum_col=col ...} ->
       List.fold((row, result -> 
                   match result with
                   | {~failure} -> {~failure}
                   | {success=names} ->
                     match row with
                     | [~{label; ty}] ->
                        if OpaType.is_void(ty)
                        then {success=[label|names]}
                        else {failure="enum_field_names: Label {label} is not void ({ty})"}
                     | _ -> {failure="enum_field_names: Bad type {ty}"}
                     end),col,{success=[]})
    | _ -> {failure="enum_field_names: Bad type {ty}"}
    end

  record_enum_types(ty:OpaType.ty) : list((string,OpaType.ty)) =
    match name_type(ty) with
    | {TyRecord_row=row ...} ->
       List.fold(({label=_; ty=rty}, enums ->
                    match rty with
                    | {TyName_args=[]; TyName_ident=name} ->
                      nrty = name_type(rty)
                      match enum_field_names(nrty) with
                      | {success=_} -> [(name,rty)|enums]
                      | {failure=_} -> enums
                      end
                    | _ -> enums
                    end),row,[])
    | _ -> []

  enum_to_pg(type_id:int, key:string, v:'a, ty:OpaType.ty) : Postgres.oparowl =
    //do jlog("enum_to_pg: type_id={type_id}  key={key}  v={v}  ty={ty}")
    field_names = %%BslNativeLib.record_fields%%(v)
    //do jlog("field_names:{field_names}")
    match field_names with
    | [] -> [(key,{Null})]
    | [name] ->
       match enum_field_names(ty) with
       | {success=enames} ->
          //do jlog("enames:{enames}")
          if List.mem(name,enames)
          then [(key,{TypeId=(type_id,{text=name})})]
          else [(key,{BadEnum=[name]})]
       | {failure=_} -> [(key,{BadEnum=[name]})]
       end
    | _ -> [(key,{BadEnum=field_names})]

  list_to_pg(key:string, v:'a, ty:OpaType.ty, level:int) : Postgres.oparowl =
    match (level,ty) with
    | (_,{TyName_args=[ty]; TyName_ident="list"}) -> list_to_pg(key, v, ty, level+1)
    | (1,{TyConst={TyInt={}}}) -> [(key,{IntArray1=@unsafe_cast(v)})]
    | (2,{TyConst={TyInt={}}}) -> [(key,{IntArray2=@unsafe_cast(v)})]
    | (3,{TyConst={TyInt={}}}) -> [(key,{IntArray3=@unsafe_cast(v)})]
    | (1,{TyName_args=[]; TyName_ident="Postgres.smallint"}) -> [(key,{Int16Array1=@unsafe_cast(v)})]
    | (2,{TyName_args=[]; TyName_ident="Postgres.smallint"}) -> [(key,{Int16Array2=@unsafe_cast(v)})]
    | (3,{TyName_args=[]; TyName_ident="Postgres.smallint"}) -> [(key,{Int16Array3=@unsafe_cast(v)})]
    | (1,{TyName_args=[]; TyName_ident="int64"}) -> [(key,{Int64Array1=@unsafe_cast(v)})]
    | (2,{TyName_args=[]; TyName_ident="int64"}) -> [(key,{Int64Array2=@unsafe_cast(v)})]
    | (3,{TyName_args=[]; TyName_ident="int64"}) -> [(key,{Int64Array3=@unsafe_cast(v)})]
    | (1,{TyConst={TyString={}}}) -> [(key,{StringArray1=@unsafe_cast(v)})]
    | (2,{TyConst={TyString={}}}) -> [(key,{StringArray2=@unsafe_cast(v)})]
    | (3,{TyConst={TyString={}}}) -> [(key,{StringArray3=@unsafe_cast(v)})]
    | (1,{TyName_args=[]; TyName_ident="Postgres.real"}) -> [(key,{RealArray1=@unsafe_cast(v)})]
    | (2,{TyName_args=[]; TyName_ident="Postgres.real"}) -> [(key,{RealArray2=@unsafe_cast(v)})]
    | (3,{TyName_args=[]; TyName_ident="Postgres.real"}) -> [(key,{RealArray3=@unsafe_cast(v)})]
    | (1,{TyConst={TyFloat={}}}) -> [(key,{FloatArray1=@unsafe_cast(v)})]
    | (2,{TyConst={TyFloat={}}}) -> [(key,{FloatArray2=@unsafe_cast(v)})]
    | (3,{TyConst={TyFloat={}}}) -> [(key,{FloatArray3=@unsafe_cast(v)})]
    | _ -> error("unknown value {Debug.dump(v)} of type {OpaType.to_pretty(ty)}")

  opa_to_pg(conn:Postgres.connection, key:string, v:'a, ty:OpaType.ty) : Postgres.oparowl =
    v = Magic.id(v)
    match ty with
    | {TyName_args=[]; TyName_ident="void"} -> [(key,{Null})]
    | {TyConst={TyInt={}}} -> [(key,{Int=(@unsafe_cast(v):int)})]
    | {TyConst={TyString={}}} -> [(key,{String=(@unsafe_cast(v):string)})]
    | {TyConst={TyFloat={}}} -> [(key,{Float=(@unsafe_cast(v):float)})]
    | {TyName_args=[]; TyName_ident="Postgres.real"} -> [(key,{Real=(@unsafe_cast(v):float)})]
    | {TyName_args=[]; TyName_ident="Postgres.smallint"} -> [(key,{Int16=(@unsafe_cast(v):int)})]
    | {TyName_args=[]; TyName_ident="int64"} -> [(key,{Int64=(@unsafe_cast(v):int64)})]
    | {TyName_args=[]; TyName_ident="bool"} -> [(key,{Bool=(@unsafe_cast(v):bool)})]
    | {TyName_args=[]; TyName_ident="Postgres.date"}
    | {TyName_args=[]; TyName_ident="Date.date"} -> [(key,{Date=(@unsafe_cast(v):Date.date)})]
    | {TyName_args=[]; TyName_ident="Postgres.time"} -> [(key,{Time=(@unsafe_cast(v):Date.date)})]
    | {TyName_args=[]; TyName_ident="Postgres.timestamp"} -> [(key,{Timestamp=(@unsafe_cast(v):Date.date)})]
    | {TyName_args=[]; TyName_ident="Postgres.timestamptz"} -> [(key,{Timestamptz=(@unsafe_cast(v):Date.date)})]
    | {TyName_args=[]; TyName_ident="Postgres.bytea"}
    | {TyName_args=[]; TyName_ident="binary"} -> [(key,{Bytea=(@unsafe_cast(v):binary)})] //TODO
    | {TyName_args=[]; TyName_ident="Postgres.money"} -> [(key,{Money=(@unsafe_cast(v):Postgres.money)})]
    | {TyName_args=[]; TyName_ident="Postgres.numeric"} -> [(key,{Numeric=(@unsafe_cast(v):Postgres.numeric)})]
    | {TyName_args=[]; TyName_ident="Duration.duration"} -> [(key,{Duration=(@unsafe_cast(v):Duration.duration)})]
    | {TyName_args=_; TyName_ident="Postgres.oparow"} ->
       @unsafe_cast(StringMap.fold((k, v, l -> [(k,v)|l]),@unsafe_cast(v),[])):Postgres.oparowl
    | {TyName_args=_; TyName_ident="Postgres.oparowl"} -> @unsafe_cast(v):Postgres.oparowl
    | {TyName_args=[ty]; TyName_ident="list"} -> list_to_pg(key, v, ty, 1)
    | {TyName_args=[ty]; TyName_ident="option"} ->
       match (@unsafe_cast(v):option('a)) with
       | {some=v} -> opa_to_pg(conn, key, v, ty)
       | {none} -> [(key,{Null})]
       end
    | {TyName_args=tys; TyName_ident=tyid} -> opa_to_pg(conn, key, v, OpaType.type_of_name(tyid, tys))
    | _ ->
      match StringMap.get("{ty}",conn.backhandlers) with
      | {some=type_id} -> enum_to_pg(type_id, key, v, ty)
      | {none} -> error("unknown value {Debug.dump(v)} of type {OpaType.to_pretty(ty)}")
      end

  opa_to_postgres(conn:Postgres.connection, v:'a, ty_opt:option(OpaType.ty)) : Postgres.oparowl =
    ty = match ty_opt with {some=ty} -> ty | {none} -> @typeof(v)
    match name_type(ty) with
    | {TyRecord_row=row ...} ->
      (match row with
       | [] -> [("value",{Null})]
       | [{label=name; ty=ty}] ->
          if OpaType.is_void(ty)
          then [(name,{Null})]
          else rec_to_postgres(conn, v, row)
       | _ -> rec_to_postgres(conn, v, row))
    | {TySum_col=col ...} ->
      if List.mem([{label="false"; ty={TyRecord_row=[]}}],col)
      then [("value",{Bool=@unsafe_cast(v)})]
      else rec_to_postgres(conn, v, OpaType.fields_of_fields_list(v, col).f1)
    | ty ->
      opa_to_pg(conn, "value", v, ty)

  of_opa(conn:Postgres.connection, v:'a) : Postgres.oparowl = opa_to_postgres(conn,v,{some=@typeval('a)})

  ls(l,tos) = List.to_string_using("\{","}",",",List.map(tos,l))
  lls(l,tos) = List.to_string_using("\{","}",",",List.map(ls(_,tos),l))
  llls(l,tos) = List.to_string_using("\{","}",",",List.map(lls(_,tos),l))
  encode_string(s) = //TODO!!!
    if String.contains(s,",")
    then "\"{s}\""
    else s

  @private tzfmt = Date.generate_printer("%z")
  @private timestamp_fmt = Date.generate_printer("%F %T.%x")

  timestamp_to_string(d:Date.date) : string = Date.to_formatted_string(timestamp_fmt, d)
  timestamptz_to_string(d:Date.date) : string =
    tz = Date.to_formatted_string(tzfmt, d)
    tz = // timezone with colons is missing from formatted dates
      match String.length(tz) with
      | 4 -> String.sub(0,2,tz)^":"^String.sub(2,2,tz)
      | 5 -> String.sub(0,3,tz)^":"^String.sub(3,2,tz)
      | _ -> tz
      end
    "{Date.to_formatted_string(timestamp_fmt, d)}{tz}"

  duration_to_iso8601(d:Duration.duration) : string =
    hr = Duration.to_human_readable(d)
    sign = if hr.forward then 1 else -1
    get(code,amnt) = if amnt == 0 then "" else "{sign*amnt}{code}"
    t = if hr.h != 0 || hr.min != 0 || hr.s != 0 then "T" else ""
    "P{get("Y",hr.year)}{get("M",hr.month)}{get("D",hr.day)}{t}{get("H",hr.h)}{get("M",hr.min)}{get("S",hr.s)}"

  string_of_field_value(opatype:Postgres.opatype) : string =
    match opatype with
    | {Null} -> "null"
    | {Int=int} -> Int.to_string(int)
    | {Int16=int} -> Int.to_string(int)
    | {Int64=int64} -> Int64.to_string(int64)
    | {Bool=bool} -> Bool.to_string(bool)
    | {String=string} -> "'{encode_string(string)}'"
    | {Real=float} -> Float.to_string(float)
    | {Float=float} -> Float.to_string(float)
    | {Money=money} -> "'{money.currency?""}{money.amount}'"
    | {Numeric=numeric} -> "{numeric.pre}.{numeric.post}"
    | {Date=date} -> "'{Date.to_formatted_string(Date.date_only_printer,date)}'"
    | {Time=time} -> "'{Date.to_formatted_string(Date.time_only_printer,time)}'"
    | {Timestamp=timestamp} -> "'{timestamp_to_string(timestamp)}'"
    | {Timestamptz=timestamptz} -> "'{timestamptz_to_string(timestamptz)}'"
    | {Bytea=binary} -> "'{string_of_binary(binary)}'" //TODO!!!
    | {IntArray1=l} -> "'{ls(l,Int.to_string)}'"
    | {IntArray2=ll} -> "'{lls(ll,Int.to_string)}'"
    | {IntArray3=lll} -> "'{llls(lll,Int.to_string)}'"
    | {Int16Array1=l} -> "'{ls(l,Int.to_string)}'"
    | {Int16Array2=ll} -> "'{lls(ll,Int.to_string)}'"
    | {Int16Array3=lll} -> "'{llls(lll,Int.to_string)}'"
    | {Int64Array1=l} -> "'{ls(l,Int64.to_string)}'"
    | {Int64Array2=ll} -> "'{lls(ll,Int64.to_string)}'"
    | {Int64Array3=lll} -> "'{llls(lll,Int64.to_string)}'"
    | {RealArray1=l} -> "'{ls(l,Float.to_string)}'"
    | {RealArray2=ll} -> "'{lls(ll,Float.to_string)}'"
    | {RealArray3=lll} -> "'{llls(lll,Float.to_string)}'"
    | {FloatArray1=l} -> "'{ls(l,Float.to_string)}'"
    | {FloatArray2=ll} -> "'{lls(ll,Float.to_string)}'"
    | {FloatArray3=lll} -> "'{llls(lll,Float.to_string)}'"
    | {StringArray1=l} -> "'{ls(l,encode_string)}'"
    | {StringArray2=ll} -> "'{lls(ll,encode_string)}'"
    | {StringArray3=lll} -> "'{llls(lll,encode_string)}'"
    | {Duration=duration} -> "'{duration_to_iso8601(duration)}'"
    | {TypeId=(_,data)} ->
       match data with
       | {~text} -> "'{text}'"
       | {~binary} -> "'{string_of_binary(binary)}'"
       end
    | {BadData=_} -> "null"
    | {BadText=_} -> "null"
    | {BadCode=_} -> "null"
    | {BadDate=_} -> "null"
    | {BadEnum=_} -> "null"

  postgres_type(conn:Postgres.connection, opatype:Postgres.opatype) : string =
    match opatype with
    | {Null} -> "int" // arbitrary, this can actually be anything
    | {Int=_} -> "int"
    | {Int16=_} -> "smallint"
    | {Int64=_} -> "bigint"
    | {Bool=_} -> "bool"
    | {String=_} -> "text"
    | {Real=_} -> "float4"
    | {Float=_} -> "float8"
    | {Money=_} -> "money"
    | {Numeric=_} -> "numeric"
    | {Date=_} -> "date"
    | {Time=_} -> "time"
    | {Timestamp=_} -> "timestamp"
    | {Timestamptz=_} -> "timestamptz"
    | {Bytea=_} -> "bytea"
    | {IntArray1=_} -> "int[]"
    | {IntArray2=_} -> "int[][]"
    | {IntArray3=_} -> "int[][][]"
    | {Int16Array1=_} -> "int2[]"
    | {Int16Array2=_} -> "int2[][]"
    | {Int16Array3=_} -> "int2[][][]"
    | {Int64Array1=_} -> "int8[]"
    | {Int64Array2=_} -> "int8[][]"
    | {Int64Array3=_} -> "int8[][][]"
    | {RealArray1=_} -> "float4[]"
    | {RealArray2=_} -> "float4[][]"
    | {RealArray3=_} -> "float4[][][]"
    | {FloatArray1=_} -> "float8[]"
    | {FloatArray2=_} -> "float8[][]"
    | {FloatArray3=_} -> "float8[][][]"
    | {StringArray1=_} -> "text[]"
    | {StringArray2=_} -> "text[][]"
    | {StringArray3=_} -> "text[][][]"
    | {Duration=_} -> "interval"
    | {TypeId=(type_id,_)} ->
       match IntMap.get(type_id,conn.handlers) with
       | {some=(name,_,_)} -> name
       | {none} -> "int"
       end
    | {BadData=_} -> "int"
    | {BadText=_} -> "int"
    | {BadCode=_} -> "int"
    | {BadDate=_} -> "int"
    | {BadEnum=_} -> "int"

  field_names(oparowl:Postgres.oparowl) : list(string) =
    List.map(((key,_) -> key),oparowl)

  field_values(oparowl:Postgres.oparowl) : list(string) =
    List.map(((_,val) -> string_of_field_value(val)),oparowl)

  field_types(conn:Postgres.connection, oparowl:Postgres.oparowl) : list(string) =
    List.map(((_,val) -> postgres_type(conn,val)),oparowl)

  @private csl(l) = String.concat(",",l)

  create(conn:Postgres.connection, dbase:string, temp:bool, v:'a) : string =
    row = of_opa(conn, v)
    flds = List.map2((n, t -> "{n} {t}"),field_names(row),field_types(conn,row))
    temp = if temp then "TEMP " else ""
    "CREATE {temp}TABLE {dbase}({csl(flds)})"

  insert(conn:Postgres.connection, dbase:string, v:'a) : string =
    row = of_opa(conn, v)
    "INSERT INTO {dbase}({csl(field_names(row))}) VALUES({csl(field_values(row))})"

  @private StringMap_keys(sm) : list(string) = StringMap.fold((k, _, keys -> [k|keys]),sm,[])

  @private intersect(l1, l2) =
    rec aux(l1,l2) =
      match ((l1,l2)) with
      | ([],_) -> ([],[])
      | (_,[]) -> ([],[])
      | ([e1|l1],[e2|l2]) ->
         match String.ordering(e1.f1,e2.label) with
         | {eq} ->
           (l1,l2) = aux(l1,l2)
           ([e1|l1],[e2|l2])
         | {lt} -> aux(l1,[e2|l2])
         | {gt} -> aux([e1|l1],l2)
         end
      end
    aux(l1,l2)

  name_type(ty:OpaType.ty): OpaType.ty =
    nty =
      match ty with
      | {TyName_args=[]; TyName_ident="Postgres.money"}
      | {TyName_args=[]; TyName_ident="Postgres.numeric"}
      | {TyName_args=[]; TyName_ident="Date.date"}
      | {TyName_args=[]; TyName_ident="Postgres.date"}
      | {TyName_args=[]; TyName_ident="Postgres.time"}
      | {TyName_args=[]; TyName_ident="Postgres.timestamp"}
      | {TyName_args=[]; TyName_ident="Postgres.timestamptz"}
      | {TyName_args=[]; TyName_ident="Postgres.bytea"}
      | {TyName_args=[]; TyName_ident="binary"}
      | {TyName_args=[_]; TyName_ident="option"}
      | {TyName_args=[]; TyName_ident="bool"}
      | {TyName_args=[]; TyName_ident="void"}
      | {TyName_args=[_]; TyName_ident="int64"}
      | {TyName_args=[_,_]; TyName_ident="map"}
      | {TyName_args=[_,_,_]; TyName_ident="ordered_map"}
      | {TyName_args=[_]; TyName_ident="list"} -> ty
      | {TyName_args=tys; TyName_ident=tyid} -> name_type(OpaType.type_of_name(tyid, tys))
      | ty ->
        match OpaTypeUnification.unify(@typeval(list), ty) with
        | {success = {~var ...}} ->
          (var, ins) = StringMap.extract_min_binding(var)
          if StringMap.is_empty(var) then
            match ins with
            | {some=lty} -> {TyName_args=[lty.f2]; TyName_ident="list"}
            | {none} -> ty
          else ty
        | {failure=_} -> ty
    nty

  rec row_to_opa_aux(conn:Postgres.connection, row:Postgres.oparow, ty:OpaType.ty, dflt:option('a)): option('a) =

    error_msg(str) = "Try to unserialize {row} with {ty}\n Error : {str}"

    error_no_retry(str, v) =
      do Log.error("Postgres.row_to_opa", error_msg(str))
      v

    error(str) = error_no_retry("{error_msg(str)}", {none})

    fatal(str) =
      do Log.error("Postgres.row_to_opa", str)
      @fail(str)

    make_option(vopt:option('a)): option('b) =
      match vopt with
      | {some=v} -> {some=@unsafe_cast({some=v})}
      | {none} -> {some=@unsafe_cast({none})}

    rec element_to_rec(row:Postgres.oparow, fields:OpaType.fields, dflt:option('a)): option('a) =
      match fields with
      | [{label=name; ty=ty}] ->
        if OpaType.is_void(ty)
        then
          match OpaValue.Record.field_of_name(name) with
          | {none} -> {none}
          | {some=field} -> {some=@unsafe_cast(OpaValue.Record.make_simple_record(field))}
          end
        else element_to_rec2(row, fields, dflt)
      | _ -> element_to_rec2(row, fields, dflt)

    and element_to_rec2(row:Postgres.oparow, fields:OpaType.fields, dflt:option('a)): option('a) =
      rec optreg(name, field, frest, elements, acc) =
        match OpaValue.Record.field_of_name(field.label) with
          | {none} -> error_no_retry("Missing runtime field {name}", (acc, true))
          | {some=backfield} ->
            match dflt with
            | {none} ->
              match field.ty with
              | {TyName_args=[_]; TyName_ident="option"} ->
                aux(elements,frest,[(backfield,@unsafe_cast({none}))|acc])
              | _ -> error_no_retry("name mismatch \"{field.label}\" vs. \"{name}\"",(acc, true))
              end
            | {some=dflt} ->
              dflt = OpaValue.Record.unsafe_dot(dflt, backfield)
              aux(elements, frest, [(backfield, dflt) | acc])
            end
          end

      and aux(elements, fields, acc) =
        match (elements, fields) with
        | ([element|erest],[field|frest]) ->
            name = element.f1
            next() =
              match OpaValue.Record.field_of_name(name) with
              | {none} -> error_no_retry("Missing field {name}", (acc, true))
              | {some=backfield} ->
                dflt = Option.map(OpaValue.Record.unsafe_dot(_, backfield), dflt)
                match opatype_to_opa(element.f2, field.ty, dflt) with
                | {none} ->
                  error_no_retry("Failed with field {name}, document {row} and type {OpaType.to_pretty(field.ty)}",
                                 (acc, true))
                | {some=value} -> aux(erest,frest,[(backfield,value)|acc])
                end
              end
            match String.ordering(field.label,name)
            | {eq} -> next()
            | {lt} -> optreg(name, field, frest, [element|erest], acc)
            | {gt} -> (acc, true)
            end
        | ([],[]) -> (acc,false)
        | ([],[field|frest]) -> optreg("absent field", field, frest, [], acc)
        | (_erest,_frest) -> (acc,true)
      elements = StringMap.fold((name, opatype, elements -> [(name,opatype)|elements]),row,[])
      elements = List.sort_by((e -> e.f1),elements)
      fields = List.sort_by((f -> f.label),fields)
      (elements,fields) = intersect(elements, fields)
      (flds, err) = aux(elements, fields, [])
      rcrd = List.fold(((field,value), rcrd -> OpaValue.Record.add_field(rcrd, field, value)),
                       flds,OpaValue.Record.empty_constructor())
      if err
      then error("Failed with fields {OpaType.to_pretty_fields(fields)} document {row}")
      else {some=@unsafe_cast(OpaValue.Record.make_record(rcrd))}

    and column_to_rec(row:Postgres.oparow, col) =
      ltyfield = List.sort(StringMap_keys(row))
      match OpaSerialize.fields_of_fields_list2(ltyfield, col) with
      | {some=fields} -> element_to_rec(row, fields, none)
      | {none} -> error("Fields ({OpaType.to_pretty_lfields(col)}) not found in sum type ({List.to_string(ltyfield)})")

    and getel(name, row, ty, dflt) =
      match StringMap.get(name, row) with
      | {some=opatype} -> opatype_to_opa(opatype, ty, dflt)
      | {none} -> error("Missing name {name}")

    and opatype_to_list(opatype:Postgres.opatype, ty:OpaType.ty, dflt:option('a), level:int): option('a) =
      err(name) =
        pre = String.repeat(level,"list(")
        post = String.repeat(level,")")
        error("expected {pre}{name}{post}, got {opatype}")
      list(get,name) =
        match opatype with
        | {Null} -> dflt
        | opatype ->
           match get(opatype) with
           | {some=a} -> {some=@unsafe_cast(a)}
           | {none} -> err(name)
           end
        end
      match (level,ty) with
      | (_,{TyName_args=[ty]; TyName_ident="list"}) ->
         opatype_to_list(opatype, ty, dflt, level+1)
      | (1,{TyConst={TyInt={}}}) ->
         list((ot -> match ot with {IntArray1=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"int")
      | (2,{TyConst={TyInt={}}}) ->
         list((ot -> match ot with {IntArray2=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"int")
      | (3,{TyConst={TyInt={}}}) ->
         list((ot -> match ot with {IntArray3=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"int")
      | (1,{TyName_args=[]; TyName_ident="Postgres.smallint"}) ->
         list((ot -> match ot with {Int16Array1=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"smallint")
      | (2,{TyName_args=[]; TyName_ident="Postgres.smallint"}) ->
         list((ot -> match ot with {Int16Array2=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"smallint")
      | (3,{TyName_args=[]; TyName_ident="Postgres.smallint"}) ->
         list((ot -> match ot with {Int16Array3=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"smallint")
      | (1,{TyName_args=[]; TyName_ident="int64"}) ->
         list((ot -> match ot with {Int64Array1=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"int64")
      | (2,{TyName_args=[]; TyName_ident="int64"}) ->
         list((ot -> match ot with {Int64Array2=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"int64")
      | (3,{TyName_args=[]; TyName_ident="int64"}) ->
         list((ot -> match ot with {Int64Array3=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"int64")
      | (1,{TyConst={TyString={}}}) ->
         list((ot -> match ot with {StringArray1=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"string")
      | (2,{TyConst={TyString={}}}) ->
         list((ot -> match ot with {StringArray2=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"string")
      | (3,{TyConst={TyString={}}}) ->
         list((ot -> match ot with {StringArray3=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"string")
      | (1,{TyName_args=[]; TyName_ident="Postgres.real"}) ->
         list((ot -> match ot with {RealArray1=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"real")
      | (2,{TyName_args=[]; TyName_ident="Postgres.real"}) ->
         list((ot -> match ot with {RealArray2=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"real")
      | (3,{TyName_args=[]; TyName_ident="Postgres.real"}) ->
         list((ot -> match ot with {RealArray3=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"real")
      | (1,{TyConst={TyFloat={}}}) ->
         list((ot -> match ot with {FloatArray1=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"float")
      | (2,{TyConst={TyFloat={}}}) ->
         list((ot -> match ot with {FloatArray2=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"float")
      | (3,{TyConst={TyFloat={}}}) ->
         list((ot -> match ot with {FloatArray3=a} -> {some=@unsafe_cast(a)} | _ -> {none}),"float")
      | _ ->
         match opatype with
         | {Null} -> dflt
         | _ -> err("'a")
         end
      end

    and opatype_to_opa(opatype:Postgres.opatype, ty:OpaType.ty, dflt:option('a)): option('a) =
      match ty with
      | {TyName_args=[]; TyName_ident="void"} ->
        (match opatype with
         | {Null} -> {some=@unsafe_cast(void)}
         | opatype -> error("expected void, got {opatype}"))
      | {TyName_args=[]; TyName_ident="Postgres.smallint"} ->
        (match opatype with
         | {Null} -> dflt
         | {Int16=i} ->
            if i < -32768 || i > 32767
            then error("int16 value out of range {i}")
            else {some=@unsafe_cast(i)}
         | opatype -> error("expected int16, got {opatype}"))
      | {TyName_args=[]; TyName_ident="int64"} ->
        (match opatype with
         | {Null} -> dflt
         | {Int64=i} -> {some=@unsafe_cast(i)}
         | opatype -> error("expected int64, got {opatype}"))
      | {TyConst={TyInt={}}} ->
        (match opatype with
         | {Null} -> dflt
         | {Bool=tf} -> {some=@unsafe_cast(if tf then 1 else 0)}
         | {Int=i} -> {some=@unsafe_cast(i)}
         | {Int16=i} -> {some=@unsafe_cast(i)}
         | {Int64=i} -> {some=@unsafe_cast(Int64.to_int(i))}
         | {Real=f} -> {some=@unsafe_cast(Float.to_int(f))}
         | {Float=f} -> {some=@unsafe_cast(Float.to_int(f))}
         | {String=s} -> {some=@unsafe_cast(Int.of_string(s))}
         | opatype -> error("expected int, got {opatype}"))
      | {TyConst={TyString={}}} ->
        (match opatype with
         | {Null} -> {some=@unsafe_cast("")} // Empty strings are returned as Null
         | {Bool=tf} -> {some=@unsafe_cast(Bool.to_string(tf))}
         | {Int=i} -> {some=@unsafe_cast(Int.to_string(i))}
         | {Int16=i} -> {some=@unsafe_cast(Int.to_string(i))}
         | {Int64=i} -> {some=@unsafe_cast(Int64.to_string(i))}
         | {Real=f} -> {some=@unsafe_cast(Float.to_string(f))}
         | {Float=f} -> {some=@unsafe_cast(Float.to_string(f))}
         | {String=s} -> {some=@unsafe_cast(s)}
         | opatype -> error("expected string, got {opatype}"))
      | {TyName_args=[]; TyName_ident="Postgres.real"}
      | {TyConst={TyFloat={}}} ->
        (match opatype with
         | {Null} -> dflt
         | {Bool=tf} -> {some=@unsafe_cast(if tf then 1.0 else 0.0)}
         | {Int=i} -> {some=@unsafe_cast(Float.of_int(i))}
         | {Int16=i} -> {some=@unsafe_cast(Float.of_int(i))}
         | {Int64=i} -> {some=@unsafe_cast(Float.of_int(Int64.to_int(i)))}
         | {Real=f} -> {some=@unsafe_cast(f)}
         | {Float=f} -> {some=@unsafe_cast(f)}
         | {String=s} -> {some=@unsafe_cast(Float.of_string(s))}
         | opatype -> error("expected float, got {opatype}"))
      | {TyName_args=[]; TyName_ident="bool"} ->
        (match opatype with
         | {Null} -> dflt
         | {Bool=tf} -> {some=@unsafe_cast(tf)}
         | {Int=i} -> {some=@unsafe_cast(i != 0)}
         | {Int16=i} -> {some=@unsafe_cast(i != 0)}
         | {Int64=i} -> {some=@unsafe_cast(Int64.op_ne(i,Int64.zero))}
         | {Real=d} -> {some=@unsafe_cast(d != 0.0)}
         | {Float=d} -> {some=@unsafe_cast(d != 0.0)}
         | {String="true"} | {String="t"} -> {some=@unsafe_cast(true)}
         | {String="false"} | {String="f"} -> {some=@unsafe_cast(false)}
         | opatype -> error("expected bool, got {opatype}"))
      | {TyName_args=[ty]; TyName_ident="option"} ->
        (match opatype with
         | {Null} -> {some=@unsafe_cast({none})}
         | _ -> make_option(opatype_to_opa(opatype, ty, none)))
      | {TyName_args=[ty]; TyName_ident="list"} -> opatype_to_list(opatype, ty, dflt, 1)
      | {TyName_args=[]; TyName_ident="Postgres.date"}
      | {TyName_args=[]; TyName_ident="Date.date"} ->
        (match opatype with
         | {Null} -> dflt
         | {Date=dt} -> {some=@unsafe_cast(dt)}
         | opatype -> error("expected date, got {opatype}"))
      | {TyName_args=[]; TyName_ident="Postgres.time"} ->
        (match opatype with
         | {Null} -> dflt
         | {Time=t} -> {some=@unsafe_cast(t)}
         | opatype -> error("expected time, got {opatype}"))
      | {TyName_args=[]; TyName_ident="Postgres.timestamp"} ->
        (match opatype with
         | {Null} -> dflt
         | {Timestamp=t} -> {some=@unsafe_cast(t)}
         | opatype -> error("expected timestamp, got {opatype}"))
      | {TyName_args=[]; TyName_ident="Postgres.timestamptz"} ->
        (match opatype with
         | {Null} -> dflt
         | {Timestamptz=t} -> {some=@unsafe_cast(t)}
         | opatype -> error("expected timestamptz, got {opatype}"))
      | {TyName_args=[]; TyName_ident="Postgres.bytea"}
      | {TyName_args=[]; TyName_ident="binary"} ->
        (match opatype with
         | {Null} -> dflt
         | {Bytea=bin} -> {some=@unsafe_cast(bin)}
         | {String=str} -> {some=@unsafe_cast(binary_of_string(str))}
         | opatype -> error("expected binary, got {opatype}"))
      | {TyName_args=[]; TyName_ident="Duration.duration"} ->
        (match opatype with
         | {Null} -> dflt
         | {Duration=d} -> {some=@unsafe_cast(d)}
         | opatype -> error("expected duration, got {opatype}"))
      | {TyName_args=[]; TyName_ident="Postgres.money"} ->
        (match opatype with
         | {Null} -> dflt
         | {Money=m} -> {some=@unsafe_cast(m)}
         | opatype -> error("expected Postgres.money, got {opatype}"))
      | {TyName_args=[]; TyName_ident="Postgres.numeric"} ->
        (match opatype with
         | {Null} -> dflt
         | {Numeric=m} -> {some=@unsafe_cast(m)}
         | opatype -> error("expected Postgres.numeric, got {opatype}"))
      | {TyName_args=tys; TyName_ident=tyid} ->
        opatype_to_opa(opatype, OpaType.type_of_name(tyid, tys), dflt)
      | _ ->
        match opatype with
        | {Null} -> dflt
        | {TypeId=(type_id,data)} ->
           match IntMap.get(type_id,conn.handlers) with
           | {some=(_,hty,handler)} ->
              if ty == hty
              then
                match handler(type_id, ty, data) with
                | {some=val} -> {some=@unsafe_cast(val)}
                | {none} -> fatal("unknown type {OpaType.to_pretty(ty)}")
                end
              else fatal("unknown type {OpaType.to_pretty(ty)}")
           | {none} -> fatal("unknown type {OpaType.to_pretty(ty)}")
           end
        | _ -> fatal("unknown type {OpaType.to_pretty(ty)}")

    ty_name = name_type(ty)
    match (StringMap_keys(row),ty_name) with
    | (["value"],_) -> getel("value", row, ty_name, dflt)
    | ([],{TyName_args=[_]; TyName_ident="option"}) -> {some=@unsafe_cast({none})}
    | ([name],{TyName_args=[_]; TyName_ident="option"})
    | ([name],{TyName_args=[]; TyName_ident="bool"})
    | ([name],{TyName_args=[_]; TyName_ident="list"}) -> getel(name, row, ty_name, dflt)
    | (_,{TyRecord_row=trow ...}) -> element_to_rec(row, trow, dflt)
    | (_,{TySum_col=col ...}) -> column_to_rec(row, col)
    | _ -> @fail

  to_opa_ty(conn:Postgres.connection, row:Postgres.oparow, ty:OpaType.ty, dflt:option('a)) : option('a) =
    row_to_opa_aux(conn, row, ty, dflt)

  to_opa(conn:Postgres.connection, row:Postgres.oparow) : option('a) = to_opa_ty(conn, row, @typeval('a), none)

}}

