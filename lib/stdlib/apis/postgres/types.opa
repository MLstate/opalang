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

type Postgres.money = {currency:option(string); amount:float}

type Postgres.numeric = {pre:string; post:string} // TODO: arbitrary precision numbers in Opa

type Postgres.opatype =
   {Null}
 / {Int:int}
 / {Int64:int64}
 / {Bool:bool}
 / {String:string}
 / {Float:float}
 / {Money:Postgres.money}
 / {Numeric:Postgres.numeric}
 / {Date:Date.date}
 / {Binary:binary}
 / {IntArray1:list(int)}
 / {IntArray2:list(list(int))}
 / {IntArray3:list(list(list(int)))}
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

type Postgres.oparow = stringmap(Postgres.opatype)

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

  getTextDate(s:string) : Postgres.opatype =
    match Parser.try_parse(date1p, s) with
    | {some=date} -> {Date=date}
    | {none} -> {BadDate=s}

  getTextByteA(s:string) : Postgres.opatype =
    if String.has_prefix("\\x",s)
    then {Binary=Binary.of_hex(String.sub(2,String.length(s)-2,s))}
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
                          | str=(!"," .)* -> Text.to_string(Text.ltconcat(str))

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
        | 21 | 23 | 26 -> add({Int=Int.of_string(text)})
        | 790 -> add(getTextMoney(text))
        | 700 | 701 -> add({Float=Float.of_string(text)})
        | 1042 | 1043 -> add({String=text})
        | 1082 | 1083 | 1114 | 1184 -> add(getTextDate(text))
        | 1005 | 1007 | 1016 | 1231 -> add(getTextIntArray(text))
        | 1015 -> add(getTextStringArray(text))
        | 1021 | 1022 -> add(getTextFloatArray(text))
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

    and opatype_to_opa(opatype:Postgres.opatype, ty:OpaType.ty, dflt:option('a)): option('a) =
      match ty with
      | {TyName_args=[]; TyName_ident="void"} ->
        (match opatype with
         | {Null} -> {some=@unsafe_cast(void)}
         | opatype -> error("expected void, got {opatype}"))
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
         | {Int64=i} -> {some=@unsafe_cast(Int64.to_int(i))}
         | {Float=f} -> {some=@unsafe_cast(Float.to_int(f))}
         | {String=s} -> {some=@unsafe_cast(Int.of_string(s))}
         | opatype -> error("expected int, got {opatype}"))
      | {TyConst={TyString={}}} ->
        (match opatype with
         | {Null} -> {some=@unsafe_cast("")} // Empty strings are returned as Null
         | {Bool=tf} -> {some=@unsafe_cast(Bool.to_string(tf))}
         | {Int=i} -> {some=@unsafe_cast(Int.to_string(i))}
         | {Int64=i} -> {some=@unsafe_cast(Int64.to_string(i))}
         | {Float=f} -> {some=@unsafe_cast(Float.to_string(f))}
         | {String=s} -> {some=@unsafe_cast(s)}
         | opatype -> error("expected string, got {opatype}"))
      | {TyConst={TyFloat={}}} ->
        (match opatype with
         | {Null} -> dflt
         | {Bool=tf} -> {some=@unsafe_cast(if tf then 1.0 else 0.0)}
         | {Int=i} -> {some=@unsafe_cast(Float.of_int(i))}
         | {Int64=i} -> {some=@unsafe_cast(Float.of_int(Int64.to_int(i)))}
         | {Float=f} -> {some=@unsafe_cast(f)}
         | {String=s} -> {some=@unsafe_cast(Float.of_string(s))}
         | opatype -> error("expected float, got {opatype}"))
      | {TyName_args=[]; TyName_ident="bool"} ->
        (match opatype with
         | {Null} -> dflt
         | {Bool=tf} -> {some=@unsafe_cast(tf)}
         | {Int=i} -> {some=@unsafe_cast(i != 0)}
         | {Int64=i} -> {some=@unsafe_cast(Int64.op_ne(i,Int64.zero))}
         | {Float=d} -> {some=@unsafe_cast(d != 0.0)}
         | {String="true"} | {String="t"} -> {some=@unsafe_cast(true)}
         | {String="false"} | {String="f"} -> {some=@unsafe_cast(false)}
         | opatype -> error("expected bool, got {opatype}"))
      | {TyName_args=[ty]; TyName_ident="option"} ->
        (match opatype with
         | {Null} -> {some=@unsafe_cast({none})}
         | _ -> make_option(opatype_to_opa(opatype, ty, none)))
      | {TyName_args=[{TyConst={TyInt={}}}]; TyName_ident="list"} ->
        (match opatype with
         | {Null} -> dflt
         | {IntArray1=ia} -> {some=@unsafe_cast(ia)}
         | opatype -> error("expected list(int), got {opatype}"))
      | {TyName_args=[{TyName_args=[{TyConst={TyInt={}}}]; TyName_ident="list"}]; TyName_ident="list"} ->
        (match opatype with
         | {Null} -> dflt
         | {IntArray2=ia} -> {some=@unsafe_cast(ia)}
         | opatype -> error("expected list(list(int)), got {opatype}"))
      | {TyName_args=[{TyName_args=[{TyName_args=[{TyConst={TyInt={}}}]; TyName_ident="list"}]; TyName_ident="list"}]; TyName_ident="list"} ->
        (match opatype with
         | {Null} -> dflt
         | {IntArray3=ia} -> {some=@unsafe_cast(ia)}
         | opatype -> error("expected list(list(list(int))), got {opatype}"))
      | {TyName_args=[{TyConst={TyString={}}}]; TyName_ident="list"} ->
        (match opatype with
         | {Null} -> dflt
         | {StringArray1=ia} -> {some=@unsafe_cast(ia)}
         | opatype -> error("expected list(string), got {opatype}"))
      | {TyName_args=[{TyName_args=[{TyConst={TyString={}}}]; TyName_ident="list"}]; TyName_ident="list"} ->
        (match opatype with
         | {Null} -> dflt
         | {StringArray2=ia} -> {some=@unsafe_cast(ia)}
         | opatype -> error("expected list(list(string)), got {opatype}"))
      | {TyName_args=[{TyName_args=[{TyName_args=[{TyConst={TyString={}}}]; TyName_ident="list"}]; TyName_ident="list"}]; TyName_ident="list"} ->
        (match opatype with
         | {Null} -> dflt
         | {StringArray3=ia} -> {some=@unsafe_cast(ia)}
         | opatype -> error("expected list(list(list(string))), got {opatype}"))
      | {TyName_args=[{TyConst={TyFloat={}}}]; TyName_ident="list"} ->
        (match opatype with
         | {Null} -> dflt
         | {FloatArray1=ia} -> {some=@unsafe_cast(ia)}
         | opatype -> error("expected list(float), got {opatype}"))
      | {TyName_args=[{TyName_args=[{TyConst={TyFloat={}}}]; TyName_ident="list"}]; TyName_ident="list"} ->
        (match opatype with
         | {Null} -> dflt
         | {FloatArray2=ia} -> {some=@unsafe_cast(ia)}
         | opatype -> error("expected list(list(float)), got {opatype}"))
      | {TyName_args=[{TyName_args=[{TyName_args=[{TyConst={TyFloat={}}}]; TyName_ident="list"}]; TyName_ident="list"}]; TyName_ident="list"} ->
        (match opatype with
         | {Null} -> dflt
         | {FloatArray3=ia} -> {some=@unsafe_cast(ia)}
         | opatype -> error("expected list(list(list(float))), got {opatype}"))
      | {TyName_args=[]; TyName_ident="Date.date"} ->
        (match opatype with
         | {Null} -> dflt
         | {Date=dt} -> {some=@unsafe_cast(dt)}
         | opatype -> error("expected date, got {opatype}"))
      | {TyName_args=[]; TyName_ident="binary"} ->
        (match opatype with
         | {Null} -> dflt
         | {Binary=bin} -> {some=@unsafe_cast(bin)}
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
           | {some=(hty,handler)} ->
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

