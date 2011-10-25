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
 * Bson support for MongoDB driver
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * This is a binding for MongoDB for OPA, loosely based around the C drivers.
 *
 * Module [Bson] contains support for creating, decoding and converting to/from OPA
 * values for Bson documents.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/* Major TODOs, there are minor ones elsewhere. */

import stdlib.core.{date}

/** Some convenience types **/

type Bson.numeric = int
type Bson.oid = string
type Bson.binary = string
type Bson.regexp = (string, string)
type Bson.code = string
type Bson.symbol = string
type Bson.codescope = (Bson.code, Bson.document)
type Bson.int32 = int
type Bson.timestamp = (int, int)
type Bson.register('a) = {present:'a} / {absent}

/**
 * Register.  Exactly like Option but for Bson.register.
 **/
Register = {{
  get(reg:Bson.register('a)): 'a = match reg with {present=a} -> a | {absent} -> error("Register.get called on \{absent}")
  default(def:'a, reg:Bson.register('a)): 'a = match reg with {present=a} -> a | {absent} -> def
  is_present(reg:Bson.register('a)): bool = match reg with {present=_} -> true | {absent} -> false
  is_absent(reg:Bson.register('a)): bool = match reg with {present=_} -> false | {absent} -> true
  map(f:'a->'b, reg:Bson.register('a)): Bson.register('b) = match reg with {present=a} -> {present=f(a)} | {absent} -> {absent}
  iter(f:'a->void, reg:Bson.register('a)): void = match reg with {present=a} -> f(a) | {absent} -> void
}}

/**
 * OPA representation of a BSON object.
 *
 * These are called documents in BSON terminology.
 **/

/**
 * A value encasupates the types used by MongoDB.
 **/
type Bson.value =
    { Double: float }
  / { String: string }
  / { Document: Bson.document }
  / { Array: Bson.document }
  / { Binary: string }
  / { ObjectID: string }
  / { Boolean: bool }
  / { Date: Date.date }
  / { Null }
  / { Regexp: (string, string) }
  / { Code: string }
  / { Symbol: string }
  / { CodeScope: (string, Bson.document) }
  / { Int32: int }
  / { Timestamp: (int, int) }
  / { Int64: int }
  / { Min }
  / { Max }

/**
 * An element is a named value.
 **/
type Bson.element = { name:string; value:Bson.value }

/**
 * The main exported type, a document is just a list of elements.
 */
type Bson.document = list(Bson.element)

/**
 * Helper functions for constructing Bson values.
 *
 * Short names intended to be abbreviations for {name=...; value={Xyz=...}}.
 **/
@server_private
H = {{
  v(n:string,v:Bson.value):Bson.element = {name=n; value=v}
  dbl(n:string,d:float):Bson.element = {name=n; value={Double=d}}
  str(n:string,s:string):Bson.element = {name=n; value={String=s}}
  doc(n:string,d:Bson.document):Bson.element = {name=n; value={Document=d}}
  arr(n:string,d:Bson.document):Bson.element = {name=n; value={Array=d}}
  docarr(n:string,l:list(Bson.document)):Bson.element =
    {name=n; value={Array=List.mapi((i, d -> ({name="{i}"; value={Document=d}}:Bson.element)),l)}}
  binary(n:string,b:Bson.binary):Bson.element = {name=n; value={Binary=b}}
  oid(n:string,id:string):Bson.element = {name=n; value={ObjectID=id}}
  bool(n:string,b:bool):Bson.element = {name=n; value={Boolean=b}}
  date(n:string,d:Date.date):Bson.element = {name=n; value={Date=d}}
  regexp(n:string,re:Bson.regexp):Bson.element = {name=n; value={Regexp=re}}
  null(n:string):Bson.element = {name=n; value={Null=void}}
  code(n:string,c:Bson.code):Bson.element = {name=n; value={Code=c}}
  symbol(n:string,s:Bson.symbol):Bson.element = {name=n; value={Symbol=s}}
  codescope(n:string,cs:Bson.codescope):Bson.element = {name=n; value={CodeScope=cs}}
  i32(n:string,i:int):Bson.element = {name=n; value={Int32=i}}
  timestamp(n:string,ts:Bson.timestamp):Bson.element = {name=n; value={Timestamp=ts}}
  i64(n:string,i:int):Bson.element = {name=n; value={Int64=i}}
  de(e:Bson.element):Bson.document = [e]
  dl(l:list(Bson.element)):Bson.document = l
  dd(n:string,d:Bson.document):Bson.document = [{name=n; value={Document=d}}]
}}

/**
 * Log functions for MongoDB driver.
 *
 * We can choose various logging methods but these apply globally
 * to all Mongo instances.
 **/

type Mongo.logtype = {stdout} / {stderr} / {logger} / {nomongolog}

MongoLog = {{

  logtype = Mutable.make({stdout})

  @private log_(from, what, logfn, str, v) =
    do match logtype.get() with
      | {stdout} -> println("{from}({what}): {str}")
      | {stderr} -> prerrln("{from}({what}): {str}")
      | {logger} -> logfn(from,str)
      | {nomongolog} -> void
    v

  info(from, str, v) = log_(from,"Info",Log.info,str,v)
  debug(from, str, v) = log_(from,"Debug",Log.debug,str,v)
  warning(from, str, v) = log_(from,"Warning",Log.warning,str,v)
  error(from, str, v) = log_(from,"Error",Log.error,str,v)

  fatal(from, str, v) =
    do match logtype.get() with
      | {stdout} -> println("{from}(Fatal): {str}")
      | {stderr} -> prerrln("{from}(Fatal): {str}")
      | {logger} -> Log.fatal(from,str)
      | {nomongolog} -> void
    System.exit(v)

}}

ML = MongoLog

@server_private
Bson = {{

  /**
   * Type codes as per BSON spec.
   **/
  tEoo = 0x00
  tDouble = 0x01
  tString = 0x02
  tDocument = 0x03
  tArray = 0x04
  tBinary = 0x05
  tObjectID = 0x07
  tBoolean = 0x08
  tDate = 0x09
  tNull = 0x0a
  tRegexp = 0x0b
  tCode = 0x0d
  tSymbol = 0x0e
  tCodeScope = 0x0f
  tInt32 = 0x10
  tTimestamp = 0x11
  tInt64 = 0x12
  tMin = 0xff
  tMax = 0x7f

  /** Convenience function, dump string as hex and ascii */
  dump = (%% BslMongo.Bson.dump %%: int, string -> string)

  /** Return new Bson Object ID */
  new_oid = (%% BslMongo.Bson.new_oid %%: void -> string)

  /** Get OID from string */
  oid_of_string = (%% BslMongo.Bson.oid_of_string %%: string -> string)

  /** Get string from OID */
  oid_to_string = (%% BslMongo.Bson.oid_to_string %%: string -> string)

  /**
   * Return the type number (BSON) of an element.
   **/
  etype(element:Bson.element): int =
    match element.value with
    | {Double=_} -> tDouble
    | {String=_} -> tString
    | {Document=_} -> tDocument
    | {Array=_} -> tArray
    | {Binary=_} -> tBinary
    | {ObjectID=_} -> tObjectID
    | {Boolean=_} -> tBoolean
    | {Date=_} -> tDate
    | {Null=_} -> tNull
    | {Regexp=_} -> tRegexp
    | {Code=_} -> tCode
    | {Symbol=_} -> tSymbol
    | {CodeScope=_} -> tCodeScope
    | {Int32=_} -> tInt32
    | {Timestamp=_} -> tTimestamp
    | {Int64=_} -> tInt64
    | {Min=_} -> tMin
    | {Max=_} -> tMax

  /**
   * Return the key of an element.
   **/
  key(element:Bson.element): string = element.name

  /**
   * Update the key of an element.
   **/
  set_key(element:Bson.element, name:string): Bson.element = { element with ~name }

  /**
   * Find an element by key in a bson object.
   **/
  find_element(bson:Bson.document, name:string): option(Bson.element) =
    List.find((b0 -> key(b0) == name),bson)

  /**
   * Find the first of one of a list of keys in a document.
   **/
  find_elements(bson:Bson.document, names:list(string)): option((string, Bson.element)) =
    rec aux(d:list(Bson.element)) =
      match d with
      | {hd=e; tl=rest} ->
        ekey = key(e)
        (match List.find((n -> n == ekey),names) with
         | {some=k} -> {some=(k,e)}
         | {none} -> aux(rest))
      | {nil} -> {none}
    aux(bson)

  /**
   * Find key of given name in bson object.
   * We only look at the current level, mostly it's for finding
   * "ok" or "errval" etc. in replies.
   **/
  find(bson:Bson.document, name:string): option(Bson.document) =
    Option.map((b -> [b]),find_element(bson, name))

  /**
   * Some type-specific versions of [find], search for [key]
   * in [bson] object and return required type, if possible.
   * Note that if the key exists but is of the wrong type
   * then you will still get [\{none\}].
   **/

  find_bool(bson:Bson.document, name:string): option(bool) =
    match Option.map((e -> e.value),find_element(bson, name)) with
    | {some={Boolean=tf}} -> {some=tf}
    | {some={Int32=0}} -> {some=false}
    | {some={Int32=_}} -> {some=true}
    | {some={Int64=0}} -> {some=false}
    | {some={Int64=_}} -> {some=true}
    | {some={Double=0.0}} -> {some=false}
    | {some={Double=_}} -> {some=true}
    | _ -> {none}

  find_int(bson:Bson.document, name:string): option(int) =
    match Option.map((e -> e.value),find_element(bson, name)) with
    | {some={Int32=i}} -> {some=i}
    | {some={Int64=i}} -> {some=i}
    | {some={Double=d}} -> {some=Float.to_int(d)}
    | _ -> {none}

  find_float(bson:Bson.document, name:string): option(float) =
    match Option.map((e -> e.value),find_element(bson, name)) with
    | {some={Int32=i}} -> {some=Float.of_int(i)}
    | {some={Int64=i}} -> {some=Float.of_int(i)}
    | {some={Double=d}} -> {some=d}
    | _ -> {none}

  find_string(bson:Bson.document, name:string): option(string) =
    match Option.map((e -> e.value),find_element(bson, name)) with
    | {some={String=str}} -> {some=str}
    | {some={Int32=i}} -> {some=Int.to_string(i)}
    | {some={Int64=i}} -> {some=Int.to_string(i)}
    | {some={Double=d}} -> {some=Float.to_string(d)}
    | {some={Null=_}} -> {some=""}
    | _ -> {none}

  find_doc(bson:Bson.document, name:string): option(Bson.document) =
    match Option.map((e -> e.value),find_element(bson, name)) with
    | {some={Document=doc}} -> {some=doc}
    | {some=element} -> {some=[{~name; value=element}]}
    | _ -> {none}

  find_dot(doc:Bson.document, dot:string, find:(Bson.document, string -> option('a))): option('a) =
    rec aux(doc, l) =
      match l with
      | [] -> {none}
      | [key] -> find(doc,key)
      | [key|rest] ->
         (match find_doc(doc,key) with
          | {some=subdoc} -> aux(subdoc, rest)
          | {none} -> {none})
    aux(doc, String.explode(".",dot))

  dot_bool(doc:Bson.document, dot:string): option(bool) = find_dot(doc, dot, find_bool)
  dot_int(doc:Bson.document, dot:string): option(int) = find_dot(doc, dot, find_int)
  dot_float(doc:Bson.document, dot:string): option(float) = find_dot(doc, dot, find_float)
  dot_string(doc:Bson.document, dot:string): option(string) = find_dot(doc, dot, find_string)
  dot_doc(doc:Bson.document, dot:string): option(Bson.document) = find_dot(doc, dot, find_doc)

  /**
   * Return the type of a matching Bson key.
   **/
  find_type(bson:Bson.document, name:string): option(int) = Option.map(etype,find_element(bson,name))

  /**
   * Return a list of the keys in a bson object.
   **/
  keys(bson:Bson.document): list(string) = List.map(key, bson)

  /**
   * Iterate over the elements in a bson object.
   **/
  iter(f:(Bson.element -> void), bson:Bson.document) : void =
    List.iter(f,bson)

  /**
   * Map over the elements in a bson object.
   **/
  map(f:(Bson.element -> Bson.element), bson:Bson.document) : Bson.document =
    List.map(f,bson)

  /**
   * Fold over the elements in a bson object.
   **/
  fold(f:(Bson.element, 'a -> 'a), bson:Bson.document, acc:'a) : 'a =
    List.fold(f,bson,acc)

  /**
   * Find a particular element.
   **/
  find_raw(f:(Bson.element -> bool), bson:Bson.document): option(Bson.element) =
    List.find(f,bson)

  /**
   * Remove the ObjectID element from a document.
   **/
  remove_id(doc:Bson.document): Bson.document =
    List.filter((e -> match e.value with | {ObjectID=_} -> {false} | _ -> {true}),doc)

  /**
   * Sort the elements in a document by lexicographic order on keys.
   **/
  sort_document(doc:Bson.document): Bson.document = List.sort_by(Bson.key,doc)

  /**
   * Attempt to turn a bson document into a string which looks like
   * the mongo shell syntax but with explicit element types.
   **/
  @private string_of_value(value:Bson.value): string =
    match value with
    | {Double=v} -> "Double {v}"
    | {String=v} -> "String {v}"
    | {Document=v} -> "Document {to_string(v)}"
    | {Array=v} -> "Array {to_string(v)}"
    | {Binary=v} -> "Binary {v}"
    | {ObjectID=v} -> "ObjectID {oid_to_string(v)}"
    | {Boolean=v} -> "Boolean {v}"
    | {Date=v} -> "Date {v}"
    | {Null=_} -> "Null"
    | {Regexp=v} -> "Regexp {v}"
    | {Code=v} -> "Code {v}"
    | {Symbol=v} -> "Symbol {v}"
    | {CodeScope=v} -> "CodeScope {v}"
    | {Int32=v} -> "Int32 {v}"
    | {Timestamp=(t,i)} -> "Timestamp \{ \"t\" : {t}, \"i\" : {i}"
    | {Int64=v} -> "Int64 {v}"
    | {Min=_} -> "Min"
    | {Max=_} -> "Max"

  @private string_of_element(element:Bson.element): string = "\"{element.name}\" : {string_of_value(element.value)}"

  to_string(bson:Bson.document): string = "\{ "^(String.concat(", ",List.map(string_of_element,bson)))^" \}"

  /**
   * Same as to_string except we miss out the tags showing the
   * actual name of the element.
   **/
  @private pretty_of_value(value:Bson.value): string =
    match value with
    | {Double=v} -> "{v}"
    | {String=v} -> "\"{v}\""
    | {Document=v} -> "{to_pretty(v)}"
    | {Array=v} -> "{pretty_of_array(v)}"
    | {Binary=_} -> "<BINARY>"
    | {ObjectID=v} -> "{oid_to_string(v)}"
    | {Boolean=v} -> "{v}"
    | {Date=v} -> "{v}"
    | {Null=_} -> "null"
    | {Regexp=(re,opts)} -> "REGEXP(/{re}/{opts})"
    | {Code=v} -> "CODE({v})"
    | {Symbol=v} -> "SYMBOL({v})"
    | {CodeScope=v} -> "{v}"
    | {Int32=v} -> "{v}"
    | {Timestamp=(t,i)} -> "\{ \"t\" : {t}, \"i\" : {i} \}"
    | {Int64=v} -> "{v}L"
    | {Min=_} -> "min"
    | {Max=_} -> "max"

  @private pretty_of_element(element:Bson.element): string =
    "\"{element.name}\" : {pretty_of_value(element.value)}"

  @private pretty_of_array(a:Bson.document): string =
    "["^(String.concat(", ",List.map((e -> pretty_of_value(e.value)),a)))^"]"

  to_pretty(bson:Bson.document): string = "\{ "^(String.concat(", ",List.map(pretty_of_element,bson)))^" \}"

  /**
   * Convert a result value into a more friendly string.
   * Errors can be internal (just a string) or could be document
   * returned by mongo.  Which may be an error even if the
   * outcome is "success".
   **/
  string_of_doc(doc:Bson.document): string =
    ok =
      match find_int(doc,"ok") with
      | {some=0} -> "<not ok>"
      | {some=1} -> "<ok>"
      | {some=n} -> "<not ok> (Weird ok number {n})"
      | {none} -> "<unknown ok status>"
    err = match find_string(doc, "err") with | {some=""} -> "" | {some=err} -> "<err=\"{err}\">" | {none} -> ""
    code = match find_int(doc, "code") with | {some=code} -> "<code={code}>" | {none} -> ""
    n = match find_int(doc, "n") with | {some=n} -> "<n={n}>" | {none} -> ""
    errmsg = match find_string(doc, "errmsg") with | {some=""} -> "" | {some=errmsg} -> "<errmsg=\"{errmsg}\">" | {none} -> ""
    String.concat(" ",List.filter((s -> s != ""),[ok,err,code,n,errmsg]))

  /**
   * OPA to Bson
   **/

  rec_to_bson(v:'a, fields:OpaType.fields): Bson.document =
    List.flatten(OpaValue.Record.fold_with_fields((field, tyfield, value, bson ->
                                                    name = OpaValue.Record.name_of_field_unsafe(field)
                                                    res = opa_to_document(name, value, tyfield)
                                                    [res | bson]), v, fields, []))

  list_to_bson(key:string, v:'a, ty:OpaType.ty): Bson.document =
    doc = List.flatten(List.fold_index((i, v, acc ->
                                         (doc = opa_to_document("{i}", v, ty)
                                          ((doc +> acc):list(Bson.document)))), @unsafe_cast(v), []))
    [H.arr(key,doc)]

  opa_to_document(key:string, v:'a, ty:OpaType.ty): Bson.document =
    match ty with
    | {TyName_args=[]; TyName_ident="void"} -> [H.null(key)]
    | {TyConst={TyInt={}}} -> [H.i64(key,(@unsafe_cast(v):int))]
    | {TyConst={TyString={}}} -> [H.str(key,(@unsafe_cast(v):string))]
    | {TyConst={TyFloat={}}} -> [H.dbl(key,(@unsafe_cast(v):float))]
    | {TyName_args=[]; TyName_ident="bool"} -> [H.bool(key,(@unsafe_cast(v):bool))]
    | {TyRecord_row = row}
    | {TyRecord_row = row; TyRecord_rowvar = _} ->
      (match row with
       | [] ->
          [H.null(key)]
       | [{label=name; ty=ty}] ->
          if OpaType.is_void(ty)
          then [H.doc(key,[H.null(name)])]
          else [H.doc(key,rec_to_bson(v, row))]
       | _ ->
          [H.doc(key,rec_to_bson(v, row))])
    | {TyName_args=[]; TyName_ident="Date.date"} -> [H.date(key,(@unsafe_cast(v):Date.date))]
    | {TyName_args=[]; TyName_ident="Bson.binary"} -> [H.binary(key,(@unsafe_cast(v):Bson.binary))]
    | {TyName_args=[]; TyName_ident="Bson.regexp"} -> [H.regexp(key,(@unsafe_cast(v):Bson.regexp))]
    | {TyName_args=[]; TyName_ident="Bson.code"} -> [H.code(key,(@unsafe_cast(v):Bson.code))]
    | {TyName_args=[]; TyName_ident="Bson.symbol"} -> [H.symbol(key,(@unsafe_cast(v):Bson.symbol))]
    | {TyName_args=[]; TyName_ident="Bson.codescope"} -> [H.codescope(key,(@unsafe_cast(v):Bson.codescope))]
    | {TyName_args=[]; TyName_ident="Bson.int32"} -> [H.i32(key,(@unsafe_cast(v):Bson.int32))]
    | {TyName_args=[]; TyName_ident="Bson.timestamp"} -> [H.timestamp(key,(@unsafe_cast(v):Bson.timestamp))]
    | {TyName_args=[ty]; TyName_ident="Bson.register"} ->
       (match (@unsafe_cast(v):Bson.register('a)) with
        | {present=sv} -> opa_to_document(key,sv,ty)
        | {absent} -> [])
    | {TySum_col = col}
    | {TySum_col = col; TySum_colvar = _} ->
      if List.mem([{label="false"; ty={TyRecord_row=[]}}],col) // <-- ? ! :-(
      then [H.bool(key,(@unsafe_cast(v):bool))]
      else [H.doc(key,rec_to_bson(v, OpaType.fields_of_fields_list(v, col).f1))]
    | {TyName_args=[{TyName_args=[]; TyName_ident="Bson.element"}]; TyName_ident="list"}
    | {TyName_ident="Bson.document"; TyName_args=_} -> [H.doc(key,@unsafe_cast(v))]
    | {TyName_args=[lty]; TyName_ident="list"} -> list_to_bson(key, @unsafe_cast(v), lty)
    | {TyName_args = tys; TyName_ident = tyid} -> opa_to_document(key, v, OpaType.type_of_name(tyid, tys))
    | _ -> ML.fatal("Bson.opa_to_bson","unknown value {v} of type {OpaType.to_pretty(ty)}",-1)

  opa_to_bson(v:'a, ty_opt:option(OpaType.ty)): Bson.document =
    match
      (match (match ty_opt with {some=ty} -> ty | {none} -> @typeof(v)) with
       | {TyName_args = tys; TyName_ident = tyid} -> OpaType.type_of_name(tyid, tys)
       | ty -> ty)
    with
    | {TyRecord_row=row}
    | {TyRecord_row=row; TyRecord_rowvar=_} ->
      (match row with
       | [] -> [H.null("value")]
       | [{label=name; ty=ty}] -> if OpaType.is_void(ty) then [H.null(name)] else rec_to_bson(v, row)
       | _ -> rec_to_bson(v, row))
    | {TySum_col=col}
    | {TySum_col=col; TySum_colvar=_} ->
      if List.mem([{label="false"; ty={TyRecord_row=[]}}],col)
      then [H.bool("value",(@unsafe_cast(v):bool))]
      else rec_to_bson(v, OpaType.fields_of_fields_list(v, col).f1)
    | ty ->
      opa_to_document("value", v, ty)

  opa2doc(v:'a): Bson.document = opa_to_bson(v,{some=@typeval('a)})

  /**
   * Bson to OPA
   **/

  rec bson_to_opa(bson:Bson.document, ty:OpaType.ty, valname:string): option('a) =
    //do println("bson_to_opa:\n  bson={Bson.to_pretty(bson)}\n  ty={OpaType.to_pretty(ty)}\n  valname={valname}")

    error(str, v) = ML.error("Bson.bson_to_opa", str, v)
    fatal(str) = ML.fatal("Bson.bson_to_opa", str, -1)

    isrcrdtype(ty:OpaType.ty): bool =
      match ty with
      | {TyName_args=[_]; TyName_ident="option"} -> true
      | {TyName_args=[_]; TyName_ident="Bson.register"} -> true
      | _ -> false

    rec element_to_rec(doc:Bson.document, fields:OpaType.fields): option('a) =
      match fields with
      | [{label=name; ty=ty}] ->
        if OpaType.is_void(ty)
        then
          match OpaValue.Record.field_of_name(name) with
          | {none} -> {none}
          | {some=field} -> {some=@unsafe_cast(OpaValue.Record.make_simple_record(field))}
          end
        else element_to_rec2(doc,fields)
      | _ -> element_to_rec2(doc,fields)

    and element_to_rec2(doc:Bson.document, fields:OpaType.fields): option('a) =
      //do println("element_to_rec2:\n  doc={to_pretty(doc)}\n  fields={OpaType.to_pretty_fields(fields)}")

      rec optreg(name, field, frest, elements, acc) =
        match field.ty with
        | {TyName_args=[_]; TyName_ident="option"} ->
          (match OpaValue.Record.field_of_name(field.label) with
           | {none} -> error("Missing field {name}", (acc, true))
           | {some=fieldname} -> aux(elements,frest,[(fieldname,@unsafe_cast({none}))|acc]))
        | {TyName_args=[_]; TyName_ident="Bson.register"} ->
          (match OpaValue.Record.field_of_name(field.label) with
           | {none} -> error("Missing field {name}", (acc, true))
           | {some=fieldname} -> aux(elements,frest,[(fieldname,@unsafe_cast({absent}))|acc]))
        | _ -> error("name mismatch \"{field.label}\" vs. \"{name}\"",(acc, true))

      and aux(elements, fields, acc) =
        //do println("element_to_rec2(aux):\n  elements={to_pretty(elements)}")
        //do println("  fields={OpaType.to_pretty_fields(fields)}")
        match (elements, fields) with
        | ([element|erest],[field|frest]) ->
            name = Bson.key(element)
            //do println("element_to_rec2:\n  element={pretty_of_element(element)}")
            //do println("  name={name}\n  field={OpaType.to_pretty_fields([field])}")
            (match String.ordering(field.label,name) with
             | {eq} ->
               (match element_to_opa(element, field.ty) with
                | {none} ->
                  error("Failed with field {name}, document {to_pretty(doc)} and type {OpaType.to_pretty(field.ty)}", (acc, true))
                | {some=value} ->
                  (match OpaValue.Record.field_of_name(name) with
                   | {none} -> error("Missing field {name}", (acc, true))
                   | {some=field} -> aux(erest,frest,[(field,value)|acc])))
              | {lt} -> optreg(name, field, frest, [element|erest], acc)
              | {gt} -> error("name mismatch \"{field.label}\" vs. \"{name}\"",(acc, true)))
        | ([],[]) -> (acc,false)
        | ([],[field|frest]) -> optreg("absent field", field, frest, [], acc)
        | (_erest,_frest) -> (acc,true)
      (flds, err) = aux(Bson.sort_document(doc), fields, [])
      rcrd = List.fold(((field,value), rcrd ->OpaValue.Record.add_field(rcrd, field, value)),
                       flds,OpaValue.Record.empty_constructor())
      if err
      then error("Failed with fields {OpaType.to_pretty_fields(fields)} document {to_pretty(doc)}",{none})
      else {some=@unsafe_cast(OpaValue.Record.make_record(rcrd))}

    and element_to_opa(element:Bson.element, ty:OpaType.ty): option('a) =
      //do println("element_to_opa:\n  element={pretty_of_element(element)}\n  ty={OpaType.to_pretty(ty)}")
      match ty with
      | {TyName_args=[({TyName_args=[]; TyName_ident="Bson.element"}:OpaType.ty)]; TyName_ident="list"}
      | {TyName_args=_; TyName_ident="Bson.document"} ->
        (match element with
         | {value={Document=doc} ...} -> {some=@unsafe_cast(doc)}
         | element -> error("expected Bson.document, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="void"} ->
        (match element with
         | {value={Null=_} ...} -> {some=@unsafe_cast(void)}
         | element -> error("expected void, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="Bson.int32"}
      | {TyConst={TyInt={}}} ->
        (match element with
         | {value={Boolean=tf} ...} -> {some=@unsafe_cast(if tf then 1 else 0)}
         | {value={Int32=i} ...} -> {some=@unsafe_cast(i)}
         | {value={Int64=i} ...} -> {some=@unsafe_cast(i)}
         | {value={Double=d} ...} -> {some=@unsafe_cast(Float.to_int(d))}
         | {value={String=s} ...} -> {some=@unsafe_cast(Int.of_string(s))}
         | element -> error("expected int, got {element}",{none}))
      | {TyConst={TyString={}}} ->
        (match element with
         | {value={Boolean=tf} ...} -> {some=@unsafe_cast(Bool.to_string(tf))}
         | {value={Int32=i} ...} -> {some=@unsafe_cast(Int.to_string(i))}
         | {value={Int64=i} ...} -> {some=@unsafe_cast(Int.to_string(i))}
         | {value={Double=d} ...} -> {some=@unsafe_cast(Float.to_string(d))}
         | {value={String=s} ...} -> {some=@unsafe_cast(s)}
         | element -> error("expected string, got {element}",{none}))
      | {TyConst={TyFloat={}}} ->
        (match element with
         | {value={Boolean=tf} ...} -> {some=@unsafe_cast(if tf then 1.0 else 0.0)}
         | {value={Int32=i} ...} -> {some=@unsafe_cast(Float.of_int(i))}
         | {value={Int64=i} ...} -> {some=@unsafe_cast(Float.of_int(i))}
         | {value={Double=d} ...} -> {some=@unsafe_cast(d)}
         | {value={String=s} ...} -> {some=@unsafe_cast(Float.of_string(s))}
         | element -> error("expected float, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="bool"} ->
        (match element with
         | {value={Boolean=tf} ...} -> {some=@unsafe_cast(tf)}
         | {value={Int32=i} ...} -> {some=@unsafe_cast(i != 0)}
         | {value={Int64=i} ...} -> {some=@unsafe_cast(i != 0)}
         | {value={Double=d} ...} -> {some=@unsafe_cast(d != 0.0)}
         | {value={String="true"} ...} -> {some=@unsafe_cast(true)}
         | {value={String="false"} ...} -> {some=@unsafe_cast(false)}
         | element -> error("expected bool, got {element}",{none}))
      | {TyName_args=[ty]; TyName_ident="option"} ->
        (match element with
         | {name=_key; value={Document=doc}} ->
           //do println("ty={OpaType.to_pretty(ty)} key={key} doc={doc}")
           (match Bson.find_elements(doc,["some","none"]) with
            | {some=("some",element)} ->
              (match element_to_opa(element, ty) with
              | {some=v} -> {some=@unsafe_cast({some=v})}
              | {none} -> {none})
            | {some=("none",_)} -> {some=@unsafe_cast({none})}
            | _ -> {none}
           )
         | element -> error("expected option, got {element}",{none}))
      | {TyName_args=[ty]; TyName_ident="Bson.register"} ->
        //do println("register: element={to_pretty([element])}")
        (match element_to_opa(element,ty) with
         | {some=v} -> {some=@unsafe_cast({present=v})}
         | {none} -> {some=@unsafe_cast({absent})})
      | {TyName_args=[ty]; TyName_ident="list"} ->
        (match element with
         | {name=_key; value={Array=doc}} ->
           //do println("list:\n  ty={OpaType.to_pretty(ty)}\n  key={key}\n  doc={to_pretty(doc)}")
           lst =
             (match doc with
              | [] -> []
              | [e|_] ->
                 /* We can't actually rely upon the order in which these
                  * array elements are stored, we could be at the mercy
                  * of user-defined values so we allow either storage order.
                  * We do insist upon consecutive values, however.
                  */
                 doc = if e.name == "0" then List.rev(doc) else doc
                 len = List.length(doc) - 1
                 List.fold_index((i, element, l ->
                                  do if "{len-i}" != Bson.key(element)
                                     then fatal("Array to list index mismatch {doc}")
                                  //do println("list({i}): element={pretty_of_element(element)}")
                                  match element_to_opa(element, ty) with
                                  | {some=v} -> [v | l]
                                  | {none} -> fatal("Failed for list element {element} type {OpaType.to_pretty(ty)}")),
                                 doc,[]))
             {some=@unsafe_cast(lst)}
         | element -> error("expected list, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="Date.date"} ->
        (match element with
         | {value={Date=dt} ...} -> {some=@unsafe_cast(dt)}
         | element -> error("expected date, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="Bson.binary"} ->
        (match element with
         | {value={Binary=bin} ...} -> {some=@unsafe_cast(bin)}
         | element -> error("expected binary, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="Bson.regexp"} ->
        (match element with
         | {value={Regexp=re} ...} -> {some=@unsafe_cast(re)}
         | element -> error("expected regexp, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="Bson.code"} ->
        (match element with
         | {value={Code=c} ...} -> {some=@unsafe_cast(c)}
         | element -> error("expected code, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="Bson.symbol"} ->
        (match element with
         | {value={Symbol=s} ...} -> {some=@unsafe_cast(s)}
         | element -> error("expected symbol, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="Bson.codescope"} ->
        (match element with
         | {value={CodeScope=cs} ...} -> {some=@unsafe_cast(cs)}
         | element -> error("expected codescope, got {element}",{none}))
      | {TyName_args=[]; TyName_ident="Bson.timestamp"} ->
        (match element with
         | {value={Timestamp=ts} ...} -> {some=@unsafe_cast(ts)}
         | element -> error("expected timestamp, got {element}",{none}))
      | {TyRecord_row=row}
      | {TyRecord_row=row; TyRecord_rowvar=_} ->
          //do println("row:\n  row={OpaType.to_pretty(ty)}")
          (match element with
           | {value={Document=doc} ...} ->
             /* Normally, nested values will be documents but some basic types,
              * for example, options are also records so we need to specifically
              * exclude them here.  Unfortunately, we have to go digging into the
              * type to get the record's target type.
              */
             irt = (match (doc,ty) with
                    | ([_],{TyRecord_row=[{~ty; ...}]; ...}) -> isrcrdtype(ty)
                    | _ -> false)
             doc = Bson.remove_id(doc)
             //do println("  doc={to_pretty(doc)}\n  keys={Bson.keys(doc)} irt={irt}")
             if irt
             then element_to_rec([element],row)
             else element_to_rec(Bson.remove_id(doc), row)
           | _ -> element_to_rec([element],row))
      | {TySum_col=col}
      | {TySum_col=col; TySum_colvar=_} ->
        (ltyfield, doc) =
          (match element with
           | {value={Document=doc} ...} -> (List.sort(Bson.keys(doc)), doc) // <-- We might get away with List.rev here???
           | _ -> ([Bson.key(element)], [element]))
        (match OpaSerialize.fields_of_fields_list2(ltyfield, col) with
         | {some=fields} -> element_to_rec(doc, fields)
         | {none} -> error("Fields ({OpaType.to_pretty_lfields(col)}) not found in sum type ({List.to_string(ltyfield)})",{none}))
      | {TyName_args = tys; TyName_ident = tyid} ->
        element_to_opa(element, OpaType.type_of_name(tyid, tys))
      | _ -> fatal("unknown type {OpaType.to_pretty(ty)}")

    and _internal_document_to_opa(key:string, doc:Bson.document, ty:OpaType.ty): option('a) =
      match Bson.find_element(doc,key) with
      | {some=element} -> element_to_opa(element, ty)
      | {none} -> {none}

    bson_noid = Bson.remove_id(bson)
    match bson_noid with
    | [element] ->
       element_to_opa(element, ty)
    | bson ->
       (match Bson.find_element(bson,valname) with
        | {some=element} -> element_to_opa(element, ty)
        | {none} -> element_to_opa(H.doc(valname,bson_noid), ty)) // assume bare record

  doc2opa(doc:Bson.document): option('a) = bson_to_opa(doc,@typeval('a),"value")

}}

// End of file bson.opa
