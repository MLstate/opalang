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
 * A program-level method for constructing Bson documents for select
 * and update in a friendly manner.
 *
 * Module [MongoSelectUpdate] provides common support for the
 * [MongoSelect] and [MongoUpdate] modules.
 *
 * The latter modules provide a type-safety layer for queries
 * to the MongoDB server for OPA values stored there.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

type Mongo.su_status =
    {su_select} /** specific to select, $gt **/
  / {su_update} /** specific to update, $inc **/
  / {su_either} /** applies to either select or update, $comment **/
  / {su_key}    /** neither, a valid key, "a" **/

MongoSelectUpdate = {{

  @private ML = MongoLog
  @private H = Bson.Abbrevs

  empty(): Bson.document = []

  key(name:string, s:Bson.document): Bson.document = [H.doc(name,s)]

  path(path:list(string), s:Bson.document): Bson.document = List.fold_right((s, name -> [H.doc(name,s)]),path,s)

  double(s:Bson.document, name:string, d:float): Bson.document = [H.dbl(name,d)|s]
  string(s:Bson.document, name:string, str:string): Bson.document = [H.str(name,str)|s]
  doc(s:Bson.document, name:string, d:Bson.document): Bson.document = [H.doc(name,d)|s]
  array(s:Bson.document, name:string, l:list('b)): Bson.document = List.flatten([Bson.list_to_bson(name,l,@typeval('b)),s])
  binary(s:Bson.document, name:string, bin:Bson.binary): Bson.document = [H.binary(name,bin)|s]
  id(s:Bson.document, name:string, id:Bson.oid): Bson.document = [H.oid(name,Bson.oid_of_string(id))|s]
  newid(s:Bson.document, name:string): Bson.document = [H.oid(name,Bson.new_oid(void))|s]
  bool(s:Bson.document, name:string, b:bool): Bson.document = [H.bool(name,b)|s]
  date(s:Bson.document, name:string, d:Date.date): Bson.document = [H.date(name,d)|s]
  null(s:Bson.document, name:string): Bson.document = [H.null(name)|s]
  regexp(s:Bson.document, name:string, re:Bson.regexp): Bson.document = [H.regexp(name,re)|s]
  code(s:Bson.document, name:string, c:Bson.code): Bson.document = [H.code(name,c)|s]
  symbol(s:Bson.document, name:string, sym:Bson.symbol): Bson.document = [H.symbol(name,sym)|s]
  codescope(s:Bson.document, name:string, cs:Bson.codescope): Bson.document = [H.codescope(name,cs)|s]
  int32(s:Bson.document, name:string, i:int): Bson.document = [H.i32(name,i)|s]
  timestamp(s:Bson.document, name:string, ts:Bson.timestamp): Bson.document = [H.timestamp(name,ts)|s]
  int64(s:Bson.document, name:string, i:int): Bson.document = [H.i64(name,i)|s]

  oppoly(v:'a, s:Bson.document, op:string): Bson.document =
    rec aux(ty:OpaType.ty) =
      match ty with
      | {TyName_args=[]; TyName_ident="void"} -> null(s,op)
      | {TyConst={TyInt={}}} -> int64(s,op,@unsafe_cast(v))
      | {TyConst={TyString={}}} -> string(s,op,@unsafe_cast(v))
      | {TyConst={TyFloat={}}} -> double(s,op,@unsafe_cast(v))
      | {TyName_args=[]; TyName_ident="bool"} -> bool(s,op,@unsafe_cast(v))
      | {TyName_args=[_]; TyName_ident="list"} -> array(s,op,@unsafe_cast(v))
      | {TyName_args=[]; TyName_ident="Date.date"} -> date(s,op,@unsafe_cast(v))
      | {TyName_args=[]; TyName_ident="Bson.timestamp"} -> timestamp(s,op,@unsafe_cast(v))
      | {TyName_args = tys; TyName_ident = tyid} -> aux(OpaType.type_of_name(tyid,tys))
      | _ -> doc(s,op,Bson.opa2doc(v))
    aux(@typeval('a))

  gti32(i:int, s:Bson.document): Bson.document = int32(s, "$gt", i)
  lti32(i:int, s:Bson.document): Bson.document = int32(s, "$lt", i)
  gtei32(i:int, s:Bson.document): Bson.document = int32(s, "$gte", i)
  ltei32(i:int, s:Bson.document): Bson.document = int32(s, "$lte", i)
  nei32(i:int, s:Bson.document): Bson.document = int32(s, "$ne", i)

  gti64(i:int, s:Bson.document): Bson.document = int64(s, "$gt", i)
  lti64(i:int, s:Bson.document): Bson.document = int64(s, "$lt", i)
  gtei64(i:int, s:Bson.document): Bson.document = int64(s, "$gte", i)
  ltei64(i:int, s:Bson.document): Bson.document = int64(s, "$lte", i)
  nei64(i:int, s:Bson.document): Bson.document = int64(s, "$ne", i)

  gtd(d:float, s:Bson.document): Bson.document = double(s, "$gt", d)
  ltd(d:float, s:Bson.document): Bson.document = double(s, "$lt", d)
  gted(d:float, s:Bson.document): Bson.document = double(s, "$gte", d)
  lted(d:float, s:Bson.document): Bson.document = double(s, "$lte", d)
  ned(d:float, s:Bson.document): Bson.document = double(s, "$ne", d)

  gts(str:string, s:Bson.document): Bson.document = string(s, "$gt", str)
  lts(str:string, s:Bson.document): Bson.document = string(s, "$lt", str)
  gtes(str:string, s:Bson.document): Bson.document = string(s, "$gte", str)
  ltes(str:string, s:Bson.document): Bson.document = string(s, "$lte", str)
  nes(str:string, s:Bson.document): Bson.document = string(s, "$ne", str)

  gtdate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$gt", dt)
  ltdate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$lt", dt)
  gtedate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$gte", dt)
  ltedate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$lte", dt)
  nedate(dt:Date.date, s:Bson.document): Bson.document = date(s, "$ne", dt)

  gtts(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$gt", ts)
  ltts(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$lt", ts)
  gtets(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$gte", ts)
  ltets(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$lte", ts)
  nets(ts:Bson.timestamp, s:Bson.document): Bson.document = timestamp(s, "$ne", ts)

  set_op(s:Bson.document, op:string): Bson.document =
    ((match s with
      | [] -> []
      | [e] -> [H.doc(Bson.key(e),[Bson.set_key(e,op)])]
      | l -> List.map((e -> H.doc(Bson.key(e),[Bson.set_key(e,op)])),l)):Bson.document)

  gt(s:Bson.document): Bson.document = set_op(s, "$gt")
  lt(s:Bson.document): Bson.document = set_op(s, "$lt")
  gte(s:Bson.document): Bson.document = set_op(s, "$gte")
  lte(s:Bson.document): Bson.document = set_op(s, "$lte")
  ne(s:Bson.document): Bson.document = set_op(s, "$ne")

  @private
  boolop_private(op:string, s1:Bson.document, s2:Bson.document): Bson.document =
    [H.arr(op,([H.doc("0",s1),H.doc("1",s2)]:Bson.document))]

  @private
  lboolop_private(op:string, ss:list(Bson.document)): Bson.document =
    match ss with
    | [] -> empty()
    | [s|t] ->
       doc = List.fold_index((i, ss, doc -> [H.doc("{i}",ss)|doc]),[s|t],[])
       [H.arr(op,(doc:Bson.document))]

  and(s1:Bson.document, s2:Bson.document): Bson.document = boolop_private("$and",s1,s2)
  andalso(ss:list(Bson.document)): Bson.document = lboolop_private("$and",ss)
  or(s1:Bson.document, s2:Bson.document): Bson.document = boolop_private("$or",s1,s2)
  orelse(ss:list(Bson.document)): Bson.document = lboolop_private("$or",ss)
  nor(s1:Bson.document, s2:Bson.document): Bson.document = boolop_private("$nor",s1,s2)
  noreither(ss:list(Bson.document)): Bson.document = lboolop_private("$nor",ss)

  all(s:Bson.document, a:list('b)): Bson.document = array(s, "$all", a)
  in(s:Bson.document, a:list('b)): Bson.document = array(s, "$in", a)
  nin(s:Bson.document, a:list('b)): Bson.document = array(s, "$nin", a)

  @private docbool(s:Bson.document, name:string, op:string, tf:bool): Bson.document = doc(s,name,[H.bool(op,tf)])

  exists(s:Bson.document, name:string, tf:bool): Bson.document = docbool(s, name, "$exists", tf)

  mod(s:Bson.document, x:'b, y:'b): Bson.document = array(s, "$mod", [x,y])

  size(s:Bson.document, x:int): Bson.document = int64(s, "$size", x)
  typ(s:Bson.document, t:int): Bson.document = int64(s, "$type", t)

  regex(s:Bson.document, re:string, opts:string): Bson.document = [H.regexp("$regex",(re,opts))|s]

  inc(s:Bson.document): Bson.document = key("$inc",s)
  set(s:Bson.document): Bson.document = key("$set",s)
  unset(s:Bson.document): Bson.document = key("$unset",s)
  push(s:Bson.document): Bson.document = key("$push",s)
  pushAll(s:Bson.document): Bson.document = key("$pushAll",s)
  addToSet(s:Bson.document): Bson.document = key("$addToSet",s)
  pop(s:Bson.document): Bson.document = key("$pop",s)
  pull(s:Bson.document): Bson.document = key("$pull",s)
  pullAll(s:Bson.document): Bson.document = key("$pullAll",s)
  rename(s:Bson.document): Bson.document = key("$rename",s)
  bit(s:Bson.document): Bson.document = key("$bit",s)

  elemMatch(s:Bson.document): Bson.document = key("$elemMatch",s)

  not(s:Bson.document): Bson.document = key("$not",s)

  where(s:Bson.document, whr:string): Bson.document = [H.code("$where",whr)|s]

  returnKey(s:Bson.document, tf:bool): Bson.document = bool(s, "$returnKey", tf)
  maxScan(s:Bson.document, i:int): Bson.document = int64(s, "$maxScan", i)
  query(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$query", d)
  orderby(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$orderby", d)
  explain(s:Bson.document, tf:bool): Bson.document = bool(s, "$explain", tf)
  snapshot(s:Bson.document, tf:bool): Bson.document = bool(s, "$snapshot", tf)
  min(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$min", d)
  max(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$max", d)
  showDiskLoc(s:Bson.document, tf:bool): Bson.document = bool(s, "$showDiskLoc", tf)
  hint(s:Bson.document, d:Bson.document): Bson.document = doc(s, "$hint", d)
  comment(s:Bson.document, c:string): Bson.document = string(s, "$comment", c)
  natural(s:Bson.document, i:int): Bson.document = int32(s, "$natural", i)

  @private T = MongoTypeSelect

  @private union(ss) = List.fold(StringSet.union,ss,StringSet.empty)
  // I'm not guaranteeing that all of these have been classified correctly!!!
  @private update_names =
    StringSet.From.list(["$inc", "$set", "$unset", "$push", "$pushAll", "$addToSet",
                         "$pop", "$pull", "$pullAll", "$rename", "$bit"])
  @private no_array_select_names =
    StringSet.From.list(["$gt", "$lt", "$gte", "$lte", "$ne",
                         "$regex", "$mod",
                         "$not", "$elemMatch",
                         "$where",
                         "$query", "$orderby"])
  @private transparent_select_names =
    StringSet.From.list(["$exists", "$type", "$size", ])
  @private array_select_names =
    StringSet.From.list(["$and", "$or", "$nor", "$all", "$in", "$nin"])
  @private select_names = union([no_array_select_names,transparent_select_names,array_select_names])
  @private select_or_update_names =
    StringSet.From.list(["$returnKey", "$maxScan", "$explain", "$snapshot",
                         "$min", "$max", "$showDiskLoc", "$hint", "$comment"])
  //@private all_names = union([update_names,select_names,select_or_update_names])
  //@private string_of_set(set) = "[{String.concat(", ",StringSet.fold((s, strs -> [s|strs]),set,[]))}]"

  @private string_of_su_status(sut:Mongo.su_status): string =
    match sut with
    | {su_select} -> "select"
    | {su_update} -> "update"
    | {su_either} -> "either"
    | {su_key} -> "key"

  // Note there will be shenanigans here, you can get both reduce and $reduce!!!
  @private status(name:string): Mongo.su_status =
    if StringSet.mem(name,select_names)
    then {su_select}
    else if StringSet.mem(name,update_names)
    then {su_update}
    else if StringSet.mem(name,select_or_update_names)
    then {su_either}
    else {su_key}

  // Ordering update >> select >> anything else
  @private merge(sus1:Mongo.su_status, sus2:Mongo.su_status): Mongo.su_status =
    match (sus1, sus2) with
    | ({su_update},_) -> {su_update}
    | (_,{su_update}) -> {su_update}
    | ({su_select},_) -> {su_select}
    | (_,{su_select}) -> {su_select}
    | (_,_) -> {su_either}

  // We should have removed keys before calling this
  @private sutok(sus1:Mongo.su_status, sus2:Mongo.su_status): bool =
    match (sus1, sus2) with
    | ({su_either},_) -> true
    | (_,{su_either}) -> true
    | ({su_select},{su_select}) -> true
    | ({su_update},{su_update}) -> true
    | _ -> false // we don't get su_key here

  @private
  type_of_bson_value(value:Bson.value): (Mongo.su_status, OpaType.ty) =
    match value with
    | {Double=_} -> ({su_either},T.tfloat)
    | {String=_} -> ({su_either},T.tstring)
    | {Document=d} -> type_of_bson_document(d)
    | {Array=[]} -> ({su_either},T.tempty) // or maybe list('a) or list({})???
    | {Array=[{name=_name; ~value}|_]} -> // comes from an OPA list or intmap so all same type
       //dbg do println("type_of_bson_value: Array({name}) value={value}")
       (sut,ty) = type_of_bson_value(value)
       (sut,T.tlist(ty))
    | {Binary=_} -> ({su_either},T.tbinary)
    | {ObjectID=_} -> ({su_either},T.toid)
    | {Boolean=_} -> ({su_either},T.tbool)
    | {Date=_} -> ({su_either},T.tdate)
    | {Null=_} -> ({su_either},T.tvoid)
    | {Regexp=_} -> ({su_either},T.tregexp)
    | {Code=_} -> ({su_either},T.tcode)
    | {Symbol=_} -> ({su_either},T.tsymbol)
    | {CodeScope=_} -> ({su_either},T.tcodescope)
    | {Int32=_} -> ({su_either},T.tint)
    | {RealInt32=_} -> ({su_either},T.tint32)
    | {Timestamp=_} -> ({su_either},T.ttimestamp)
    | {Int64=_} -> ({su_either},T.tint)
    | {RealInt64=_} -> ({su_either},T.tint64)
    | {Min=_} -> ({su_select},T.tvoid)
    | {Max=_} -> ({su_select},T.tvoid)

  @private sutymrg((sut,ty), (asut,aty)) = (merge(sut,asut),T.tmrgrecs(ty,aty))

  @private
  type_of_bson_element(element:Bson.element): (Mongo.su_status, OpaType.ty) =
    stat = status(element.name)
    if StringSet.mem(element.name,transparent_select_names)
    then (stat,T.tempty)
    else if StringSet.mem(element.name,array_select_names)
    then
      match element.value with
      | {Array=adoc} ->
         tys = List.map(type_of_bson_value,List.map((e -> e.value),adoc))
         //dbg do println("type_of_bson_element: Array({element.name}) tys={List.list_to_string((_,ty) -> OpaType.to_pretty(ty),tys)}")
         List.fold(sutymrg,tys,(stat,T.tempty))
      | _ -> ML.fatal("MongoSelectUpdate.type_of_bson_element",
                      "key {element.name} requires an array value, actually {Bson.to_pretty([element])}",-1)
    else
      match element.name with
      | "$mod" -> (stat,T.tnumeric)
      | _ ->
         (sut,ty) = type_of_bson_value(element.value)
         sut1 = merge(sut,stat)
         if stat == {su_key}
         then (sut1,{TyRecord_row=[{label=element.name; ~ty}]})
         else (sut1,ty)

  @private
  type_of_bson_document(doc:Bson.document): (Mongo.su_status, OpaType.ty) =
    //dbg do println("type_of_bson_document: doc={Bson.to_pretty(doc)}")
    List.fold(sutymrg,List.map(type_of_bson_element,doc),({su_either},T.tempty))

  @private empty_ty(ty) = ty == T.tempty || T.istvar(ty)

  @private /*improper*/subtype(sty:OpaType.ty, ty:OpaType.ty): bool =
    //dbg do println("subtype: sty={OpaType.to_pretty(sty)}\n         ty={OpaType.to_pretty(ty)}")
    missing_label(row, label) =
      labels = List.list_to_string((s -> s),List.map((f -> f.label),row))
      ML.warning("MongoSelectUpdate.subtype","Missing label {label} in row {labels}",false)
    incomparable() =
      ML.warning("MongoSelectUpdate.subtype","Incomparable types {OpaType.to_pretty(sty)} and {OpaType.to_pretty(ty)}",false)
    sty = MongoTypeSelect.explode_dot(sty)
    //dbg do println("explode={OpaType.to_pretty(sty)}")
    esty = empty_ty(sty)
    if sty == ty || esty
    then true
    else if empty_ty(ty)
    then esty
    else
      match (sty,ty) with
         // TODO: We do get sum types from type_of_bson_document, patched for lists, we need to generalise this...
      | ({TySum_col=[[{label="hd"; ty=shdty}, {label="tl"; ty=stlty}], [{label="nil"; ty=snilty}]]},
         {TySum_col=[[{label="hd"; ty=hdty}, {label="tl"; ty=tlty}], [{label="nil"; ty=nilty}]]}) ->
         subtype(shdty,hdty) && subtype(stlty,tlty) && subtype(snilty,nilty)
      | ({TyRecord_row=strow; ...},{TySum_col=tcol; ...}) ->
         (match T.find_row_in_col(strow,tcol) with
          | {some=trow} -> subtype(sty,{TyRecord_row=trow})
          | {none} -> incomparable())
      | ({TyRecord_row=strow; ...},{TyRecord_row=trow; ...}) ->
         List.fold((stf, isty ->
                     isty &&
                     (match List.find((tf -> tf.label == stf.label),trow) with
                      | {some=tf} -> subtype(stf.ty,tf.ty)
                      | {none} -> missing_label(trow, stf.label))),strow,true)
      | ({TyName_args=_; TyName_ident="Bson.numeric"},{TyConst={TyInt={}}})
      | ({TyName_args=_; TyName_ident="Bson.numeric"},{TyConst={TyFloat={}}}) ->
         true // Some arithmetic ops, $mod
      | ({TyName_args=[]; TyName_ident="Bson.regexp"},_)
      | ({TyName_args=[]; TyName_ident="Bson.code"},_)
      | ({TyName_args=[]; TyName_ident="Bson.codescope"},_) ->
         true // For now, until we get types from RE's and Javascript
      | ({TyName_args=stys; TyName_ident=styid},{TyName_args=tys; TyName_ident=tyid}) ->
         if styid == tyid
         then subtypes(stys,tys)
         else subtype(OpaType.type_of_name(styid, stys),OpaType.type_of_name(tyid, tys))
      | ({TyName_args=tys; TyName_ident=tyid},_) -> subtype(OpaType.type_of_name(tyid, tys),ty)
      | (_,{TyName_args=tys; TyName_ident=tyid}) -> subtype(sty,OpaType.type_of_name(tyid, tys))
      | _ -> incomparable()

  @private subtypes(stys:list(OpaType.ty), tys:list(OpaType.ty)): bool =
    match List.for_all2((sty, ty -> subtype(sty,ty)),stys,tys) with
    | {result=tf} -> tf
    | _ -> false

  /**
   * Validate the given document agains the type of the document
   * and the select/update status.
   *
   * Currently, we log a warning.
   **/
  check_strict_select_value_against_type(doc:Bson.document, ty:OpaType.ty, sut:Mongo.su_status): void =
    //dbg do println("check_strict_select_value_against_type:\n  doc={Bson.to_pretty(doc)}\n  ty={OpaType.to_pretty(ty)}")
    //dbg do println("  status={sut}")
    (dsut, dty) = type_of_bson_document(doc)
    //dbg do println("  dsut={dsut}  dty={OpaType.to_pretty(dty)}")
    if sutok(dsut,sut)
    then
      is_subtype = subtype(dty,ty)
      //dbg do println("is_subtype={is_subtype}")
      if is_subtype
      then void
      else
        sutstr = string_of_su_status(sut)
        dtystr = OpaType.to_pretty(dty)
        tystr = OpaType.to_pretty(ty)
        ML.warning("MongoSelectUpdate.check","Inappropriate {sutstr} type {dtystr} for collection({tystr})",void)
    else ML.warning("MongoSelectUpdate.check","Applying {string_of_su_status(dsut)} to {string_of_su_status(sut)}",void)

}}

@abstract type Mongo.select('a) = Bson.document

MongoSelect = {{

  /** Pretty-print a select document **/
  to_pretty(select:Mongo.select('a)): string = "{Bson.to_pretty(select)}"

  /** Create a select wityout any type-checks **/
  unsafe_create(s : Bson.document): Mongo.select('a) = s

  /** Make a select from an arbitrary OPA type **/
  unsafe_make(x:'b): Mongo.select('a) = unsafe_create(Bson.opa2doc(x))

  /** Create a select but enforcing run-time type checks **/
  create(s : Bson.document): Mongo.select('a) =
    do MongoSelectUpdate.check_strict_select_value_against_type(s, @typeval('a), {su_select}) 
    s

  /** Make a select from an OPA type with type checking **/
  make(x:'b): Mongo.select('a) = create(Bson.opa2doc(x))

  /** An empty select document **/
  empty() : Mongo.select('a) = MongoSelectUpdate.empty()

}}

@abstract type Mongo.update('a) = Bson.document

MongoUpdate = {{

  /** Pretty-print a update document **/
  to_pretty(update:Mongo.update('a)): string = "{Bson.to_pretty(update)}"

  /** Create a update wityout any type-checks **/
  unsafe_create(u : Bson.document): Mongo.update('a) = u

  /** Make a update from an arbitrary OPA type **/
  unsafe_make(x:'b): Mongo.update('a) = unsafe_create(Bson.opa2doc(x))

  /** Create a update but enforcing run-time type checks **/
  create(u : Bson.document): Mongo.update('a) =
    do MongoSelectUpdate.check_strict_select_value_against_type(u, @typeval('a), {su_update}) 
    u

  /** Make a update from an OPA type with type checking **/
  make(x:'b): Mongo.update('a) = create(Bson.opa2doc(x))

  /** An empty update document **/
  empty() : Mongo.update('a) = MongoSelectUpdate.empty()

}}

