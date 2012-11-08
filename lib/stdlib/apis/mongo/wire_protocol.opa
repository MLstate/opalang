/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
 * This is a binding for MongoDB for OPA, loosely based around the C drivers.
 *
 * Module [WireProtocol] has low-level routines to generate the binary data understood
 * by the MongoDB server.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

#<Debugvar:MONGO_DEBUG>

type WireProtocol.MsgHeader = {
  messageLength:int;
  requestID:int;
  responseTo:int;
  opCode:int;
}

type WireProtocol.Insert = {
  flags:int;
  fullCollectionName:string;
  documents:list(Bson.document);
}

type WireProtocol.Update = {
  fullCollectionName:string;
  flags:int;
  selector:Bson.document;
  update:Bson.document;
}

type WireProtocol.Query = {
  flags:int;
  fullCollectionName:string;
  numberToSkip:int;
  numberToReturn:int;
  query:Bson.document;
  returnFieldSelector:option(Bson.document);
}

type WireProtocol.GetMore = {
  fullCollectionName:string;
  numberToReturn:int;
  cursorID:int64;
}

type WireProtocol.Delete = {
  fullCollectionName:string;
  flags:int;
  selector:Bson.document;
}

type WireProtocol.KillCursors = {
  numberOfCursorIDs:int;
  cursorIDs:list(int64);
}

type WireProtocol.Msg = {
  message:string;
}

type WireProtocol.Reply = {
  responseFlags:int;
  cursorID:int64;
  startingFrom:int;
  numberReturned:int;
  documents:list(Bson.document);
}

type WireProtocol.MsgBody =
   {Insert:WireProtocol.Insert}
 / {Update:WireProtocol.Update}
 / {Query:WireProtocol.Query}
 / {GetMore:WireProtocol.GetMore}
 / {Delete:WireProtocol.Delete}
 / {KillCursors:WireProtocol.KillCursors}
 / {Msg:WireProtocol.Msg}
 / {Reply:WireProtocol.Reply}

type WireProtocol.Message = {
  MsgHeader:WireProtocol.MsgHeader;
  MsgBody:WireProtocol.MsgBody;
}

@private E = Pack.Encode
@private U = Pack.Unser

WireProtocol = {{

  /* Element codes */
  el_eoo = 0         /*                  End of object? */
  el_double = 1      /* double           Floating point */
  el_string = 2      /* string           UTF-8 string */
  el_object = 3      /* document         Embedded document */
  el_array = 4       /* document         Array */
  el_bindata = 5     /* binary           Binary data */
  el_undefined = 6   /*                  Undefined — Deprecated */
  el_oid = 7         /* (byte*12)        ObjectId */
  el_bool = 8        /* 0                Boolean "false"
                        1                Boolean "true" */
  el_date = 9        /* int64            UTC datetime */
  el_null = 10       /*                  Null value */
  el_regex = 11      /* cstring cstring  Regular expression */
  el_dbref = 12      /* string (byte*12) DBPointer — Deprecated */
  el_code = 13       /* string           JavaScript code */
  el_symbol = 14     /* string           Symbol */
  el_codewscope = 15 /* code_w_s         JavaScript code w/ scope */
  el_int = 16        /* int32            32-bit Integer */
  el_timestamp = 17  /* int64            Timestamp */
  el_long = 18       /* int64            64-bit integer */
  el_minkey = 255    /*                  Min key */
  el_maxkey = 127    /*                  Max key */

  /* Binary subtype */
  st_bin_binary = 0
  st_bin_func = 1
  st_bin_binary_old = 2
  st_bin_uuid = 3
  st_bin_md5 = 5
  st_bin_user = 128

  /* OP codes */
  _OP_REPLY         = 1    /* Reply to a client request. responseTo is set */
  _OP_MSG           = 1000 /* generic msg command followed by a string */
  _OP_UPDATE        = 2001 /* update document */
  _OP_INSERT        = 2002 /* insert new document */
  _RESERVED         = 2003 /* formerly used for OP_GET_BY_OID */
  _OP_QUERY         = 2004 /* query a collection */
  _OP_GET_MORE      = 2005 /* Get more data from a query. See Cursors */
  _OP_DELETE        = 2006 /* Delete documents */
  _OP_KILL_CURSORS  = 2007 /* Tell database client is done with a cursor  */

  string_of_opcode(code) =
    match code with
    | 1    -> "OP_REPLY"
    | 1000 -> "OP_MSG"
    | 2001 -> "OP_UPDATE"
    | 2002 -> "OP_INSERT"
    | 2003 -> "RESERVED"
    | 2004 -> "OP_QUERY"
    | 2005 -> "OP_GET_MORE"
    | 2006 -> "OP_DELETE"
    | 2007 -> "OP_KILL_CURSORS"
    | n -> "OP_UNKNOWN({n})"

  /* Flags */

  /* OP_INSERT */
  _ContinueOnError  = 0x00000001

  /* OP_UPDATE */
  _Upsert           = 0x00000001
  _MultiUpdate      = 0x00000002

  /* OP_QUERY */
  _TailableCursor   = 0x00000002
  _SlaveOk          = 0x00000004
  _OplogReplay      = 0x00000008
  _NoCursorTimeout  = 0x00000010
  _AwaitData        = 0x00000020
  _Exhaust          = 0x00000040
  _Partial          = 0x00000080

  /* OP_DELETE */
  _SingleRemove     = 0x00000001

  /* OP_REPLY */
  _CursorNotFound   = 0x00000001
  _QueryFailure     = 0x00000002
  _ShardConfigStale = 0x00000003
  _AwaitCapable     = 0x00000004

  /*struct MsgHeader {
      int32   messageLength; // total message size, including this
      int32   requestID;     // identifier for this message
      int32   responseTo;    // requestID from the original request (used in reponses from db)
      int32   opCode;        // request type - see table below
  }*/

  @private hex2(i) = String.padding_left("0",2,Int.to_hex(i))
  @private hex8(i) = String.padding_left("0",8,Int.to_hex(i))
  @private hex1664(i) = String.padding_left("0",16,Int64.to_string_radix(i,16))

  rid = ServerReference.create(Random.int(0xffffffff))
  nextrid() =
    requestID = ServerReference.get(rid)
    do ServerReference.set(rid, requestID+1)
    requestID

  make_header : int, int, int, int -> Pack.data =
    (messageLength, requestID, responseTo, opCode ->
       requestID =
         if requestID == 0
         then nextrid()
         else requestID
      [{Le}, {Unsigned}, {Long=messageLength}, {Long=requestID}, {Long=responseTo}, {Long=opCode}, {Signed}])

  string_of_MsgHeader(hdr) =
    "messageLength = {hdr.messageLength}\nrequestId = 0x{hex8(hdr.requestID)}\nresponseTo = 0x{hex8(hdr.responseTo)}\nopCode = {string_of_opcode(hdr.opCode)}\n"

  refresh_requestId(msg:WireProtocol.Message) : WireProtocol.Message =
    {msg with MsgHeader={msg.MsgHeader with requestID=nextrid()}}

  estart(typ,name,u,usize) = (2+usize+String.length(name),[{Byte=typ}, {Cstring=name}, u])
  int(name,i) = estart(el_int, name, {Long=i}, 4)
  long(name,l) = estart(el_long, name, {Longlong=l}, 8)
  int64(name,i64) = estart(el_long, name, {Int64=i64}, 8)
  double(name,d) = estart(el_double, name, {Float=d}, 8)
  bool(name,b) = estart(el_bool, name, {Bool=b}, 1)

  estart0(typ,name) = (2+String.length(name),[{Byte=typ}, {Cstring=name}])
  null(name) = estart0(el_null, name)
  minkey(name) = estart0(el_minkey, name)
  maxkey(name) = estart0(el_maxkey, name)
  undefined(name) = estart0(el_undefined, name)

  string_base(name,value,typ) =
    valuesize = String.byte_length(value)
    (7+String.length(name)+valuesize,[{Byte=typ}, {Cstring=name}, {Long=valuesize+1}, {Cstring=value}])
  string(name,value) = string_base(name,value,el_string)
  symbol(name,value) = string_base(name,value,el_symbol)
  code(name,value) = string_base(name,value,el_code)

  code_w_scope(name,code,scope) =
    codesize = String.length(code)
    (docsize,doc) = packDocument(scope)
    (11+codesize+String.length(name)+docsize,
     [{Byte=el_codewscope}, {Cstring=name}, {Long=codesize+docsize+9}, {Long=codesize+1}, {Cstring=code}, {Pack=doc}])

  binary(name,typ,bin) =
    strsize = Binary.length(bin)
    size = 7+String.length(name)+strsize
    if typ == st_bin_binary_old
    then (size+4,[{Byte=el_bindata}, {Cstring=name}, {Long=strsize+4}, {Byte=typ}, {Binary=bin; le=true; size={L}}])
    else (size,[{Byte=el_bindata}, {Cstring=name}, {Binary=bin; payload=[{Byte=typ}]; le=true; size={L}}])

  new_oid =
    counter = ServerReference.create(0)
    (->
      b = Binary.create(12)
      ts = Date.in_milliseconds(Date.now()) / 1000
      do Binary.add_uint32_be(b, ts)
      do Binary.add_uint32_be(b, Random.int(0xffffffff))
      cnt = ServerReference.get(counter)
      do ServerReference.set(counter, cnt+1)
      do Binary.add_uint32_be(b, cnt)
      b)

  oid(name,oid) = (14+String.length(name),[{Byte=el_oid}, {Cstring=name}, {FixedBinary=(12,oid)}])

  oid_of_string(str:string) : binary =
    c2h(c1,c2) : option(int) =
      ch(c:string) : option(int) =
        match c with
        | "0" -> {some=0} | "1" -> {some=1} | "2" -> {some=2} | "3" -> {some=3} | "4" -> {some=4}
        | "5" -> {some=5} | "6" -> {some=6} | "7" -> {some=7} | "8" -> {some=8} | "9" -> {some=9}
        | "a" -> {some=10} | "b" -> {some=11} | "c" -> {some=12} | "d" -> {some=13} | "e" -> {some=14} | "f" -> {some=15}
        | "A" -> {some=10} | "B" -> {some=11} | "C" -> {some=12} | "D" -> {some=13} | "E" -> {some=14} | "F" -> {some=15}
        | _ -> {none}
      match (ch(c1),ch(c2)) with
      | ({some=i1},{some=i2}) -> {some=i1*16+i2}
      | _ -> {none}
    null = binary_of_string("\000\000\000\000\000\000\000\000\000\000\000\000")
    if String.length(str) < 24
    then null
    else
      oid = Binary.create(12)
      rec aux(i) =
        if i >= 12
        then oid
        else
          (match c2h(String.sub(i*2,1,str),String.sub(i*2+1,1,str)) with
           | {some=n} ->
             do Binary.add_uint8(oid, n)
             aux(i+1)
           | {none} ->
             null)
      aux(0)

  oid_to_string(oid:binary) : string =
    h2c(h:int) : string =
      match h with
      | 0 -> "0" | 1 -> "1" | 2 -> "2" | 3 -> "3" | 4 -> "4" | 5 -> "5" | 6 -> "6" | 7 -> "7" | 8 -> "8"
      | 9 -> "9" | 10 -> "a" | 11 -> "b" | 12 -> "c" | 13 -> "d" | 14 -> "e" | 15 -> "f" | _ -> "\000"
    null = "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
    if Binary.length(oid) != 12
    then null
    else
      rec aux(i,sl) =
        if i >= 12
        then String.concat("",List.rev(sl))
        else
          n = Binary.get_uint8(oid,i)
          c1 = Bitwise.lsr(n,4)
          c2 = Bitwise.land(0x0f,n)
          aux(i+1,["{h2c(c1)}{h2c(c2)}"|sl])
      aux(0,[])

  regex(name,pattern,opts) = (4+String.length(name)+String.length(pattern)+String.length(opts),
                              [{Byte=el_regex}, {Cstring=name}, {Cstring=pattern}, {Cstring=opts}])

  bson(name,bson) =
    (docsize,doc) = packDocument(bson)
    (2+String.length(name)+docsize,[{Byte=el_object}, {Cstring=name}, {Pack=doc}])

  timestamp(name,(i,t)) = (10+String.length(name),[{Byte=el_timestamp}, {Cstring=name}, {Long=i}, {Long=t}])

  date(name,millis) = (10+String.length(name),[{Byte=el_date}, {Cstring=name}, {Longlong=millis}])

  time_t(name,t) = date(name,Date.in_milliseconds(t))

  object(name, obj) =
    (docsize,doc) = packDocument(obj)
    (2+String.length(name)+docsize,[{Byte=el_object}, {Cstring=name}, {Pack=doc}])

  array(name, arr) =
    (docsize,doc) = packDocument(arr)
    (2+String.length(name)+docsize,[{Byte=el_array}, {Cstring=name}, {Pack=doc}])

  packDocument(doc:Bson.document) : (int,Pack.data) =
    rec aux(doc) =
      match doc with
      | [] -> (1,[[{Byte=0}]])
      | [el|doc] ->
        (elsize,el) =
          match el.value with
           | {~Double} -> double(el.name,Double)
           | {~String} -> string(el.name,String)
           | {~Document} -> object(el.name,Document)
           | {~Array} -> array(el.name,Array)
           | {~Binary} -> binary(el.name,st_bin_binary,Binary)
           | {~ObjectID} -> oid(el.name,ObjectID)
           | {~Boolean} -> bool(el.name,Boolean)
           | {~Date} -> time_t(el.name,Date)
           | {Null=_} -> null(el.name)
           | {Regexp=(pattern,opts)} -> regex(el.name,pattern,opts)
           | {~Code} -> code(el.name,Code)
           | {~Symbol} -> symbol(el.name,Symbol)
           | {CodeScope=(code,scope)} -> code_w_scope(el.name,code,scope)
           | {~Int32} -> int(el.name,Int32)
           | {RealInt32=_} -> @fail //int(el.name,RealInt32) // TODO: implement Int32
           | {Timestamp=(i,t)} -> timestamp(el.name,(i,t))
           | {~Int64} -> long(el.name,Int64)
           | {~RealInt64} -> int64(el.name,RealInt64)
           | {Min=_} -> minkey(el.name)
           | {Max=_} -> maxkey(el.name)
        (elssize,els) = aux(doc)
        (elsize+elssize,[el|els])
    (datasize,data) = aux(doc)
    (4+datasize,[{Long=4+datasize}|List.flatten(data)])

  unser_item(input:Pack.input) : Pack.result(option(Bson.element)) =
    do Pack.pinput("unser_item",input)
    match U.uoctet(input) with
    | {success=(input,0)} -> {success=(input,none)}
    | {success=(input,code)} ->
       (match U.cstring(input) with
        | {success=(input,name)} ->
           (match code with
            | /*el_eoo*/0 ->
              {success=(input,none)}
            | /*el_double*/1 ->
              (match U.float(true, input) with
               | {success=(input,f)} -> {success=(input,{some={~name; value={Double=f}}})}
               | {~failure} -> {~failure})
            | /*el_string*/2 ->
              (match U.tuple2(input,U.skip(_, 4),U.cstring) with
               | {success=(input,(_,s))} -> {success=(input,{some={~name; value={String=s}}})}
               | {~failure} -> {~failure})
            | /*el_object*/3 ->
              (match unser_document(input) with
               | {success=(input,doc)} -> {success=(input,{some={~name; value={Document=doc}}})}
               | {~failure} -> {~failure})
            | /*el_array*/4 ->
              (match unser_document(input) with
               | {success=(input,arr)} -> {success=(input,{some={~name; value={Array=arr}}})}
               | {~failure} -> {~failure})
            | /*el_bindata*/5 ->
              (match U.tuple2(input,U.ulong_le,U.uoctet) with
               | {success=(input,(size, stbin))} ->
                  (match U.skip(input, if stbin == 2 then 4 else 0) with
                   | {success=(input,_)} ->
                      {success=({input with pos=input.pos+size},
                                {some={~name; value={Binary=Binary.get_binary(input.binary,input.pos,size)}}})}
                   | {~failure} -> {~failure})
               | {~failure} -> {~failure})
            | /*el_oid*/7 ->
              (match U.fixed_binary(input, 12) with
               | {success=(input,oid)} ->
                  {success=(input,{some={~name; value={ObjectID=oid}}})}
               | {~failure} -> {~failure})
            | /*el_bool*/8 ->
              (match U.bool(input) with
               | {success=(input,b)} -> {success=(input,{some={~name; value={Boolean=b}}})}
               | {~failure} -> {~failure})
            | /*el_date*/9 ->
              (match U.longlong_le(input) with
               | {success=(input,milli)} -> {success=(input,{some={~name; value={Date=Date.milliseconds(milli)}}})}
               | {~failure} -> {~failure})
            | /*el_null*/10 ->
              {success=(input,{some={~name; value={Null}}})}
            | /*el_regex*/11 ->
              (match U.tuple2(input,U.cstring,U.cstring) with
               | {success=(input,(pat,opts))} -> {success=(input,{some={~name; value={Regexp=(pat,opts)}}})}
               | {~failure} -> {~failure})
            | /*el_code*/13 ->
              (match U.tuple2(input,U.skip(_,4),U.cstring) with
               | {success=(input,(_,code))} -> {success=(input,{some={~name; value={Code=code}}})}
               | {~failure} -> {~failure})
            | /*el_symbol*/14 ->
              (match U.tuple2(input,U.skip(_,4),U.cstring) with
               | {success=(input,(_,symbol))} -> {success=(input,{some={~name; value={Symbol=symbol}}})}
               | {~failure} -> {~failure})
            | /*el_codewscope*/15 ->
              (match U.tuple3(input,U.skip(_,8),U.cstring,unser_document) with
               | {success=(input,(_,code,scope))} -> {success=(input,{some={~name; value={CodeScope=(code,scope)}}})}
               | {~failure} -> {~failure})
            | /*el_int*/16 ->
              (match U.long_le(input) with
               | {success=(input,i)} -> {success=(input,{some={~name; value={Int32=i}}})}
               | {~failure} -> {~failure})
            | /*el_timestamp*/17 ->
              (match U.tuple2(input,U.ulong_le,U.ulong_le) with
               | {success=(input,(i,t))} -> {success=(input,{some={~name; value={Timestamp=(i,t)}}})}
               | {~failure} -> {~failure})
            | /*el_long*/18 ->
              (match U.int64_le(input) with
               | {success=(input,i64)} ->
                  match Int64.to_int_signed_opt(i64)
                  | {none} ->
                    {success=(input,{some={~name; value={RealInt64=i64}}})}
                  | {some = i} ->
                    {success=(input,{some={~name; value={Int64=i}}})}
                  end
               | {~failure} -> {~failure})
            | /*el_minkey*/255 ->
              {success=(input,{some={~name; value={Max}}})}
            | /*el_maxkey*/127 ->
              {success=(input,{some={~name; value={Min}}})}
            | _ -> {failure="unser_item: Bad code {code}"}
           )
        | {~failure} -> {~failure})
    | {~failure} -> {~failure}

  unser_document(input:Pack.input) : Pack.result(Bson.document) =
    match U.tuple2(input,U.skip(_,4),U.unser_to_none(unser_item,_)) with
    | {success=(input,(_,doc))} -> {success=(input,doc)}
    | {~failure} -> {~failure}

  string_of_Docs(names,docs) =
    rec aux(names,docs,i) =
      match docs with
      | [] -> []
      | [doc|rest] ->
         (name,names) = match names with | [name|names] -> (name,names) | _ -> ("doc{i}",[])
         ["{name} = {Bson.to_pretty(doc)}"|aux(names,rest,i+1)]
    String.concat("\n  ",aux(names,docs,0))

  /*struct OP_INSERT {
      MsgHeader header;             // standard message header
      int32     flags;              // bit vector - see below
      cstring   fullCollectionName; // "dbname.collectionname"
      document* documents;          // one or more documents to insert into the collection
  }*/

  string_of_insert_flags(flags:int) : string =
    l = if Bitwise.land(flags,0x01) != 0 then ["ContinueOnError"] else []
    List.to_string(l)

  string_of_Insert(ins:WireProtocol.Insert) =
    "  flags={string_of_insert_flags(ins.flags)}\n  fullCollectionName={ins.fullCollectionName}\n  {string_of_Docs([],ins.documents)}"

  packInsert(hdr:WireProtocol.MsgHeader, ins:WireProtocol.Insert) : Pack.data =
    (bodysize,bodies) = List.fold_backwards((doc, (s,bs) -> (s2,b2) = packDocument(doc) (s+s2,[b2|bs])),ins.documents,(0,[]))
    nssize = String.length(ins.fullCollectionName) + 1
    head = make_header(20+nssize+bodysize,hdr.requestID,hdr.responseTo,_OP_INSERT)
    List.flatten([[{Pack=head}, {Long=ins.flags}, {Cstring=ins.fullCollectionName}], List.map((b -> {Pack=b}),bodies)])

  Insert(rid:int, flags:int, fullCollectionName:string, documents:list(Bson.document)) : WireProtocol.Message =
    {MsgHeader={messageLength=0; requestID=rid; responseTo=0; opCode=_OP_INSERT};
     MsgBody={Insert=~{flags; fullCollectionName; documents}}}

  unser_insert(input:Pack.input) : Pack.result(WireProtocol.MsgBody) =
    match U.tuple3(input,U.ulong_le,U.cstring,U.unser_to_end(unser_document,_)) with
    | {success=(input,(flags, fullCollectionName, documents))} ->
       {success=(input,{Insert=~{flags; fullCollectionName; documents}})}
    | {~failure} -> {~failure}

  /*struct OP_UPDATE {
      MsgHeader header;             // standard message header
      int32     ZERO;               // 0 - reserved for future use
      cstring   fullCollectionName; // "dbname.collectionname"
      int32     flags;              // bit vector. see below
      document  selector;           // the query to select the document
      document  update;             // specification of the update to perform
  }*/

  string_of_update_flags(flags:int) : string =
    l = if Bitwise.land(flags,0x01) != 0 then ["Upsert"] else []
    l = if Bitwise.land(flags,0x02) != 0 then ["MultiUpdate"|l] else l
    List.to_string(l)

  string_of_Update(upd:WireProtocol.Update) =
    names = ["selector","update"]
    "  flags={string_of_update_flags(upd.flags)}\n  fullCollectionName={upd.fullCollectionName}\n  {string_of_Docs(names,[upd.selector,upd.update])}"

  packUpdate(hdr:WireProtocol.MsgHeader, bdy:WireProtocol.Update) : Pack.data =
    (selectorsize,selectordata) = packDocument(bdy.selector)
    (updatesize,updatedata) = packDocument(bdy.update)
    nssize = String.length(bdy.fullCollectionName) + 1
    head = make_header(24+nssize+selectorsize+updatesize,hdr.requestID,hdr.responseTo,_OP_UPDATE)
    [{Pack=head}, {Long=0}, {Cstring=bdy.fullCollectionName}, {Long=bdy.flags}, {Pack=selectordata}, {Pack=updatedata}]

  Update(rid:int, flags:int, fullCollectionName:string, selector:Bson.document, update:Bson.document) : WireProtocol.Message =
    {MsgHeader={messageLength=0; requestID=rid; responseTo=0; opCode=_OP_INSERT};
     MsgBody={Update=~{flags; fullCollectionName; selector; update}}}

  unser_update(input:Pack.input) : Pack.result(WireProtocol.MsgBody) =
    match U.tuple5(input,U.skip(_,4),U.cstring,U.ulong_le,unser_document,unser_document) with
    | {success=(input,(_, fullCollectionName, flags, selector, update))} ->
       {success=(input,{Update=~{fullCollectionName; flags; selector; update}})}
    | {~failure} -> {~failure}

  /*struct OP_QUERY {
      MsgHeader header;                // standard message header
      int32     flags;                 // bit vector of query options.
      cstring   fullCollectionName;    // "dbname.collectionname"
      int32     numberToSkip;          // number of documents to skip
      int32     numberToReturn;        // number of documents to return in the first OP_REPLY batch
      document  query;                 // query object.
    [ document  returnFieldSelector; ] // Optional. Selector indicating the fields to return.
  }*/

  string_of_query_flags(flags:int) : string =
    l = if Bitwise.land(flags,0x01) != 0 then ["Reserved"] else []
    l = if Bitwise.land(flags,0x02) != 0 then ["TailableCursor"|l] else l
    l = if Bitwise.land(flags,0x04) != 0 then ["SlaveOk"|l] else l
    l = if Bitwise.land(flags,0x08) != 0 then ["OplogReplay"|l] else l
    l = if Bitwise.land(flags,0x10) != 0 then ["NoCursorTimeout"|l] else l
    l = if Bitwise.land(flags,0x20) != 0 then ["AwaitData"|l] else l
    l = if Bitwise.land(flags,0x40) != 0 then ["Exhaust"|l] else l
    l = if Bitwise.land(flags,0x80) != 0 then ["Partial"|l] else l
    List.to_string(l)

  string_of_Query(que:WireProtocol.Query) =
    names = ["query","returnFieldSelector"]
    "  flags={string_of_query_flags(que.flags)}\n  fullCollectionName={que.fullCollectionName}\n  numberToSkip={que.numberToSkip}\n  numberToReturn={que.numberToReturn}\n  {string_of_Docs(names,List.flatten([[que.query],match que.returnFieldSelector with | {some=rfs} -> [rfs] | _ -> []]))}"

  packQuery(hdr:WireProtocol.MsgHeader, bdy:WireProtocol.Query) : Pack.data =
    (querysize,querydata) = packDocument(bdy.query)
    (rfssize,rfsdata) =
      match bdy.returnFieldSelector with
      | {some=rfs} ->
         (rfssize,rfsdata) = packDocument(rfs)
         (rfssize,[{Pack=rfsdata}])
      | {none} -> (0,[])
    nssize = String.length(bdy.fullCollectionName)
    head = make_header(29+nssize+querysize+rfssize,hdr.requestID,hdr.responseTo,_OP_QUERY)
    List.flatten([[{Pack=head},
                   {Long=bdy.flags}, {Cstring=bdy.fullCollectionName},
                   {Long=bdy.numberToSkip}, {Long=bdy.numberToReturn}, {Pack=querydata}], rfsdata])

  Query(rid:int, flags:int, fullCollectionName:string,
        query:Bson.document, numberToSkip:int, numberToReturn:int,
        returnFieldSelector:option(Bson.document)) : WireProtocol.Message =
    {MsgHeader={messageLength=0; requestID=rid; responseTo=0; opCode=_OP_QUERY};
     MsgBody={Query=~{flags; fullCollectionName; query; numberToSkip; numberToReturn; returnFieldSelector}}}

  unser_query(input:Pack.input) : Pack.result(WireProtocol.MsgBody) =
    match U.tuple6(input,U.ulong_le,U.cstring,U.ulong_le,U.ulong_le,unser_document,U.unser_optional_at_end(unser_document,_)) with
    | {success=(input,(flags, fullCollectionName, numberToSkip, numberToReturn, query, returnFieldSelector))} ->
       {success=(input,{Query=~{flags; fullCollectionName; numberToSkip; numberToReturn; query; returnFieldSelector}})}
    | {~failure} -> {~failure}

  set_query_flags(msg:WireProtocol.Message, flags:int) : WireProtocol.Message =
    {msg with
     MsgBody=(match msg.MsgBody with
              | {~Query} -> {Query={Query with ~flags}}
              | _ -> msg.MsgBody)}

  get_opCode(msg:WireProtocol.Message) : int = msg.MsgHeader.opCode

  /*struct OP_GETMORE {
      MsgHeader header;             // standard message header
      int32     ZERO;               // 0 - reserved for future use
      cstring   fullCollectionName; // "dbname.collectionname"
      int32     numberToReturn;     // number of documents to return
      int64     cursorID;           // cursorID from the OP_REPLY
  }*/

  string_of_GetMore(gm:WireProtocol.GetMore) =
    "  fullCollectionName={gm.fullCollectionName}\n  numberToReturn={gm.numberToReturn}\n  cursorID=0x{hex1664(gm.cursorID)}"

  packGetMore(hdr:WireProtocol.MsgHeader, bdy:WireProtocol.GetMore) : Pack.data =
    nssize = String.length(bdy.fullCollectionName) + 1
    head = make_header(32+nssize,hdr.requestID,hdr.responseTo,_OP_GET_MORE)
    [{Pack=head}, {Long=0}, {Cstring=bdy.fullCollectionName}, {Long=bdy.numberToReturn}, {Int64=bdy.cursorID}]

  GetMore(rid:int, fullCollectionName:string, numberToReturn:int, cursorID:int64) : WireProtocol.Message =
    {MsgHeader={messageLength=0; requestID=rid; responseTo=0; opCode=_OP_GET_MORE};
     MsgBody={GetMore=~{fullCollectionName; numberToReturn; cursorID}}}

  unser_get_more(input:Pack.input) : Pack.result(WireProtocol.MsgBody) =
    match U.tuple4(input,U.ulong_le,U.cstring,U.ulong_le,U.int64_le) with
    | {success=(input,(_, fullCollectionName, numberToReturn, cursorID))} ->
       {success=(input,{GetMore=~{fullCollectionName; numberToReturn; cursorID}})}
    | {~failure} -> {~failure}

  /*struct OP_DELETE {
      MsgHeader header;             // standard message header
      int32     ZERO;               // 0 - reserved for future use
      cstring   fullCollectionName; // "dbname.collectionname"
      int32     flags;              // bit vector - see below for details.
      document  selector;           // query object.  See below for details.
  }*/

  string_of_delete_flags(flags:int) : string =
    l = if Bitwise.land(flags,0x01) != 0 then ["SingleRemove"] else []
    List.to_string(l)

  string_of_Delete(del:WireProtocol.Delete) =
    names = ["selector"]
    "  flags={string_of_delete_flags(del.flags)}\n  fullCollectionName={del.fullCollectionName}\n  {string_of_Docs(names,[del.selector])}"

  packDelete(hdr:WireProtocol.MsgHeader, bdy:WireProtocol.Delete) : Pack.data =
    (selectorsize,selectordata) = packDocument(bdy.selector)
    nssize = String.length(bdy.fullCollectionName) + 1
    head = make_header(24+nssize+selectorsize,hdr.requestID,hdr.responseTo,_OP_DELETE)
    [{Pack=head}, {Long=0}, {Cstring=bdy.fullCollectionName}, {Long=bdy.flags}, {Pack=selectordata}]

  Delete(rid:int, flags:int, fullCollectionName:string, selector:Bson.document) : WireProtocol.Message =
    {MsgHeader={messageLength=0; requestID=rid; responseTo=0; opCode=_OP_DELETE};
     MsgBody={Delete=~{flags; fullCollectionName; selector}}}

  unser_delete(input:Pack.input) : Pack.result(WireProtocol.MsgBody) =
    match U.tuple4(input,U.ulong_le,U.cstring,U.ulong_le,unser_document) with
    | {success=(input,(_, fullCollectionName, flags, selector))} ->
       {success=(input,{Delete=~{fullCollectionName; flags; selector}})}
    | {~failure} -> {~failure}

  /*struct OP_KILL_CURSORS {
      MsgHeader header;            // standard message header
      int32     ZERO;              // 0 - reserved for future use
      int32     numberOfCursorIDs; // number of cursorIDs in message
      int64*    cursorIDs;         // sequence of cursorIDs to close
  }*/

  string_of_KillCursors(kc:WireProtocol.KillCursors) =
    cursorIDsstr = String.concat(", 0x",List.map(hex1664,kc.cursorIDs))
    "  numberOfCursorIDs={kc.numberOfCursorIDs}\n  cursorIDs=[0x{cursorIDsstr}]"

  packKillCursors(hdr:WireProtocol.MsgHeader, bdy:WireProtocol.KillCursors) : Pack.data =
    head = make_header(25+List.length(bdy.cursorIDs)*8,hdr.requestID,hdr.responseTo,_OP_KILL_CURSORS)
    [{Pack=head}, {Long=0}, {List=([{Int64=Int64.zero}],List.map((cid -> [{Int64=cid}]),bdy.cursorIDs))}]

  KillCursors(rid:int, cursorIDs:list(int64)) : WireProtocol.Message =
    {MsgHeader={messageLength=0; requestID=rid; responseTo=0; opCode=_OP_KILL_CURSORS};
     MsgBody={KillCursors=~{numberOfCursorIDs=List.length(cursorIDs); cursorIDs}}}

  unser_kill_cursors(input:Pack.input) : Pack.result(WireProtocol.MsgBody) =
    match U.tuple3(input,U.ulong_le,U.ulong_le,U.unser_to_end(U.int64_le,_)) with
    | {success=(input,(_, numberOfCursorIDs, cursorIDs))} ->
       {success=(input,{KillCursors=~{numberOfCursorIDs; cursorIDs}})}
    | {~failure} -> {~failure}

  /*struct OP_MSG {
      MsgHeader header;  // standard message header
      cstring   message; // message for the database
  }*/

  string_of_Msg(msg:WireProtocol.Msg) = "  msg={msg.message}"

  packMsg(hdr:WireProtocol.MsgHeader, bdy:WireProtocol.Msg) : Pack.data =
    msgsize = String.length(bdy.message) + 1
    head = make_header(17+msgsize,hdr.requestID,hdr.responseTo,_OP_MSG)
    [{Pack=head}, {Cstring=bdy.message}]

  Msg(rid:int, message:string) : WireProtocol.Message =
    {MsgHeader={messageLength=0; requestID=rid; responseTo=0; opCode=_OP_MSG};
     MsgBody={Msg=~{message}}}

  unser_msg(input:Pack.input) : Pack.result(WireProtocol.MsgBody) =
    match U.cstring(input) with
    | {success=(input,message)} -> {success=(input,{Msg=~{message}})}
    | {~failure} -> {~failure}

  /*struct OP_REPLY {
      MsgHeader header;         // standard message header
      int32     responseFlags;  // bit vector - see details below
      int64     cursorID;       // cursor id if client needs to do get more's
      int32     startingFrom;   // where in the cursor this reply is starting
      int32     numberReturned; // number of documents in the reply
      document* documents;      // documents
  }*/

  // We don't send this, it's for debug purposes
  /*reply(rid:int, flags:int, cursorID:int64, startingFrom:int, numberReturned:int, docs:list(Bson.document)) : Pack.data =
    (bodysize,bodies) = List.fold_backwards((doc, (s,bs) -> (s2,b2) = packDocument(doc) (s+s2,[b2|bs])),docs,(0,[]))
    head = make_header(37+bodysize,rid,0,_OP_REPLY)
    List.flatten([[{Pack=head}, {Long=flags}, {Int64=cursorID}, {Long=startingFrom}, {Long=numberReturned}],
                  List.map((b -> {Pack=b}),bodies)])*/

  unser_reply(input:Pack.input) : Pack.result(WireProtocol.MsgBody) =
    match U.tuple4(input,U.ulong_le,U.int64_le,U.ulong_le,U.ulong_le) with
    | {success=(input,(responseFlags, cursorID, startingFrom, numberReturned))} ->
       rec aux(n, input) =
         if n <= 0
         then {success=(input,[])}
         else
           (match unser_document(input) with
            | {success=(input,doc)} ->
              (match aux(n-1,input) with
               | {success=(input,docs)} -> {success=(input,[doc|docs])}
               | {~failure} -> {~failure})
            | {~failure} -> {~failure})
       (match aux(numberReturned, input) with
        | {success=(input, documents)} ->
           Reply = ~{responseFlags; cursorID; startingFrom; numberReturned; documents}
           {success=(input,{~Reply})}
        | {~failure} -> {~failure})
    | {~failure} -> {~failure}

  string_of_reply_flags(flags:int) : string =
    l = if Bitwise.land(flags,0x01) != 0 then ["CursorNotFound"] else []
    l = if Bitwise.land(flags,0x02) != 0 then ["QueryFailure"|l] else l
    l = if Bitwise.land(flags,0x04) != 0 then ["ShardConfigStale"|l] else l
    l = if Bitwise.land(flags,0x08) != 0 then ["AwaitCapable"|l] else l
    List.to_string(l)

  string_of_Reply(rep:WireProtocol.Reply) =
    "  flags={string_of_reply_flags(rep.responseFlags)}\n  cursorID=0x{hex1664(rep.cursorID)}\n  startingFrom={rep.startingFrom}\n  numberReturned={rep.numberReturned}\n  {string_of_Docs([],rep.documents)}"

  reply_messageLength(msg:WireProtocol.Message) : int = msg.MsgHeader.messageLength
  reply_requestId(msg:WireProtocol.Message) : int = msg.MsgHeader.requestID
  reply_responseTo(msg:WireProtocol.Message) : int = msg.MsgHeader.responseTo
  reply_opCode(msg:WireProtocol.Message) : int = msg.MsgHeader.opCode
  reply_responseFlags(msg:WireProtocol.Message) : int =
    match msg.MsgBody with
    | {~Reply} -> Reply.responseFlags
    | _ -> @fail("WireProtocol.reply_responseFlags: bad reply")
  reply_cursorID(msg:WireProtocol.Message) : int64 =
    match msg.MsgBody with
    | {~Reply} -> Reply.cursorID
    | _ -> @fail("WireProtocol.reply_cursorID: bad reply")
  reply_startingFrom(msg:WireProtocol.Message) : int =
    match msg.MsgBody with
    | {~Reply} -> Reply.startingFrom
    | _ -> @fail("WireProtocol.reply_startingFrom: bad reply")
  reply_numberReturned(msg:WireProtocol.Message) : int =
    match msg.MsgBody with
    | {~Reply} -> Reply.numberReturned
    | _ -> @fail("WireProtocol.reply_numberReturned: bad reply")

  reply_document_pos(msg:WireProtocol.Message, n:int) : option(Bson.document) =
    if reply_numberReturned(msg) <= n
    then none
    else
      match msg.MsgBody with
      | {~Reply} -> List.nth(n, Reply.documents)
      | _ -> none

  string_of_Message(message:WireProtocol.Message) : string =
    headstr = string_of_MsgHeader(message.MsgHeader)
    bodystr =
      match message.MsgBody with
      | {~Reply} -> string_of_Reply(Reply)
      | {~Msg} -> string_of_Msg(Msg)
      | {~Update} -> string_of_Update(Update)
      | {~Insert} -> string_of_Insert(Insert)
      | {~Query} -> string_of_Query(Query)
      | {~GetMore} -> string_of_GetMore(GetMore)
      | {~Delete} -> string_of_Delete(Delete)
      | {~KillCursors} -> string_of_KillCursors(KillCursors)
    headstr^bodystr

  packMessage(msg:WireProtocol.Message) : Pack.data =
    match msg.MsgBody with
    | {~Insert} -> packInsert(msg.MsgHeader, Insert)
    | {~Update} -> packUpdate(msg.MsgHeader, Update)
    | {~Query} -> packQuery(msg.MsgHeader, Query)
    | {~GetMore} -> packGetMore(msg.MsgHeader, GetMore)
    | {~Delete} -> packDelete(msg.MsgHeader, Delete)
    | {~KillCursors} -> packKillCursors(msg.MsgHeader, KillCursors)
    | {~Msg} -> packMsg(msg.MsgHeader, Msg)
    | _ -> @fail

  binary_export(msgs:list(WireProtocol.Message)) : outcome(binary,string) =
    E.pack(List.flatten(List.map(packMessage,msgs)))

  unser_header(input:Pack.input) : Pack.result(WireProtocol.MsgHeader) =
    match U.tuple4(input,U.ulong_le,U.ulong_le,U.ulong_le,U.ulong_le) with
    | {success=(input,(messageLength, requestID, responseTo, opCode))} ->
       {success=(input,~{messageLength; requestID; responseTo; opCode})}
    | {~failure} -> {~failure}

  unser_message(input:Pack.input) : Pack.result(WireProtocol.Message) =
    match unser_header(input) with
    | {success=(input, MsgHeader)} ->
       result(fn) =
         (match fn(input) with
          | {success=(input,MsgBody)} -> {success=(input,~{MsgHeader; MsgBody})}
          | {~failure} -> {~failure})
       (match MsgHeader.opCode with
        | 1 -> /*_OP_REPLY*/ result(unser_reply)
        | 1000 -> /*_OP_MSG*/ result(unser_msg)
        | 2001 -> /*_OP_UPDATE*/ result(unser_update)
        | 2002 -> /*_OP_INSERT*/ result(unser_insert)
      //| 2003 -> /*_RESERVED*/
        | 2004 -> /*_OP_QUERY*/ result(unser_query)
        | 2005 -> /*_OP_GET_MORE*/ result(unser_get_more)
        | 2006 -> /*_OP_DELETE*/ result(unser_delete)
        | 2007 -> /*_OP_KILL_CURSORS*/ result(unser_kill_cursors)
        | _ -> {failure="unser_message: unknown Op code {MsgHeader.opCode}"}
       )
    | {~failure} -> {~failure}

  string_of_message_binary(binary:binary) : string =
    match unser_message({~binary; pos=0}) with
    | {success=(_,msg)} -> string_of_Message(msg)
    | {~failure} -> "WireProtocol.string_of_message_binary: bad binary (\"{failure}\")"

  read_mongo(conn:Socket.connection, timeout:int, mailbox:Mongo.mailbox) : outcome((Mailbox.t,Mongo.reply),string) =
    match Socket.read_fixed(conn, timeout, 4, mailbox) with
    | {success=mailbox} ->
       len = Binary.get_uint32_le(mailbox.buf, mailbox.start)
       (match Socket.read_fixed(conn, timeout, len-4, mailbox) with
        | {success=mailbox} ->
           (match unser_message({binary=mailbox.buf; pos=mailbox.start}) with
            | {success=(_,~{MsgHeader; MsgBody={~Reply}})} ->
               (match Mailbox.skip(mailbox, len) with
                | {success=mailbox} ->
                   {success=(mailbox, ~{MsgHeader; MsgBody={~Reply}})}
                | {~failure} -> {~failure})
            | {success=_} -> {failure="WireProtocol.read_mongo: message is not a reply"}
            | {~failure} -> {~failure})
        | {~failure} -> {~failure})
    | {~failure} -> {~failure}

}}



