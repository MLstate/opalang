(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

module BslUtils = OpabslgenMLRuntime.BslUtils
module BslNativeLib = OpabslgenMLRuntime.BslNativeLib

(** TODO - plugins dependencies *)
##property[mli]
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation
##property[endmli]

##opa-type list('a)
##opa-type tuple_2('a, 'b)
##opa-type outcome('a, 'b)

let opa_list_to_ocaml_list f l =
  BslNativeLib.opa_list_to_ocaml_list f
    (BslNativeLib.wrap_opa_list (unwrap_opa_list l))
let ocaml_tuple_2 x =
  BslNativeLib.ocaml_tuple_2 (BslNativeLib.wrap_opa_tuple_2 (unwrap_opa_tuple_2 x))
let opa_tuple_2 x =
  wrap_opa_tuple_2 (BslNativeLib.unwrap_opa_tuple_2 (BslNativeLib.opa_tuple_2 x))
let create_outcome x =
  wrap_opa_outcome (BslUtils.unwrap_opa_outcome (BslUtils.create_outcome x))
(** *****************************)

module C = QmlCpsServerLib
open C.Ops
module FillbufString = Bson.FillbufString

##opa-type Bson.document
##opa-type Bson.element
##extern-type bson_buf = Bson.buf
##extern-type Mongo.mongo_buf = Mongo.mongo_buf
##extern-type Mongo.cursorID = int64
##extern-type Mongo.mailbox = (Buf.t * int ref)
##extern-type Mongo.reply = (Buf.t * int * int)
##extern-type Socket.connection = Scheduler.connection_info

##module Bson

exception Overflow of string
let imax64 = Int64.of_int max_int
let imin64 = Int64.of_int min_int
let i64toi i64 =
  if i64 > imax64 || i64 < imin64 then raise (Overflow (Printf.sprintf "i64toi(%Ld)" i64));
  Int64.to_int i64
let itoi64 = Int64.of_int
#<Ifstatic:OCAML_WORD_SIZE 64>
let i32toi = Int32.to_int
let i32max = Int32.to_int Int32.max_int
let i32min = Int32.to_int Int32.min_int
let itoi32 i =
  if i > i32max || i < i32min then raise (Overflow (Printf.sprintf "itoi32(%d)" i));
  Int32.of_int i
#<Else>
let imax32 = Int32.of_int max_int
let imin32 = Int32.of_int min_int
let i32toi i32 =
  if i32 > imax32 || i32 < imin32 then raise (Overflow (Printf.sprintf "i32toi(%ld)" i32));
  Int32.to_int i32
let itoi32 = Int32.of_int
#<End>

##register dump: int, string -> string
let dump base s =
  let bb = Buffer.create 1024 in
  let bh = Buffer.create 1024 in
  let ba = Buffer.create 1024 in
  let len = String.length s in
  let m, n = len / base, len mod base in
  for i = 0 to m do
    let row = i * base in
    for j = 0 to (if i = m then n-1 else base-1) do
      let idx = i * base + j in
      let code = Char.code s.[idx] in
      Printf.bprintf bh "%02x " code;
      Printf.bprintf ba "%c" (if code >= 32 && code < 127 then s.[idx] else '.');
      if j = base-1 || (i = m && j = n-1)
      then
        (if base = 10
         then Printf.bprintf bb "%04d %-30s %-10s\n" row (Buffer.contents bh) (Buffer.contents ba)
         else Printf.bprintf bb "%04x %-48s %-16s\n" row (Buffer.contents bh) (Buffer.contents ba);
         Buffer.clear bh; Buffer.clear ba)
    done
  done;
  Buffer.contents bb

##register new_oid: void -> string
let new_oid () = Bson.Oid.gen ()

##register oid_of_string: string -> string
let oid_of_string = Bson.Oid.from_string

##register oid_to_string: string -> string
let oid_to_string = Bson.Oid.to_string

let field_name = ServerLib.static_field_of_name "name"
let field_value = ServerLib.static_field_of_name "value"

let ocaml_element opa =
  let record = unwrap_opa_bson_element opa in
  let name = ServerLib.unsafe_dot record field_name in
  let value = ServerLib.unsafe_dot record field_value in
  (name, value)

let serialize bsons b =
  let rec aux bsons =
    let rec aux3 r =
      match ServerLib.dot r BslNativeLib.field_hd with
      | None -> ()
      | Some element ->
          let (name, value) = ocaml_element element in
          ServerLib.fold_record
            (fun f value () ->
               let value = Obj.magic(value) in
               match ServerLib.name_of_field f with
               | Some "Double" -> Bson.Append.double b name (ServerLib.unwrap_float value)
               | Some "String" -> Bson.Append.string b name (ServerLib.unwrap_string value)
               | Some "Document" ->
                   Bson.Append.start_object b name;
                   aux (unwrap_opa_bson_document value);
                   Bson.Append.finish_object b
               | Some "Array" ->
                   Bson.Append.start_array b name;
                   aux (unwrap_opa_bson_document value);
                   Bson.Append.finish_array b
               | Some "Binary" ->
                   let bin = ServerLib.unwrap_string value in
                   Bson.Append.binary b name Bson.st_bin_binary bin (String.length bin)
               | Some "ObjectID" -> Bson.Append.oid b name (ServerLib.unwrap_string value)
               | Some "Boolean" -> Bson.Append.bool b name (ServerLib.unwrap_bool value)
               | Some "Date" -> Bson.Append.date b name (itoi64 (ServerLib.unwrap_int value))
               | Some "Null" -> Bson.Append.null b name
               | Some "Min" -> Bson.Append.minkey b name
               | Some "Max" -> Bson.Append.maxkey b name
               | Some "Regexp" ->
                   (match ocaml_tuple_2 value with
                    | (regexp, regexp_opts) ->
                        Bson.Append.regex b name (ServerLib.unwrap_string regexp) (ServerLib.unwrap_string regexp_opts))
               | Some "Code" -> Bson.Append.code b name (ServerLib.unwrap_string value)
               | Some "Symbol" -> Bson.Append.symbol b name (ServerLib.unwrap_string value)
               | Some "CodeScope" ->
                   (match ocaml_tuple_2 value with
                    | (code, scope) ->
                        Bson.Append.start_codewscope b name code;
                        aux scope;
                        Bson.Append.finish_codewscope b code)
               | Some "Int32" -> Bson.Append.int b name (itoi32 (ServerLib.unwrap_int value))
               | Some "Timestamp" ->
                   (match ocaml_tuple_2 value with
                    | (i, t) ->
                        Bson.Append.timestamp b name ((itoi32 (ServerLib.unwrap_int i)), (itoi32 (ServerLib.unwrap_int t))))
               | Some "Int64" -> Bson.Append.long b name (itoi64 (ServerLib.unwrap_int value))
               | Some str ->
                   Printf.eprintf "Unknown code: %s\n%!" str;
                   assert false
               | None ->
                   assert false)
            value ();
          aux3 (ServerLib.unsafe_dot r BslNativeLib.field_tl)
    in
    aux3 bsons
  in
  aux (unwrap_opa_bson_document bsons)

let field_fst       = ServerLib.static_field_of_name "f1"
let field_snd       = ServerLib.static_field_of_name "f2"
let field_hd        = ServerLib.static_field_of_name "hd"
let field_tl        = ServerLib.static_field_of_name "tl"
let field_nil       = ServerLib.static_field_of_name "nil"
let field_int32     = ServerLib.static_field_of_name "Int32"
let field_realint32 = ServerLib.static_field_of_name "RealInt32"
let field_int64     = ServerLib.static_field_of_name "Int64"
let field_realint64 = ServerLib.static_field_of_name "RealInt64"
let field_double    = ServerLib.static_field_of_name "Double"
let field_bool      = ServerLib.static_field_of_name "Boolean"
let field_string    = ServerLib.static_field_of_name "String"
let field_document  = ServerLib.static_field_of_name "Document"
let field_null      = ServerLib.static_field_of_name "Null"
let field_minkey    = ServerLib.static_field_of_name "Min"
let field_maxkey    = ServerLib.static_field_of_name "Max"
let field_array     = ServerLib.static_field_of_name "Array"
let field_binary    = ServerLib.static_field_of_name "Binary"
let field_objectid  = ServerLib.static_field_of_name "ObjectID"
let field_date      = ServerLib.static_field_of_name "Date"
let field_regexp    = ServerLib.static_field_of_name "Regexp"
let field_code      = ServerLib.static_field_of_name "Code"
let field_symbol    = ServerLib.static_field_of_name "Symbol"
let field_codescope = ServerLib.static_field_of_name "CodeScope"
let field_timestamp = ServerLib.static_field_of_name "Timestamp"

let shared_nil     = ServerLib.make_simple_record field_nil
let make_element name value =
  ServerLib.make_record
    (ServerLib.add_field
       (ServerLib.add_field ServerLib.empty_record_constructor field_name name)
       field_value value)
let make_pair x y =
  ServerLib.make_record (ServerLib.add_field (ServerLib.add_field ServerLib.empty_record_constructor field_fst x) field_snd y)
let make_cons hd tl =
  ServerLib.make_record (ServerLib.add_field (ServerLib.add_field ServerLib.empty_record_constructor field_hd hd) field_tl tl)

let make_val fld n x =
  make_element n (ServerLib.make_record (ServerLib.add_field ServerLib.empty_record_constructor fld x))

let make_null = make_val field_null
let make_minkey = make_val field_minkey
let make_maxkey = make_val field_maxkey
let make_int32 = make_val field_int32
let make_realint32 = make_val field_realint32
let make_int64 = make_val field_int64
let make_realint64 = make_val field_realint64
let make_double = make_val field_double
let make_bool n x = make_val field_bool n (ServerLib.wrap_bool x)
let make_string = make_val field_string
let make_document = make_val field_document
let make_array = make_val field_array
let make_binary = make_val field_binary
let make_objectid = make_val field_objectid
let make_date = make_val field_date
let make_regexp n r ro = make_val field_regexp n (make_pair r ro)
let make_code = make_val field_code
let make_symbol = make_val field_symbol
let make_codescope n c s = make_val field_codescope n (make_pair c s)
let make_timestamp n (i,t) = make_val field_timestamp n (make_pair (i32toi i) (i32toi t))

let deserialize s =
  let i = Bson.IteratorSS.from_buffer s in
  let rec aux i =
    (function
     | c when c = Bson.el_eoo -> shared_nil
     | c when c = Bson.el_int ->
         let i32 = Bson.IteratorSS.int i in
         let e =
           try make_int32 (Bson.IteratorSS.key i) (i32toi i32)
           with (Overflow _) -> make_realint32 (Bson.IteratorSS.key i) i32
         in
         auxn e i
     | c when c = Bson.el_long ->
         let i64 = Bson.IteratorSS.long i in
         let e =
           try make_int64 (Bson.IteratorSS.key i) (i64toi i64)
           with (Overflow _) -> make_realint64 (Bson.IteratorSS.key i) i64
         in
         auxn e i
     | c when c = Bson.el_double -> let e = make_double (Bson.IteratorSS.key i) (Bson.IteratorSS.double i) in auxn e i
     | c when c = Bson.el_bool -> let e = make_bool (Bson.IteratorSS.key i) (Bson.IteratorSS.bool i) in auxn e i
     | c when c = Bson.el_string -> let e = make_string (Bson.IteratorSS.key i) (Bson.IteratorSS.string i) in auxn e i
     | c when c = Bson.el_object ->
         let key =  Bson.IteratorSS.key i in
         let ii = Bson.IteratorSS.subiterator i in
         let bsons = aux ii (Bson.IteratorSS.next ii) in
         let e = make_document key bsons in auxn e i
     | c when c = Bson.el_array ->
         let key = Bson.IteratorSS.key i in
         let ii = Bson.IteratorSS.subiterator i in
         let bsons = aux ii (Bson.IteratorSS.next ii) in
         let e = make_array key bsons in auxn e i
     | c when c = Bson.el_bindata -> let e = make_binary (Bson.IteratorSS.key i) (Bson.IteratorSS.bin_data i) in auxn e i
     | c when c = Bson.el_oid -> let e = make_objectid (Bson.IteratorSS.key i) (Bson.IteratorSS.oid i) in auxn e i
     | c when c = Bson.el_date -> let e = make_date (Bson.IteratorSS.key i) (i64toi (Bson.IteratorSS.date i)) in auxn e i
     | c when c = Bson.el_null -> let e = make_null (Bson.IteratorSS.key i) ServerLib.void in auxn e i
     | c when c = Bson.el_minkey -> let e = make_minkey (Bson.IteratorSS.key i) ServerLib.void in auxn e i
     | c when c = Bson.el_maxkey -> let e = make_maxkey (Bson.IteratorSS.key i) ServerLib.void in auxn e i
     | c when c = Bson.el_regex ->
         let e = make_regexp (Bson.IteratorSS.key i) (Bson.IteratorSS.regex i) (Bson.IteratorSS.regex_opts i) in auxn e i
     | c when c = Bson.el_code -> let e = make_code (Bson.IteratorSS.key i) (Bson.IteratorSS.code i) in auxn e i
     | c when c = Bson.el_symbol -> let e = make_symbol (Bson.IteratorSS.key i) (Bson.IteratorSS.symbol i) in auxn e i
     | c when c = Bson.el_codewscope ->
         let key = Bson.IteratorSS.key i in
         let code = Bson.IteratorSS.code i in
         let b = Bson.IteratorSS.code_scope i in
         let ii = Bson.IteratorSS.init b in
         let scope = aux ii (Bson.IteratorSS.next ii) in
         let e = make_codescope key code scope in auxn e i
     | c when c = Bson.el_timestamp -> let e = make_timestamp (Bson.IteratorSS.key i) (Bson.IteratorSS.timestamp i) in auxn e i
     | c ->
         Printf.eprintf "Unknown Bson code: %c\n%!" c;
         assert false)
  and auxn e i = make_cons (wrap_opa_bson_document e) (aux i (Bson.IteratorSS.next i))
  in
  wrap_opa_bson_document (aux i (Bson.IteratorSS.next i))

##endmodule

##module Mongo

let rid = ref(Random.int32(Int32.max_int))
let nextrid() = let id = !rid in rid := Int32.add id 1l; id

##register create: int -> Mongo.mongo_buf
let create = Mongo.create

##register insert: Mongo.mongo_buf, int, string, 'a -> void
let insert m f ns (bson:'a) =
  let (bson:opa_bson_document) = Obj.magic bson in
  Mongo.start_insert m (nextrid()) f ns;
  Mongo.bson_init m;
  Bson.serialize bson m;
  Mongo.bson_finish m;
  Mongo.finish m

##register insert_batch: Mongo.mongo_buf, int, string, opa[list('a)] -> void
let insert_batch m f ns (bsons:'a) =
  let (bsons:opa_bson_document list) = Obj.magic (opa_list_to_ocaml_list (fun x -> x) bsons) in
  Mongo.start_insert m (nextrid()) f ns;
  List.iter (fun bson ->
               Mongo.bson_init m;
               Bson.serialize bson m;
               Mongo.bson_finish m) bsons;
  Mongo.finish m

##register update: Mongo.mongo_buf, int, string, 'a, 'a -> void
let update m flags ns selector update =
  let (selector:opa_bson_document) = Obj.magic selector in
  let (update:opa_bson_document) = Obj.magic update in
  Mongo.start_update m (nextrid()) flags ns;
  Mongo.bson_init m;
  Bson.serialize selector m;
  Mongo.bson_finish m;
  Mongo.bson_init m;
  Bson.serialize update m;
  Mongo.bson_finish m;
  Mongo.finish m

##register query: Mongo.mongo_buf, int, string, int, int, 'a, option('a) -> void
let query m flags ns numberToSkip numberToReturn query returnFieldSelector_opt =
  let (query:opa_bson_document) = Obj.magic query in
  let (returnFieldSelector_opt:opa_bson_document option) = Obj.magic returnFieldSelector_opt in
  Mongo.start_query m (nextrid()) flags ns numberToSkip numberToReturn;
  Mongo.bson_init m;
  Bson.serialize query m;
  Mongo.bson_finish m;
  (match returnFieldSelector_opt with
   | Some returnFieldSelector ->
       Mongo.bson_init m;
       Bson.serialize returnFieldSelector m;
       Mongo.bson_finish m
   | None -> ());
  Mongo.finish m

##register set_query_flags: Mongo.mongo_buf, int -> void
let set_query_flags m flags = Mongo.set_query_flags m flags

##register get_opCode: Mongo.mongo_buf -> int
let get_opCode m = Mongo.get_opCode m

##register get_more: Mongo.mongo_buf, string, int, Mongo.cursorID -> void
let get_more m ns numberToReturn cursorID =
  Mongo.start_getmore m (nextrid()) ns numberToReturn cursorID;
  Mongo.finish m

##register delete: Mongo.mongo_buf, int, string, 'a -> void
let delete m flags ns selector =
  let (selector:opa_bson_document) = Obj.magic selector in
  Mongo.start_delete m (nextrid()) flags ns;
  Mongo.bson_init m;
  Bson.serialize selector m;
  Mongo.bson_finish m;
  Mongo.finish m

##register kill_cursors: Mongo.mongo_buf, opa[list('a)] -> void
let kill_cursors m clist =
  let clist = Obj.magic clist in
  Mongo.start_kill_cursors m (nextrid()) (opa_list_to_ocaml_list (fun x -> x) clist);
  Mongo.finish m

##register msg: Mongo.mongo_buf, string -> void
let msg m msg =
  Mongo.start_msg m (nextrid()) msg;
  Mongo.finish m

##register get: Mongo.mongo_buf -> string
let get = Mongo.get

##register export: Mongo.mongo_buf -> opa[tuple_2(string, int)]
let export m =
  let (str, i) = Mongo.export m in
  opa_tuple_2 (ServerLib.wrap_string str, ServerLib.wrap_int i)

##register import: string -> Mongo.mongo_buf
let import = Mongo.import

##register copy: Mongo.mongo_buf -> Mongo.mongo_buf
let copy = Mongo.copy

##register concat: Mongo.mongo_buf, Mongo.mongo_buf -> Mongo.mongo_buf
let concat = Mongo.concat

##register append: Mongo.mongo_buf, Mongo.mongo_buf -> void
let append = Mongo.append

##register length: Mongo.mongo_buf -> int
let length = Mongo.length

##register clip: Mongo.mongo_buf, int -> void
let clip = Mongo.clip

##register clear: Mongo.mongo_buf -> void
let clear = Mongo.clear

##register reset: Mongo.mongo_buf -> void
let reset = Mongo.reset

##register free: Mongo.mongo_buf -> void
let free = Mongo.free

##register new_mailbox: int -> Mongo.mailbox
let new_mailbox size = (Mongo.get_buf ~hint:size (), ref 0)

##register reset_mailbox: Mongo.mailbox -> void
let reset_mailbox (b,_) = Mongo.free_buf b

##register [cps-bypass] read_mongo : Socket.connection, int, Mongo.mailbox, continuation(outcome(Mongo.reply,string)) -> void
let read_mongo conn timeout mailbox k =
  let timeout = Time.milliseconds timeout in
  HttpTools.fixed_stream_cps2_buf Scheduler.default conn mailbox 4 ()
    ~err_cont:(fun exn -> create_outcome (`failure (Printexc.to_string exn)) |> k)
    ~timeout
    (fun (b,s,l) ->
       if l < 4
       then create_outcome (`failure "BslMongo.Mongo.read_mongo insufficient data") |> k
       else
         let len = FillbufString.ldi32 (Buf.sub b s 4) 0 in
         HttpTools.fixed_stream_cps2_buf Scheduler.default conn mailbox (len-4) ()
           ~err_cont:(fun exn -> create_outcome (`failure (Printexc.to_string exn)) |> k)
           ~timeout
           (fun (b,s,l) ->
              if l < len - 4
              then create_outcome (`failure "BslMongo.Mongo.read_mongo insufficient data") |> k
              else create_outcome (`success (b,s,l)) |> k))

##register export_reply: Mongo.reply -> string
let export_reply (b,s,l) = Buf.sub b s l

##register reply_messageLength : Mongo.reply -> int
let reply_messageLength = Mongo.reply_messageLength

##register reply_requestId : Mongo.reply -> int
let reply_requestId = Mongo.reply_requestId

##register reply_responseTo : Mongo.reply -> int
let reply_responseTo = Mongo.reply_responseTo

##register reply_opCode : Mongo.reply -> int
let reply_opCode = Mongo.reply_opCode

##register reply_responseFlags : Mongo.reply -> int
let reply_responseFlags = Mongo.reply_responseFlags

##register reply_cursorID : Mongo.reply -> Mongo.cursorID
let reply_cursorID = Mongo.reply_cursorID

##register reply_startingFrom : Mongo.reply -> int
let reply_startingFrom = Mongo.reply_startingFrom

##register reply_numberReturned : Mongo.reply -> int
let reply_numberReturned = Mongo.reply_numberReturned

##register reply_document : Mongo.reply, int -> option(Bson.document)
let reply_document (b,s,l) n =
  match Mongo.reply_document_pos (b,s,l) n with
  | Some (pos, size) -> Some (Bson.deserialize (BaseStringSlice.import (b.Buf.str,pos,size)))
  | None -> None

##register string_of_message : string -> string
let string_of_message = Mongo.string_of_message_str

##register string_of_message_reply : Mongo.reply -> string
let string_of_message_reply = Mongo.string_of_message_reply

##register null_cursorID : void -> Mongo.cursorID
let null_cursorID () = 0L

##register string_of_cursorID : Mongo.cursorID -> string
let string_of_cursorID cid = Printf.sprintf "%016Lx" cid

##register is_null_cursorID : Mongo.cursorID -> opa[bool]
let is_null_cursorID cid = ServerLib.wrap_bool (cid = 0L)

##register mongo_buf_requestId : Mongo.mongo_buf -> int
let mongo_buf_requestId = Mongo.mongo_buf_requestId

##register mongo_buf_refresh_requestId : Mongo.mongo_buf -> void
let mongo_buf_refresh_requestId m = Mongo.mongo_buf_refresh_requestId m (nextrid())

##register mongo_buf_responseTo : Mongo.mongo_buf -> int
let mongo_buf_responseTo = Mongo.mongo_buf_responseTo

##register encode_field : string -> string
let encode_field field =
  Encodings.pc_encode_string
    (function '.' | '$' -> false | _ -> true)
    field

##register decode_field : string  -> string
let decode_field = Encodings.pc_decode_string

##endmodule
