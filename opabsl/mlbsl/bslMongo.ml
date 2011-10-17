(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

module C = QmlCpsServerLib
open C.Ops

##opa-type Bson.document
##opa-type Bson.element
##extern-type bson_buf = Bson.buf
##extern-type mongo_buf = Mongo.mongo_buf
##extern-type cursorID = int64
##extern-type mailbox = (Buf.t * int ref)
##extern-type reply = (Buf.t * int * int)

##module Bson

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
               | Some "Date" ->Bson.Append.date b name (ServerLib.unwrap_int value)
               | Some "Null" -> Bson.Append.null b name
               | Some "Min" -> Bson.Append.minkey b name
               | Some "Max" -> Bson.Append.maxkey b name
               | Some "Regexp" ->
                   (match BslNativeLib.ocaml_tuple_2 value with
                    | (regexp, regexp_opts) ->
                        Bson.Append.regex b name (ServerLib.unwrap_string regexp) (ServerLib.unwrap_string regexp_opts))
               | Some "Code" -> Bson.Append.code b name (ServerLib.unwrap_string value)
               | Some "Symbol" -> Bson.Append.symbol b name (ServerLib.unwrap_string value)
               | Some "CodeScope" ->
                   (match BslNativeLib.ocaml_tuple_2 value with
                    | (code, scope) ->
                        Bson.Append.start_codewscope b name code;
                        aux scope;
                        Bson.Append.finish_codewscope b code)
               | Some "Int32" ->Bson.Append.int b name (ServerLib.unwrap_int value)
               | Some "Timestamp" ->
                   (match BslNativeLib.ocaml_tuple_2 value with
                    | (i, t) ->
                        Bson.Append.timestamp b name ((ServerLib.unwrap_int i), (ServerLib.unwrap_int t)))
               | Some "Int64" -> Bson.Append.long b name (ServerLib.unwrap_int value)
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
let field_int64     = ServerLib.static_field_of_name "Int64"
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
let make_int64 = make_val field_int64
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
let make_timestamp n (i,t) = make_val field_timestamp n (make_pair i t)

let deserialize s =
  let i = Bson.IteratorSS.from_buffer s in
  let rec aux i =
    (function
     | c when c = Bson.el_eoo -> shared_nil
     | c when c = Bson.el_int -> let e = make_int32 (Bson.IteratorSS.key i) (Bson.IteratorSS.int i) in auxn e i
     | c when c = Bson.el_long -> let e = make_int64 (Bson.IteratorSS.key i) (Bson.IteratorSS.long i) in auxn e i
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
     | c when c = Bson.el_date -> let e = make_date (Bson.IteratorSS.key i) (Bson.IteratorSS.date i) in auxn e i
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

##register create: int -> mongo_buf
let create = Mongo.create

(* AAAAAAARGHHHHHHH!!!!!! OPA just can't equate opa_bson_document with ServerLib.ty_record *)

##register insert: mongo_buf, int, string, 'a -> void
let insert m f ns (bson:'a) =
  let (bson:opa_bson_document) = Obj.magic bson in
  Mongo.start_insert m 0l f ns;
  Mongo.bson_init m;
  Bson.serialize bson m;
  Mongo.bson_finish m;
  Mongo.finish m

##register insert_batch: mongo_buf, int, string, opa[list('a)] -> void
let insert_batch m f ns (bsons:'a) =
  let (bsons:opa_bson_document list) = Obj.magic (BslNativeLib.opa_list_to_ocaml_list (fun x -> x) bsons) in
  Mongo.start_insert m 0l f ns;
  List.iter (fun bson ->
               Mongo.bson_init m;
               Bson.serialize bson m;
               Mongo.bson_finish m) bsons;
  Mongo.finish m

##register update: mongo_buf, int, string, 'a, 'a -> void
let update m flags ns selector update =
  let (selector:opa_bson_document) = Obj.magic selector in
  let (update:opa_bson_document) = Obj.magic update in
  Mongo.start_update m 0l flags ns;
  Mongo.bson_init m;
  Bson.serialize selector m;
  Mongo.bson_finish m;
  Mongo.bson_init m;
  Bson.serialize update m;
  Mongo.bson_finish m;
  Mongo.finish m

##register query: mongo_buf, int, string, int, int, 'a, option('a) -> void
let query m flags ns numberToSkip numberToReturn query returnFieldSelector_opt =
  let (query:opa_bson_document) = Obj.magic query in
  let (returnFieldSelector_opt:opa_bson_document option) = Obj.magic returnFieldSelector_opt in
  Mongo.start_query m 0l flags ns numberToSkip numberToReturn;
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

##register get_more: mongo_buf, string, int, cursorID -> void
let get_more m ns numberToReturn cursorID =
  Mongo.start_getmore m 0l ns numberToReturn cursorID;
  Mongo.finish m

##register delete: mongo_buf, int, string, 'a -> void
let delete m flags ns selector =
  let (selector:opa_bson_document) = Obj.magic selector in
  Mongo.start_delete m 0l flags ns;
  Mongo.bson_init m;
  Bson.serialize selector m;
  Mongo.bson_finish m;
  Mongo.finish m

##register kill_cursors: mongo_buf, opa[list('a)] -> void
let kill_cursors m clist =
  let clist = Obj.magic clist in
  Mongo.start_kill_cursors m 0l (BslNativeLib.opa_list_to_ocaml_list (fun x -> x) clist);
  Mongo.finish m

##register msg: mongo_buf, string -> void
let msg m msg =
  Mongo.start_msg m 0l msg;
  Mongo.finish m

##register get: mongo_buf -> string
let get = Mongo.get

##register export: mongo_buf -> opa[tuple_2(string, int)]
let export m =
  let (str, i) = Mongo.export m in
  BslNativeLib.opa_tuple_2 (ServerLib.wrap_string str, ServerLib.wrap_int i)

##register clear: mongo_buf -> void
let clear = Mongo.clear

##register reset: mongo_buf -> void
let reset = Mongo.reset

##register new_mailbox: int -> mailbox
let new_mailbox size = (Buf.create size, ref 0)

##register reset_mailbox: mailbox -> void
let reset_mailbox (b,_) = Buf.reset b

##register [cps-bypass] read_mongo : Socket.connection, mailbox, continuation(reply) -> void
let read_mongo conn mailbox k =
  HttpTools.fixed_stream_cps2_buf Scheduler.default conn mailbox 4 ()
    (fun (b,s,l) ->
       if l < 4 then raise (Failure "BslMongo.Mongo.read_mongo insufficient data");
       let len = Stuff.StuffString.ldi32 (Buf.sub b s 4) 0 in
       HttpTools.fixed_stream_cps2_buf Scheduler.default conn mailbox (len-4) ()
         (fun (b,s,l) ->
            if l < len - 4 then raise (Failure "BslMongo.Mongo.read_mongo insufficient data");
            (b,s,l) |> k))

##register export_reply: reply -> string
let export_reply (b,s,l) = Buf.sub b s l

##register reply_messageLength : reply -> int
let reply_messageLength = Mongo.reply_messageLength 

##register reply_requestId : reply -> int
let reply_requestId = Mongo.reply_requestId 

##register reply_responseTo : reply -> int
let reply_responseTo = Mongo.reply_responseTo 

##register reply_opCode : reply -> int
let reply_opCode = Mongo.reply_opCode 

##register reply_responseFlags : reply -> int
let reply_responseFlags = Mongo.reply_responseFlags 

##register reply_cursorID : reply -> cursorID
let reply_cursorID = Mongo.reply_cursorID 

##register reply_startingFrom : reply -> int
let reply_startingFrom = Mongo.reply_startingFrom 

##register reply_numberReturned : reply -> int
let reply_numberReturned = Mongo.reply_numberReturned 

##register reply_document : reply, int -> option(Bson.document)
let reply_document (b,s,l) n =
  match Mongo.reply_document_pos (b,s,l) n with
  | Some (pos, size) -> Some (Bson.deserialize (BaseStringSlice.import (b.Buf.str,pos,size)))
  | None -> None

##register null_cursorID : void -> cursorID
let null_cursorID () = 0L

##register string_of_cursorID : cursorID -> string
let string_of_cursorID cid = Printf.sprintf "%016Lx" cid

##register is_null_cursorID : cursorID -> opa[bool]
let is_null_cursorID cid = ServerLib.wrap_bool (cid = 0L)

##endmodule
