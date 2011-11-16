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

#<Debugvar:MONGO_DEBUG>

module St = Stuff.StuffString

(* OP codes *)
let _OP_REPLY         = 1    (* Reply to a client request. responseTo is set *)
let _OP_MSG           = 1000 (* generic msg command followed by a string *)
let _OP_UPDATE        = 2001 (* update document *)
let _OP_INSERT        = 2002 (* insert new document *)
let _RESERVED         = 2003 (* formerly used for OP_GET_BY_OID *)
let _OP_QUERY         = 2004 (* query a collection *)
let _OP_GET_MORE      = 2005 (* Get more data from a query. See Cursors *)
let _OP_DELETE        = 2006 (* Delete documents *)
let _OP_KILL_CURSORS  = 2007 (* Tell database client is done with a cursor  *)

let string_of_opcode = function
  | 1    -> "OP_REPLY"
  | 1000 -> "OP_MSG"
  | 2001 -> "OP_UPDATE"
  | 2002 -> "OP_INSERT"
  | 2003 -> "RESERVED"
  | 2004 -> "OP_QUERY"
  | 2005 -> "OP_GET_MORE"
  | 2006 -> "OP_DELETE"
  | 2007 -> "OP_KILL_CURSORS"
  | n -> Printf.sprintf "OP_UNKNOWN(%d)" n

let geti32 b s = Stuff.StuffString.ldi32 (Buf.sub b s 4) 0
let geti64L b s = Stuff.StuffString.ldi64L (Buf.sub b s 8) 0
let cstring b s =
  let pos = ref 0 in
  while Buf.get b (s + !pos) <> '\x00' do incr pos done;
  (s + !pos + 1, Buf.sub b s !pos)

let docmap b pos len num =
  let rec aux i pos idxs =
    if i >= num || len - pos < 4
    then idxs
    else 
      let size = geti32 b pos in
      if len - pos < size
      then idxs
      else aux (i+1) (pos+size) ((pos,size)::idxs)
  in
  aux 0 pos []

(* Flags *)

(* OP_INSERT *)
let _ContinueOnError  = 0x00000001

(* OP_UPDATE *)
let _Upsert           = 0x00000001
let _MultiUpdate      = 0x00000002

(* OP_QUERY *)
let _TailableCursor   = 0x00000002
let _SlaveOk          = 0x00000004
let _OplogReplay      = 0x00000008
let _NoCursorTimeout  = 0x00000010
let _AwaitData        = 0x00000020
let _Exhaust          = 0x00000040
let _Partial          = 0x00000080

(* OP_DELETE *)
let _SingleRemove     = 0x00000001

(* OP_REPLY *)
let _CursorNotFound   = 0x00000001
let _QueryFailure     = 0x00000002
let _ShardConfigStale = 0x00000003
let _AwaitCapable     = 0x00000004

type mongo_buf = Bson.buf

let add_bson m bson =
  Buf.append m.Bson.buf bson.Bson.buf.Buf.str (Bson.Append.size bson)

let get m = Buf.to_string m.Bson.buf

let export m = (m.Bson.buf.Buf.str, m.Bson.buf.Buf.i)

let import s = { Bson.buf = Buf.of_string s; stack = []; finished = true }

let copy m = { Bson.buf = Buf.copy m.Bson.buf; stack = m.Bson.stack; finished = m.Bson.finished }

let concat m1 m2 = { Bson.buf = Buf.of_string(m1.Bson.buf.Buf.str^m2.Bson.buf.Buf.str); stack = []; finished = true }

let append m1 m2 = Buf.add_buf m1.Bson.buf m2.Bson.buf

(*
struct MsgHeader {
    int32   messageLength; // total message size, including this
    int32   requestID;     // identifier for this message
    int32   responseTo;    // requestID from the original request (used in reponses from db)
    int32   opCode;        // request type - see table below
}
*)

let mongo_buf_requestId mb = geti32 mb.Bson.buf 4
let mongo_buf_responseTo mb = geti32 mb.Bson.buf 8

let header_messageLength s = geti32 s 0
let header_requestId s = geti32 s 4
let header_responseTo s = geti32 s 8
let header_opCode s = geti32 s 12

let string_of_MsgHeader hdr =
  Printf.sprintf "messageLength = %d\nrequestId = 0x%08x\nresponseTo = 0x%08x\nopCode = %s\n"
    (header_messageLength hdr) (header_requestId hdr) (header_responseTo hdr) (string_of_opcode (header_opCode hdr))

let set_header_len m messageLength =
  St.lei32 m.Bson.buf.Buf.str 0 messageLength

let set_header m requestId responseTo opCode =
  St.lei32l m.Bson.buf.Buf.str 4 (if requestId = 0l then Random.int32 Int32.max_int else requestId);
  St.lei32 m.Bson.buf.Buf.str 8 responseTo;
  St.lei32 m.Bson.buf.Buf.str 12 opCode;
  m.Bson.buf.Buf.i <- 16

let buflst = ref ([]:Buf.t list)
let bufcnt = ref 0
let buflog = ref (fun str -> Printf.eprintf "%s\n%!" str)

let get_buf ?(hint=4096) () =
  match !buflst with
  | [] -> (#<If$minlevel 2>!buflog (Printf.sprintf "get_buf(%d): new" !bufcnt)#<End>; Buf.create hint)
  | b::t -> (#<If$minlevel 2>!buflog (Printf.sprintf "get_buf(%d): old" !bufcnt)#<End>; buflst := t; decr bufcnt; Buf.clear b; b)

let free_buf b =
  if Buf.length b <= (10*1024*1024)
  then (#<If$minlevel 2>!buflog (Printf.sprintf "free_buf(%d): return" !bufcnt)#<End>; buflst := b::(!buflst); incr bufcnt)
  else (#<If$minlevel 2>!buflog (Printf.sprintf "free_buf(%d): reset" !bufcnt)#<End>; Buf.reset b)

let create size =
  if size < 16 then raise (Failure "Mongo.create: ridiculous size value");
  let b = { Bson.buf=get_buf ~hint:size (); stack=[]; finished=false; } in
  b.Bson.buf.Buf.i <- 16;
  b

let init ?(hint=100) messageLength requestId responseTo opCode =
  let m = create hint in
  set_header_len m messageLength;
  set_header m requestId responseTo opCode;
  m

let clear m = m.Bson.buf.Buf.i <- 16

let reset m = Buf.reset m.Bson.buf

let free m = free_buf m.Bson.buf

(*struct OP_INSERT {
    MsgHeader header;             // standard message header
    int32     flags;              // bit vector - see below
    cstring   fullCollectionName; // "dbname.collectionname"
    document* documents;          // one or more documents to insert into the collection
}*)

let string_of_insert_flags flags =
  let l = if flags land 0x01 <> 0 then ["ContinueOnError "] else [] in
  BaseList.to_string (fun s -> s) l

let string_of_insert b =
  let pos, cname = cstring b 20 in
  let dm = docmap b pos (header_messageLength b) max_int in
  let docs = BaseList.mapi (fun i (pos, size) ->
                              "doc"^(string_of_int i)^" = "^
                              (Bson.Print.to_pretty_raw (Buf.sub b pos size) 0)) dm in
  (Printf.sprintf "  flags = %s\n  fullCollectionName = %s\n  "
     (string_of_insert_flags (geti32 b 16)) cname)^
  (String.concat "\n  " docs)

let start_insert m rid flags ns =
  set_header m rid 0 _OP_INSERT;
  Stuff.add_le_int32 m.Bson.buf flags;
  Buf.add_string m.Bson.buf ns;
  Buf.add_char m.Bson.buf '\x00'

(*struct OP_UPDATE {
    MsgHeader header;             // standard message header
    int32     ZERO;               // 0 - reserved for future use
    cstring   fullCollectionName; // "dbname.collectionname"
    int32     flags;              // bit vector. see below
    document  selector;           // the query to select the document
    document  update;             // specification of the update to perform
}*)

let string_of_update_flags flags =
  let l = if flags land 0x01 <> 0 then ["Upsert "] else [] in
  let l = if flags land 0x02 <> 0 then "MultiUpdate"::l else l in
  BaseList.to_string (fun s -> s) l

let string_of_update b =
  let pos, cname = cstring b 20 in
  let dm = docmap b (pos+4) (header_messageLength b) 2 in
  let docs = BaseList.mapi (fun i (pos, size) ->
                             (match i with 0 -> "selector" | 1 -> "update" | _ -> "extraneous")^" = "^
                             (Bson.Print.to_pretty_raw (Buf.sub b pos size) 0)) dm in
  (Printf.sprintf "  fullCollectionName = %s\n  flags = %s\n  "
     cname (string_of_update_flags (geti32 b pos)))^
  (String.concat "\n  " docs)

let start_update m rid flags ns =
  set_header m rid 0 _OP_UPDATE;
  Stuff.add_le_int32 m.Bson.buf 0;
  Buf.add_string m.Bson.buf ns;
  Buf.add_char m.Bson.buf '\x00';
  Stuff.add_le_int32 m.Bson.buf flags

(*struct OP_QUERY {
    MsgHeader header;                // standard message header
    int32     flags;                 // bit vector of query options.
    cstring   fullCollectionName;    // "dbname.collectionname"
    int32     numberToSkip;          // number of documents to skip
    int32     numberToReturn;        // number of documents to return in the first OP_REPLY batch
    document  query;                 // query object.
  [ document  returnFieldSelector; ] // Optional. Selector indicating the fields to return.
}*)

let string_of_query_flags flags =
  let l = if flags land 0x01 <> 0 then ["Reserved"] else [] in
  let l = if flags land 0x02 <> 0 then "TailableCursor"::l else l in
  let l = if flags land 0x04 <> 0 then "SlaveOk"::l else l in
  let l = if flags land 0x08 <> 0 then "OplogReplay"::l else l in
  let l = if flags land 0x10 <> 0 then "NoCursorTimeout"::l else l in
  let l = if flags land 0x20 <> 0 then "AwaitData"::l else l in
  let l = if flags land 0x40 <> 0 then "Exhaust"::l else l in
  let l = if flags land 0x80 <> 0 then "Partial"::l else l in
  BaseList.to_string (fun s -> s) l

let string_of_query b =
  let pos, cname = cstring b 20 in
  let dm = docmap b (pos+8) (header_messageLength b) 2 in
  let docs = BaseList.mapi (fun i (pos, size) ->
                              (match i with 0 -> "query" | 1 -> "returnFieldSelector" | _ -> "extraneous")^" = "^
                                (Bson.Print.to_pretty_raw (Buf.sub b pos size) 0)) dm in
  (Printf.sprintf "  flags = %s\n  fullCollectionName = %s\n  numberToSkip = %d\n  numberToReturn = %d\n  "
     (string_of_query_flags (geti32 b 16)) cname (geti32 b pos) (geti32 b (pos+4)))^
  (String.concat "\n  " docs)

let start_query m rid flags ns numberToSkip numberToReturn =
  set_header m rid 0 _OP_QUERY;
  Stuff.add_le_int32 m.Bson.buf flags;
  Buf.add_string m.Bson.buf ns;
  Buf.add_char m.Bson.buf '\x00';
  Stuff.add_le_int32 m.Bson.buf numberToSkip;
  Stuff.add_le_int32 m.Bson.buf numberToReturn

(*struct OP_GETMORE {
    MsgHeader header;             // standard message header
    int32     ZERO;               // 0 - reserved for future use
    cstring   fullCollectionName; // "dbname.collectionname"
    int32     numberToReturn;     // number of documents to return
    int64     cursorID;           // cursorID from the OP_REPLY
}*)

let string_of_get_more b =
  let pos, cname = cstring b 20 in
  Printf.sprintf "  fullCollectionName = %s\n  numberToReturn = %d\n  cursorID = 0x%016Lx"
    cname (geti32 b pos) (geti64L b (pos+4))

let start_getmore m rid ns numberToReturn cursorID =
  set_header m rid 0 _OP_GET_MORE;
  Stuff.add_le_int32 m.Bson.buf 0;
  Buf.add_string m.Bson.buf ns;
  Buf.add_char m.Bson.buf '\x00';
  Stuff.add_le_int32 m.Bson.buf numberToReturn;
  Stuff.add_le_int64L m.Bson.buf cursorID

(*struct OP_DELETE {
    MsgHeader header;             // standard message header
    int32     ZERO;               // 0 - reserved for future use
    cstring   fullCollectionName; // "dbname.collectionname"
    int32     flags;              // bit vector - see below for details.
    document  selector;           // query object.  See below for details.
}*)

let string_of_delete_flags flags =
  let l = if flags land 0x01 <> 0 then ["SingleRemove "] else [] in
  BaseList.to_string (fun s -> s) l

let string_of_delete b =
  let pos, cname = cstring b 20 in
  let dm = docmap b (pos+4) (header_messageLength b) 1 in
  let docs = BaseList.mapi (fun i (pos, size) ->
                             (match i with 0 -> "selector" | _ -> "extraneous")^" = "^
                             (Bson.Print.to_pretty_raw (Buf.sub b pos size) 0)) dm in
  (Printf.sprintf "  fullCollectionName = %s\n  flags = %s\n  "
     cname (string_of_delete_flags (geti32 b pos)))^
  (String.concat "\n  " docs)

let start_delete m rid flags ns =
  set_header m rid 0 _OP_DELETE;
  Stuff.add_le_int32 m.Bson.buf 0;
  Buf.add_string m.Bson.buf ns;
  Buf.add_char m.Bson.buf '\x00';
  Stuff.add_le_int32 m.Bson.buf flags

(*struct OP_KILL_CURSORS {
    MsgHeader header;            // standard message header
    int32     ZERO;              // 0 - reserved for future use
    int32     numberOfCursorIDs; // number of cursorIDs in message
    int64*    cursorIDs;         // sequence of cursorIDs to close
}*)

let string_of_kill_cursors b =
  let numberOfCursorIDs = geti32 b 20 in
  let cursors = BaseList.init numberOfCursorIDs (fun i -> geti64L b (24+i*8)) in
  Printf.sprintf "  numberOfCursorIDs = %d\n  cursorIDs = %s\n  "
    numberOfCursorIDs (BaseString.concat_map ~left:"[" ~right:"]" "; " (fun i -> Printf.sprintf "0x%016Lx" i) cursors)

let start_kill_cursors m rid clist =
  set_header m rid 0 _OP_KILL_CURSORS;
  Stuff.add_le_int32 m.Bson.buf 0;
  Stuff.add_le_int32 m.Bson.buf (List.length clist);
  List.iter (fun cursorID -> Stuff.add_le_int64L m.Bson.buf cursorID) clist

(*struct OP_MSG {
    MsgHeader header;  // standard message header
    cstring   message; // message for the database
}*)

let string_of_msg b =
  let _, cname = cstring b 16 in
  Printf.sprintf "  message = %s" cname

let start_msg m rid msg =
  set_header m rid 0 _OP_MSG;
  Buf.add_string m.Bson.buf msg;
  Buf.add_char m.Bson.buf '\x00'

let bson_init m =
  m.Bson.stack <- m.Bson.buf.Buf.i :: m.Bson.stack;
  Buf.extend m.Bson.buf 4

let bson_finish m =
  let start = List.hd m.Bson.stack in
  m.Bson.stack <- List.tl m.Bson.stack;
  if not m.Bson.finished
  then (Buf.add_char m.Bson.buf '\x00';
        St.lei32 m.Bson.buf.Buf.str start (m.Bson.buf.Buf.i-start))

let finish m =
  set_header_len m (Buf.length m.Bson.buf);
  m.Bson.finished <- true

(*struct OP_REPLY {
    MsgHeader header;         // standard message header
    int32     responseFlags;  // bit vector - see details below
    int64     cursorID;       // cursor id if client needs to do get more's
    int32     startingFrom;   // where in the cursor this reply is starting
    int32     numberReturned; // number of documents in the reply
    document* documents;      // documents
}*)

let string_of_response_flags flags =
  let l = if flags land 0x01 <> 0 then ["CursorNotFound"] else [] in
  let l = if flags land 0x02 <> 0 then "QueryFailure"::l else l in
  let l = if flags land 0x04 <> 0 then "ShardConfigStale"::l else l in
  let l = if flags land 0x08 <> 0 then "AwaitCapable"::l else l in
  BaseList.to_string (fun s -> s) l

let string_of_reply b =
  let numberReturned = geti32 b 32 in
  let dm = docmap b 36 (header_messageLength b) numberReturned in
  let docs = List.map (fun (pos, size) -> (Bson.Print.to_pretty_raw (Buf.sub b pos size) 0)) dm in
  (Printf.sprintf "  responseFlags = %s\n  cursorID=%016Lx\n  startingFrom=%d\n  numberReturned=%d\n  "
     (string_of_response_flags (geti32 b 16)) (geti64L b 20) (geti32 b 28) numberReturned)^
  (String.concat "\n  " docs)

let reply_messageLength (_,_,l) = l + 4
let reply_requestId (b,s,_) = geti32 b (s+0)
let reply_responseTo (b,s,_) = geti32 b (s+4)
let reply_opCode (b,s,_) = geti32 b (s+8)
let reply_responseFlags (b,s,_) = geti32 b (s+12)
let reply_cursorID (b,s,_) = geti64L b (s+16)
let reply_startingFrom (b,s,_) = geti32 b (s+24)
let reply_numberReturned (b,s,_) = geti32 b (s+28)

let reply_document_pos (b,s,l) n =
  let messageLength = l + 4 in
  let numberReturned = reply_numberReturned (b,s,l) in
  let rec aux i pos =
    if i >= numberReturned || messageLength - pos < 4
    then None
    else 
      let size = geti32 b pos in
      if messageLength - pos < size
      then None
      else
        if i = n
        then Some (pos, size)
        else aux (i+1) (pos+size)
  in
  aux 0 (s+32)

let string_of_message_buf msg =
  (string_of_MsgHeader msg)^
  (match header_opCode msg with
   | c when c = _OP_REPLY        -> string_of_reply msg
   | c when c = _OP_MSG          -> string_of_msg msg
   | c when c = _OP_UPDATE       -> string_of_update msg
   | c when c = _OP_INSERT       -> string_of_insert msg
   | c when c = _RESERVED        -> "  reserved"
   | c when c = _OP_QUERY        -> string_of_query msg
   | c when c = _OP_GET_MORE     -> string_of_get_more msg
   | c when c = _OP_DELETE       -> string_of_delete msg
   | c when c = _OP_KILL_CURSORS -> string_of_kill_cursors msg
   | c -> Printf.sprintf "  unknown (%d)" c)

let string_of_message_str str = string_of_message_buf (Buf.of_string str)

let string_of_message_reply (b,s,l) =
  let buf = Buf.create (l+4) in
  Stuff.add_le_int32 buf (l+4);
  Buf.add_string buf (Buf.sub b s l);
  string_of_message_buf buf

(*
(* Test code *)

let dump ?(base=10) s =
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
  Buffer.contents bb;;

let rid = 0x44495152l;;
let flags = 1195461702;;

let b = Bson.Append.init ();;
let () = Bson.Append.oid b "_id" "OIDOIDOIDOID";;
let () = Bson.Append.string b "name" "Joe";;
let () = Bson.Append.int b "age" 33;;
let () = Bson.Append.finish b;;

(*let m1 = create 100;;
let () = insert m1 rid flags "tutorial.persons" [b];;
let () = print_string (dump (get m1));;*)

let m2 = create 100;;
let () = start_insert m2 rid flags "tutorial.persons";;
let () = bson_init m2;;
let () = Bson.Append.oid m2 "_id" "OIDOIDOIDOID";;
let () = Bson.Append.string m2 "name" "Joe";;
let () = Bson.Append.int m2 "age" 33;;
let () = bson_finish m2;;
let () = finish m2;;
let () = print_string (dump (get m2));;
let good = (get m1) = (get m2);;

let reply_str = "\005\001\000\000\157\229\020\141\1554]F\001\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000-\000\000\000\007_id\000Ni\219z\236\174\136e\000\000\000\001\002name\000\004\000\000\000Joe\000\016age\000\"\000\000\000\000-\000\000\000\007_id\000Nj\024}T\145\2385\000\000\000\001\002name\000\004\000\000\000Joe\000\016age\000!\000\000\000\000-\000\000\000\007_id\000Nj\028\007\193K\175\012\000\000\000\001\002name\000\004\000\000\000Joe\000\016age\000!\000\000\000\000-\000\000\000\007_id\000Nj!\006\249\179\222P\000\000\000\001\002name\000\004\000\000\000Joe\000\016age\000!\000\000\000\000-\000\000\000\007_id\000Nj!H\020\003\186#\000\000\000\001\002name\000\004\000\000\000Joe\000\016age\000!\000\000\000\000";;
let reply_buf = Buf.create (String.length reply_str);;
Buf.add_string reply_buf reply_str;;
let reply = (reply_buf,4,String.length reply_str - 4);;
let messageLength = reply_messageLength reply;;
let requestId = reply_requestId reply;;
let responseTo = reply_responseTo reply;;
let opCode = reply_opCode reply;;
let responseFlags = reply_responseFlags reply;;
let cursorID = reply_cursorID reply;;
let startingFrom = reply_startingFrom reply;;
let numberReturned = reply_numberReturned reply;;
let Some (pos, size) = reply_document_pos reply 0;;
let doc1 = reply_document_pos reply 1;;
let doc2 = reply_document_pos reply 2;;
let doc3 = reply_document_pos reply 3;;
let doc4 = reply_document_pos reply 4;;

let () = Printf.printf "reply=\n%s%!\n" (string_of_message reply_buf);;
(*let () = Printf.printf "doc0_str = %s\n" (Print.to_pretty_raw (String.sub reply_str 36 45) 0);;*)
let reply_str2 = "V\000\000\000\x0e\xf0\xa8\xa7\xff\xff\xff\xff\xd4\007\000\000\000\000\000\000db.collection2\000\000\000\000\000\000\000\000\000+\000\000\000\003query\000\005\000\000\000\000\003$orderby\000\x10\000\000\000\001a\000\000\000\000\000\000\000\xf0?\000\000";;
let () = Printf.printf "query=\n%s%!\n" (string_of_message_str reply_str2);;
let () = Printf.printf "reply=\n%s%!\n" (string_of_message_reply reply);;

*)

