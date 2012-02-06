
(* OP codes *)
val _OP_REPLY : int
val _OP_MSG : int
val _OP_UPDATE : int
val _OP_INSERT : int
val _RESERVED : int
val _OP_QUERY : int
val _OP_GET_MORE : int
val _OP_DELETE : int
val _OP_KILL_CURSORS : int

(* OP_INSERT *)
val _ContinueOnError : int

(* OP_UPDATE *)
val _Upsert : int
val _MultiUpdate : int

(* OP_QUERY *)
val _TailableCursor : int
val _SlaveOk : int
val _OplogReplay : int
val _NoCursorTimeout : int
val _AwaitData : int
val _Exhaust : int
val _Partial : int

(* OP_DELETE *)
val _SingleRemove : int

(* OP_REPLY *)
val _CursorNotFound : int
val _QueryFailure : int
val _ShardConfigStale : int
val _AwaitCapable : int

type mongo_buf = Bson.buf

val add_bson : mongo_buf -> Bson.buf -> unit
val get : mongo_buf -> string
val export : mongo_buf -> string * int
val import : string -> mongo_buf
val copy : mongo_buf -> mongo_buf
val concat : mongo_buf -> mongo_buf -> mongo_buf
val append : mongo_buf -> mongo_buf -> unit
val length : mongo_buf -> int
val clip : mongo_buf -> int -> unit
val set_header_len : mongo_buf -> int -> unit
val set_header : mongo_buf -> int32 -> int -> int -> unit
val get_buf : ?hint:int -> unit -> Buf.t
val free_buf : Buf.t -> unit
val create : int -> mongo_buf
val init : ?hint:int -> int -> int32 -> int -> int -> mongo_buf
val clear : mongo_buf -> unit
val reset : mongo_buf -> unit
val free : mongo_buf -> unit
val start_insert : mongo_buf -> int32 -> int -> string -> unit
val start_update : mongo_buf -> int32 -> int -> string -> unit
val start_query : mongo_buf -> int32 -> int -> string -> int -> int -> unit
val set_query_flags : mongo_buf -> int -> unit
val get_opCode : mongo_buf -> int
val start_getmore : mongo_buf -> int32 -> string -> int -> int64 -> unit
val start_delete : mongo_buf -> int32 -> int -> string -> unit
val start_kill_cursors : mongo_buf -> int32 -> int64 list -> unit
val start_msg : mongo_buf -> int32 -> string -> unit
val bson_init : mongo_buf -> unit
val bson_finish : mongo_buf -> unit
val finish : mongo_buf -> unit

val reply_messageLength : (Buf.buf * int * int) -> int
val reply_requestId : (Buf.buf * int * int) -> int
val reply_responseTo : (Buf.buf * int * int) -> int
val reply_opCode : (Buf.buf * int * int) -> int
val reply_responseFlags : (Buf.buf * int * int) -> int
val reply_cursorID : (Buf.buf * int * int) -> int64
val reply_startingFrom : (Buf.buf * int * int) -> int
val reply_numberReturned : (Buf.buf * int * int) -> int
val reply_document_pos : (Buf.buf * int * int) -> int -> (int * int) option

val string_of_message_buf : Buf.buf -> string
val string_of_message_str : string -> string
val string_of_message_reply : Buf.buf * int * int -> string

val mongo_buf_requestId : mongo_buf -> int
val mongo_buf_refresh_requestId : mongo_buf -> int32 -> unit
val mongo_buf_responseTo : mongo_buf -> int
