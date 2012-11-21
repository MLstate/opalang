(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
(*
    @author Raja Boujbel
    @author Louis Gesbert (review, rewrite to add channel specs with specialised (de)serialisers)
**)

module IM = IntMap
module H = Hashtbl
module String = BaseString
module List = BaseList
module NA = NetAddr
open Cps.Ops

 (* FIXME, this is too generic; HLnet should be split into protocols *)
let protocol = NA.mk_protocol "HLnet"

(* ------------------------------------------------------------ *)
(* Utility functions & constants                                *)
(* ------------------------------------------------------------ *)

let protocol_version = 1

let gc_finalise _sched = Scheduler.finalise _sched

(*let apply_fun_option opt =
  match opt with
  | Some f -> f
  | None -> fun _ -> ()*)


(* ------------------------------------------------------------ *)
(* Type definitions                                             *)
(* ------------------------------------------------------------ *)

type 'a cps = ('a -> unit) -> unit

type 'a errcps = (exn -> unit) -> ('a -> unit) -> unit

type 'a stream_unserialise = string -> int -> [ `data of 'a * int | `needmore of int | `failure of string ]

type endpoint =
  | Tcp of Unix.inet_addr * int
  | Ssl of Unix.inet_addr * int * SslAS.secure_type option
  (* | Udp of Unix.inet_addr * int *)

exception Hlnet_ssl
exception Disconnected of endpoint

type channel_id = int

type service_id = { name: string; version: int; }

(* Used for dummy instanciation of the ['out'] and ['in'] types in channels,
   which are abstracted in most of the code here (only before serialisation, and
   after deserialisation in the user code do we need to be careful about
   them) *)
type black

module rec Types : sig
  (* don't look at this, it's a copy-paste of the definitions below (ocaml needs
     an explicit sig for functors and here the module only contains type
     definitions *)
  type ('out','in') channel_spec = { service: service_id; out_serialise: 'out' -> string; in_unserialise: ('out','in') channel -> 'in' stream_unserialise; } and ('out','in') channel = { id: channel_id; spec: ('out','in') channel_spec; connection: connection; mutable handler : ('in' -> ('out' -> unit) -> unit) option; mutable waiting_handler : ((exn -> unit) * ('in' -> unit)) IM.t; mutable pending: 'in' list IM.t ; on_disconnect: ((bool -> unit) -> unit) ref; mutable propagate_removal: bool ref option } and black_channel = (black,black) channel and connection = { remote: endpoint; mutable local: endpoint; scheduler: Scheduler.t; mutable info: Scheduler.connection_info option Cps.Lazy.t; channels: Wchannels.t; mutable num_listeners: int; mutable finalised: bool; }
  val _define_at_least_one_value_in_the_module_to_avoid_ocaml_bug_: unit
end = struct
  type ('out','in') channel_spec = {
    service: service_id;
    out_serialise: 'out' -> string;
    in_unserialise: ('out','in') channel -> 'in' stream_unserialise;
    (** the de-serialisation function to get back received values *)
  }

  and ('out','in') channel = {
    id: channel_id;
    spec: ('out','in') channel_spec;
    connection: connection;

    (* Generic handler for received requests (setup_respond) *)
    mutable handler : ('in' -> ('out' -> unit) -> unit) option;

    (* Handler for specific request-ids (responses to sendreceive) *)
    mutable waiting_handler : ((exn -> unit) * ('in' -> unit)) IM.t;

    (* stored received messages, by request ids *)
    mutable pending: 'in' list IM.t ;

    (* Cps function to be run whenever the channel is disconnected. The bool
       passed to the continuation is [true] if the channel has been reconnected.
       (it's reinitialised every time before called, persistent handlers should
       re-push themselves. *)
    on_disconnect: ((bool -> unit) -> unit) ref;

    (* Indicates the finalisation status: None if no finaliser is set; otherwise,
       if set to true, the finalisation function should send a message to inform
       the other hand of the deletion of this channel *)
    mutable propagate_removal: bool ref option;
  }

  and black_channel = (black,black) channel

  and connection = {
    remote: endpoint; (* also used as key to recover connections *)
    mutable local: endpoint; (* real local endpoint may only be known once connected *)
    scheduler: Scheduler.t;
    mutable info: Scheduler.connection_info option Cps.Lazy.t;
    channels: Wchannels.t;
    (* each connection needs to know about all channels it hosts, to trigger
       their [on_disconnect] handler in case of problem *)
    mutable num_listeners: int;
    (* keep track of listeners, to detect when that list becomes empty *)
    mutable finalised: bool; (* used to finalise only once *)
  }

  (* Without this, it won't compile on windows with 3.12 (generates bad assembly) *)
  let _define_at_least_one_value_in_the_module_to_avoid_ocaml_bug_ = ()
end

(* weak channel set, by id *)
and Wchannels : sig
  include Weak.S with type data = Types.black_channel
  val get_opt: t -> channel_id -> Types.black_channel option
end = struct
  include Weak.Make
    (struct
       type t = Types.black_channel
       let equal c1 c2 = c1.Types.id = c2.Types.id
       let hash c = c.Types.id
     end)
  let dummy_chan = (* for lookup, we rewrite field [id] and the others never get accessed *)
    { Types. id = Obj.magic(); spec = Obj.magic(); connection = Obj.magic();
      handler = None; waiting_handler = IM.empty;
      pending = IM.empty; on_disconnect = Obj.magic(); propagate_removal = None; }
  let get_opt t id = try Some (find t { dummy_chan with Types. id }) with Not_found -> None
end

include Types (* Defined in a sub-module because of mutual recursion with a functor instance *)

(* weak connection set, by remote *)
module Wconnections : sig
  include Weak.S with type data = connection
  val get_opt: t -> endpoint -> connection option (** get connetion by remote *)
end = struct
  include Weak.Make
    (struct
       type t = connection
       let equal c1 c2 = c1.remote = c2.remote
       let hash c = Hashtbl.hash c.remote
     end)
  let dummy_connection = (* for lookup, we rewrite field [remote] and the others never get accessed *)
    { remote = Obj.magic(); local = Obj.magic(); scheduler = Obj.magic();
      info = Obj.magic(); channels = Obj.magic(); num_listeners = 0;
      finalised = false; }
  let get_opt t remote = try Some (find t { dummy_connection with remote }) with Not_found -> None
end

(* Used when unserialising, until we lookup the tables to rebuild the whole channel *)
type pre_channel = {
  pc_cid: channel_id;
  pc_remote_endpoint: endpoint;
  pc_service_name: string;
  pc_service_version: int;
}

(* Type stored in the EPH table, that is used to dispatch channel handling from
   incoming connections *)
type ('out', 'in') connection_handler = {
  chan_handlers : (service_id, ('out','in') channel_spec * (('out', 'in') channel -> unit)) Hashtbl.t;
  abort_listen : (unit -> unit) (* Scheduler key used to abort a listen *)
}


(* -- Request tracking -- *)

module RequestId =
struct
  type id = int

  let dummy_request_id = max_int

  let is_dummy req = req = dummy_request_id

  let fresh_request_id =
    Base.Random.ensure_init ();
    let r = ref (Base.Random.max_int ()) in
    fun () ->
      let id = !r in
      r := if id == min_int then max_int else pred id;
      id
end


(* -- Reading loop -- *)

type reading_loop_status =
  | Ready of string (* data already available *)
  | Waiting_for_more of string * int (* receiving buffer (only partially full), how much I already got *)
  | Inconsistent
      (* We received something we don't understand; we don't even know how long it should be, so the only option is to close the connection *)


(* -- Protocol definition interface -- *)

type ('query,'response) protocol = {
  client_spec: ('query,'response) channel_spec;
  server_spec: ('response,'query) channel_spec;
}



(* ------------------------------------------------------------ *)
(* Debugging & logging                                          *)
(* ------------------------------------------------------------ *)

#<Debugvar:HLNET_DEBUG>

(* -- generic -- *)

let debug fmt =
  #<If> Printf.fprintf stderr ("[32m[hlnet][0m "^^fmt^^"\n%!")
  #<Else> Printf.ifprintf stderr fmt
  #<End>

let warning fmt = Logger.warning ("[hlnet] " ^^ fmt)

let hexprint ?(chars_per_line=32) s =
  let pfx0 = "[22m" and pfx1 = "[01m" in
  let pfxlen = String.length pfx0 in
  let sfx = "[0m" in
  let sfxlen = String.length sfx in
  let len = String.length s in
  let buf = String.create (6 * len + ((len - 1) / chars_per_line + 1) * 2 * pfxlen + sfxlen) in
  for i = 0 to len - 1 do
    let c = s.[i] in
    let line_len = min chars_per_line (len - (i / chars_per_line) * chars_per_line) in
    let pad = if i mod chars_per_line = line_len - 1 then '\n' else ' ' in
    let start = (i / chars_per_line) * (chars_per_line * 3 + pfxlen) * 2 + (i mod chars_per_line) * 3 in
    let offset = start in
    if i mod chars_per_line = 0 then String.blit pfx0 0 buf offset pfxlen;
    let offset = offset + pfxlen in
    String.blit (Printf.sprintf "%02x" (int_of_char c)) 0 buf offset 2;
    let offset = offset + 2 in
    buf.[offset] <- if pad = ' ' && i mod 8 = 7 then '.' else pad;
    let offset = start + pfxlen + line_len * 3 in
    if i mod chars_per_line = 0 then String.blit pfx1 0 buf offset pfxlen;
    let offset = offset + pfxlen in
    buf.[offset] <- ' ';
    let offset = offset + 1 in
    buf.[offset] <- if ' ' <= c && c <= '~' then c else ' ';
    let offset = offset + 1 in
    buf.[offset] <- pad;
  done;
  String.blit sfx 0 buf (String.length buf - sfxlen) sfxlen;
  buf

(*let int64_to_debug_string ld =
  let color = 31 + Int64.to_int (Int64.rem ld (Int64.of_int 6)) in
  Printf.sprintf "[%dm%016Lx[39m" color ld*)

let int_to_debug_string i =
  let color = 31 + i mod 6 in
  Printf.sprintf "[%dm%016x[39m" color i

(*let print_marshalled str beg =
  let s = ref "[35m" in
  for i = beg to String.length str -1 do
    let topr =
      let code = Char.code str.[i] in
      if code < 32 || code > 126 then
        string_of_int code
      else Char.escaped str.[i] in
    s := !s ^ "-" ^ topr
  done;
  !s^"[0m"*)

(*let string_of_htable keystr valstr ht =
  Format.fprintf Format.str_formatter "(%d) - %a"
    (H.length ht)
    (fun f -> H.iter (fun k v -> Format.fprintf f "{ %s+%s }|" (keystr k) (valstr v))) ht;
  Format.flush_str_formatter ()*)

(* -- Type specific -- *)

let endpoint_to_string =
  #<If:TESTING> function (* don't show random output ports when testing *)
    | Tcp (addr, _port) ->
        Printf.sprintf "tcp://%s" (Unix.string_of_inet_addr addr)
    | Ssl (addr, _port, _) ->
        Printf.sprintf "ssl://%s" (Unix.string_of_inet_addr addr)
    (* | Udp (addr, _port) -> *)
    (*   Printf.sprintf "udp://%s" (Unix.string_of_inet_addr addr) *)
  #<Else> function
    | Tcp (addr, port) ->
        Printf.sprintf "tcp://%s:%d" (Unix.string_of_inet_addr addr) port
    | Ssl (addr, port, _) ->
        Printf.sprintf "ssl://%s:%d" (Unix.string_of_inet_addr addr) port
    (* | Udp (addr, port) -> *)
    (*     Printf.sprintf "udp://%s:%d" (Unix.string_of_inet_addr addr) port *)
  #<End>

let connection_to_string conn =
  Printf.sprintf "(%s -> %s, [1m%s[22m)"
    (endpoint_to_string conn.local) (endpoint_to_string conn.remote)
    (match Cps.Lazy.get_state conn.info with
     | None -> "PENDING"
     | Some None -> "DISCONNECTED"
     | Some (Some info) ->
         let fd = NA.get_fd (Scheduler.get_connection_addr info) in
         Printf.sprintf "%s (fd %d)"
           (if Scheduler.check_connection conn.scheduler info then "UP" else "DOWN")
           (Obj.magic fd))

let channel_id_to_debug_string id =
  (int_to_debug_string id)

let print_service_id { name; version } =
  Printf.sprintf "%s, v.%d" name version

let channel_to_string channel =
  String.concat ""
    [ "service: "; channel.spec.service.name; ", version: "; string_of_int channel.spec.service.version; "|";
      "endpoint: "; (endpoint_to_string channel.connection.remote); "|";
      "id: "; channel_id_to_debug_string channel.id; "|";
      "pending: ("; string_of_int (IM.size channel.pending); ") "; List.print string_of_int (IM.keys channel.pending); "|";
      "handler: "; if Option.is_some channel.handler then "SET" else "UNSET"; "|";
      "waiting_handler: ("; string_of_int (IM.size channel.waiting_handler); ") "; List.print string_of_int (IM.keys channel.waiting_handler); "|";
    ]


let request_id_to_debug_string x = Printf.sprintf "[7m%s[27m" (int_to_debug_string x)


(* ------------------------------------------------------------ *)
(* Auxiliary functions on types                                 *)
(* ------------------------------------------------------------ *)

(* -- Endpoints -- *)

(* Clean ssl endpoint (to be able to found the connection) *)
let clean_ssl_endpoint = function
  | Ssl (a,p,_) -> Ssl (a,p,None)
  | ep -> ep

(* -- Connections -- *)

(* Returns the endpoint of given connection *)
let get_remote_endpoint conn = conn.remote

let get_local_endpoint conn = conn.local

let get_connection_specification conn_info get =
  let addr = Scheduler.get_connection_addr conn_info in
  let fd = NA.get_fd addr in
  match get fd with
  | Unix.ADDR_INET (addr,port) -> addr, port
  | _ -> assert false

let local_of_conn_info conn_info =
  let addr, port = get_connection_specification conn_info Unix.getsockname in
  match NA.get_type (Scheduler.get_connection_addr conn_info) with
  | NA.TCP -> Tcp (addr, port)
  | NA.SSL -> Ssl (addr, port, None)
  | _ -> assert false

let remote_of_conn_info conn_info =
  let addr, port = get_connection_specification conn_info Unix.getpeername in
  match NA.get_type (Scheduler.get_connection_addr conn_info) with
  | NA.TCP -> Tcp (addr, port)
  | NA.SSL -> Ssl (addr, port, None)
  | _ -> assert false

(* -- Channels and services -- *)

let fresh_channel_id =
  Base.Random.ensure_init();
  let r = ref (Base.Random.max_int()) in
  fun () ->
    let x = !r in
    r := (if x == max_int then min_int else succ x);
    x

let make_service_id ~name ~version =
  if version < 0 || version > 9999 then raise (Invalid_argument "make_service_id");
  let name =
    if String.length name <= 12 then name else
    let h = Hashtbl.hash name in
    Base.String.init 8 (fun b -> char_of_int ((h lsr (7-b)) land 0xFF)) ^
    "/" ^ String.sub name (String.length name - 3) 3
  in
  { name = name; version = version }


let remote_of_channel chan = chan.connection.remote

let local_of_channel chan = chan.connection.local

let scheduler_of_channel chan = chan.connection.scheduler

let channel_to_black : ('out','in') channel -> black_channel = Obj.magic
let black_to_channel : black_channel -> ('out','in') channel = Obj.magic

let is_channel_listening ch =
  ch.handler <> None || not (IM.is_empty ch.waiting_handler)

(* ------------------------------------------------------------ *)
(* Serialisation / deserialisation                              *)
(* ------------------------------------------------------------ *)

(*let write_int64 buf offset i =
  let (>>) = Int64.shift_right_logical in
  for byte = 0 to 7 do
    buf.[offset + byte] <- Char.chr (Int64.to_int (i >> (8* (7 - byte))) land 0xFF)
  done*)

(*let read_int64 buf offset =
  let (<<) = Int64.shift_left in
  let rec aux offset value byte =
    if byte >= 8 then value else
      aux offset (Int64.logor value (Int64.of_int (Char.code buf.[offset+byte]) << (8 * (7 - byte)))) (byte+1)
  in
  aux offset Int64.zero 0*)

let write_int buf offset i =
  for byte = 0 to 7 do
    buf.[offset + byte] <- Char.chr ((i lsr (8* (7 - byte))) land 0xFF)
  done

let read_int buf offset =
  let rec aux offset value byte =
    if byte >= 8 then value else
      aux offset (value lor (Char.code buf.[offset+byte] lsl (8 * (7 - byte)))) (byte+1)
  in
  aux offset 0 0


let serialised_endpoint_size = 8
let serialise_endpoint ep =
  let buf = String.make serialised_endpoint_size '\000' in
  let addr, port =
    match ep with
    | Tcp (addr,port) -> (* ['T'|0|address(4bytes)|port(2bytes)] *)
        buf.[0] <- 'T';
        addr, port
    | Ssl (addr,port,_) -> (* ['S'|0|address(4bytes)|port(2bytes)] *)
        buf.[0] <- 'S';
        addr, port
  in
  (* only for tcp & ssl *)
  let saddr = (Obj.magic addr : string) in
  (* ocaml provides no conversion, but it's actually "\127\000\000\001" internally *)
  String.blit saddr 0 buf 2 4;
  assert (0 <= port && port <= 0xFFFF);
  buf.[6] <- Char.chr ((port lsr 8) land 0xFF );
  buf.[7] <- Char.chr (port land 0xFF);
  buf
  (* end tcp & ssl *)

  (* | Udp (addr,port) -> (\* ['U'|0|address(4bytes)|port(2bytes)] *\) *)
  (*     buf.[0] <- 'U'; *)
  (*     let saddr = (Obj.magic addr : string) in *)
  (*     (\* ocaml provides no conversion, but it's actually "\127\000\000\001" internally *\) *)
  (*     String.blit saddr 0 buf 2 4; *)
  (*     assert (0 <= port && port <= 0xFFFF); *)
  (*     buf.[6] <- Char.chr ((port lsr 8) land 0xFF ); *)
  (*     buf.[7] <- Char.chr (port land 0xFF); *)
  (*     buf *)

let unserialise_endpoint ?(offset=0) s =
  let offset0 = offset in
  if String.length s - offset < serialised_endpoint_size then raise (Invalid_argument "unserialise_endpoint");
  let cons a p = 
    match s.[offset0] with
    | 'T' -> Tcp (a,p)
    | 'S' -> Ssl (a,p,None)
    | _ ->
      raise (Invalid_argument "unserialise_endpoint")
  in
  (* only for tcp & ssl *)
  let offset = offset + 2 in
  let saddr = String.sub s offset 4 in
  let addr = (Obj.magic saddr : Unix.inet_addr) in (* see comment in "serialise" *)
  let offset = offset + 4 in
  let port = (Char.code s.[offset] lsl 8) lor Char.code s.[offset + 1] in
  let offset = offset + 2 in
  assert (offset0 + serialised_endpoint_size = offset);
  cons addr port
  (* end tcp & ssl *)
  
  (* | 'U' -> *)
  (*     let offset = offset + 2 in *)
  (*     let saddr = String.sub s offset 4 in *)
  (*     let addr = (Obj.magic saddr : Unix.inet_addr) in (\* see comment in "serialise" *\) *)
  (*     let offset = offset + 4 in *)
  (*     let port = (Char.code s.[offset] lsl 8) lor Char.code s.[offset + 1] in *)
  (*     let offset = offset + 2 in *)
  (*     assert (offset0 + serialised_endpoint_size = offset); *)
  (*     Udp (addr, port) *)

let serialised_channel_size =
  8 (* numeric channel id (an int64) *)
  + serialised_endpoint_size
  + 16 (* length of serialised service: 12 for name and 4 for version *)

let serialise_channel_with_endpoint chan endpoint =
  (* format:
     [  channel_id(8bytes)  |            local_endpoint(8bytes)                 ]
     [service_name_padded_with_\000(12bytes)|stringified_service_version(4bytes)]
  *)
  let service_name = chan.spec.service.name in
  let service_version = Printf.sprintf "%04d" chan.spec.service.version in
  let chan_id = chan.id
  in
  let buf = String.make serialised_channel_size '\000' in
  let offset = 0 in
  write_int buf offset chan_id;
  let offset = offset + 8 in
  String.blit (serialise_endpoint endpoint) 0 buf 8 serialised_endpoint_size;
  let offset = offset + serialised_endpoint_size in
  assert (String.length service_name <= 12);
  String.blit service_name 0 buf offset (String.length service_name);
  let offset = offset + 12 in
  assert (String.length service_version = 4);
  String.blit service_version 0 buf offset 4;
  let offset = offset + 4 in
  assert (offset = serialised_channel_size);
  buf

let serialise_channel chan =
  serialise_channel_with_endpoint chan chan.connection.local

(* This first pass doesn't do any registering in the tables for channel
   handling, and returns a simplified structure. The internal function
   used to rebuild a real channel is [first_channel_treatment], and you
   should see the [_secondpass] function for the user binding *)
let unserialise_remote_channel_firstpass ?(offset=0) s =
  if String.length s - offset < serialised_channel_size then
    raise (Invalid_argument (Printf.sprintf "unserialise_remote_channel: wrong message length (%d/%d)" (String.length s - offset) serialised_channel_size));
  let offset0 = offset in
  let pc_cid = read_int s offset in
  let offset = offset + 8 in
  let pc_remote_endpoint = unserialise_endpoint ~offset s in
  let offset = offset + serialised_endpoint_size in
  let pc_service_name = Base.String.rtrim ~is_space:(fun c -> c = '\000') (String.sub s offset 12) in
  let offset = offset + 12 in
  let pc_service_version = try int_of_string (String.sub s offset 4) with
    | Failure "int_of_string" -> raise (Invalid_argument "unserialise_remote_channel: invalid header")
  in
  let offset = offset + 4 in
  assert (offset = offset0 + serialised_channel_size);
  { pc_cid; pc_remote_endpoint; pc_service_name; pc_service_version }


(* -- (De)Serialisation of the messages in the HLnet protocol -- *)

module Serialise :
sig
  type operation =
    | Message of channel_id * RequestId.id
    | Delete of channel_id
    | Channel of pre_channel
    | Reset
  val unserialise_operation : endpoint -> operation stream_unserialise
  val serialise_message : RequestId.id -> ('out','in') channel -> 'out' -> string
  val serialise_delchan: channel_id -> string
  val serialise_channel: black_channel -> endpoint (* local *) -> string
  val reset_message : string
end =
struct

  type operation =
    | Message of channel_id * RequestId.id
    | Delete of channel_id
    | Channel of pre_channel
    | Reset

  (** Format specification for a message :
      - (1c) char M, the "shebang" for a message
      - (8c) one int64, the id of the channel
      - (8c) one int64, the id of the request
      - a string using the serialisation functions provided by the user in the channel_spec
  *)

  (*let message_shebang = 'M'*)
  let shebang_length = 1
  let message_header_length = shebang_length + 8 + 8

  (** Format specification for a channel
      - (1c) char S, the "shebang" for a channel
      - (1c) unused ('\000')
      - (2c) the HLnet protocol version
      - a serialised channel (using the functions above)
  *)
  (* A channel is always the first message sent to open a dialog: therefore,
     it's here that we insert HLnet's version number & check it (so that it's
     not done in every subsequent message) *)
  let channel_shebang = 'S'
  let serialised_protocol_version =
    assert (0 <= protocol_version && protocol_version < 0xFFFF);
    let s = String.create 2 in
    s.[0] <- Char.chr (protocol_version lsr 8);
    s.[1] <- Char.chr (protocol_version land 0xFF);
    s
  let channel_prefix =
    let s = String.make 4 '\000' in
    s.[0] <- channel_shebang;
    String.blit serialised_protocol_version 0 s 2 2;
    s
  let protocol_version_of_channel_prefix s = (Char.code s.[2] lsl 8) lor (Char.code s.[3])
  let channel_prefix_size = 4
  let channel_size = channel_prefix_size + serialised_channel_size

  let serialise_channel chan local_endpoint =
    let buf = String.create channel_size in
    let offset = 0 in
    String.blit channel_prefix 0 buf offset channel_prefix_size;
    let offset = offset + channel_prefix_size in
    String.blit (serialise_channel_with_endpoint chan local_endpoint) 0 buf offset serialised_channel_size;
    let offset = offset + serialised_channel_size in
    assert (offset = channel_size);
    buf

  (** Format specification for deleting a channel
      - (1c) char D, the "shebang" for deleting a channel
      - (8c) one int64, the id of the channel
  *)
  let delchan_shebang = 'D'
  let delchan_length = shebang_length + 8

  let serialise_message request_id chan msg =
    let message = chan.spec.out_serialise msg in
    let message_length = String.length message in
    let buf = String.create (shebang_length + 8 + 8 + message_length) in
    let offset = 0 in
    buf.[offset] <- 'M';
    let offset = offset + 1 in
    write_int buf offset chan.id;
    let offset = offset + 8 in
    write_int buf offset request_id;
    let offset = offset + 8 in
    assert (offset = message_header_length);
    String.blit message 0 buf offset message_length;
    buf

  let unserialise_message_header ?(offset=0) msg =
    let offset0 = offset in
    let offset = offset + shebang_length in
    let chanid = read_int msg offset in
    let offset = offset + 8 in
    let reqid = read_int msg offset in
    let offset = offset + 8 in
    assert (offset = offset0 + message_header_length);
    (chanid, reqid)

  let serialise_delchan chanid =
    let buf = String.create delchan_length in
    let offset = 0 in
    buf.[offset] <- delchan_shebang;
    let offset = offset + 1 in
    write_int buf offset chanid;
    let offset = offset + 8 in
    assert (offset = delchan_length);
    buf

  let unserialise_delchan msg =
    read_int msg shebang_length

  let reset_message = "R"

  let unserialise_operation remote_endpoint msg offset =
    assert (offset=0); (* only called that way, but would be a nice optimisation *)
    let len = String.length msg in
    assert (len > 0);
    match msg.[0] with
    | 'S' -> (* channel creation *)
        if len < channel_size then `needmore (offset + channel_size)
        else if Base.String.is_prefix channel_prefix msg then
          try
            let pre_channel = unserialise_remote_channel_firstpass ~offset:channel_prefix_size msg in
            let pre_channel = { pre_channel with pc_remote_endpoint = remote_endpoint } in
            `data (Channel pre_channel, offset + channel_size)
          with Invalid_argument _ as _exc->
            #<If> Printf.eprintf "at unserialise of operation: %s\n%s\n" (Printexc.to_string _exc) (Printexc.get_backtrace ()) #<End>;
            `failure "Invalid channel creation message"
        else
           `failure
             (Printf.sprintf "Ignoring message from a different HLnet version: %d (current exe uses %d)"
                (protocol_version_of_channel_prefix msg) protocol_version)
    | 'D' -> (* channel deletion *)
        if len < delchan_length then `needmore (offset + delchan_length)
        else
          let channel_id = unserialise_delchan msg in
          `data (Delete (channel_id), offset + delchan_length)
    | 'M' -> (* message within an open channel *)
        if len < message_header_length then `needmore (offset + message_header_length)
        else
          let channel_id,request_id = unserialise_message_header msg in
          `data (Message (channel_id, request_id), offset + message_header_length)
    | 'R' -> (* reset *)
        `data (Reset, offset + 1)
    | _ ->
        `failure "Invalid message"

end



(* ------------------------------------------------------------ *)
(* Connection-handling engine                                   *)
(* ------------------------------------------------------------ *)

module Connection : sig
  (** Returns a registered connection to the given [endpoint]. Connects if
      needed. Calls err_cont if the connection couldn't be made. Simultaneous
      attempts to connect to the same remote are safe. *)
  val get: ?ssl:SslAS.secure_type -> Scheduler.t -> endpoint -> connection

  (** Same as [get], but does not attempt to connect, just return the connection
      if existing *)
  (*val find: endpoint -> connection option*)

  (** Registers an already open low-level connection (e.g. returned by
      [accept]). Warning, the connection will be closed if you let the returned
      value be garbage collected. *)
  val register: Scheduler.t -> Scheduler.connection_info -> connection

  (** Closes and unregisters a connection (and then calls the [on_disconnect]
      handlers of hosted channels, which may attempt to reconnect) *)
  val disconnect: connection -> unit

  (** Register a channel to the connection it uses, so that it can be
      notified in clase of disconnection *)
  val register_channel: ('out','in') channel -> unit

  (** Returns the channel with given id if it is registered to the given
      connection (and either alive, or not GCed yet) *)
  val get_channel: connection -> channel_id -> black_channel option

  (** Returns true if the connection hosts any channel expecting to receive a
      message. You probably also want to check if we accept incoming channels
      on the local endpoint, see module [EP] below ; *)
  val is_listening: connection -> bool


  (* -- Operation on connections -- *)

  val write: connection -> string -> failure:(exn -> unit) -> success:(int -> unit) -> unit

  val read: connection -> failure:(exn -> unit) -> success:(int * string -> unit) -> unit

  (* -- Debug -- *)

  val dump: unit -> string

  (*val live_channels: unit -> int*)
end = struct

  let table = Wconnections.create 11
  let certificates = Hashtbl.create 11

  let disconnect conn =
    if conn.finalised then () else
    let _ = conn.finalised <- true in
    let channels = conn.channels in
    #<If$minlevel 12> debug "Closing connection to %s" (endpoint_to_string conn.remote) #<End>;
    let channels_handling () =
      Wchannels.iter
        (fun ch ->
           (* Abort all expected answers *)
           IM.iter
             (fun _ (errcont,_) ->
                Scheduler.push (conn.scheduler) (fun () -> errcont (Disconnected conn.remote))
             )
             ch.waiting_handler;
           (* Reset and call the disconnection handlers *)
           let on_disconnect = ch.on_disconnect in
           let f = !on_disconnect in
           on_disconnect := (fun cont -> false |> cont);
           f @> fun _reconnected -> ())
        channels
    in
    match Cps.Lazy.get_state conn.info with
    | Some (Some info) when Scheduler.check_connection conn.scheduler info ->
        let cont _ =
          Scheduler.remove_connection conn.scheduler info;
          conn.info <- Cps.Lazy.lazy_from_val None;
          channels_handling ()
        in
        Scheduler.write conn.scheduler info "" (* make sure everything has been flushed before closing *)
          ~err_cont:cont cont;
    | _ ->
        conn.info <- Cps.Lazy.lazy_from_val None;
        channels_handling()

  let register sched connection_info =
    let endpoint = remote_of_conn_info connection_info in
    let connection = {
      local = local_of_conn_info connection_info;
      remote = endpoint;
      scheduler = sched;
      info = Cps.Lazy.lazy_from_val (Some connection_info);
      channels = Wchannels.create 17;
      num_listeners = 0;
      finalised = false;
    } in
    gc_finalise sched disconnect connection;
    assert (not (Wconnections.mem table connection));
    Wconnections.merge table connection

  let get ?ssl sched remote =
    let connection_opt = Wconnections.get_opt table remote in
    (* The use of Cps.Lazy is so that we don't open several connections if
       several channels have been opened just at once to the same remote. *)
    match connection_opt with
    | Some connection when Cps.Lazy.get_state connection.info <> Some None ->
        (* we are either connected or connecting *)
        connection
    | _ ->
        (* we are either unconnected, or disconnected *)
        let addr, port, encryption, local =
          match remote with
          | Tcp (addr, port) -> addr, port, Network.Unsecured, Tcp (Unix.inet_addr_any, 0)
          | Ssl (addr, port, None) ->
              let sec =
                match ssl with
                | Some sec ->
                    (try
                       let found = Hashtbl.find certificates remote in
                       if sec <> found then
                         warning "Want to replace the certificate ??";
                     with Not_found -> Hashtbl.add certificates remote sec);
                    sec
                | None -> Hashtbl.find certificates remote in
              addr, port, Network.Secured sec, Ssl (Unix.inet_addr_any, 0, None)
          | _ -> assert false
        in
        match connection_opt with
        | Some connection -> (* the structure exists, but is disconnected *)
            let reconnect cont =
              Logger.info "Reconnecting to %s" (endpoint_to_string remote);
              Network.connect sched (Network.make_port_spec ~protocol addr port) encryption
                ~socket_flags:[Unix.SO_KEEPALIVE]
                ~err_cont:(
                  fun _ ->
                    Logger.info "Reconnection to %s failed" (endpoint_to_string remote);
                    disconnect connection; None |> cont
                )
              @> fun connection_info ->
                Logger.info "Reconnected to %s" (endpoint_to_string remote);
                connection.local <- (local_of_conn_info connection_info);
                connection.finalised <- false;
                Some connection_info |> cont
            in
            connection.info <- Cps.Lazy.make (Scheduler.push sched) reconnect;
            connection
        | None -> (* we have yet to connect *)
            let update_local_ref = ref (fun _ -> assert false) in
            let disconnect_ref = ref (fun _ -> assert false) in
            let connect cont =
              #<If$minlevel 10> debug "Connecting to %s" (endpoint_to_string remote) #<End>;
              Network.connect sched (Network.make_port_spec ~protocol addr port) encryption
                ~socket_flags:[Unix.SO_KEEPALIVE]
                ~err_cont:(fun _ -> !disconnect_ref (); None |> cont)
              @> fun connection_info ->
                Logger.info "Connected to %s" (endpoint_to_string remote);
                !update_local_ref (local_of_conn_info connection_info);
                Some connection_info |> cont
            in
            let connection = {
              local = local; (* to be initialised below *)
              remote = remote;
              scheduler = sched;
              info = Cps.Lazy.make (Scheduler.push sched) connect;
              channels = Wchannels.create 17;
              num_listeners = 0;
              finalised = false;
            } in
            update_local_ref := (fun local -> connection.local <- local);
            disconnect_ref := (fun () -> disconnect connection);
            Wconnections.add table connection;
            gc_finalise sched disconnect connection;
            connection

  (*let find = Wconnections.get_opt table*)

  let register_channel channel =
    let channel = channel_to_black channel in
    Wchannels.add channel.connection.channels channel

  let get_channel connection id =
    Wchannels.get_opt connection.channels id

  let is_listening connection = connection.num_listeners > 0

  let write connection message ~failure ~success =
    Cps.Lazy.force connection.info
    @> function
    | Some info ->
        Scheduler.write connection.scheduler info message
          ~err_cont:(fun e -> failure e; disconnect connection)
        @> success
    | None ->
        failure (Failure "Connection closed")

  let read connection ~failure ~success =
    Cps.Lazy.force connection.info
    @> function
    | Some info ->
        Scheduler.read connection.scheduler info
          ~err_cont:(fun e -> failure e; disconnect connection)
        @> success
    | None ->
        failure (Failure "Connection closed")

  let dump () =
    Wconnections.fold
      (fun c acc -> Printf.sprintf "%s%s\n" acc (connection_to_string c))
      table ""

  (*let live_channels () =
    Wconnections.fold
      (fun c acc -> acc + Wchannels.count c.channels)
      table 0*)
end


(* ------------------------------------------------------------ *)
(* Tables of stored handlers                                    *)
(* ------------------------------------------------------------ *)


(*
  Overview of the tables that are maintained inside hlnet:

  - [Connection]: connection ID (weak) -> connection
  - [EP]: endpoint -> (handlers = service -> channel handler)
  - [ChanH]: channel ID -> channel
  - in [reading_loop]: connection -> err_cont (only for the connections on which we're reading)

  References:

  - in each channel, to a connection ID

  GC mechanism:

  - Connection IDs are bound to a finaliser closing the connection
  - Channels are removed manually from ChanH if there is no handler
  left after a message was received for that channel
  - Whenever a channel is collected, it sends notice to the other end
  (unless the other hand already asked to close)
  - When all channels referring to a connection ID are closed, the
  connection can be collected and closed naturally

*)

(* -- Hashtable functor used in the tables below -- *)

module PolyHash (V :
  sig
    type key
    type ('out', 'in') value
    (*val key2str: key -> string*)
  end) :
sig
  (* This module provides semi-heterogeneous hashtables, needed to handle channels
     and connection-handlers that deal with different types (for example, a
     connection handler needs to know all channels registered to its connection,
     and those may receive different types).
     Values in a PolyHash have type [exists('out','in'). ('out','in') value]. Since
     the user can only access this data if he already got a (typed) channel, type-
     safety is regained before he gets his hands on the value. *)
  type t
  val create : unit -> t
  val add    : t -> V.key -> ('out','in') V.value -> unit
  val find   : t -> V.key -> ('out','in') V.value option
  val mem    : t -> V.key -> bool
  val remove : t -> V.key -> unit
  (*val update : t -> V.key -> (('out','in') V.value -> ('out','in') V.value) -> unit*)
  (*val fold   : t -> (V.key -> ('out','in') V.value -> 'acc -> 'acc) -> 'acc -> 'acc*)
  (*val to_string : t -> string*)
end = struct
  type t = (V.key, Obj.t) H.t

  let create () = H.create 5

  let add tbl k v =
    H.replace tbl k (Obj.repr v)

  let find tbl k =
    try Some (Obj.obj(H.find tbl  k ) : ('out','in') V.value)
    with Not_found -> None

  let mem = H.mem

  (*let to_string tbl =
    BaseList.print V.key2str (H.fold (fun k _v acc -> k::acc) tbl [])*)

  let remove tbl k =
    H.remove tbl k

  (*let update tbl k f =
    try
      let v = H.find tbl k in
      let new_v = Obj.repr (f ((Obj.obj v) : ('out','in') V.value)) in
      H.replace tbl k new_v
    with Not_found -> ()*)

  (*let fold (tbl : t) f acc =
    H.fold (fun k v acc -> f (k:V.key) ((Obj.obj v) : ('out','in') V.value) acc) tbl acc*)
end


(* -- Table of handlers for incoming connections -- *)

module EP : sig
  val add: endpoint -> ('out', 'in') connection_handler -> unit
  val find: endpoint -> ('out', 'in') connection_handler option
  (*val update: endpoint -> (('out', 'in') connection_handler -> ('out', 'in') connection_handler) -> unit*)
  val remove: endpoint -> unit
  (*val to_string: unit -> string*)
end = struct

  module E = struct
    type key = endpoint
    type ('out','in') value = ('out', 'in') connection_handler
    (*let key2str = endpoint_to_string*)
  end

  module EPH = PolyHash(E) (** Hash from (local) endpoints to their connection handler *)
    (* Only concerns endpoints which have a handler, consequently, not weak: it's
       cleaned by hand when one doesn't want to listen anymore *)

  let table = EPH.create ()

  let find ep =
    EPH.find table ep

  let add ep connection_handler =
    if not (EPH.mem table ep) then
      EPH.add table ep connection_handler

  let remove ep =
    EPH.remove table ep

  (*let update ep f =
    EPH.update table ep f*)

  (*let to_string () = EPH.to_string table*)
end


(* -- Table of the channels with pending handlers (kept from the GC) -- *)

module ChanH : sig
  val add : ('out','in') channel -> unit
  val find: channel_id -> black_channel option
  (*val mem: channel_id -> bool*)

  val remove: channel_id -> unit
    (** If [propagate] (the default), inform the other end of the deletion *)

  val to_string: unit -> string
  (*val count: unit -> int*)
end = struct

  let table = H.create 89

  let add chan =
    chan.connection.num_listeners <- chan.connection.num_listeners + 1;
    H.replace table chan.id (channel_to_black chan)
    (* Strong ref, CH is for channels with pending operations, we hold it in memory *)

  (*let mem id = H.mem table id*)

  let find id = Base.Hashtbl.find_opt table id

  let remove id =
    #<If$minlevel 20> debug "Channel %s unregistered" (channel_id_to_debug_string id) #<End>;
    try
      let ch = Hashtbl.find table id in
      ch.connection.num_listeners <- ch.connection.num_listeners - 1;
      H.remove table id
    with Not_found -> ()

  let to_string () =
    H.fold
      (fun id _chan acc -> Printf.sprintf "%s, %s" acc (channel_id_to_debug_string id))
      table ""

  (*let count () = H.length table*)
end

(* Channels have two special closing treatments:
   - at disconnection, the on_disconnect field within the channel is triggered
   (this should also remove it from ChanH if present)
   - when garbage collected, inform the other hand so that it can clean it up too
   this is what these functions handle

   The two are mutually exclusive, and disable each other *)

(* Whenever a channel is collected, inform the other end by sending a delchan
   message (if [propagate_removal] is [Some true]). Otherwise used to add the
   finalise function only once *)
let register_channel =
  let finaliser channel =
    let propagate_ref = Option.get channel.propagate_removal in
    let propagate = !propagate_ref in
    propagate_ref := false;
    #<If$minlevel 25>
      debug "--- Call finalise function for channel %s" (channel_id_to_debug_string channel.id)
    #<End>;
    if propagate then
      Connection.write channel.connection (Serialise.serialise_delchan channel.id)
        ~failure:(fun _ -> ())
        ~success:(fun _ -> ())
  in
  fun chan -> match chan.propagate_removal with
  | Some propagate_ref -> propagate_ref := true
  | None ->
      Connection.register_channel chan;
      #<If$minlevel 25>
        debug "--- Set a finalise function on channel %s" (channel_id_to_debug_string chan.id)
      #<End>;
      let id = chan.id in
      let propagate_ref = ref true in
      chan.propagate_removal <- Some propagate_ref;
      (let f = !(chan.on_disconnect) in
       chan.on_disconnect := fun cont -> ChanH.remove id; propagate_ref := false; f @> cont);
      gc_finalise chan.connection.scheduler finaliser chan

(* No need to propagate when the channel deletion was already propagated to us *)
let channel_stop_removal_propagation chan =
  match chan.propagate_removal with Some r -> r := false | None -> ()

let channel_write channel value errcont cont =
  Connection.write channel.connection value
    ~failure: (
      fun e ->
        Logger.error "Could not write to channel %016x (%s)"
          channel.id (Printexc.to_string e);
        errcont (Disconnected (channel.connection.remote))
    )
    ~success:cont
(*
    (fun _ ->
       let f = !(channel.on_disconnect) in
       channel.on_disconnect :=
         (fun cont1 -> f @> function
          | true ->
              Scheduler.push scheduler (fun () -> channel_write channel value @> cont);
              true |> cont1
          | false -> false |> cont1))
*)


(*
let get_channel_connection chan cont =
  Connection.get chan.connection
  @> fun connection ->
    Connection.register_channel connection channel;
    channel_propagate_removal connection channel;
    connection |> cont
*)

(* ------------------------------------------------------------ *)
(* Core engine: operation handlers                              *)
(* ------------------------------------------------------------ *)

(* send over a channel *)
(* The error continuation does not need to trigger the on_disconnect handler,
   because in case of problem the whole connection will be marked as
   disconnected, and trigger that for all hosted channels including this one *)
let send_aux chan ?(request_id=RequestId.dummy_request_id) ?(errcont=(fun _ -> ())) ?(cont=(fun _ -> ())) value =
  let msg = Serialise.serialise_message request_id chan value in
  #<If$minlevel 21>
    debug "Sending a packet on channel %s, over %s" (channel_id_to_debug_string chan.id) (connection_to_string chan.connection)
    #<End>;
  #<If$minlevel 30>
    debug "«\n[34m%s[0m»" (hexprint msg)
    #<End>;
  channel_write chan msg errcont @> fun buflen -> cont buflen

(* -- Treatment of incoming messages -- *)

(* Treatment on incoming channels : find out the handler and apply it *)
let first_channel_treatment connection pre_channel =
  let local_ep = get_local_endpoint connection
  and remote_ep = connection.remote in
  match EP.find local_ep with
  | None ->
      warning "Refusing a request for opening a channel from %s (not accepting any service on %s): %s, v%d"
        (endpoint_to_string remote_ep) (endpoint_to_string local_ep)
        pre_channel.pc_service_name pre_channel.pc_service_version
  | Some { chan_handlers; _ } ->
      let service_id = {
        name = pre_channel.pc_service_name;
        version = pre_channel.pc_service_version
      } in
      match Base.Hashtbl.find_opt chan_handlers service_id with
      | None ->
          warning "Refusing a request for opening a channel from %s (not accepting this service on %s): %s, v%d"
            (endpoint_to_string remote_ep) (endpoint_to_string local_ep)
            pre_channel.pc_service_name pre_channel.pc_service_version
      | Some (spec,handler) ->
          let channel = {
            id = pre_channel.pc_cid;
            spec = spec;
            connection = connection;
            pending = IM.empty;
            handler = None;
            waiting_handler = IM.empty;
            on_disconnect = ref (fun cont -> false |> cont);
            propagate_removal = None;
          } in
          register_channel channel;
          #<If$minlevel 20>
            debug "Applying set channel handler for service %s of %s on %s"
            pre_channel.pc_service_name (endpoint_to_string local_ep)
            (channel_id_to_debug_string channel.id)
          #<End>;
          handler channel


let store_message channel reqid msg =
  channel.pending <- IM.update_default reqid (fun l -> msg::l) [msg] channel.pending


let first_message_treatment (channel:black_channel) reqid msg =
  match IM.find_opt reqid channel.waiting_handler with
  | Some (_errh,inh) ->
      #<If$minlevel 10>
        debug "-- Received a message, waiting handler for %s" (request_id_to_debug_string reqid)
      #<End>;
      channel.waiting_handler <- IM.remove reqid channel.waiting_handler;
      if not (is_channel_listening channel) then ChanH.remove channel.id;
      inh msg
  | None ->
      match channel.handler with
      | Some f ->
          #<If$minlevel 10>
            debug "-- Received a message, handler set (r: %s; c: %s)"
            (request_id_to_debug_string reqid) (channel_id_to_debug_string channel.id)
          #<End>;
          f msg (send_aux ~request_id:reqid channel)
      | None ->
          #<If$minlevel 10>
            debug "-- Received a message : %d messages stored, add r: %s on %s"
            (IM.size channel.pending) (request_id_to_debug_string reqid)
            (channel_id_to_debug_string channel.id)
          #<End>;
          store_message channel reqid msg


(*
let on_delete_channel msg conn =
  let local_endpoint = get_local_endpoint conn in
  let cid = Serialise.unserialise_delchan msg 0 in
  Option.iter close_channel (ChanH.find (ChannelId.get ~cid local_endpoint))
*)

(* -- Reading loop -- *)

let reading_loop_aux sched connection loop_end =
  let reading_status = ref (Ready "")
  in
  let rec raw_read : 'a. 'a stream_unserialise -> 'a Cps.t = fun unserialise k ->
    let try_unserialise (len, buf) =
      if len = 0 then
        (warning "Connection lost ! Please handle me.";
         reading_status := Inconsistent);
      (* re-initiate the connection and start a new loop ? Remove it from the channel ?
         We'd need to make sure only the client end attempts to reconnect *)
      match unserialise buf 0 with
      | `data (x,rest) ->
          #<If$minlevel 30>
            debug "unserialise: consumed %d chars (rest %d%s) «\n[34m%s[0m»\n"
            rest (len - rest) (if len > rest then Printf.sprintf ", starting with '%s'" (Char.escaped buf.[rest]) else "")
            (hexprint (String.sub buf 0 rest))
          #<End>;
          reading_status := Ready (String.sub buf rest (len - rest));
          x |> k
      | `needmore n ->
          #<If$minlevel 10>
            debug "unserialise: read again, I got only %d chars and need %d to continue\n"
            len n
          #<End>;
          let receive_buf = String.create n in
          String.blit buf 0 receive_buf 0 len;
          reading_status := Waiting_for_more (receive_buf,len);
          raw_read unserialise @> k
      | `failure msg ->
          Logger.warning "invalid message received on connection %s: %s\n« %s »"
            (connection_to_string connection) msg
            (String.escaped (String.citation "[...]" 60 buf));
          #<If$minlevel 10>
            debug "Bad message from %s: «\n[33m%s[0m»" (connection_to_string connection) (hexprint buf)
          #<End>;
          Connection.write connection Serialise.reset_message ~success:(fun _ -> ()) ~failure:(fun _ -> ());
          loop_end ()
    in
    match !reading_status with
    | Ready "" ->
        Connection.read connection ~failure:(fun _ -> loop_end()) ~success:try_unserialise
    | Ready s ->
        try_unserialise (String.length s, s)
    | Waiting_for_more (buf,amount_full) ->
        Connection.read connection
          ~failure:
          (fun _ -> loop_end())
          ~success:
          (fun (len,buf2) ->
             let amount_needed = String.length buf in
             let amount_received = amount_full + len in
             if amount_received < amount_needed then
               (String.blit buf2 0 buf amount_full len;
                reading_status := Waiting_for_more (buf, amount_received);
                raw_read unserialise @> k)
             else if amount_received = amount_needed then
               (String.blit buf2 0 buf amount_full len;
                try_unserialise (amount_received, buf))
             else
               (* we're screwed, Scheduler returned too much data, we can't use our
                  buffer and have to realloc *)
               (let rebuf = String.create amount_received in
                String.blit buf 0 rebuf 0 amount_full;
                String.blit buf2 0 rebuf amount_full len;
                try_unserialise (amount_received, rebuf)))
    | Inconsistent ->
        Logger.warning "Aborting connection to %s" (connection_to_string connection);
        loop_end()
  in
  let remote_endpoint = get_remote_endpoint connection in
  let rec loop () =
    let loop () = (* make sure to run the loop at least once *)
      if
        EP.find connection.local <> None || Connection.is_listening connection
      then
        loop()
      else
        (#<If$minlevel 10> debug "Stopping reading loop on %s" (connection_to_string connection) #<End>;
         #<If$minlevel 40> debug "Existing connections:\n%s" (Connection.dump()) #<End>;
         loop_end())
    in
    raw_read (Serialise.unserialise_operation remote_endpoint)
    @> function
    | Serialise.Reset ->
        Logger.warning "remote end reset connection %s" (connection_to_string connection);
        loop_end();
        Connection.disconnect connection
    | Serialise.Channel pre_channel ->
        first_channel_treatment connection pre_channel;
        loop ()
    | Serialise.Delete channel_id ->
        Option.iter channel_stop_removal_propagation (Connection.get_channel connection channel_id);
        ChanH.remove channel_id;
        loop ()
    | Serialise.Message (channel_id, req_id) ->
        match ChanH.find channel_id with
        | None ->
            warning "Incoming message on an unknown channel%s"
            (#<If:TESTING> ""
            #<Else>
              Printf.sprintf " (r: %s; c: %s)" (request_id_to_debug_string req_id) (channel_id_to_debug_string channel_id)
            #<End>);
            #<If$minlevel 30> debug "Existing channels:\n%s" (ChanH.to_string()) #<End>;
            reading_status := Ready ""; (* reinit the receiving buffer (maybe we should close the channel) *)
            loop ()
        | Some (channel: black_channel) ->
            raw_read (channel.spec.in_unserialise (black_to_channel channel))
            @> fun message ->
              Scheduler.push sched (fun () -> loop ());
              first_message_treatment channel req_id message
  in
  loop ()

let reading_loop =
  let connections = (Hashtbl.create 53 : (endpoint, unit) Hashtbl.t) in
  fun connection ->
    if not (Hashtbl.mem connections connection.remote) then (
      #<If$minlevel 10> debug "-- Begin reading loop on %s" (connection_to_string connection) #<End>;
      Hashtbl.add connections connection.remote ();
      reading_loop_aux connection.scheduler connection
        (fun () -> Hashtbl.remove connections connection.remote)
    )



(* ------------------------------------------------------------ *)
(* Listening functions                                          *)
(* ------------------------------------------------------------ *)

(** Open a listening socket and setup the receiving queue on the given local
    endpoint. Does nothing if already listening on that endpoint *)
let listen sched endpoint =

  let handler_aux =
    fun connection_info ->
      let connection = Connection.register sched connection_info in
      #<If> debug "Incoming connection %s" (connection_to_string connection) #<End>;
      (* duplicate the handler we have set for the specific local endpoint
         (for the case we were listening on several addresses at once, eg 0.0.0.0) *)
      if connection.local <> endpoint then Option.iter (EP.add connection.local) (EP.find endpoint);
      reading_loop connection
  in

  let tcp_handler _ = handler_aux in
  let ssl_handler = function
    | SslAS.UnsecuredRes ->
        (fun _ -> warning "Ssl: Can not connect in secure mode."; raise Hlnet_ssl)
    | SslAS.SecuredRes (is_valid, _secure) ->
        if not is_valid then
          (fun _ -> warning "Ssl: Invalid certificate."; raise Hlnet_ssl)
        else handler_aux in

  let encryption, handler =
    match endpoint with
      | Ssl (_,_, None) -> warning "Ssl: no secure_type specified."; raise Hlnet_ssl
      | Ssl (_,_, Some ssl) -> (Network.Secured ssl), ssl_handler
      | _ -> Network.Unsecured, tcp_handler in

  match endpoint with
  | Tcp (addr, port)
  | Ssl (addr, port, _) ->
      let port_spec = (Network.make_port_spec ~protocol addr port) in
      let abort_listen = Network.listen sched port_spec encryption ~socket_flags:[Unix.SO_KEEPALIVE] handler in
      EP.add endpoint { chan_handlers = Hashtbl.create 11;
                        abort_listen = abort_listen };
      #<If:TESTING> () #<Else> Logger.debug "Listening on %s" (endpoint_to_string endpoint) #<End>
  (* | _ -> assert false *)

let accept ?(safe=false) sched endpoint chan_spec chan_handler =
  #<If$minlevel 5>
    debug "Accepting local service on endpoint %s/%s"
    (endpoint_to_string endpoint) chan_spec.service.name
  #<End>;
  match EP.find endpoint with
  | None ->
      #<If$minlevel 5>
        debug "Try to listen on endpoint %s"
        (endpoint_to_string endpoint)
      #<End>;
      listen sched endpoint;
      Hashtbl.add (Option.get (EP.find endpoint)).chan_handlers chan_spec.service (chan_spec,chan_handler)
  | Some connection_handler ->
      if safe && Hashtbl.mem connection_handler.chan_handlers chan_spec.service
      then failwith ("Hlnet.safe_accept");
      Hashtbl.add connection_handler.chan_handlers chan_spec.service (chan_spec,chan_handler)



(* ------------------------------------------------------------ *)
(* Operation handlers                                           *)
(* ------------------------------------------------------------ *)

(* -- Sending -- *)

(* Always inform the other hand about a channel before we can send data through it *)
let send_channel chan cont =
  let msg = Serialise.serialise_channel (channel_to_black chan) chan.connection.local in
  channel_write chan msg (fun _ -> () |> cont) @> fun _ -> () |> cont


(* Receives, or sets up the receiving function, for one packet on the given channel *)
let receive_aux chan ?(request_id=RequestId.dummy_request_id) errcont inhandler =
  #<If>
    debug "Receiving a packet on channel %s" (channel_id_to_debug_string chan.id)
  #<End>;
  let msgs =
    try List.rev (IM.find request_id chan.pending)
    with Not_found -> []
  in
  match msgs with
  | [] ->
      let handl msg = inhandler msg in
      #<If$minlevel 10>
        debug "-- Receive: adding a waiting handler for %s (%d)"
        (request_id_to_debug_string request_id) (IM.size chan.waiting_handler)
      #<End>;
      let should_register = not (is_channel_listening chan) in
      chan.waiting_handler <- IM.add request_id (errcont,handl) chan.waiting_handler;
      if should_register then ChanH.add chan;
      reading_loop chan.connection
  | msg::y ->
      (let newreqids =
         match y with
         | [] -> IM.remove request_id chan.pending
         | _ ->
             if (RequestId.is_dummy request_id) then
               Logger.warning "Waiting for several responses to the same request";
             IM.add request_id y chan.pending
       in
       chan.pending <- newreqids;
       let msg : 'in' = msg in
       #<If$minlevel 10> debug "-- Receive: found a stored message for %s (%d)" (request_id_to_debug_string request_id) (IM.size chan.pending) #<End>;
       inhandler msg)


(* ------------------------------------------------------------ *)
(* Core user functions                                          *)
(* ------------------------------------------------------------ *)

let setup_respond chan inouthandler  =
  #<If> debug "Setting answering machine on channel %s" (channel_id_to_debug_string chan.id) #<End>;
  let should_register = not (is_channel_listening chan) in
  chan.handler <- Some inouthandler;
  if should_register then ChanH.add chan;
  reading_loop chan.connection;
  register_channel chan


(** Stop listening on local endpoint. Does nothing if it was not listening there *)
let refuse _scheduler endpoint =
  let endpoint = clean_ssl_endpoint endpoint in
  Option.iter
    (fun connection_handler ->
       #<If:TESTING> () #<Else> Logger.debug "Stop listening on %s" (endpoint_to_string endpoint) #<End>;
       connection_handler.abort_listen ();
       EP.remove endpoint)
    (EP.find endpoint)

let send chan value = send_aux chan value

let receive chan inhandler = receive_aux chan (fun _ -> ()) inhandler
let receive' chan errcont inhandler = receive_aux chan errcont inhandler

let sendreceive' chan msg errcont inhandler =
  let request_id = RequestId.fresh_request_id () in
  #<If$minlevel 10> debug "-- SendReceive with reqid : %s" (request_id_to_debug_string request_id) #<End>;
  let cont =
    fun _ ->
      receive_aux chan ~request_id errcont inhandler in
  send_aux chan ~request_id ~errcont ~cont msg
let sendreceive chan msg inhandler = sendreceive' chan msg (fun _ -> ()) inhandler

let multi_sendreceive chan_list inhandlers =
  let total = List.length inhandlers in

  let receive_handler : ('in' -> unit) -> 'in' -> unit =
    let list_response = Queue.create () in
    fun inh ->
      (fun (msg : 'in') ->
         Queue.push (msg, inh) list_response;
         if (Queue.length list_response = total) then
           Queue.iter (fun (msg,handler) -> handler msg) list_response)
  in

  let fiter :(('out','in') channel * 'out') -> ('in' -> unit) -> unit =
    (fun  (chan,msg) inh ->
       sendreceive chan msg (receive_handler inh))
  in
  List.iter2 fiter (chan_list : (('out','in') channel * 'out') list) (inhandlers : ('in' -> unit)  list)

let close_channel chan =
  #<If> debug "Closing channel %s" (channel_id_to_debug_string chan.id) #<End>;
  ChanH.remove chan.id

let panic chan =
  #<If> debug "Alert on channel %s" (channel_id_to_debug_string chan.id) #<End>;
  Connection.disconnect chan.connection

let dup : ('out','in') channel -> ('out'2,'in2) channel_spec -> ('out'2,'in2) channel =
  fun (chan : ('out', 'in') channel) spec ->
    let chan = channel_to_black chan in
    let new_chan : ('out'2,'in2) channel  =
      { (black_to_channel chan) with
          id = fresh_channel_id();
          spec = spec;
          pending = IM.empty;
          handler = None;
          waiting_handler = IM.empty;
          on_disconnect = ref (fun cont -> false |> cont);
          propagate_removal = None;
      } in
    #<If$minlevel 20>
      debug "Dup a channel : %s -> %s"
      (channel_id_to_debug_string chan.id)
      (channel_id_to_debug_string new_chan.id)
    #<End>;
    new_chan


let open_channel sched remote_endpoint channel_spec ?(on_disconnect=fun () -> `abort) cont =

  let remote_endpoint, connection_get =
    match remote_endpoint with
    | Ssl (a,p,Some ssl) -> let r = Ssl (a,p,None) in r, (fun s ->  Connection.get ~ssl s r)
    | Ssl (_,_, None) -> warning "Ssl: no secure_type specified."; raise Hlnet_ssl
    | _ -> remote_endpoint, (fun s -> Connection.get s remote_endpoint) in

  let rec rec_on_disconnect chan =
    let rec handler cont =
      #<If$minlevel 15> debug "Channel %s disconnected" (channel_id_to_debug_string chan.id) #<End>;
      match on_disconnect() with
      | `abort -> false |> cont
      | `retry delay ->
          let _akey =
            Scheduler.sleep sched delay
              (fun () ->
                 let _ = Connection.get sched remote_endpoint in
                 Cps.Lazy.force chan.connection.info
                 @> function
                 | Some info when Scheduler.check_connection chan.connection.scheduler info ->
                     chan.on_disconnect := rec_on_disconnect chan;
                     send_channel chan
                     @> fun () ->
                       chan.waiting_handler <- IM.empty;
                       true |> cont
                 | _ -> handler cont)
          in ()
    in
    let rec aux cont =
      (* Re-push the handler when it's executed *)
      let disco_ref = chan.on_disconnect in
      let disco = !disco_ref in
      disco_ref := (fun cont -> disco (fun reconnected -> assert (not reconnected); aux @> cont));
      handler @> cont
    in aux
  in
  let connection = connection_get sched in
  let on_disconnect_ref = ref (fun _ -> ()) in
  let chan : ('out', 'in') channel = {
    id = fresh_channel_id();
    spec = channel_spec;
    connection = connection;
    pending = IM.empty;
    handler = None;
    waiting_handler = IM.empty;
    on_disconnect = on_disconnect_ref;
    propagate_removal = None;
  } in
  on_disconnect_ref := rec_on_disconnect chan;
  #<If>
    debug "Open a channel to %s/%s" (endpoint_to_string remote_endpoint) channel_spec.service.name
  #<End>;
  (send_channel chan @> fun () -> chan |> cont);
  register_channel chan

(* Fixme: what does this do in the exported interface ? *)
let channel_is_listening chan =
  Option.is_some (ChanH.find chan.id)

let is_open chan =
  match Cps.Lazy.get_state chan.connection.info with
  | Some (Some _) -> true
  | _ -> false

(* Add last in on_disconnect handlers, execute only if not reconnected,
   otherwise re-push instead *)
let on_disconnect chan user_on_disconnect =
  let rec insert_user_f on_disconnect cont =
    on_disconnect @> function
    | false -> (* disconnected: f() and continue *)
        user_on_disconnect (); false |> cont
    | true -> (* reconnected: repush instead *)
        chan.on_disconnect := insert_user_f !(chan.on_disconnect);
        true |> cont
  in
  chan.on_disconnect := insert_user_f !(chan.on_disconnect)


let unserialise_remote_channel_secondpass hosting_channel spec (pc: pre_channel) =
  if pc.pc_service_name <> spec.service.name then
    raise (Invalid_argument (Printf.sprintf "unserialise_remote_channel: bad service %s, expected %s"
                               pc.pc_service_name spec.service.name));
  if pc.pc_service_version <> spec.service.version then
    raise (Invalid_argument
             (Printf.sprintf "unserialise_remote_channel: version conflict for service %s: got %d, expected %d"
                pc.pc_service_name pc.pc_service_version spec.service.version));
  let connection =
    Connection.get hosting_channel.connection.scheduler pc.pc_remote_endpoint in
  let chan = {
    id = pc.pc_cid;
    spec = spec;
    connection = connection;
    pending = IM.empty;
    handler = None;
    waiting_handler = IM.empty;
    on_disconnect = ref (fun cont -> false |> cont);
    propagate_removal = None;
  }
  in
  #<If$minlevel 5>
    debug "- Registered a remote serialised channel (%s)" (channel_id_to_debug_string chan.id)
  #<End>;
  register_channel chan;
  chan

let unserialise_remote_channel spec hosting_channel s offset =
  let len = String.length s - offset in
  if len < serialised_channel_size
  then `needmore (offset + serialised_channel_size)
  else
    try
      `data
        (unserialise_remote_channel_secondpass hosting_channel spec
           (unserialise_remote_channel_firstpass ~offset s),
         offset + serialised_channel_size)
    with
      Invalid_argument msg -> `failure msg


(*
let flush_waiting_channels sched endpoint connection_handler =
  #<If>
    let waiting_channels = connection_handler.chan_inbox in
    debug "Already listening, %d channel to treat"
      (List.length waiting_channels)
      #<End>;
    connection_handler.chan_inbox <- [];
    List.iter (first_channel_treatment sched ~endpoint) connection_handler.chan_inbox
*)

let reverse_channel : ('out','in') channel -> ('in','out') channel = fun chan -> { chan with
  spec = {
    service = chan.spec.service;
    out_serialise = (fun _ -> failwith "Write on reversed channel");
    in_unserialise = (fun _ _ -> failwith "Read on reversed channel");
  };
  (* local_endpoint = chan.local_endpoint; *)
  (* connection = chan.connection; *)
  (* channel_id = chan.id; *)
  pending = IM.empty;
  handler = None;
  waiting_handler = IM.empty;
  (* current_scheduler = chan.current_scheduler; *)
  on_disconnect = ref (fun cont -> false |> cont);
  propagate_removal = None;
}


(* -- Command-line -- *)

(* Type for options (now it's just an int but you can add any
   options...)*)
type options = {
  port : int;
  (* ... *)
}

(* Command line parser *)
let parse =
  ServerArg.make_parser "Hlnet" [
    ["--hlnet-port"], ServerArg.func ServerArg.int
      (fun _ i -> {port = i}), "<int>",
    "Set the port used by hlnet. By default it's 1086.";
  ]

let opt = fst (
  ServerArg.filter_functional (ServerArg.extract_prefix "--hlnet-") {
    port = 1086;
  } parse
)

let default_port = opt.port

let default_endpoint = Tcp (Unix.inet_addr_any, default_port)

let port_of_endpoint = function | Tcp (_, p) | Ssl (_,p,_) (* | Udp (_, p) *) -> p

let define_protocol ~name ~version ~serialise_query ~unserialise_query ~serialise_response ~unserialise_response =
  let service = make_service_id ~name ~version in
  {
    client_spec =
      { service; out_serialise = serialise_query; in_unserialise = unserialise_response; };
    server_spec =
      { service; out_serialise = serialise_response; in_unserialise = unserialise_query; };
  }

(*
let _tmpdebug_loop =
  let _ = Gc.create_alarm (fun () -> prerr_endline "\nGC!") in
  Scheduler.timer Scheduler.default (Time.seconds 1)
    (fun () ->
       (* Gc.full_major(); *)
       Printf.eprintf "[live %d] [stored %d]\r%!" (Connection.live_channels()) (ChanH.count()))
*)

(* ------------------------------------------------------------ *)
(* Auxiliary user functions                                     *)
(* ------------------------------------------------------------ *)

module Aux = struct

  let magic_unserialise buf ofs =
    let len = String.length buf in
    if len < ofs + Marshal.header_size then `needmore (ofs + Marshal.header_size)
    else
      let need = ofs + Marshal.total_size buf ofs in
      if len < need then `needmore need
      else `data (Marshal.from_string buf ofs, need)

  let map_unserialise f unserialiser = fun buf ofs ->
    match unserialiser buf ofs with
    | `data (x,rest) -> `data (f x, rest)
    | `needmore n -> `needmore n
    | `failure s -> `failure s

  let magic_spec = fun service -> {
    service = service;
    out_serialise = (fun x -> Marshal.to_string x []);
    in_unserialise = (fun _chan -> magic_unserialise);
  }

  let easy_spec ~name ~version ~serialise ~unserialise =
    let service = make_service_id ~name ~version in
    let out_serialise x =
      (* we need to add a header to know the size of the message (it's a big-endian int32) *)
      let s = serialise x in
      let len = 4 + String.length s in
      let buf = String.create len in
      for i = 0 to 3 do buf.[i] <- char_of_int ((len lsr (8*(3 - i))) land 0xFF) done;
      String.blit s 0 buf 4 (String.length s);
      buf
    in
    let in_unserialise channel s offset =
      let len = String.length s - offset in
      if len < 4 then
        `needmore (offset + 4)
      else
        let need =
          let i = int_of_char in
          (i s.[0] lsl 24) lor (i s.[1] lsl 16) lor (i s.[2] lsl 8) lor (i s.[3])
        in
        if len < need then `needmore (offset + need)
        else
          match unserialise channel (String.sub s (offset + 4) (need - 4)) with
          | Some x -> `data (x, offset + need)
          | None -> `failure "Unserialisation of message failed in user function"
    in
    { service; out_serialise; in_unserialise }

  let easy_protocol ~name ~version ~serialise_query ~unserialise_query ~serialise_response ~unserialise_response =
    {
      client_spec = easy_spec ~name ~version ~serialise:serialise_query ~unserialise:unserialise_response;
      server_spec = easy_spec ~name ~version ~serialise:serialise_response ~unserialise:unserialise_query;
    }

  let respond_on_new_channel chan spec handler =
    let new_chan = dup chan spec in
    let _ = setup_respond new_chan handler in
    reverse_channel new_chan

end
