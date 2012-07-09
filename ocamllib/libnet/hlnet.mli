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
   @author Louis Gesbert (original interface)
   @author Raja Boujbel (review and implementation)
**)

(** Version number of the HLnet internal protocol *)
val protocol_version: int

(** High level network management *)

(** The type of channels *)
type ('out','in') channel

(** This type is used for asynchronous functions: returning ['a cps] means
    returning right away but taking a continuation of ['a] that will be run
    asynchronously.
*)
type 'a cps = ('a -> unit) -> unit

(** Same as ['a cps], but also taking an error continuation *)
type 'a errcps = (exn -> unit) -> ('a -> unit) -> unit

(** An [endpoint] is one end of a connection. It may be local or remote. *)
type endpoint =
  | Tcp of Unix.inet_addr * int
  | Ssl of Unix.inet_addr * int * SslAS.secure_type option
  (* | Udp of Unix.inet_addr * int *)
      (*| ... *)

(** Not raised, passed as parameter to error continuations *)
exception Disconnected of endpoint

(** A label for a service: used to provide or ask for a given service, with given version *)
type service_id = private {
  name: string; (** the name of the service *)
  version: int; (** current version of your protocol (so that you can handle upgrades) (must be in [0; 9999]) *)
}

(** May raise [Invalid_argument "make_service_id"] if the above restrictions are not respected *)
val make_service_id : name:string -> version:int -> service_id

val print_service_id : service_id -> string

(** The type of a de-serialisation function: takes a buffer and an offset.
    In case of success, should return [`data] with the data successfully
    unserialised and the new offset after you've consumed your data ;
    If there is not enough data yet, should return [`needmore offset_needed]
    (can be returned multiple times, e.g. if you first need a fixed-size
    header, then only know the full size of the packet.) *)
type 'a stream_unserialise = string -> int -> [ `data of 'a * int | `needmore of int | `failure of string ]

(** Type of what should be provided to define a safe ('out','in') channel *)
type ('out','in') channel_spec = {
  service: service_id;            (** The id of the service provided/used by this channel *)
  out_serialise: 'out' -> string; (** The serialisation function for outputting the values *)
  in_unserialise: ('out','in') channel -> 'in' stream_unserialise;
  (** the de-serialisation function to get back received values (the channel is
      provided for cases where you need service or connection information) *)
}

(** Defines at once the types and specs for both ends of a channel. This
    guarantees consistent typing between the client and server.

    {b Guideline:} always explicitely coerce the return value of define_protocol,
    to make the types used in the protocol visible. This will really help understanding
    what your code does. *)
type ('query,'response) protocol = {
  client_spec: ('query,'response) channel_spec;
  server_spec: ('response,'query) channel_spec;
}
val define_protocol:
  name:string -> version:int ->
  serialise_query:('query -> string) ->
  unserialise_query:(('response,'query) channel -> 'query stream_unserialise) ->
  serialise_response:('response -> string) ->
  unserialise_response:(('query,'response) channel -> 'response stream_unserialise)
  -> ('query,'response) protocol

(** {6 Accept input channels (Server side)} *)

(** The default listening endpoint. *)
val default_endpoint: endpoint

(** Open a listening socket and setup the receiving queue on the given
    local endpoint. Does nothing if already listening on that
    endpoint *)
val listen: Scheduler.t -> endpoint -> unit

(** Setup a function to deal with incoming channels requesting the
    service described by [channel_spec] on the local [endpoint].

    Unless [safe], any previously existing handler for the same
    service is overriden. Otherwise, may raise [Failure "Hlnet.safe_accept"].
*)
val accept: ?safe:bool -> Scheduler.t ->
  endpoint -> ('out','in') channel_spec ->
  (('out','in') channel -> unit)
  -> unit

(* refuse_service: endpoint -> service_id -> unit : to stop listening for new channels on a
   specific service. Write it if needed *)

(** Stop listening on local endpoint. Open channels will be kept open, but
    remotes won't be able to open new ones. Does nothing if the local host was
    not listening on the given endpoint. Called before closing endpoint's
    channels *)
val refuse: Scheduler.t -> endpoint -> unit

(** {6 Openning channels (Client side)} *)

(** Just create a [channel] *)
val open_channel: Scheduler.t ->
   endpoint -> ('out','in') channel_spec ->
  ?on_disconnect:(unit -> [ `retry of Time.t | `abort ])
  -> ('out','in') channel cps

(** {6 Using channels} *)

(** Sends a packet on a channel *)
val send: ('out','in') channel -> 'out' -> unit

(** Handles one incoming packet on channel *)
val receive: ('out','in') channel -> 'in' cps
val receive': ('out','in') channel -> 'in' errcps

(** Sends a packet on the given channel, then gets ready to treat the answer with the given continuation.
    * Returns immediately *)
val sendreceive: ('out','in') channel -> 'out' -> 'in' cps
val sendreceive': ('out','in') channel -> 'out' -> 'in' errcps

(** Sends packets on givens channels, waits for all answer before treating them with given continuations *)
val multi_sendreceive: (('out','in') channel * 'out') list -> ('in' -> unit) list -> unit

(** Setups a handler for any incoming packets on channel *)
val setup_respond: ('out','in') channel -> ('in' -> ('out' -> unit) -> unit) -> unit

(** Closes given channel *)
val close_channel: ('out','in') channel -> unit

(** Closes the given channel and the underlying connection, triggering any
    registered handlers *)
val panic: ('out','in') channel -> unit

(** Registers a function to be called if the connection the channel relies on is lost.
    It's not called if the channel is closed normally (by hand, or when we know nobody
    may write to it anymore) *)
val on_disconnect: ('out','in') channel -> (unit -> unit) -> unit

(** Creates a new channel, linked to same endpoint as [chan] *)
val dup: ('out','in') channel -> ('out2','in2') channel_spec -> ('out2','in2') channel

(** Support for serialisation/deserialisation of channels:

    - [serialise_channel] turns a local channel into a string of fixed length
    [serialised_channel_size]

    - [unserialise_remote_channel] uses a channel spec to re-build a channel
    from that string on the other end (you must also provide the hosting
    channel, used internally to get some connection information)

    [unserialise_remote_channel] returns [`failure] if the string does not
    describe a channel or if it doesn't agree with the service described by
    [channel_spec] *)
val serialised_channel_size : int
val serialise_channel : ('out','in') channel -> string
val unserialise_remote_channel : ('out','in') channel_spec -> ('a,'b) channel -> ('out','in') channel stream_unserialise

(** Gives you the reverse of the channel, to use on a local channel before
    serialising and sending. This is not mandatory, just helps the typing of
    protocols ; the reversed channel should obviously not be used locally. *)
val reverse_channel : ('out','in') channel -> ('in','out') channel

val scheduler_of_channel: ('out', 'in') channel -> Scheduler.t
val remote_of_channel: ('out', 'in') channel -> endpoint
val local_of_channel: ('out','in') channel -> endpoint

(** {6 Debug} *)

(** Returns a string of given channel (for debug)*)
val channel_to_string : ('out', 'in') channel -> string

(** check that the channel is registered on this side,
 *  ie : a message handler is set, or a waiting handler or messages *)
val channel_is_listening : ('out', 'in') channel -> bool

(** Returns [true] if the channel is open, {e i.e.} if it can still
    be used for communications *)
val is_open: ('out','in') channel -> bool

(** Returns a readable string from a [endpoint]. *)
val endpoint_to_string : endpoint -> string

(** Returns port from an [endpoint] *)
val port_of_endpoint : endpoint -> int

(** {6 Some useful tools and auxiliary functions} *)
module Aux : sig

  (** A few tools on stream_unserialise: Stream unserialiser using
      [Marshal.from_string] -- Use at your own risk ;) -- and a map function *)
  val magic_unserialise : 'a stream_unserialise
  val map_unserialise : ('a -> 'b) -> 'a stream_unserialise -> 'b stream_unserialise


  (** Returns a polymorphic channel_spec using module Marshal. Fast and unsafe. *)
  val magic_spec : service_id -> ('out','in') channel_spec

  (** Returns a channel spec from simple to-string and from-string
      functions. Less efficient than building your own stream_unserialiser (more
      allocations), but easier to use. Strings are simply prefixed with 4 bytes
      that indicate their length. In the unserialise function, return None to
      indicate a failure to unserialise *)
  val easy_spec : name:string -> version:int ->
    serialise:('out' -> string) ->
    unserialise:(('out','in') channel -> string -> 'in' option)
    -> ('out','in') channel_spec

  (** The easy version of [define_protocol] above *)
  val easy_protocol : name:string -> version:int ->
    serialise_query:      ('query -> string) ->
    unserialise_query:    (('response,'query) channel -> string -> 'query option) ->
    serialise_response:   ('response -> string) ->
    unserialise_response: (('query, 'response) channel -> string -> 'response option)
    -> ('query,'response) protocol

  (** Dups the given channel, registers a handler on it with [setup_respond] and
      returns a (reverse) channel ready for sending back *)
  val respond_on_new_channel:
    ('out0,'in0) channel ->
    ('out','in') channel_spec ->
    ('in' -> ('out' -> unit) -> unit)
    -> ('in','out') channel
end
