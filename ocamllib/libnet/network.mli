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

(**
   Network specific operations.

   @author Henri Binsztok
   @author Laurent Le Brun
   @author Frederic Ye
   @author Cedric Soulas
*)

(**
    This module provides functions to make connections, secured or not,
    by listening or connecting on a port specification.
    Connection returned by those functions, of type [Scheduler.connection_info],
    should then be used with the Scheduler module.

    {b Current use:}
    For the moment it is used only to establish TCP connection (connected mode).
    {b Intented future use:}
    UPD specific function may be added in that module
*)

(** {6 Types} *)

(**
    The type describing how secure have to be the connection.
    An "Optional" mode may be added for future use.
*)
type secure_mode =
  | Unsecured
  | Secured of SslAS.secure_type

type socket_type =
  | TCP
  | UDP

(**  The type describing the connection to establish *)
type port_spec = (* ADAM: how about hiding the type and forcing its construction via the [make_port_spec] function? *) {
  addr : Unix.inet_addr;
  port : int;
  prot : NetAddr.protocol;
  stype : Connection.socket_type
}

(** The type to plug a port in a Runtime.COMPONENT *)
type port = {
  conn_incoming :
    SslAS.secure_response -> Scheduler.connection_info -> unit;
  conn_terminating : unit -> unit;
  secure_mode : secure_mode;
  port_spec : port_spec
}

type socket

(** A function to build a port_spec type *)
val make_port_spec : ?socket_type:socket_type -> protocol:NetAddr.protocol -> Unix.inet_addr -> int -> port_spec

(** Gives the port of the given [port_spec] *)
val get_port : port_spec -> int

(** Gives the network address of the given [port_spec] *)
val get_addr : port_spec -> Unix.inet_addr

(** Gives the socket type of the given [port_spec] *)
val get_socket_type : port_spec -> socket_type

(** {6 TCP functions} *)


(**
    TCP listen over a [socket]
    A SslAS.secure_response and a Scheduler.connection_info is
    provided to your callback when a new client is connected.
*)
val listen :
  Scheduler.t ->
  port_spec ->
  secure_mode ->
  ?socket_flags:(Unix.socket_bool_option list) ->
  (SslAS.secure_response -> Scheduler.connection_info -> unit) ->
  (unit -> unit)

(**
    TCP connect over a port specification.
    A Scheduler.connection_info describing the
    socket connection between you and the server is provided to your callback.
*)
val connect :
  Scheduler.t ->
  port_spec ->
  secure_mode ->
  ?socket_flags:(Unix.socket_bool_option list) ->
  ?err_cont:(exn -> unit) ->
  (Scheduler.connection_info -> unit) ->
  unit

(** {6 Misc. functions } *)

exception Unknown_machine of string

(**
   [inet_addr_of_name m] raise [Unknown_machine m] if
   the the name can't be resolved.
*)
val inet_addr_of_name :
  string -> Unix.inet_addr

val addr_of_ipv4 :
  int * int * int * int -> Unix.inet_addr

val string_of_ipv4 :
  int * int * int * int -> string

(**
   @return a [secure_mode] [Secured (c, p)] if [c] or [p] isn't None.
   and return [Unsecured] if c and p are both None.
   Note that [Secured (None, None)] is permited in a client context.
   See [SslAS.get_secure_socket] for more information.
*)
val secure_mode_from_params :
  SslAS.ssl_certificate option -> SslAS.ssl_verify_params option -> secure_mode

(** {6 Old functions intended to be deleted} *)

(**
    Loop over a Scheduler.t
    {b For migration purpose only.
    It will be deleted when the Runtime module will be used.}
   @deprecated Use Scheduler.run
*)
val loop : Scheduler.t -> unit
