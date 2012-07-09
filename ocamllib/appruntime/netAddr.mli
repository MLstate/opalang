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
   @author Adam Koprowski
*)

(* TODO hide implementation? *)
type protocol = string

type fd = Unix.file_descr

type addr = Unix.inet_addr

type connection_type = TCP | UDP | SSL | FILE

(** A complete network address *)
type t

(* FIXME, this function is identity for now but can be used to
   ease possible futher transition to hiding implementation
   of [protocol] *)
val mk_protocol : string -> protocol

val mk_tcp : protocol:protocol -> fd:fd -> addr:addr -> t

val mk_udp : protocol:protocol -> fd:fd -> addr:addr -> t

val mk_file : fd:fd -> t

val get_type : t -> connection_type

val get_addr : t -> addr

val get_fd : t -> fd

val get_protocol : t -> protocol

val get_type_and_fd : t -> [`Tcp of fd | `Udp of fd | `File of fd | `Ssl of Ssl.socket]

val secured_from_normal : Ssl.socket -> t -> t

val string_of_sockaddr : Unix.sockaddr -> string

val to_string : t -> string
