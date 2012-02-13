(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
