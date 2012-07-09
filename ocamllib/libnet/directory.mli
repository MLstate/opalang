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
(** A shared directory beetween several server.

    NOTE : Currently is a very simple implementation which support
    only one server and doesn't support any failure of server.
    Implementation may change for support several servers and server
    failure, but interface should not be change (or maybe just the
    make function...)
*)

(** {6 Who} *)
(** A type used for indicates who put a value on directory. *)
type who = Me | Other of Unix.inet_addr

(** Returns a readable string from a [who]. *)
val who_to_string : who -> string

(** {6 Directory interface} *)

(** A directory is parametrized by a type of ['key] and ['value]. *)
type ('key, 'value) t

(** [make scheduler endpoint path kind] Create a new directory. Behaviour
    depends of [kind] :
    - [`server] creates a directory and launch a server that
    listen on [endpoint].
    - [`client] creates a client directory which request a server
    directory at [endpoint].
    [path] it's using for distinguishes directories if you have
    several. Limited to 8 characters.
*)
val make : ?err_cont:(exn -> unit) -> Scheduler.t -> Hlnet.endpoint -> string ->
  [ `client | `server ] -> ('key, 'value) t Cps.t

(** Add a new binding of [key] to [value]. That doesn't erase previous
    bindings. *)
val add : ('key, 'value) t -> 'key -> 'value -> unit

(** [remove directory key] Remove the last binding of [key]. *)
val remove : ('key, 'value) t -> 'key -> unit

(** [find_or_replace directory key value] Returns [value] binded to
    [key] if already exists, else returns [None] and add a binding of
    [key] to [value].*)
val find_or_replace : ('key, 'value) t -> 'key -> 'value ->
  ('value * who) option Cps.t

(** [find_opt directory key] Returns the last binded value to [key] or
    [None] if any value is binded to [key].*)
val find_opt : ('key, 'value) t -> 'key ->
  ('value * who) option Cps.t

(** [find_all directory key] Find all value binded to [key].*)
val find_all : ('key, 'value) t -> 'key ->
  ('value * who) list Cps.t

(** {6 Localizer interface} *)

(** [my_public_addr_opt] Returns your public [inet_addr]. (It's the
    addresses use by the server directory for communicates with
    you). Can return [None] if you are the server directory and any
    client make a request to you. *)
val my_public_addr_opt : ('key, 'value) t -> Unix.inet_addr option Cps.t

(** Like [my_public_addr_opt] but in server case wait the first client
    request for get the public address.*)
val my_public_addr : ('key, 'value) t -> Unix.inet_addr Cps.t
