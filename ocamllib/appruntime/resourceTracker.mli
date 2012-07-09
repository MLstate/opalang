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
   Module to track resources and to collect them if needed

   @author Hugo Heuzard
*)

(**
   This module provides functions to
   - create resources
   - connect resources to each other
   - kill, call resources
   - collect resources
*)

(** {6 Types definitions} *)

(** Type of resource parameterized by the type of message it can reveive and
    by the type of result it will answer.*)
type ('message,'result) t

(** Type of black resource. Same as resource with hidden type's parameters. Its
    only purpose is to be able to store different kind of resource in the same
    structure (list, table). You should only deal with this type when you need
    to give resource dependencies (a list of black resources).  *)
type b

(** Type of signal. A resource can be remove with one of the following signal *)
type signal =
    [ `Expired (** The resource has expired. *)
    | `Killed (** The resource has been forced to close. *)
    | `Closed (** The resource has been closed properly. *)
    | `Collected (** The resource has been collected by caml garbage collector *)
    ]

(** Type of resource handler parameterized by the type of message, the type of
    result and the type of the internal state *)
type ('message,'result,'state) h

(** [black resource] hide type's parameters from a ('message,'result) resource *)
external black : (_,_) t -> b = "%identity"

(** Exception raised when trying to do operation on an already removed resource *)
exception Not_Alive

(** Exception raised when trying to do synchore operation with asynchrone resource *)
exception Async_Call

module type M = sig

  (** Module expire provide helpers to set the expiration of the resource *)
  module  Expire : sig
    type 'a t = 'a state -> signal option * Time.t option

    (** Type of expiration context *)
    and context =
        [ `Date of Time.t
        | `Timeout of Time.t
        | `Limit of int
        | `And of context list
        | `Or of context list ]
    and 'a state =
        {
          mutable limit: int;
          mutable last_use: Time.t;
          mutable cancel: Scheduler.async_key option;
          mutable state: 'a;
        }

    val init : 'a -> 'a state

    (** [make context] make a value of type 'a t. Use this with handler_timer *)
    val make : context -> 'a t
  end

  (** {6 Creations} *)

  (** [handler name kill_fun expire_fun on_message_fun decide_fun] Make a resource
      handler.

      [name] is the kind of resource created with this handler (ie: Cookie, Ping)

      [kill_fun] is called in order to remove the ressource. It take the current
      internal state of the resource and the signal it has been removed with

      [expire_fun] is called to know if the resource has expired. When expired,
      it should return [Some `Expired]

      [on_message_fun] is call when you send a message to the resource. It takes
      as arguments the resource itself, the current internal state and a
      message. It returns the new state and the result.

      [decide_fun] is call when a resource you depend on (a parent resource) is removed. It allows
      to decide what to do (remove the resource aswell, ignore it, ...). It
      takes as arguments the resource itself, the current internal state and the
      signal the parent resource has been removed with. *)
  val handler :
    string ->
    ( 'state -> signal -> unit ) ->
    ( 'state -> signal option ) ->
    (('message,'result) t -> 'state -> 'message -> 'state*'result ) ->
    (('message,'result) t -> 'state -> signal -> unit) ->
    ('message,'result,'state) h

  (** Same as handler with asynchrone function (taking continuation as last arguments) *)
  val handler_cps :
    string ->
    ( 'state -> signal -> (unit -> unit) -> unit ) ->
    ( 'state -> (signal option -> unit ) -> unit ) ->
    (('message,'result) t -> 'state -> 'message -> ( ('state*'result) -> unit ) -> unit ) ->
    (('message,'result) t -> 'state -> signal -> (unit -> unit) -> unit) ->
    ('message,'result,'state) h

  (** Same as handler except that you have to create the expire_fun with
      [Expire.create]. It helps you the create resource with temporal expiration.
  *)
  val handler_timer :
    string ->
    ('state -> signal -> unit) ->
    ('state Expire.t) ->
    (('message,'result) t -> 'state -> 'message -> 'state * 'result) ->
    (('message, 'result) t -> 'state -> signal -> unit) ->
    ('message, 'result, 'state Expire.state) h

  (** [resource handler state ~depends:lst ()] Make a resource from an handler,
      the initial state of the resource and the resource it depends on.
  *)
  val resource : ('message,'result,'state) h -> 'state -> ?depends: b list -> unit -> ('message,'result) t

  (** Same as resource but has to be used with handler created by handler_timer
  *)
  val resource_timer : ('m,'r,'s Expire.state) h -> 's -> ?depends: b list -> unit -> ('m,'r)t


  (** {6 Operations on resources}*)

  (** [expire resource] return whether or not the resource has expired *)
  val expire : (_,_)t -> signal option

  (** Same has expire but has to be used when dealing with asynchone resource *)
  val expire_cps : (_,_)t -> ?err_cont:(exn -> unit) -> (signal option -> unit)  -> unit

  (** [call resource message] send a message to the resource, change its internal
      state and return a result *)
  val call : ('m,'r)t -> 'm -> 'r

  (** Same as call but has to be used when dealing with asynchrone resource *)
  val call_cps : ('m,'r)t -> 'm -> ?err_cont:(exn -> unit) -> ('r -> unit) -> unit

  (** [kill resource signal] remove the resource with the signal *)
  val kill : (_,_)t -> signal ->  unit

  (** [alive resource] return true if the resource has not been already remove *)
  val alive : (_,_)t -> bool

  (** [collect ()] goes throw all resources, checks their expiration and remove
      the expired one *)
  val collect : unit -> unit

  (** [register resource f] register a callback to the resource. This callback
      will be called when the resource will be removed *)
  val register : (_,_)t -> (signal -> unit) -> unit

  (** Same as register but has to be used when the function is asynchrone *)
  val register_cps : (_,_)t -> (signal -> unit) -> unit

end

module Default : M
