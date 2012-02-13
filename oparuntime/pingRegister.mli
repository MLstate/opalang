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
(** Communications with clients. *)

type json = JsonTypes.json

(** Client module should be define type of client identifier and
    message and a serialization function for ping register. *)
module type CLIENT = sig

  (** Type of identifier of a client. *)
  type key

  (** Type of message can be sent to clients. *)
  type msg

  (** Serialiaze a message to json which can be interpreted by a client. *)
  val serialize : msg -> json Cps.t

  (** {6 For debug}*)
  (** Used by [PING_DEBUG]. *)
  val key_to_string : key -> string

end

module type SCHEDULER = sig
  type async_key
  val sleep : int -> (unit -> unit) -> async_key
  val abort : async_key -> unit
end

(** Make a ping register. A ping register is a module which allows to
    communicates with cient.*)
module Make (S : SCHEDULER) (C : CLIENT) : sig

  (** Kind of ping register events. *)
  type event =
    | Connect (** Launched when a client connects to ping register. *)
    | Disconnect (** Launched when a client is disconnected to ping register. *)
    | Inactive (** Launched when a client is inactive for a while *)

  (** Type of key binded to an event handler (see section Events). *)
  type event_key

  (** {6 Sending messages} *)

  (** [send message id] Send a message to client identified by
      [id]. *)
  val send : C.msg -> C.key -> unit

  (** Broadcast a message to all connected clients. *)
  val broadcast : C.msg -> unit

  (** {6 Interactions with client}*)

  (** [ping id winfo nb] Ping the register form client [id], [winfo]
      must allows to reply to client. [nb] is the ping number. *)
  val ping : C.key -> HttpServerTypes.web_info -> int -> unit

  (** [pang id winfo nb] Like as [ping id winfo nb] but you can reply
      to this specific [pang] with [return id nb response]. *)
  val pang : C.key -> HttpServerTypes.web_info -> int -> bool -> unit

  (** Returns a [pang]. *)
  val return : C.key -> int -> string -> unit

  (** {6 Events} *)

  (** Register a callback that will be executed when a corresponding
      event will be launched. The first parameters indicates on
      which client the callback will be registered, if it's None
      then the callback will be executed for all client. *)
  val register_event : C.key option -> event -> (C.key -> unit) -> event_key

  (** Remove callback event registered associated with [event_key]. *)
  val remove_event : event_key -> unit

  (** Set delay before raising an Inactive even. *)
  val set_inactive_delay : C.key option -> Time.t option -> unit

  (** {6 Utils} *)

  (** Check if the client is already connected to the ping
      register. *)
  val mem : C.key -> bool

  (** Delete a client entry. *)
  val delete : C.key -> unit

  (** Create a client entry (Is needed ...). *)
  val create : C.key -> unit

  (** Return the number of connections. *)
  val size : unit -> int

  val update_activity : ?nb:int -> ?is_ping:bool -> ?is_active:bool -> ?winfo:HttpServerTypes.web_info -> C.key -> bool

end
