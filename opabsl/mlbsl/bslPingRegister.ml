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
(** Communications with opa client *)

open Cps.Ops

module JS = JsonTypes

type json = JS.json

let serialize_uu, unserialize_uu, set_uu =
  let serialize = ref (fun (_ : unit -> unit) (_ : json -> unit) -> ((failwith "Unregistered serialization function") : unit)) in
  let unserialize = ref (fun (_ : json) (_ : (unit -> unit) -> unit) -> ((failwith "Unregistered unserialization function") : unit)) in
  (fun x -> !serialize x), (fun x -> !unserialize x),
  (fun s u -> serialize := s; unserialize := u)

(** Define an identifier for opa client and which message they can
    be receive from server (with PingRegister) *)
module Client : sig
  (** Type of a client connection identifier.  Current implementation
      it's an opa record that represents the thread context. *)
  type key

  (** Create a cid from cookie and page identifier. *)
  val key : string -> int -> key

  (** To string (for debug) *)
  val key_to_string : key -> string

  (** To record (for embedding in thread context)*)
  val key_to_record: key -> ServerLib.ty_record

  type msg =
    | SendCChan of (string * json)
    | SendCChanThen of (string * json * (unit -> unit) * (unit -> unit))
    | RPC of (string * string * json)
    | AsyncRPC of (string * json)

  val serialize : msg -> json Cps.t

end = struct

  (* It's the opa records that represents the thread context.
     see thread_context.opa *)
  type key = ServerLib.ty_record

  let fclient  = ServerLib.static_field_of_name "client"
  let fpage    = ServerLib.static_field_of_name "page"

  let key =
    fun cookie page ->
      try
        let rpage   = ServerLib.wrap_int page in
        let rclient = ServerLib.wrap_string cookie in
        let ctx =
          ServerLib.add_field
            (ServerLib.add_field ServerLib.empty_record_constructor
               (fpage) rpage)
            (fclient) rclient in
        ServerLib.make_record ctx
      with _ -> failwith ("Error on ket generation")

  let key_to_string t = Base.Obj.dump t
  let key_to_record x = x

  type msg =
    | SendCChan of (string * json)
    | SendCChanThen of (string * json * (unit -> unit) * (unit -> unit))
    | RPC of (string * string * json)
    | AsyncRPC of (string * json)

  let serialize json k =
    match json with
    | SendCChan (id, json) ->
        (JS.Record [("type", JS.String "chan");
                    ("id", JS.String id);
                    ("msg", json)]) |> k
    | SendCChanThen (id, json, herror, hsuccess) ->
        serialize_uu herror @>
          (function herror ->
             serialize_uu hsuccess @>
               function hsuccess ->
                 JS.Record [("type", JS.String "chan");
                            ("id", JS.String id);
                            ("msg", json);
                            ("herror", herror);
                            ("hsuccess", hsuccess);
                           ] |> k)
    | RPC (id, name, json) ->
        JS.Record [("type", JS.String "rpc");
                   ("id", JS.String id);
                   ("name", JS.String name);
                   ("args", json)] |> k
    | AsyncRPC (name, json) ->
        JS.Record [("type", JS.String "asyncrpc");
                   ("name", JS.String name);
                   ("args", json)] |> k

end

(** Scheduler module for ping register *)
module PingScheduler = struct
  include BslScheduler
  type async_key = Scheduler.async_key
  let sleep = asleep
end

(** Instantiated ping register with opa scheduler and op client *)
module M = PingRegister.Make(PingScheduler)(Client)

##register client_start : opa[ThreadContext.client] -> void
let client_start ck = M.create (Obj.magic ck)

##register nb_connection : -> int
let nb_connection = M.size

##register client_stop : opa[ThreadContext.client] -> void
let client_stop ck = M.delete (Obj.magic ck)
