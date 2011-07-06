(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)
module Ping = BslPingRegister.M
module Client = BslPingRegister.Client
module PingScheduler = BslPingRegister.PingScheduler

(*
  A delay for a specific RPC call.
  A client may be still connected, and continue to ping, but an error occurred
  and this client will never respond to a rpc_call.
  This timeout is meant to abort the rpc_call, and to raise an exception on the server side.
  It is also used as timeout of distant cellules calls.
  Keep consistent with values defined in [pingRegister.ml]
*)
##register rpc_response_delay : int
let rpc_response_delay = 45 * 1000

(** Primitive for make rpc call to the client. *)
module RPC : sig

  (** Call an rpc on the client identified by [cid] and send an
      identifier. Register the cps continuation, this continuation
      will be called on [return].*)
  val call : string -> string -> string QmlCpsServerLib.continuation -> Client.key
    -> bool

  (** [return id response] Call the continuation corresponding to
      the given identifier. This identifier has been sent to the
      client... *)
  val return : string -> string -> bool

end = struct

  let random_int () = Random.int 1073741823 (* 2^30 -1 *)

  let generate_without_conflicts exists =
    let rec aux () =
      let id = random_int () in
      if exists id then
        aux ()
      else id
    in aux ()

  (** Store the rpc continuation while waiting the response. *)
  let rpc_ids = Hashtbl.create 512

  let generate_id () =
    generate_without_conflicts (fun id -> Hashtbl.mem rpc_ids id)

  let set_rpc_timeout (cid : Client.key) fun_id id =
    let abort () =
      (* if the id is still in the rpc table, then remove it, and abort *)
      try
        let k = Hashtbl.find rpc_ids id in
        Hashtbl.remove rpc_ids id ;
        let exc = BslNativeLib.OpaExc.OpaRPC.timeout cid fun_id in
        let k_exc = QmlCpsServerLib.handler_cont k in
        QmlCpsServerLib.push_cont k_exc exc
      with
      | Not_found -> ()
    in
    let _async_key = PingScheduler.sleep rpc_response_delay abort in
    ()

  let call fun_id args k cid =
    let id = generate_id () in
    #<If:PING_DEBUG>
      Logger.debug "[RPC] Try to call rpc %s on client %s"
      fun_id (Client.key_to_string cid);
    #<End>;
    Hashtbl.add rpc_ids id k;
    (* TODOK1 : args is a string but it should be a json! *)
    let mess = Client.RPC (string_of_int id, fun_id, JsonTypes.String args) in
    if Ping.mem cid then (
      Ping.send mess cid ;
      set_rpc_timeout cid fun_id id ;
      true
    ) else false

  let return id response =
    try
      let id = int_of_string id in
      try
        #<If:PING_DEBUG>
          Logger.debug "[RPC] Return %d received" id;
        #<End>;
        let k = Hashtbl.find rpc_ids id in
        Hashtbl.remove rpc_ids id;
        QmlCpsServerLib.push_cont k response;
        true
      with Not_found ->
        Logger.error "[RPC] Any continuation stored for %d" id;
        false
    with Failure "int_of_string" ->
      Logger.error "[RPC] Identifier %s isn't an int" id;
      false

end

(** Given continuation must be a string continuation. That works
    because OPA string and ML string have the same representation,
    but it's back end dependent. We should use a ServerLib
    function for translate ML string to OPA string... coming
    soon? *)
##register call : string, string, continuation('a), 'ctx -> bool
let call fun_id args k key =
   RPC.call fun_id args (Obj.magic k) (Obj.magic key)

(** This module is very dangerous, don't use it directly. It's a
    module for RPC.*)
##module Dispatcher

  let rpctbl : (string, Obj.t) Hashtbl.t = Hashtbl.create 1024

  ##register register : string, 'a -> void
  let register name rpc_fun =
    Hashtbl.add rpctbl name (Obj.repr rpc_fun)

  ##register get : string -> option('a)
  let get name =
    try
      Some (Obj.obj (Hashtbl.find rpctbl name))
    with Not_found -> None

##endmodule
