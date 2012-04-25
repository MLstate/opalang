(*
    Copyright © 2011, 2012 MLstate

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
(* This module defin bypasse defined for Hlnet use on opa,
    "synchronous" (according cps), and asynchronous (for s2 compatibility)
*)

module C = QmlCpsServerLib
open C.Ops

let (@@) a b = fun x -> a (b x)
let scheduler = Scheduler.default
module BslUtils = OpabslgenMLRuntime.BslUtils

##opa-type outcome('a, 'b)

##property [mli]
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation
##extern-type SSL.secure_type = SslAS.secure_type
##extern-type endpoint = Hlnet.endpoint
##property [endmli]

##extern-type [normalize] channel('o, 'i) = ('o, 'i) Hlnet.channel
##extern-type [normalize] channel_spec('o, 'i) = ('o, 'i) Hlnet.channel_spec

##opa-type Hlnet.error

(** TODO - Remove this hack we need plugins depends*)
let create_outcome outcome k =
  QmlCpsServerLib.return k (wrap_opa_outcome (BslUtils.unwrap_opa_outcome (BslUtils.create_outcome outcome)))


(** Projection of ocaml exn raised by hlnet to an opa record
    (Hlnet.error). *)
let hlnetexn_ml_to_opa =
  let disconnected =
    let fdisconnected = ServerLib.static_field_of_name "disconnected" in
    function (e:Hlnet.endpoint) ->
      wrap_opa_hlnet_error (
        ServerLib.make_record
          (ServerLib.add_field ServerLib.empty_record_constructor
             fdisconnected e
          )
      )
  in function
    | Hlnet.Disconnected ep -> disconnected ep
    | e -> failwith (Printf.sprintf "Unknow hlnet exn: %s" (Printexc.to_string e))

##register new_endpoint : string, int -> endpoint
let new_endpoint addr port =
    (Hlnet.Tcp (Unix.inet_addr_of_string addr, port))

##register new_ssl_endpoint : string, int, SSL.secure_type -> endpoint
let new_ssl_endpoint addr port ssl =
    (Hlnet.Ssl (Unix.inet_addr_of_string addr, port, Some ssl))

(* Inspection of endpoint from opa *)
##module EndpointGet
  type ep_flat = { protocol:string; addr:string; port:int }
  let flat = function
    | Hlnet.Tcp (addr, port) ->
      {protocol="tcp";addr=Unix.string_of_inet_addr addr;port=port}
    | Hlnet.Ssl (addr, port, _secure) ->
      {protocol="ssl";addr=Unix.string_of_inet_addr addr;port=port}
(*    | Hlnet.Udp (addr, port) ->
      {protocol="udp";addr=Unix.string_of_inet_addr addr;port=port} *)

  ##register protocol : endpoint -> string
  let protocol ep = (flat ep).protocol
  ##register addr     : endpoint -> string
  let addr     ep = (flat ep).addr
  ##register port     : endpoint -> int
  let port     ep = (flat ep).port
##endmodule


##register make_channel_spec : string, int, ('o -> string), (channel('o,'i), string -> option('i)) -> channel_spec('o,'i)
let make_channel_spec name version serialise unserialise =
  Hlnet.Aux.easy_spec ~name ~version ~serialise ~unserialise

##register[cps-bypass] open_channel: endpoint, channel_spec('o,'i), continuation(channel('o, 'i)) -> void
let open_channel ep spec k =
  let on_disconnect () =
    Logger.error "Can not connect to %s" (Hlnet.endpoint_to_string ep);
    `abort in
  Hlnet.open_channel scheduler ep ~on_disconnect spec @> fun chan -> chan |> k

##register listen: endpoint -> void
let listen = Hlnet.listen scheduler

##register accept: endpoint, channel_spec('o,'i), (channel('o, 'i) -> void) -> void
let accept ep spec ch_hand = Hlnet.accept scheduler ep spec ch_hand

##register refuse: endpoint -> void
let refuse endpoint = Hlnet.refuse scheduler endpoint

##register[cps-bypass] local_endpoint: channel('o, 'i), continuation(endpoint) -> void
let local_endpoint chan k = Hlnet.local_of_channel chan |> k

##register[cps-bypass] remote_endpoint: channel('o, 'i), continuation(endpoint) -> void
let remote_endpoint chan k = Hlnet.remote_of_channel chan |> k

##register send: channel('o, 'i), 'o -> void
let send chan opack = Hlnet.send chan opack

##register[cps-bypass] receive: channel('o, 'i), continuation('i) -> void
let receive chan k =
  Hlnet.receive chan @> fun x -> x |> k

##register[cps-bypass] sendreceive: channel('o, 'i), 'o, continuation('i) -> void
let sendreceive chan opack k =
  Hlnet.sendreceive chan opack @> fun recv -> recv |> k

##register[cps-bypass] sendreceiverr: channel('o, 'i), 'o, continuation(outcome('i, Hlnet.error)) -> void
let sendreceiverr chan opack k =
  Hlnet.sendreceive' chan opack
    (fun e -> create_outcome (`failure (hlnetexn_ml_to_opa e)) k)
    (fun r -> create_outcome (`success r) k)

##register async_receive: channel('o, 'i), ('i -> void) -> void
let async_receive chan handler =
  Hlnet.receive chan handler

##register async_sendreceive: channel('o, 'i), 'o, ('i -> void) -> void
let async_sendreceive chan x handler =
  Hlnet.sendreceive chan x handler

##register [cps-bypass] setup_receive: channel('o, 'i), ('i, continuation(opa[void]) -> void), continuation(opa[void]) -> void
let setup_receive chan handler k =
  let handler = BslUtils.proj_cps k handler in
  Hlnet.setup_respond chan (fun i _respond -> handler i);
  ServerLib.void |> k

##register [cps-bypass] setup_receive_cps: channel('o, 'i), ('i, continuation(opa[void]) -> void), continuation(opa[void]) -> void
let setup_receive_cps chan handler k =
  let handler = BslUtils.proj_cps k handler in
  Hlnet.setup_respond chan (fun i _respond -> handler i);
  ServerLib.void |> k


##register[cps-bypass] setup_respond: channel('o, 'i), ('i, continuation('o) -> void), continuation(opa[void]) -> void
let setup_respond chan iohand k =
  Hlnet.setup_respond chan
    (fun i respond -> iohand i @> C.cont_ml respond);
         (* do not dup the transaction info in k (no [C.ccont_ml k])
            -- but what about the thread context ?? *FIXME* *)
  ServerLib.void |> k

##register close_channel \ `Hlnet.close_channel` : channel('o, 'i) -> void

##register[cps-bypass] dup: channel('o, 'i), channel_spec('o2,'i2), continuation(channel('o2,'i2)) -> void
let dup chan spec k =
  Hlnet.dup chan spec |> k

##register[cps-bypass] respond_on_new_channel : channel('o0,'i0), channel_spec('o,'i), ('i, continuation('o) -> void), continuation(channel('i,'o)) -> void
let respond_on_new_channel chan spec handl k =
  Hlnet.Aux.respond_on_new_channel chan spec
    (fun i fk -> handl i @> C.ccont_ml k fk)
  |> k

##register serialise_channel \ `Hlnet.serialise_channel` : channel('o, 'i) -> string

##register unserialise_remote_channel : channel_spec('o, 'i), channel('o0, 'i0), string -> opa[option(channel('o, 'i))]
let unserialise_remote_channel chan spec s =
  let chanopt = match Hlnet.unserialise_remote_channel chan spec s 0 with
    | `data(chan,_) -> Some chan
    | `needmore _ -> Logger.warning "Unable to deserialise channel: string too short"; None
    | `failure msg -> Logger.warning "Unable to deserialise channel: bad format (%s)" msg; None
  in ServerLib.wrap_option chanopt

##register channel_is_open \ `Hlnet.is_open` : channel('o, 'i) -> bool

##register channel_exists \ `Hlnet.channel_is_listening` : channel('o, 'i) -> bool

##register channel_to_string \ `Hlnet.channel_to_string` : channel('o,'i) -> string

##register default_endpoint \ `Hlnet.default_endpoint` : endpoint

##register remote_of_channel \ `Hlnet.remote_of_channel` : channel('o, 'i) -> endpoint
##register local_of_channel  \ `Hlnet.local_of_channel`  : channel('o, 'i) -> endpoint
