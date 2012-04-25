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
open Requestdef
open HttpServerCore_parse
open HttpServerTypes
open HttpServerCore
open HttpServer

open Cps.Ops

module BslUtils = OpabslgenMLRuntime.BslUtils
module Client = BslPingRegister.Client
module Ping   = BslPingRegister.M
module WebChannel = Session.WebChannel

module JS     = JsonTypes
type json = JS.json


let send_txt_response winfo txt =
   make_response_modified_since
     (Time.now ())
     winfo.HttpServerTypes.request
     Requestdef.SC_OK
     "text/plain; charset=utf-8"
     (Http_common.Result txt)
     winfo.HttpServerTypes.cont

let send_json_response winfo json =
  let txt = Json_utils.to_string json in
  send_txt_response winfo txt

let send_error winfo txt =
  let txt = #<If:PING_DEBUG> txt #<Else>
    let _ = txt in  "Unauthorized request"
    #<End>
  in
  make_response ~req:winfo.HttpServerTypes.request Requestdef.SC_Unauthorized
    "text/plain"
    (Http_common.Result txt)
    winfo.HttpServerTypes.cont

let string2json str =
  try
    Some (Json_utils.from_string str)
  with _ -> None

let get_request_params req = Rcontent.get_content req.HttpServerTypes.request_message_body

##register [cps-bypass] complete_dispatcher_cps : \
string, \
    (WebInfo.private.native, continuation(opa[void]) -> void), \
    continuation(continuation(WebInfo.private.native)) -> \
    void
let complete_dispatcher_cps base_url dispatcher k =
  let r = Str.regexp (base_url^"/_internal_/\\([0-9]+\\)/+\\(.*\\)") in
  let rec aux_complete_dispatcher winfo =
       let uri = winfo.HttpServerTypes.request.HttpServerTypes.request_line.HttpServerTypes.request_uri in
       (* Get the page number and remove it from winfo *)
       let page, uri, winfo =
         if (Str.string_match r uri 0) then
           let page = Str.matched_group 1 uri in
           let uri = Str.matched_group 2 uri in
           let winfo = {
             winfo with HttpServerTypes.request = {
               winfo.HttpServerTypes.request with HttpServerTypes.request_line = {
                 winfo.HttpServerTypes.request.HttpServerTypes.request_line with
                   request_uri = Printf.sprintf "%s/_internal_/%s" base_url uri
               }
             }
           } in
           (Some page, (`internal uri), winfo)
         else
           (None, `user, winfo)
       in
       let cookie = HttpServer.get_cookie winfo.HttpServerTypes.request in
       let page =
         match page with
         | Some str -> int_of_string str
         | None -> Random.int 1073741823 in
       let key = Client.key cookie page in
       let context = BslUtils.create_ctx
         (`client (Client.key_to_record key))
         (Some (winfo.HttpServerTypes.request, winfo.HttpServerTypes.connection))
       in
       let cont_with_context =
         QmlCpsServerLib.with_thread_context context
           (QmlCpsServerLib.cont_ml (fun _ -> ())) in
       let need_cpr f =
         let str = (get_request_params (winfo.HttpServerTypes.request)) in
         (match string2json str with
          | Some json -> Some (f cookie page json)
          | None ->
              send_error winfo
                (Printf.sprintf "This request contains a bad formatted json (%s)" str);
              None)
       in
       match uri with
         (* Channel urls **********************)
       | `internal "chan/register" ->
           Option.default ()
             (need_cpr
                (fun c p r ->
                   match r with
                   | JS.Record [("to_register", to_register);
                                ("uri", JS.String uri);
                                ("body", JS.String body)] ->
                       if not (WebChannel.registers c p to_register) then
                         send_error winfo "Error on registering"
                       else
                         let winfo = {
                           winfo with
                             HttpServerTypes.request = {
                               winfo.HttpServerTypes.request with
                                 request_line = {
                                   winfo.HttpServerTypes.request.request_line with
                                     request_uri = uri
                                 };
                                 request_message_body = (Rcontent.ContentString body)
                             };
                         } in
                         aux_complete_dispatcher winfo
                   | _ -> send_error winfo "Bad formatted register")
                )

       | `internal "chan/send" ->
           ignore(Ping.update_activity ~is_active:true key);
           Option.iter
             (fun () -> send_txt_response winfo "")
             (need_cpr (fun c p r -> WebChannel.send c p r (Some context)))

       | `internal "chan/remove" ->
           ignore (need_cpr WebChannel.remove);
           send_json_response winfo (JS.Bool true)

       | `internal "chan/sharedaddr" ->
           Session.SharedChannel.addr @>
             (function (ip, port) ->
                let addr = Unix.string_of_inet_addr ip in
                let res = JS.Record [
                  ("addr", JS.String addr);
                  ("port", JS.Int port);
                ] in
                send_json_response winfo res)

       (* Ping urls *************************)
       | `internal "ping" ->
           Option.default ()
             (need_cpr
                (fun _ _ r ->
                   match r with
                   | JS.Int nb ->
                       Ping.ping key winfo nb
                   | _ -> send_error winfo "Bad formatted ping"
                )
             )

       | `internal "pang" ->
           Option.default ()
             (need_cpr
                (fun _ _ r ->
                   match r with
                   | JS.Record [("ping", JS.Int nb);
                                ("uri", JS.String uri);
                                ("body", JS.String body)] ->
                       Ping.pang key winfo nb true;
                       let winfo = {
                         winfo with
                           HttpServerTypes.request = {
                             winfo.HttpServerTypes.request with
                               request_line = {
                                 winfo.HttpServerTypes.request.request_line with
                                   request_uri = uri
                               };
                               request_header = HttpServer.remove_header (Accept_Encoding "") winfo.request.request_header;
                               request_message_body = (Rcontent.ContentString body)
                           };
                           cont =
                           (fun response ->
                              match response.HttpServerTypes.body with
                              | content ->
                                  Ping.return key nb (Rcontent.get_content content))
                              (*| Http_common.Result str ->
                                  Ping.return key nb str
                              | _ ->
                                  Logger.error "Unexpected result on panging";
                                  Ping.return key nb "Unexpected result on panging")*)
                       } in
                       aux_complete_dispatcher winfo
                   | JS.Int nb ->
                       Ping.pang key winfo nb false
                   | _ -> send_error winfo "Bad formatted pang"
                )
             )


       | `internal str ->
           ignore(Ping.update_activity ~is_active:true key);
           let get_id = Str.regexp "rpc_return/\\(.*\\)" in
           if ((Str.string_match get_id str 0) && ((Str.matched_string str) = str)) then
             let id = Str.matched_group 1 str in
             if BslRPC.RPC.return id (get_request_params winfo.HttpServerTypes.request) then (
               send_json_response winfo (JS.Bool true);
             ) else (
               send_error winfo (Printf.sprintf "Can't return this rpc %s" id);
             )
           else
             BslScheduler.push (fun () -> dispatcher winfo cont_with_context)

       (* User urls *************************)
       | `user ->
           BslScheduler.push (fun () -> dispatcher winfo cont_with_context)

  in QmlCpsServerLib.return k (QmlCpsServerLib.cont_ml aux_complete_dispatcher)

##register complete_dispatcher : string, (WebInfo.private.native -> void), WebInfo.private.native -> void
let complete_dispatcher base_url dispatcher winfo =
  let dispatcher a k =
    QmlCpsServerLib.return k (dispatcher a) in
  let r = ref None in
  let k = QmlCpsServerLib.cont_ml (fun x -> r := Some x) in
  complete_dispatcher_cps base_url dispatcher k;
  match !r with
  | None -> failwith ("dispatcher was not computed - Do you use no cps?")
  | Some wcont -> QmlCpsServerLib.execute wcont winfo
