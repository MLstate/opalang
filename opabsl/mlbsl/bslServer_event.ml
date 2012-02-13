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
##opa-type ServerEvent.event

let server_event_manager : (opa_serverevent_event -> ServerLib.ty_void QmlCpsServerLib.continuation -> unit) option ref = ref None
(*Note: [ServerEvent.event] is preprocessed to [opa_server_event_event]*)

(**
 * Initialize server-side error/event reporting.
 *
 * This function is called at client-initialization time.
*)
##register [cps-bypass] init: (ServerEvent.event, continuation(opa[void]) -> void), continuation(opa[void]) -> void
let init manager k =
  server_event_manager := Some (manager);
  QmlCpsServerLib.Ops.(|>) ServerLib.void  k


(**
 * Send an event to the central error/event reporting mechanism
*)
##register [cps-bypass] send: ServerEvent.event, continuation(opa[void]) -> void
let send event k =
   match !server_event_manager with
      | None   ->
          failwith "Internal error: Server event manager not initialized yet"
      | Some f ->
          f event k
