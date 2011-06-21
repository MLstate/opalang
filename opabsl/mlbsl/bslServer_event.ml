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
