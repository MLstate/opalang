(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
