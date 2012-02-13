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
(*
    @author Louis Gesbert
**)

module D = Badop_lib
open Cps.Ops

module N = Hlnet

module F = functor (Backend: Badop.S) ->
struct

  include Badop_protocol.F
    (struct
       type spoken = D.Dialog.response
       type understood = D.Dialog.query
       type revision = Backend.revision
     end)

  type t = {
    backend: Backend.database;
    endpoint: N.endpoint;
    scheduler: Scheduler.t;
  }

  let rec transaction_callback (transmap: Backend.transaction IntMap.t ref) channel
      : (D.Dialog.query transaction_op
         -> (D.Dialog.response transaction_op -> unit) -> unit)
    =
    fun request k ->
      match request with
      | Read (path, (D.Query (tr_version, op) as query)) ->
          Backend.read (IntMap.find tr_version !transmap) path op
          @> fun resp -> Read (path, D.Dialog_aux.respond query resp) |> k
      | Write (path, tr_next_version, op) ->
          Badop.Aux.map_write_op (* From Protocol.transaction to Backend.transaction *)
            ~revision:(fun r k -> r |> k)
            ~transaction:(fun () k -> assert false |> k)
            op
          @> fun op ->
            Backend.write (IntMap.find (pred tr_next_version) !transmap) path op
          @> fun backend_response ->
            let tr = Badop.Aux.result_transaction backend_response in
            transmap := IntMap.add tr_next_version tr !transmap (* no continuation needed *)
      | WriteList (tr_next_version, D.Query l_q) ->
          let l_paths,l_op = List.split l_q in
          Badop.Aux.map_write_list_op (* From Protocol.transaction to Backend.transaction *)
            ~revision:(fun r k -> r |> k)
            ~transaction:(fun _ k -> assert false |> k)
            l_op
          @> fun l_op ->
            Backend.write_list (IntMap.find (pred tr_next_version) !transmap) (List.combine l_paths l_op)
          @> fun tr ->
            transmap := IntMap.add tr_next_version tr !transmap (* no continuation needed *)
      | Prepare (D.Query tr_next_version as query) ->
          Backend.Tr.prepare (IntMap.find (pred tr_next_version) !transmap)
          @> fun (tr,success) ->
            transmap := IntMap.add tr_next_version tr !transmap;
            Prepare (D.Dialog_aux.respond query success) |> k
      | Commit (D.Query tr_version as query) ->
          Backend.Tr.commit (IntMap.find tr_version !transmap)
          @> fun resp -> Commit (D.Dialog_aux.respond query resp) |> k
      | Abort (D.Query tr_version as query) ->
          Backend.Tr.abort (IntMap.find tr_version !transmap)
          @> fun resp -> Abort (D.Dialog_aux.respond query resp) |> k
      | Fork (D.Query tr_version as query) ->
          let channel = N.dup channel transaction_channel_spec in
          let transmap = ref (IntMap.filter_keys ((<=) tr_version) !transmap) in
          N.setup_respond channel (transaction_callback transmap channel);
          Fork (D.Dialog_aux.respond query channel) |> k
      | Read (_, D.Response _) | WriteList (_, D.Response _) | Prepare (D.Response _)
      | Commit (D.Response _) | Abort (D.Response _) | Fork (D.Response _) ->
          assert false

  let main_callback db (channel: database) :
      D.Dialog.query database_query -> D.Dialog.response database_query Cps.t
    =
    fun request k ->
      let init_tr backend_tr k =
        let channel = N.dup channel transaction_channel_spec in
        N.setup_respond channel (transaction_callback (ref (IntMap.singleton 0 backend_tr)) channel);
        channel |> k
      in
      match request with
      | Transaction (D.Query () as query) ->
          Backend.Tr.start db
            (fun _exc -> N.panic channel)
          @> fun backend_tr -> init_tr backend_tr
          @> fun tr -> Transaction (D.Dialog_aux.respond query tr) |> k
      | Transaction_at (D.Query rev as query) ->
          Backend.Tr.start_at_revision db rev
            (fun _exc -> N.panic channel)
          @> fun backend_tr -> init_tr backend_tr
          @> fun tr -> Transaction_at (D.Dialog_aux.respond query tr) |> k
      | Status (D.Query () as query) ->
          Backend.status db
          @> fun st ->
            Status (D.Dialog_aux.respond query (Badop.Layer ("Server",st))) |> k
      | Transaction (D.Response _) | Transaction_at (D.Response _) | Status (D.Response _) ->
          assert false

  let listener (db: Backend.database) :
      database -> unit
    =
    fun channel -> N.setup_respond channel (main_callback db channel)

  let start scheduler bindto options k =
    N.listen scheduler bindto;
    Backend.open_database options
    @> fun db ->
      N.accept scheduler bindto database_channel_spec (listener db);
      { backend = db; endpoint = bindto; scheduler = scheduler; } |> k

  let stop { backend = db; endpoint = endpoint; scheduler = scheduler; } k =
    N.refuse scheduler endpoint; (* todo: ensure to close all channels bound to local endpoint *)
    Backend.close_database db
    @> k

end
