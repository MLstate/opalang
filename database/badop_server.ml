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

  let rec transaction_callback (backend_trans: Backend.transaction) transaction
      : (D.Dialog.query transaction_op
         -> (D.Dialog.response transaction_op -> unit) -> unit)
    =
    fun request k ->
      match request with
      | Read (path, (D.Query op as query)) ->
          Backend.read backend_trans path op
          @> fun resp -> Read (path, D.Dialog_aux.respond query resp) |> k
      | Write (path, op) ->
          Badop.Aux.map_write_op (* From Protocol.transaction to Backend.transaction *)
            ~revision:(fun r k -> r |> k)
            ~transaction:(fun _ k -> assert(not true); backend_trans |> k)
            op
          @> fun op ->
            Backend.write backend_trans path op
            @> fun backend_response ->
              Badop.Aux.map_write_op (* From Backend.transaction to Protocol.transaction *)
                ~revision:(fun r k -> r |> k)
                ~transaction:(fun backend_trans k ->
                   let transaction = N.dup transaction transaction_channel_spec in
                   N.setup_respond transaction (transaction_callback backend_trans transaction);
                   transaction |> k)
                backend_response
              @> fun resp -> Write (path,resp) |> k
      | WriteList l ->
          let l_q =
            match l with
            | D.Query l_q -> l_q
            | D.Response _ -> assert false
          in
          let l_op = List.map snd l_q in
          Badop.Aux.map_write_list_op (* From Protocol.transaction to Backend.transaction *)
            ~revision:(fun r k -> r |> k)
            ~transaction:(fun _ k -> assert(not true);
                            backend_trans |> k)
            l_op
          @> (fun l_op ->
                let l_zip = List.combine (List.map fst l_q) l_op in
                Backend.write_list backend_trans l_zip
                @> (fun backend_trans ->
                      (* From Backend.transaction to Protocol.transaction *)
                      let transaction = N.dup transaction transaction_channel_spec in
                      N.setup_respond
                        transaction
                        (transaction_callback backend_trans transaction);
                      WriteList (D.Dialog_aux.respond l transaction) |> k))
      | Prepare (D.Query () as query) ->
          Backend.Tr.prepare backend_trans
          @> fun (backend_trans,success) ->
            let transaction = N.dup transaction transaction_channel_spec in
            N.setup_respond transaction (transaction_callback backend_trans transaction);
            Prepare (D.Dialog_aux.respond query (transaction, success)) |> k
      | Commit (D.Query () as query) ->
          Backend.Tr.commit backend_trans
          @> fun resp -> Commit (D.Dialog_aux.respond query resp) |> k
      | Abort (D.Query () as query) ->
          Backend.Tr.abort backend_trans
          @> fun resp -> Abort (D.Dialog_aux.respond query resp) |> k
      | Read (_, D.Response _) | Prepare (D.Response _) | Commit (D.Response _) | Abort (D.Response _) ->
          assert false

  let main_callback db (channel: database) :
      D.Dialog.query database_query -> D.Dialog.response database_query Cps.t
    =
    fun request k ->
      let init_tr backend_tr k =
        let transaction = N.dup channel transaction_channel_spec in
        N.setup_respond transaction (transaction_callback backend_tr transaction);
        transaction |> k
      in
      match request with
      | Transaction (D.Query () as query) ->
          Backend.Tr.start db
            (fun _exc -> N.alert_channel channel)
          @> fun backend_tr -> init_tr backend_tr
          @> fun tr -> Transaction (D.Dialog_aux.respond query tr) |> k
      | Transaction_at (D.Query rev as query) ->
          Backend.Tr.start_at_revision db rev
            (fun _exc -> N.alert_channel channel)
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
