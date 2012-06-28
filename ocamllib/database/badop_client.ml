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
module Dialog = Badop_lib.Dialog
open Cps.Ops

include Badop_protocol.F
  (struct
     type spoken = Dialog.query
     type understood = Dialog.response
     type revision (* Abstract *)
   end)

module N = Hlnet

let open_database options k =
  let scheduler, remote, on_disconnect  = match options with
    | Badop.Options_Client (scheduler,(addr,port), on_disconnect) -> scheduler, N.Tcp (addr,port), on_disconnect
    | _ -> assert false
  in
  let on_disconnect () =
    Logger.error "Disconnected from %s" (N.endpoint_to_string remote);
    on_disconnect()
  in
  N.open_channel scheduler remote ~on_disconnect database_channel_spec @> k

let close_database db k = N.close_channel db |> k

let status db k =
  match N.local_of_channel db, N.remote_of_channel db with
  | N.Tcp (local_addr,_), N.Tcp (remote_addr,remote_port) ->
      (N.sendreceive' db (Status (Dialog.query ()))
         (fun _ -> Badop.Client (local_addr, (remote_addr, remote_port), Badop.Other "disconnected") |> k)
       @> function
       | Status (Dialog.Response st) ->
           Badop.Client (local_addr, (remote_addr, remote_port), st) |> k
       | _ -> assert false)
  | _ -> assert false

let tr_next tr k =
  if tr.last then
    let tr' = { tr with version = succ tr.version } in tr.last <- false; tr' |> k
  else
    N.sendreceive tr.channel (Fork (Dialog.query tr.version))
    @> function
    | Fork (Dialog.Response tr_channel) ->
        { channel = tr_channel; version = succ tr.version; last = true } |> k
    | _ -> N.panic tr.channel

module Tr = struct

  let start db errk k =
    N.sendreceive' db (Transaction (Dialog.query ())) errk
    @> function
    | Transaction (Dialog.Response tr_channel) ->
        N.on_disconnect tr_channel (fun () -> N.Disconnected (N.remote_of_channel db) |> errk);
        { channel = tr_channel; version = 0; last = true } |> k
    | _ -> N.panic db

  let start_at_revision db rev errk k =
    N.sendreceive' db (Transaction_at (Dialog.query rev)) errk
    @> function
    | Transaction_at (Dialog.Response tr_channel) ->
        N.on_disconnect tr_channel (fun () -> N.Disconnected (N.remote_of_channel db) |> errk);
        { channel = tr_channel; version = 0; last = true } |> k
    | _ -> N.panic db

  let prepare tr k =
    tr_next tr
    @> fun tr ->
      N.sendreceive tr.channel (Prepare (Dialog.query tr.version))
    @> function
    | Prepare (D.Response success) -> (tr, success) |> k
    | _ -> N.panic tr.channel

  let commit tr k =
    N.sendreceive tr.channel (Commit (Dialog.query tr.version))
    @> function
    | Commit (D.Response success) -> success |> k
    | _ -> N.panic tr.channel

  let abort tr k =
    N.sendreceive tr.channel (Abort (Dialog.query tr.version))
    @> function
    | Abort (D.Response ()) -> () |> k
    | _ -> N.panic tr.channel

end

let read tr path query k =
  N.sendreceive tr.channel (Read (path, Dialog.query (tr.version, query)))
  @> function
  | Read (path', D.Response result) -> assert (path = path'); result |> k
  | _ -> N.panic tr.channel

let write tr path query k =
  Badop.Aux.map_write_op ~transaction:(fun _tr k -> () |> k) ~revision:(fun x k -> x |> k) query
  @> fun query1 -> tr_next tr
  @> fun tr ->
    N.send tr.channel (Write (path, tr.version, query1));
    Badop.Aux.respond_set_transaction query tr |> k

let write_list tr l_path_query k =
  let paths,queries = List.split l_path_query in
  Badop.Aux.map_write_list_op ~transaction:(fun _tr k -> () |> k) ~revision:(fun x k -> x |> k) queries
  @> fun queries1 -> tr_next tr
  @> fun tr ->
    N.send tr.channel (WriteList (tr.version, Dialog.query (List.combine paths queries1)));
    tr |> k

let node_properties _db _config k =
  #<If:TESTING> () |> k #<Else>
  Printf.eprintf "[33m Can't set node properties on client \n%![0m"; () |> k #<End>

module Debug = struct
  let revision_to_string r = DebugPrint.print r
  let path_to_string = Path.to_string
end
