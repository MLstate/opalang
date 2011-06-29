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

module Tr = struct

  let start db errk k =
    N.sendreceive' db (Transaction (Dialog.query ())) errk
    @> function
    | Transaction (Dialog.Response tr) ->
        N.on_disconnect tr (fun () -> N.Disconnected (N.remote_of_channel db) |> errk);
        tr |> k
    | _ -> N.panic db

  let start_at_revision db rev errk k =
    N.sendreceive' db (Transaction_at (Dialog.query rev)) errk
    @> function
    | Transaction_at (Dialog.Response tr) ->
        N.on_disconnect tr (fun () -> N.Disconnected (N.remote_of_channel db) |> errk);
        tr |> k
    | _ -> N.panic db

  let prepare tr k =
    N.sendreceive tr (Prepare (Dialog.query ()))
    @> function
    | Prepare (D.Response (tr',success)) -> (tr', success) |> k
    | _ -> N.panic tr

  let commit tr k =
    N.sendreceive tr (Commit (Dialog.query ()))
    @> function
    | Commit (D.Response success) -> success |> k
    | _ -> N.panic tr

  let abort tr k =
    N.sendreceive tr (Abort (Dialog.query ()))
    @> function
    | Abort (D.Response ()) -> () |> k
    | _ -> N.panic tr

end

let read tr path query k =
  N.sendreceive tr (Read (path, Dialog.query query))
  @> function
  | Read (path', D.Response result) -> assert (path = path'); result |> k
  | _ -> N.panic tr

let write tr path query k =
  N.sendreceive tr (Write (path, query))
  @> function
  | Write (path', result) ->
      assert (path = path');
      Badop.Aux.map_write_op
        ~revision:(fun x k -> x |> k)
        ~transaction:(fun chan k -> chan |> k)
        result
      @> k
  | _ -> N.panic tr

let write_list tr l_path_query k =
  N.sendreceive tr (WriteList (Dialog.query l_path_query))
  @> function
  | WriteList (D.Response l_path_result) ->
      l_path_result
      |> k
  | _ -> N.panic tr

let node_properties _db _config k =
  #<If:TESTING> () |> k #<Else>
  Printf.eprintf "[33m Can't set node properties on client \n%![0m"; () |> k #<End>

module Debug = struct
  let revision_to_string r = DebugPrint.print r
  let path_to_string = Path.to_string
end
