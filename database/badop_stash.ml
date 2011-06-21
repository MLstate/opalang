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

open Cps.Ops
module Dialog = Badop_lib.Dialog

module F (Bk: Badop.S) = struct
  type database = Bk.database
  type transaction_status = Fresh | Changed | Prepared | Committed | Failed
  type transaction = { (* Extended transaction (called xtr below) *)
    db: Bk.database;
    status: transaction_status;
    tr: Bk.transaction option;
    stash: (Badop.path * Dialog.query Bk.write_op) list;
  }
  type revision = Bk.revision

  let open_database = Bk.open_database
  let close_database = Bk.close_database
  let status db k = Bk.status db @> fun st -> Badop.Layer("Stash", st) |> k

  let get_tr xtr k = match xtr.tr with
    | None -> Bk.Tr.start xtr.db @> k
    | Some tr -> tr |> k

  let flush xtr k = match xtr.stash with
    | [] -> xtr |> k
    | l ->
        get_tr xtr
        @> fun tr -> Bk.write_list tr (List.rev l)
          @> fun tr ->
            { xtr with tr = Some tr; status = Changed; stash = [] } |> k

  module Tr = struct
    let start db k =
      { db = db; tr = None; status = Fresh; stash = [] } |> k
    let start_at_revision db rev k =
      Bk.Tr.start_at_revision db rev
      @> fun tr -> { db = db; tr = Some tr; status = Fresh; stash = [] } |> k
    let prepare xtr k =
      flush xtr
      @> fun xtr ->
        match xtr.status with
        | Changed ->
            get_tr xtr
            @> fun tr -> Bk.Tr.prepare tr
              @> fun (tr,ok) -> ({ xtr with tr = Some tr; status = if ok then Prepared else Failed}, ok) |> k
        | Fresh | Prepared -> (xtr,true) |> k
        | Failed | Committed -> (xtr,false) |> k
    let rec commit xtr k =
      match xtr.status with
      | Prepared ->
          assert(xtr.stash = []);
          get_tr xtr @> fun tr -> Bk.Tr.commit tr @> k
      | Changed ->
          prepare xtr
          @> fun (xtr,ok) -> if ok then get_tr xtr
            @> fun tr -> Bk.Tr.commit tr @> k else false |> k
      | Fresh ->
          if xtr.stash = [] then true |> k else flush xtr
            @> fun xtr -> commit xtr @> k
      | Committed -> true |> k
      | Failed -> false |> k

    let abort xtr k =
      match xtr.status with
      | Failed | Committed -> () |> k
      | _ -> match xtr.tr with Some tr -> Bk.Tr.abort tr @> k | None -> () |> k
  end

  type 'which read_op = 'which Bk.read_op
  type 'which write_op = ('which,transaction,revision) Badop.generic_write_op

  let read xtr path read_op k =
    flush xtr
    @> fun xtr ->
      get_tr xtr @> fun tr -> Bk.read tr path read_op @> k

  let write xtr path write_op k =
    Badop.Aux.map_write_op ~transaction:(fun xtr k -> get_tr xtr @> k) ~revision:(fun r k -> r |> k) write_op
      (* only for types, no tr in queries *)
    @> fun bk_write_op ->
      Badop.Aux.respond_set_transaction write_op { xtr with stash = (path,bk_write_op)::xtr.stash }
      |> k

  let write_list xtr path_write_op_list k =
    let wr xtr (path, op) k =
      write xtr path op @> fun resp -> Badop.Aux.result_transaction resp |> k
    in
    Cps.List.fold wr xtr path_write_op_list k

  let node_properties db config k = Bk.node_properties db config @> k

  module Debug = struct
    let revision_to_string = Bk.Debug.revision_to_string
  end
end
