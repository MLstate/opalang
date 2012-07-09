(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
(*
    @author Louis Gesbert
**)

open Cps.Ops

(* todo: extend with details *)
let read_op_to_string = function
  | Badop.Stat _ -> "Stat"
  | Badop.Contents _ -> "Contents"
  | Badop.Children _ -> "Children"
  | Badop.Revisions _ -> "Revisions"
  | Badop.Search _ -> "Search"

let write_op_to_string = function
  | Badop.Set _ -> "Set"
  | Badop.Clear _ -> "Clear"
  | Badop.Link _ -> "Link"
  | Badop.Copy _ -> "Copy"

module F (Bk: Badop.S) = struct
  type database = { db: Bk.database; db_pfx: string; mutable last_id: int }
  type transaction = { id: int; tr: Bk.transaction; pfx: string }
  type revision = Bk.revision

  let print pfx fmt = Printf.eprintf ("%s"^^fmt^^"[0m\n%!") pfx
  let new_id xdb = xdb.last_id <- xdb.last_id + 1; xdb.last_id

  (* todo: add option to print messages also at the end of operations *)

  let open_database options k =
    let pfx, bk_options = match options with
      | Badop.Options_Debug (pfx,bk_options) -> pfx, bk_options
      | _ -> assert false
    in
    print pfx "Opening database";
    Bk.open_database bk_options @> fun db -> { db = db; db_pfx = pfx; last_id = 0 } |> k

  let close_database xdb k =
    print xdb.db_pfx "Closing database";
    Bk.close_database xdb.db @> k

  let status xdb k = Bk.status xdb.db @> fun st -> Badop.Layer("Wrapper_template", st) |> k

  module Tr = struct
    let start xdb errk k =
      let id = new_id xdb in print xdb.db_pfx "Transaction %d: START" id;
      Bk.Tr.start xdb.db errk
      @> fun tr -> { tr = tr; id = id; pfx = xdb.db_pfx } |> k
    let start_at_revision xdb rev errk k =
      let id = new_id xdb in print xdb.db_pfx "Transaction %d: START (at revision %s)" id (Bk.Debug.revision_to_string rev);
      Bk.Tr.start_at_revision xdb.db rev errk
      @> fun tr -> { tr = tr; id = id; pfx = xdb.db_pfx } |> k
    let prepare xtr k =
      print xtr.pfx "Transaction %d: PREPARE" xtr.id;
      Bk.Tr.prepare xtr.tr @> fun (tr,ok) -> ({ xtr with tr = tr }, ok) |> k
    let commit xtr k =
      print xtr.pfx "Transaction %d: COMMIT" xtr.id;
      Bk.Tr.commit xtr.tr @> k
    let abort xtr k =
      print xtr.pfx "Transaction %d: ABORT" xtr.id;
      Bk.Tr.abort xtr.tr @> k
  end

  type 'which read_op = 'which Bk.read_op
  type 'which write_op = ('which,transaction,revision) Badop.generic_write_op

  let read xtr path read_op k =
    print xtr.pfx "%d> READ.%s at %s" xtr.id (read_op_to_string read_op) (Path.to_string path);
    Bk.read xtr.tr path read_op @> k

  let write xtr path write_op k =
    print xtr.pfx "%d> WRITE.%s at %s" xtr.id (write_op_to_string write_op) (Path.to_string path);
    Badop.Aux.map_write_op ~transaction:(fun xtr k -> xtr.tr |> k) ~revision:(fun r k -> r |> k) write_op
    @> fun write_op ->
      Bk.write xtr.tr path write_op
      @> fun resp ->
        Badop.Aux.map_write_op
          ~transaction:(fun tr k -> { xtr with tr = tr } |> k)
          ~revision:(fun r k -> r |> k)
          resp
        @> k

  let write_list xtr path_write_op_list k =
    print xtr.pfx "%d> WRITE_LIST:\n" xtr.id;
    let wr xtr (path, op) k =
      write xtr path op (fun resp -> Badop.Aux.result_transaction resp |> k)
    in
    Cps.List.fold wr xtr path_write_op_list k

  let node_properties xdb config k =
    print xdb.db_pfx "Set node configuration";
    Bk.node_properties xdb.db config @> k

  module Debug = struct
    let revision_to_string = Bk.Debug.revision_to_string
  end
end
