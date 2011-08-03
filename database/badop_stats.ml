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

module F (Bk: Badop.S) = struct
  type database = Bk.database
  type transaction = Bk.transaction
  type revision = Bk.revision

  let transactions = ref 0
  let reads = ref 0
  let writes = ref 0
  let commits = ref 0
  let aborts = ref 0
  let time_start = ref 0.

  let open_database options k =
    time_start := Unix.gettimeofday ();
    Bk.open_database options @> k

  let close_database db k =
    let time_running = Unix.gettimeofday () -. !time_start in
    Printf.eprintf "Database statistics:\n";
    Printf.eprintf "Time running: %.2f seconds\n" time_running;
    Printf.eprintf "Transactions: %d started / %d commited / %d aborted / %d discarded\n"
      !transactions !commits !aborts (!transactions - !commits - !aborts);
    Printf.eprintf "Operations total: %d writes / %d reads\n" !writes !reads;
    Printf.eprintf "Operations per transaction: %.2f writes / %.2f reads\n"
      (float_of_int !writes /. float_of_int !transactions) (float_of_int !reads /. float_of_int !transactions);
    Printf.eprintf "Per second: %.2f transactions / %.2f writes / %.2f reads\n"
      (float_of_int !transactions /. time_running)
      (float_of_int !writes /. time_running) (float_of_int !reads /. time_running);
    Bk.close_database db @> k

  let status db k = Bk.status db @> fun st -> Badop.Layer("Stat", st) |> k


  module Tr = struct
    let start db errk k = incr transactions; Bk.Tr.start db errk @> k
    let start_at_revision db rev errk k = incr transactions; Bk.Tr.start_at_revision db rev errk @> k
    let prepare tr k = Bk.Tr.prepare tr @> k
    let commit tr k = incr commits; Bk.Tr.commit tr @> k
    let abort tr k = incr aborts; Bk.Tr.abort tr @> k
  end

  type 'which read_op = 'which Bk.read_op
  type 'which write_op = 'which Bk.write_op

  let read tr path read_op k = incr reads; Bk.read tr path read_op @> k
  let write tr path write_op k = incr writes; Bk.write tr path write_op @> k
  let write_list tr path_write_op_list k =
    writes := !writes + List.length path_write_op_list;
    Bk.write_list tr path_write_op_list @> k

  let node_properties db config k = Bk.node_properties db config @> k

  module Debug = struct
    let revision_to_string = Bk.Debug.revision_to_string
  end
end
