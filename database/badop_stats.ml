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
