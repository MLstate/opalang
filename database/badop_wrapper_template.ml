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

(*
  Note: use OO to do this directly with inheritance ??
  could give some interesting properties...
*)

open Cps.Ops

module F (Bk: Badop.S) = struct
  type database = Bk.database
  type transaction = Bk.transaction
  type revision = Bk.revision

  let open_database options k = Bk.open_database options @> k
  let close_database db k = Bk.close_database db @> k
  let status db k = Bk.status db @> fun st -> Badop.Layer("Wrapper_template", st) |> k


  module Tr = struct
    let start db errk k = Bk.Tr.start db errk @> k
    let start_at_revision db rev errk k = Bk.Tr.start_at_revision db rev errk @> k
    let prepare tr k = Bk.Tr.prepare tr @> k
    let commit tr k = Bk.Tr.commit tr @> k
    let abort tr k = Bk.Tr.abort tr @> k
  end

  type 'which read_op = 'which Bk.read_op
  type 'which write_op = 'which Bk.write_op

  let read tr path read_op k = Bk.read tr path read_op @> k
  let write tr path write_op k = Bk.write tr path write_op @> k
  let write_list tr l_path_write_op k = Bk.write_list tr l_path_write_op @> k

  let node_properties db config k = Bk.node_properties db config @> k

  module Debug = struct
    let revision_to_string = Bk.Debug.revision_to_string
  end
end
