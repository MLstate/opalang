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
(** A layer than adds transaction support. Designed to work with a stack
    with the Badop_sync layer (which has only primitive transaction support)
    at the bottom, possibly with a clien-server link in-between.
    Should not be used on top of expensive transactions, such as those
    in Badop_local. Otherwise it's inefficient, because the transactions
    of the lower layers are aggregated in large numbers within the higher
    level transactions, just as primitive DB write operations.

    This is WIP. For now we know and subvert the structure of the lower layer.
*)

(* Not implemented yet
module F : functor (Backend: Badop.S
                      (* TODO: remove all these constraints, one by one. *)
                    with type database = Badop_sync.database
                    and type transaction = Badop_sync.transaction
                    and type revision = Badop_sync.revision
                   ) -> Badop.S

*)
