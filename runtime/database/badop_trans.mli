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
