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
(** Binding of the local db3 db backend to the generic db interface Badop.

    Remarks:

    Of all the transaction operations, only [prepare] can execute
    its continuation asynchronously, as soon as previous, blocking commits
    are done with. If the preparation fails, the continuations is still
    executed, with the boolean value at [false], indicating failure.

    The abort operation, in case of prepared transactions, does not
    only abort them, but rolls back any state changes done by prepare.
    This functionality is not reflected in the name, because it may only
    be useful if a timeout expired between prepare and commit operation
    and only if we decide to rollback and not commit in such a situation.
    Still, this functionality is nice for low-level tests of rollbacks.

    The [read (Badop.Revisions)] query, for now, should return all revisions
    for a deleted node and only revisions since the last deletion for
    a live node. To be changed in the future.
*)

include Badop.S
