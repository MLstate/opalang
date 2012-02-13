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
