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
(** Synchronous binding of the local db3 database backend
    to the generic db interface Badop.

    This backend forbids concurrent transactions. Consequently, all reads
    and writes can be performed at once, with no waiting and no overhead.
    If a transaction is started before the previous one is comitted,
    a conflict is signalled.
    There are two additional deviatious from the standard behaviour
    of transactions: you can't abort them, neither before nor after preparing,
    and all requested single writes are always carried out, even if they
    overwrite each other inside the same transaction. OTOH, lists of writes
    and deletes performed with [write_list] are simplified in the usual way
    and only the needed ones are executed.
*)
(* Not implemented yet
include (Badop.S with type revision = Common.rev)
*)
