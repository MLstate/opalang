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
