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
(* Migrate db file to current_version
 * Apply, one by one, all migrations usefull to have a coherent db.
 * If a step is not found (missing function for migration form x to x+1),
 * raise [Migration_error]. If an error occurs on the migration application,
 * raise [Migration_error] (backtrace available on debug mode)
 *)
val update : int -> int -> DbIo.t -> unit
