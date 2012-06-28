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
exception InvalidCommand
exception CommandError

val which : string -> string
val wget : string -> string
val tar : string -> string
val ps : string -> string
val mktemp : string -> string
val cp : string -> string

val system_call : string -> string * Unix.process_status

val get_return_code : Unix.process_status -> int
