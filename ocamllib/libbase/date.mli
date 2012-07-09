(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)
val easter : int -> int * int
val month : string array
val fullmonth : string array
val date1 : Time.tm -> string
val date2 : Time.tm -> string
val date3 : Time.tm -> string
val of_string : string -> Time.t
val time : Time.tm -> string
val wkday : string array
val weekday : string array
val rfc850 : Time.tm -> string
val rfc1123 : Time.tm -> string
val date : [< `asctime | `rfc1123 | `rfc850 ] -> Time.tm -> string
val pretty_duration : float -> string
