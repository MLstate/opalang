(*
    Copyright © 2011, 2012 MLstate

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
let unwrap = Time.in_milliseconds

##register process_utime : -> time_t
let process_utime () = unwrap (Time.process_utime ())

##register process_stime : -> time_t
let process_stime () = unwrap (Time.process_stime ())

##register process_cutime : -> time_t
let process_cutime () = unwrap (Time.process_cutime ())

##register process_cstime : -> time_t
let process_cstime () = unwrap (Time.process_cstime ())
