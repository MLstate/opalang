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
