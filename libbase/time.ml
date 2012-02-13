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

#<Ifstatic:OCAML_WORD_SIZE 64>

type t = int
type tm = Unix.tm

let infinity = max_int

let of_unix_time t = int_of_float (t *. 1000.)
let to_unix_time t = float_of_int t /. 1000.
let fmi = float_of_int max_int
let of_unix_time_inf t = let t1000 = t *. 1000.0 in if t1000 >= fmi then max_int else int_of_float t1000

let adjust_mktime res ms =
  if res < 0 then res - ms else res + ms

let zero = 0
let is_positive t = t > 0
let add t1 t2 = if t1 = infinity || t2 = infinity then infinity else t1 + t2
let difference t1 t2 = if t1 = infinity then zero else if t2 = infinity then infinity else t2 - t1

let milliseconds v = v
let in_milliseconds t = t
let in_seconds t = float_of_int t /. 1000.
let round_to_sec t = t - (t mod 1000)

let gmt_msec time = (abs time) mod 1000
let local_msec time = (abs time) mod 1000

#<Else>

type t = float
type tm = Unix.tm

let infinity = infinity

let of_unix_time t = t
let to_unix_time t = t
let of_unix_time_inf t = t

let adjust_mktime res ms =
  (if res < 0. then (-.) else (+.)) res (float_of_int ms /. 1000.)

let zero = 0.
let is_positive t = t > 0.
let add t1 t2 = t1 +. t2
let difference t1 t2 = t2 -. t1

let milliseconds v = float_of_int v /. 1000.
let in_milliseconds t = int_of_float (t *. 1000.)
let in_seconds t = t
let round_to_sec t = floor t

let gmt_msec time = int_of_float (snd (modf (abs_float time)) *. 1000.)
let local_msec time = gmt_msec time

#<End>

let now () = of_unix_time (Unix.gettimeofday ())

let process_utime () = of_unix_time (Unix.times ()).Unix.tms_utime
let process_stime () = of_unix_time (Unix.times ()).Unix.tms_stime
let process_cutime () = of_unix_time (Unix.times ()).Unix.tms_cutime
let process_cstime () = of_unix_time (Unix.times ()).Unix.tms_cstime

let sleep = Unix.sleep

let gmtime t = Unix.gmtime (to_unix_time t)
let localtime t = Unix.localtime (to_unix_time t)

let gmt_sec time = (gmtime time).Unix.tm_sec
let gmt_min time = (gmtime time).Unix.tm_min
let gmt_hour time = (gmtime time).Unix.tm_hour
let gmt_mday time = (gmtime time).Unix.tm_mday
let gmt_mon time = (gmtime time).Unix.tm_mon
let gmt_year time = (gmtime time).Unix.tm_year + 1900
let gmt_wday time = (gmtime time).Unix.tm_wday
let gmt_yday time = (gmtime time).Unix.tm_yday
let gmt_isdst time = (gmtime time).Unix.tm_isdst

let local_sec time = (localtime time).Unix.tm_sec
let local_min time = (localtime time).Unix.tm_min
let local_hour time = (localtime time).Unix.tm_hour
let local_mday time = (localtime time).Unix.tm_mday
let local_mon time = (localtime time).Unix.tm_mon
let local_year time = (localtime time).Unix.tm_year + 1900
let local_wday time = (localtime time).Unix.tm_wday
let local_yday time = (localtime time).Unix.tm_yday
let local_isdst time = (localtime time).Unix.tm_isdst

let local_timezone_offset () =
  let t = Unix.time() in
  let gmt = Unix.gmtime(t) in
  let local = Unix.localtime(t) in
  let (gmt_s, _) = Unix.mktime(gmt) in
  let (local_s, _) = Unix.mktime(local) in
  int_of_float((gmt_s -. local_s) /. 60.0);;

let mktime ~year ~month ~day ~h ~min ~sec ~ms =
  let res =
    of_unix_time (
      fst (
        Unix.mktime {
          Unix.tm_sec = sec ;
          Unix.tm_min = min ;
          Unix.tm_hour = h ;
          Unix.tm_mday = day ;
          Unix.tm_mon = month ;
          Unix.tm_year = year - 1900 ;
          Unix.tm_wday = 0 ;
          Unix.tm_yday = 0 ;
          Unix.tm_isdst = false
        }
      )
    )
  in
  adjust_mktime res ms

let bound = Chrono.bound


let is_infinite t = t = infinity

let is_after t1 t2 = t1 > t2
let is_before t1 t2 = t1 < t2

let seconds v = milliseconds (v * 1000)
let seconds_float = of_unix_time
let minutes v = seconds (v * 60)
let hours v = minutes (v * 60)
let days v = hours (v * 24)

let max = Pervasives.max
let min = Pervasives.min

let get_accurate_time = Unix.gettimeofday
