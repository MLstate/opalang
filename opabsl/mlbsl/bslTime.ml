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
(** Warning : the slicer and the serialization / unserialization is not ready
    to have abstract types for values which needs to be passed client <--> server
    So, the type time cannot be abstract here.

    Todo : json handling in libbsl *)

   ##extern-type time_t = int

   let unwrap = Time.in_milliseconds
   let wrap = Time.milliseconds

   ##register now : -> time_t
   let now () = unwrap (Time.now ())

   ##register process_utime : -> time_t
   let process_utime () = unwrap (Time.process_utime ())

   ##register process_stime : -> time_t
   let process_stime () = unwrap (Time.process_stime ())

   ##register process_cutime : -> time_t
   let process_cutime () = unwrap (Time.process_cutime ())

   ##register process_cstime : -> time_t
   let process_cstime () = unwrap (Time.process_cstime ())

   ##register gmt_msec : time_t -> int
   let gmt_msec t = Time.gmt_msec (wrap t)

   ##register gmt_sec : time_t -> int
   let gmt_sec t = Time.gmt_sec (wrap t)

   ##register gmt_min : time_t -> int
   let gmt_min t = Time.gmt_min (wrap t)

   ##register gmt_hour : time_t -> int
   let gmt_hour t = Time.gmt_hour (wrap t)

   ##register gmt_mday : time_t -> int
   let gmt_mday t = Time.gmt_mday (wrap t)

   ##register gmt_mon : time_t -> int
   let gmt_mon t = Time.gmt_mon (wrap t)

   ##register gmt_year : time_t -> int
   let gmt_year t = Time.gmt_year (wrap t)

   ##register gmt_wday : time_t -> int
   let gmt_wday t = Time.gmt_wday (wrap t)

   ##register gmt_yday : time_t -> int
   let gmt_yday t = Time.gmt_yday (wrap t)

   ##register gmt_isdst : time_t -> bool
   let gmt_isdst t = Time.gmt_isdst (wrap t)

   ##register local_msec : time_t -> int
   let local_msec t = Time.local_msec (wrap t)

   ##register local_sec : time_t -> int
   let local_sec t = Time.local_sec (wrap t)

   ##register local_min : time_t -> int
   let local_min t = Time.local_min (wrap t)

   ##register local_hour : time_t -> int
   let local_hour t = Time.local_hour (wrap t)

   ##register local_timezone_offset : -> int
   let local_timezone_offset _ = Time.local_timezone_offset()

   ##register local_mday : time_t -> int
   let local_mday t = Time.local_mday (wrap t)

   ##register local_mon : time_t -> int
   let local_mon t = Time.local_mon (wrap t)

   ##register local_year : time_t -> int
   let local_year t = Time.local_year (wrap t)

   ##register local_wday : time_t -> int
   let local_wday t = Time.local_wday (wrap t)

   ##register local_yday : time_t -> int
   let local_yday t = Time.local_yday (wrap t)

   ##register local_isdst : time_t -> bool
   let local_isdst t = Time.local_isdst (wrap t)

   ##register mktime : int, int, int, int, int, int, int -> time_t
   let mktime year month day h min sec ms = unwrap (Time.mktime ~year ~month ~day ~h ~min ~sec ~ms)

   ##register import_t : time_t -> int
   let import_t t = t

   ##register export_t : int -> time_t
   let export_t t = t

   ##register get_accurate_time : -> float
   let get_accurate_time = Time.get_accurate_time
