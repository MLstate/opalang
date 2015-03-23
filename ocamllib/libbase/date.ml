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
(* depends *)
module List = BaseList

(* -- *)

(* http://en.wikipedia.org/wiki/Computus *)
let easter y =
  let a = y mod 19 in
  let b = y / 100 in
  let c = y mod 100 in
  let d = b / 4 in
  let e = b mod 4 in
  let f = (b + 8) / 25 in
  let g = (b - f + 1) / 3 in
  let h = (19 * a + b - d - g + 15) mod 30 in
  let i = c / 4 in
  let k = c mod 4 in
  let l = (32 + 2 * e + 2 * i - h - k) mod 7 in
  let m = (a + 11 * h + 22 * l) / 451 in
  let month = (h + l - 7 * m + 114) / 31 in
  let day = ((h + l - 7 * m + 114) mod 31) + 1 in
  month, day

open Unix
(* type date = Rfc1123 | Rfc850 | Asctime *)
let month = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]
let month_li = Array.to_list month

let fullmonth = [|"January"; "February"; "March"; "April"; "May"; "June"
                ; "July"; "August"; "September"; "October"; "November"; "December"|]
let date1 dt = Printf.sprintf "%02d %s %04d" dt.tm_mday month.(dt.tm_mon) (1900+dt.tm_year)
let date2 dt = Printf.sprintf "%02d-%s-%02d" dt.tm_mday month.(dt.tm_mon)
  (if dt.tm_year<100 then dt.tm_year else dt.tm_year-100)
let date3 dt = Printf.sprintf "%s %02d" month.(dt.tm_mon) dt.tm_mday
let time dt = Printf.sprintf "%02d:%02d:%02d" dt.tm_hour dt.tm_min dt.tm_sec
let wkday = [|"Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat"|]
let weekday = [|"Sunday"; "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday"|]
  (* let asctime dt = Printf.sprintf "%s %s %s %s" wkday.(dt.tm_wday) (date3 dt) (time dt) (year dt) *)
let rfc850 dt = Printf.sprintf "%s, %s %s GMT" weekday.(dt.tm_wday) (date2 dt) (time dt)
let rfc1123 dt = Printf.sprintf "%s, %s %s GMT" wkday.(dt.tm_wday) (date1 dt) (time dt)
let gmt_dec =
  let time = Time.now () in
  (Time.localtime time).tm_hour - (Time.gmtime time).tm_hour
let of_string (* rfc 1123 *) s =
  Scanf.sscanf s "%s %02d %s %04d %02d:%02d:%02d GMT"
    (fun _ day month year h min sec ->
       let month_cnv =
         match List.findi (fun i -> i = month) month_li with
         | Some s -> s
         | None -> failwith "Date.of_string"
       in
       Time.mktime ~year ~month:month_cnv ~day ~h:(h + gmt_dec) ~min ~sec ~ms:0
    )

let date f dt = match f with
  | `rfc1123 -> rfc1123 dt
  | `rfc850 -> rfc850 dt
  | `asctime -> "" (*asctime dt FIXME *)

let pretty_duration dur =
  if dur < 2. then
    Printf.sprintf "00:00:0%.3f" dur
  else if dur < 10. then
    Printf.sprintf "00:00:0%.2f" dur
  else if dur < 60. then
    Printf.sprintf "00:00:%.1f" dur
  else
    let dur_sec = int_of_float dur in
    let sec = dur_sec mod 60 in
    let dur_min = dur_sec / 60 in
    let min = dur_min mod 60 in
    let dur_hour = dur_min / 60 in
    let hour = dur_hour mod 24 in
    if dur_hour < 24 then
      Printf.sprintf "%02d:%02d:%02d" hour min sec
    else
      let dur_day = dur_hour / 24 in
      if dur_day < 2 then
        Printf.sprintf "%d day %d h %d min" dur_day hour min
      else if dur_day < 31 then
        Printf.sprintf "%d days %d h" dur_day hour
      else
        Printf.sprintf "%d days" dur_day
