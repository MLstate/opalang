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
(*
    @author Adam Koprowski
**)

module List = Base.List

let (|>) = InfixOperator.(|>)

module NA = NetAddr

type prot_stats =
  { packets : int
  ; size : int
  }

type protocol = string

let make_protocol prot = prot

module CommMap = StringMap

type stats = prot_stats CommMap.t

type t =
  { started_at : Time.t
  ; started_procTime : float
  ; mutable send_stats : stats
  ; mutable recv_stats : stats
  }

let make () =
  let empty_stats = CommMap.empty in
  { started_at = Time.now ()
  ; started_procTime = Sys.time ()
  ; send_stats = empty_stats
  ; recv_stats = empty_stats
  }

let empty_prot_stats =
  { packets=0; size=0 }

let register_packet ~size ~conn stats =
  let protocol = NA.get_protocol conn in
  let update s =
    { packets = s.packets + 1
    ; size = s.size + size
    }
  in
  let old = CommMap.find_opt protocol stats
         |> Option.default empty_prot_stats
  in
  CommMap.add protocol (update old) stats

let register_recv ~size:_size ~conn:_conn _stats =
  #<If:SERVER_STATS>
    _stats.recv_stats <- register_packet ~size:_size ~conn:_conn _stats.recv_stats
  #<Else>
    ()
  #<End>

let register_send ~size:_size ~conn:_conn _stats =
  #<If:SERVER_STATS>
    _stats.send_stats <- register_packet ~size:_size ~conn:_conn _stats.send_stats
  #<Else>
    ()
  #<End>

let stats_header =
  Printf.sprintf "%20s -------------------- %s ------------------- -------------------- %s ------------------\n" ""
    (Terminal.strong "send") (Terminal.strong "recv")

let stats_separator =
  Printf.sprintf "%20s --------------------------------------------- --------------------------------------------\n" ""

let merge_stats m1 m2 =
  { packets = m1.packets + m2.packets
  ; size = m1.size + m2.size
  }

let padding n s =
  let len = n - String.length s in
  if len > 0 then String.make len ' ' else ""

let print_denomination f d max_l units =
  let rec aux f units =
    if f < float_of_int d || List.length units == 1 then
      let unit = List.hd units in
      let txt = Printf.sprintf "%5.1f %s" f unit in
      (Terminal.emph txt) ^ padding max_l txt
    else
      aux (f /. float_of_int d) (List.tl units)
  in
  aux f units

let print_packets () f =
  print_denomination f 1000 2 [""; "k"; "M"; "G"; "T"]

let print_bytes () f =
  print_denomination f 1000 2 ["b"; "kB"; "MB"; "GB"; "TB"]

let print_time () f =
  print_denomination f 60 3 ["sec"; "min"; "h"]

let print_category_stats time cat send recv =
  let print_stats () stats =
    let packets = float_of_int stats.packets in
    let bytes = float_of_int stats.size in
    let packets_sec = packets /. time in
    let bytes_sec = bytes /. time in
    Printf.sprintf "[ %a %a | %a/sec %a/sec ]" print_packets packets print_bytes bytes
      print_packets packets_sec print_bytes bytes_sec
  in
  Printf.sprintf "[%s%s] %a %a\n" (padding 18 cat) (Terminal.strong cat) print_stats send print_stats recv

let print_categories time categories send_stats recv_stats =
  let rec aux acc (send_totals, recv_totals) = function
    | [] ->
      acc, print_category_stats time "TOTAL" send_totals recv_totals
    | x::xs ->
      let get_stats map cat = CommMap.find_opt cat map |> Option.default empty_prot_stats in
      let send = get_stats send_stats x in
      let recv = get_stats recv_stats x in
      let acc' = acc ^ print_category_stats time x send recv in
      let send_totals' = merge_stats send_totals send in
      let recv_totals' = merge_stats recv_totals recv in
      aux acc' (send_totals', recv_totals') xs
  in
  aux "" (empty_prot_stats, empty_prot_stats) categories

let to_string stats =
  let total_time = Time.in_seconds (Time.difference
                                      stats.started_at (Time.now ()))
  in
  let cpu_time = Sys.time () -. stats.started_procTime in
  let load = cpu_time /. total_time in
  let categories = CommMap.keys stats.send_stats @ CommMap.keys stats.recv_stats
                |> List.sort String.compare
                |> List.uniq
  in
  let cat_stats, totals = print_categories total_time categories stats.send_stats stats.recv_stats in
  Printf.sprintf "Total run-time:  %s (CPU load:  %s)\n%s%s%s%s\n"
    (Terminal.emph (print_time () total_time))
    (Terminal.emph (Printf.sprintf "%.2f" load))
    stats_header cat_stats stats_separator totals
