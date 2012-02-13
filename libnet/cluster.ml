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

(* depends *)
module List = BaseList
module String = BaseString

#<Debugvar:LIBNET_CLUSTER>

let (|>) = InfixOperator.(|>)

module Ux = Unix

(* FIXME, if we want to compact the messages sent over network, we
   probably could do with less bits for machine id (char would
   probably do, at least for Paxos) *)
type node_id = int

module NodeSet = IntSet
type nodeset = NodeSet.t

module NodeMap = IntMap
type 'a nodemap = 'a NodeMap.t

module NodeOrder = Abstr.IntOrder

type addr = Ux.sockaddr

type t =
    { my_id : node_id option
    ; cluster : addr NodeMap.t
    }

exception MeUnknown
let me dc =
  match dc.my_id with
  | Some x -> x
  | None-> raise MeUnknown

let get_addr dc id = NodeMap.find id dc.cluster

let sockaddr_to_string = function
  | Unix.ADDR_INET (addr, p) -> Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) p
  | Unix.ADDR_UNIX addr -> Printf.sprintf "Unix:%s" addr

(* FIXME, this is no good, change the structure to index by addresses and find it with a simple lookup *)
let get_id dc addr =
  let find_id nodeId node v =
    if node = addr then begin
      assert (v = -1);
      nodeId
    end else
      v
  in
  match NodeMap.fold find_id dc.cluster (-1) with
  | -1 -> failwith (Printf.sprintf "[Cluster] Unknown clunser ode... %s" (sockaddr_to_string addr))
  | i -> i

let node_id_to_debug_string = string_of_int

let node_id_to_string dc node_id =
  let addr = get_addr dc node_id in
  Printf.sprintf "<%s %s>" ("#" ^ string_of_int node_id |> Terminal.strong) (NetAddr.string_of_sockaddr addr)

let node_id_to_debug_string = string_of_int

let init ?me others =
  let addrs =
    match me with
    | Some x -> x::others
    | None -> others
  in
  let all = List.sort Pervasives.compare addrs in
  let my_sid =
    match me with
    | None -> None
    | Some x -> List.findi ((=) x) all in
  let n = List.length all in
  assert (List.uniq all = all);
  let sids = List.init n (fun i -> i) in
  let servers = List.combine sids all in
  let cluster = NodeMap.from_list servers in
  let dc =
    { my_id = my_sid
    ; cluster = cluster
    }
  in
  #<If>
  let print_sid sid =
    let s = Printf.sprintf "   <%s>: %s\n" (node_id_to_debug_string sid) (node_id_to_string dc sid) in
    if Some sid = my_sid then
      Terminal.emph s
    else
      s
  in
  Logger.info "cluster initialized with %d servers:\n%s" n (String.concat_map "\n" print_sid (NodeMap.keys cluster));
  #<End>;
  dc

let all_server_ids ?(including_myself = true) dc =
  let all = NodeMap.keys dc.cluster in
  if including_myself then
    all
  else
    match dc.my_id with
    | None -> all
    | Some x -> List.remove_all x all

let random_server_id ?including_myself dc =
  let all = all_server_ids ?including_myself dc in
  List.choose_random all

let servers_no ?including_myself dc =
  List.length (all_server_ids ?including_myself dc)

let all_server_endpoints ?including_myself dc =
  let ss = all_server_ids ?including_myself dc in
  List.map (get_addr dc) ss

let node_id_to_int node_id = node_id

let label dc =
  let id = string_of_int (me dc) in
  "{Cluster@" ^ Terminal.strong id ^ "}"
