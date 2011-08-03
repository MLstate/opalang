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
(*
    @author Adam Koprowski
**)


#<Debugvar:LIBNET_CLUSTER>

let (|>) = InfixOperator.(|>)

module Sched = Scheduler
module Ux = Unix
module NA = NetAddr

type connection = Sched.connection_info

type ('out', 'in') t =
    { cluster : Cluster.t
    ; sched : Scheduler.t
    ; conn : connection
    }

type addr = Unix.sockaddr

let get_inet_addr = function
  | Ux.ADDR_UNIX _ -> failwith "[Cluster] cannot use local Unix file descriptors to initialize a cluster"
  | Ux.ADDR_INET (addr, _port) -> addr

let get_port = function
  | Ux.ADDR_UNIX _ -> failwith "[Cluster] cannot use local Unix file descriptors to initialize a cluster"
  | Ux.ADDR_INET (_addr, port) -> port

let init_from ~protocol sched cluster =
  let connection =
    let make_sched_connection fd addr =
      let addr = NA.mk_udp ~protocol ~fd ~addr in
      Scheduler.make_connection sched addr
    in
    let me =
      try
        Cluster.me cluster |> Cluster.get_addr cluster
      with
        Cluster.MeUnknown-> Ux.ADDR_INET (Unix.inet_addr_any, 0)
    in
    let listen_addr = Ux.ADDR_INET (Unix.inet_addr_any, get_port me) in
    let socket = Connection.listen ~socket_type:Connection.UDP listen_addr in
    make_sched_connection socket (get_inet_addr me)
  in
  let dc =
    { cluster = cluster
    ; conn = connection
    ; sched = sched
    }
  in
  dc

let init ~protocol sched ?me others =
  let cluster = Cluster.init ?me others in
  init_from ~protocol sched cluster

let register_msg_handler dc msg_handler =
  let rec read_one () = Scheduler.read_from dc.sched dc.conn callback
  and callback (_, addr, msg_str) =
    let msg = Marshal.from_string msg_str 0 in
(*      let nodeId = Cluster.get_id cluster addr in*)
    read_one ();
    #<If> Logger.debug "%s processing response from %s\n%!" (Cluster.label dc.cluster) (NA.string_of_sockaddr addr) #<End>;
    msg_handler dc addr msg
  in
  read_one ()

let get_cluster dc =
  dc.cluster

let close dc =
  Scheduler.remove_connection dc.sched dc.conn

let send_to_aux dc remote_addr v k =
  let msg = Marshal.to_string v [] in
  Scheduler.write_to dc.sched dc.conn remote_addr msg (fun _ -> k ())

let send_to dc remote_addr v k =
  #<If> Logger.debug "%s Sending msg to %s\n%!" (Cluster.label dc.cluster) (NA.string_of_sockaddr remote_addr) #<End>;
  send_to_aux dc remote_addr v k

let send dc id v k =
  let remote_addr = Cluster.get_addr dc.cluster id in
  #<If> Logger.debug "%s Sending msg to %s\n%!" (Cluster.label dc.cluster) (Cluster.node_id_to_string dc.cluster id) #<End>;
  send_to_aux dc remote_addr v k

let broadcast ?(including_myself = true) dc v k =
  let rec send_all = function
    | [] -> ()
    | id::ids ->
      let k =
        match ids with
        | [] -> k
        | _ -> fun _ -> ()
      in
      send dc id v k;
      send_all ids
  in
  send_all (Cluster.all_server_ids ~including_myself dc.cluster)
