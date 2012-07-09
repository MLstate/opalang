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
(*
    @author Adam Koprowski
**)

(**
 *   This module introduces a notion of a cluster. A cluster is a bunch
 * of servers, where each server is uniquely identified.
 *   ClusterComm module builds on that and provides primitives for
 * communication within a cluster.
 *)

(**
 * A unique identification of a server. All operations where a server
 * needs to be identified use this type.
 *)
(* FIXME, include cluster footprint in [node_id], so that it cannot be
 * mixed up with sids from other clusters? *)
type node_id

(**
 * A total order on machine identifiers
 *)
module NodeOrder : (Abstr.SafeOrderedType with type t = node_id)

module NodeSet : (BaseSetSig.S with type elt = node_id)
type nodeset = NodeSet.t

module NodeMap : (BaseMapSig.S with type key = node_id)
type 'a nodemap = 'a NodeMap.t

(**
 * A type of a cluster (i.e. a collection of servers)
 *)
type t

type addr = Unix.sockaddr

(**
 * Initializes a cluster. Takes an address of the machine running
 * this function and of all the other machines in the cluster.
 *
 * WARNING: all the machines in a cluster must run this function
 * consistently (i.e. with the same group of addresses), which then
 * ensures that the process identifiers will be unique and correct.
 *)
(* FIXME, This of course is no good, big time. In the future we want
   a less painful way to set-up a cluster & in the long run,
   we want a cluster with re-configuration capabilities. *)
val init : ?me:addr -> addr list -> t

(**
 * Returns the [node_id] of the server it is run on.
 *)
exception MeUnknown
val me : t -> node_id

(**
 * Gives the [node_id]s of all the servers in the cluster
 *)
val all_server_ids : ?including_myself:bool -> t -> node_id list

(**
 * Gives a sid of a random server from the cluster.
 *)
val random_server_id : ?including_myself:bool -> t -> node_id

val get_addr : t -> node_id -> addr

val get_id : t -> addr -> node_id

(**
 * Gives the number of servers in the cluster
 * [servers_no () = List.length (all_servers ())], but may be
 * more efficient
 *)
val servers_no : ?including_myself:bool -> t -> int

(**
 * Converts [node_id] to integer... Paxos presentation assumes that [node_id]s
 * from a cluster are mapped to ints: 0..n-1 where [n] is the size of the cluster.
 *)
val node_id_to_int : node_id -> int

(**
 * Mainly for debugging.
 * Converts a [node_id] to a human-readable string, with server name and port.
 *)
val node_id_to_string : t -> node_id -> string
val node_id_to_debug_string : node_id -> string

val label : t -> string
