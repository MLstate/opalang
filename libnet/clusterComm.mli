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

(**
 *   This module builds on the Cluster module and provides communication
 * primitives for all nodes in the cluster (using UDP)
 *)

(**
 * A type of a cluster (i.e. a collection of servers)
 *)
type ('out', 'in') t

type addr = Unix.sockaddr

(**
 * Initializes a cluster. Takes an addr of the machine running
 * this function and of all the other machines in the cluster.
 *
 * WARNING: all the machines in a cluster must run this function
 * consistently (i.e. with the same group of addresses), which then
 * ensures that the process identifiers will be unique and correct.
 *)
(* FIXME, This of course is no good, big time. In the future we want
   a less painful way to set-up a cluster & in the long run,
   we want a cluster with re-configuration capabilities. *)
val init : protocol:NetAddr.protocol -> Scheduler.t -> ?me:addr -> addr list -> ('out', 'in') t

val init_from : protocol:NetAddr.protocol -> Scheduler.t -> Cluster.t -> ('out', 'in') t

 (* FIXME, allow easy recognition of node_ids for messages from within the cluster *)
val register_msg_handler : ('out', 'in') t -> (('out', 'in') t -> addr -> 'in' -> unit) -> unit

(**
 * Cluster clean-up; closes all channels etc.
 *)
val close : ('out', 'in') t -> unit

(**
 * Returns the cluster structure.
 *)
val get_cluster : ('out', 'in') t -> Cluster.t

(**
 * Sends a value to a machine with given id.
 *)
val send : ('out', 'in') t -> Cluster.node_id -> 'out' -> unit Cps.t

(**
 * Sends a value to a given address (possibly of a machine
 * outside of the cluster.
 *)
val send_to : ('out', 'in') t -> addr -> 'out' -> unit Cps.t

(**
 * Sends a value to all the machines in the cluster.
 *)
val broadcast : ?including_myself:bool -> ('out', 'in') t -> 'out' -> unit Cps.t
