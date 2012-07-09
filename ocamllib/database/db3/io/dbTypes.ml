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

(**
   Database structures.
   @author Raja Boujbel
*)

(**
   These types are used to represent the database in memory.
   Each kind of db-file is represented by a structure.
   These types are the interface between the phyisicall database,
   (IoManager) and the rest of the modules working on the database.

   These definitions was historically in [IoManager] but are better in
   this file without mli.
*)

type eid = Eid.t
type uid = Uid.t
type rev = Revision.t

(**
   Used for caching size of some lists, or maps
*)
type size = int

(**
   A position in a file. (in octet)
*)
type position = int

(** {6 Path} *)

(**
   The item of a path.
*)
type key = Keys.t

(**
   A db path
*)
type path = Path.t

(** {6 Datas} *)

(**
   Low level data
*)
type dataimpl = DataImpl.t

(**
   Content of a node.
*)
type data = Datas.t

(** {6 Nodes} *)

(**
   There are two sort of nodes:
   - the concrete node, containing all its infos, and datas.
   - just a diff between its last revision, and its current state.

   The delta nodes may be transformed into full nodes.
*)

(**
   Structure representing the file containing all the database nodes.
*)
type nodefile = Node.t UidMap.t

type uidmap = Uid.t RevisionMap.t EidMap.t

type index = (Path.t * float) list StringMap.t

(**
   Structure representing the arborescence of nodes in the database.
   and the index.
*)
type dbstate = {
  uidmap : uidmap ;
  index : index ;
}

(** {6 Queries} *)

(**
   Queries are intermidiate actions not yet performed on the database,
   stored in a transaction. Queries are applied to the database
   when the transaction is commited.
*)

type querymap = QueryMap.t

(** {6 Transactions} *)

type trans = {
  querymap : querymap ;
  read_only : bool ;
  remove_list : path list ;
}

(**
   Structure representing the file containing all the transactions.
*)
type transfile = trans list

(**
   Structure representing the file containing all the end positions
   in the other files.

   FIXME: change for a record.


 * - uid_rev position
 * - uid position
 * - node position
 * - timestamps position
 * - trans position

*)
type flagsfile =
    position list

(**
   Structure representing the last indexes used for unicity.
   Stored to avoid clashes during the generation of fresh indexes.
*)
type uidrevfile = {
  eid : eid ;
  uid : uid ;
  rev : rev
}

(**
   Configuration of the database.

   - Version of the database (db3 runtime).
   Used to know if the binaries of the database needs changes.

   - Snapshot: index of the last physical state of the database.
   This index may be smaller than the last Revision number if
   there are still transaction to replay. (e.g crash)
*)
type configfile = {
  version: int ;
  snapshot_rev : rev ;
}

(* Exceptions *)
type 'a posexc = 'a * position
exception CrashNode of nodefile posexc
exception CrashStateMap of uidmap posexc
exception CrashStateIndex of dbstate posexc
exception CrashTrans of transfile posexc
exception CrashUidRev of bool
exception CrashTimestamp

(*
(* Unused *)
type mesfichiers =
  | FlagFile of flagsfile
  | NodeFile of nodefile
  | UidFile of int (* position dans le node file : verifier l'histoire des fillblank *)
  | UidRevFile of uidrevfile
  | TransFile of transfile
  | DbStateFile of dbstatefile
  | TimestampFile of float
  | ConfigFile of int
*)
