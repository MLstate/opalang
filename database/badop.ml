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
    @author Louis Gesbert
**)
(** This module describes the signature of database implementations available to
    client. It is shared by local, network and distributed implementations.

    Only contains types, a sig and a signed module: no mli needed.
*)

(** {2 Exported sub-modules and types} *)

module Data = DataImpl
module Structure = Badop_structure
module Key = Badop_structure.Key
module Path = Badop_structure.Path
module Dialog = Badop_lib.Dialog
module Node_property = Badop_structure.Node_property

(** Some shortcuts to very frequent types *)

type data = Data.t
type key = Structure.Key.t
type path = Structure.Path.t
type ('which, 'q, 'r) dialog = ('which, 'q, 'r) Dialog.t

(** {2 Shared types for requests} *)

(** Optional start point and number of results. 0 is unlimited. Negative means
    go down from the start. When start is None, goes from the beginning or the
    end, respectively, if n is >=0 or <0. *)
type 'a range = 'a option * int (* * 'a option -- TODO: add end point option, we need it alongside "max results" *)

(* TODO
(** This type describes a subset of a set of values, and is used by queries
    to restrict the results returned. *)
type 'a range = {
  first: 'a option; (** The value at which the results should start; from the beginning/end if [None] **)
  last: 'a option; (** Stop after reaching that value if specified *)
  count: int; (** Absolute value gives the maximum number of results wanted (unbounded if 0)
                  Sign gives the way of the traversal:
                  - if [count >= 0] go UP from [start] (or beginning if not specified)
                  - if [count < 0] go DOWN from [start] (or end if not specified) *)
  (** All requests should guarantee that
      - all results are within [\[first, last\]], in increasing order, if [count >= 0]
      - all results are within [\[last, first\]], in decreasing order, if [count < 0] *)
}
*)

(** All the operations that query the db (generic version, for all
    backends). They operate within a given transaction and at a given path. *)
type ('which,'revision) generic_read_op =
  | Stat of ('which, unit,
             path * 'revision option * [`Data|`Link|`Unset]) dialog
      (** Checks the path, returns the path after unwinding all links
          (including the ones inside copies), except the one in the node
          at the end of the path (if the node contains a link). Does not
          unwind copies (but finds and unwinds links inside them).
          Also returns the kind of the last node at so unwound path.
          Also returns the last revision when the node was modified,
          where None means "modified in the current transaction"
          and the node to determine the revision is taken after unwinding all,
          including the last one, links on the path.
          The Absent value is returned when any key of the path
          points to a non-existing node, including when
          the links or copies on the path are dangling,
          but not when the last node contains a dangling link. *)
  | Contents  of ('which, unit, data) dialog
      (** Returns the data contained in that node *)
  | Children  of ('which, key range, path list) dialog
      (** Returns the paths of the children of the node *)
  | Revisions of ('which, 'revision range, ('revision * Time.t) list) dialog
      (** Returns the list of revisions for a given node, with timestamps *)
  | Search    of ('which, string list * int range, key list) dialog
      (** Returns a list of keys from the path, which subtrees match the
          given words. Ordered by decreasing relevance. *)

(** All the operations that write to the db (generic version, for all
    backends). They operate in a given transaction, at a given path and return
    the modified transaction *)
type ('which,'transaction,'revision) generic_write_op =
  | Set   of ('which, data, 'transaction) dialog
      (** Write some data into a node *)
  | Clear of ('which, unit, 'transaction) dialog
      (** Removes a node from the database, making its subtree unreachable *)
  | Link  of ('which, path, 'transaction) dialog
      (** Create a (symbolic) link to [path] *)
  | Copy  of ('which, path * 'revision option, 'transaction) dialog
      (** Copy the whole subtree at [(path,revision)] *)

type 'a answer = [
  | `Answer of 'a
  | `Absent
  | `Linkto of path
]

(** A type for the introspection of a running badop *)
type status =
  | Local of string (* path where the files are stored *)
  | Client of Unix.inet_addr * (Unix.inet_addr * int) * status (* local, remote, remote status *)
  | Layer of string * status
  | Layer_multi of string * status list
  | Other of string

type local_options =
  { path : string; (** path *)
    revision : int option; (** recover db at revision *)
    restore : bool option; (** restore, and do or not a backup in case of recover*)
    dot : bool; (** output a the internal representation of the db in a dot each commit *)
    readonly : bool; (** open the database on readonly mode *)
  }

type options =
  | Options_Local of local_options
  | Options_Client of Scheduler.t * (Unix.inet_addr * int) * (unit -> [ `retry of Time.t | `abort ])
      (** scheduler, server, on_disconnect *)
  | Options_Debug of string * options (** debug line prefix, backend options *)
  | Options_Dispatcher of int * options list (** flat-replication factor, backends options *)

(** {2 The shared DB interface}

    For implementations of this interface, check:
    - Badop_db3: a binding to the simpler, local database
    - Badop_db4: a binding to the deprecated distributed fork of db3 prototype
    - Badop_client: forwards requests to a remote server
    - Badop_dispatcher: distributes requests among a set of backends
    - more to come ? We may have a Badop_replicated too, for two-level
    replication handling.

    @inline doc
*)
module type S = sig

  type database
  type transaction
  type revision

  val open_database: options -> database Cps.t
  val close_database: database -> unit Cps.t
  val status: database -> status Cps.t

  (** Transaction-handling functions are grouped in this module *)
  module Tr : sig
    val start: database -> transaction Cps.t

    val start_at_revision: database -> revision -> transaction Cps.t

    (** In [prepare] and [commit], the returned boolean is [true] for success. *)
    val prepare: transaction -> (transaction * bool) Cps.t

    val commit: transaction -> bool Cps.t

    (** The abort operation may be used on a running or prepared transaction *)
    val abort: transaction -> unit Cps.t
  end

  (** All the operations that query the db (specialised version for this backend) *)
  type 'which read_op = ('which,revision) generic_read_op

  (** All the operations that write to the db (specialised version for this backend) *)
  type 'which write_op = ('which,transaction,revision) generic_write_op

  (** Generic read function to query the database. Example of use: {[
      read tr path (Contents (query ()))
        (function
         | `Answer (Contents resp) -> Some (response resp) |> k
         | `Answer _ -> assert false
         | `Absent -> None |> k)
      ]}
  *)
  val read: transaction -> path -> Dialog.query read_op -> Dialog.response read_op answer Cps.t

  (** Generic write function to query the database *)
  val write: transaction -> path -> Dialog.query write_op -> Dialog.response write_op Cps.t

  (** Performing whole lists of writes at once, atomically. *)
  val write_list: transaction -> (path * Dialog.query write_op) list -> transaction Cps.t

  (** Extract node properties from given schema, use it on the db *)
  val node_properties : database -> Node_property.config -> unit Cps.t

(** Todo: add combiners for multiple reads (possibly chained), multiple writes
    and chained reads/writes *)

  module Debug : sig
    val revision_to_string: revision -> string
  end
end

(** This module provides helper functions on the shared types above *)
module Aux : sig

  (** Helper functions to extract and set back the resulting transaction from write operations *)
  val result_transaction: (Dialog.response,'transaction,'revision) generic_write_op -> 'transaction
  val respond_set_transaction:
    (Dialog.query,'transaction,'revision) generic_write_op -> 'transaction ->
    (Dialog.response,'transaction,'revision) generic_write_op

  (** Helper functions to apply transformations inside read/write operations (for
      use by database engines, esp. wrapping ones) *)

  val map_range: ('a -> 'b Cps.t) -> 'a range -> 'b range Cps.t

  val map_read_op:
    revision:('revision1 -> 'revision2 Cps.t) ->
    ('which, 'revision1) generic_read_op ->
    ('which, 'revision2) generic_read_op Cps.t

  val map_write_op:
    transaction:('transaction1 -> 'transaction2 Cps.t) ->
    revision:('revision1 -> 'revision2 Cps.t) ->
    ('which, 'transaction1, 'revision1) generic_write_op ->
    ('which, 'transaction2, 'revision2) generic_write_op Cps.t

  val map_write_list_op:
    transaction:('transaction1 -> 'transaction2 Cps.t) ->
    revision:('revision1 -> 'revision2 Cps.t) ->
    ('which, 'transaction1, 'revision1) generic_write_op list ->
    ('which, 'transaction2, 'revision2) generic_write_op list Cps.t

  (** checks if the two sets of options may conflict (bind to the same database twice) *)
  val options_conflict: options -> options -> bool

  (** Printers and debug helpers *)

  val path_to_string: path -> string

end = struct

  module D = Badop_lib
  open Cps.Ops

  (** Helper function to extract the resulting transaction from write operations *)
  let result_transaction write_op_response =
    match write_op_response with
    | Set (D.Response tr) | Clear (D.Response tr) | Link (D.Response tr) | Copy (D.Response tr) -> tr
    | Set (D.Query _) | Clear (D.Query _) | Link (D.Query _) | Copy (D.Query _) -> assert false

  let respond_set_transaction write_op_query tr =
    match write_op_query with
    | Set q -> Set (D.Dialog_aux.respond q tr)
    | Clear q -> Clear (D.Dialog_aux.respond q tr)
    | Link q -> Link (D.Dialog_aux.respond q tr)
    | Copy q -> Copy (D.Dialog_aux.respond q tr)


  (** Helper functions to apply transformations inside read/write operations (for
      use by database engines, esp. wrapping ones) *)

  let map_range f r k = match r with
    | (Some x, len) -> f x @> fun x -> (Some x, len) |> k
    | None, len -> (None, len) |> k

  let map_read_op
      ~(revision: 'revision1 -> 'revision2 Cps.t)
      (op: ('which, 'revision1) generic_read_op)
      : ('which, 'revision2) generic_read_op Cps.t =
    fun k -> match op with
    | Stat dialog ->
        D.Dialog_aux.map_dialog
          ~query:(fun () k -> () |> k)
          ~response:(fun d k ->
                       (fun (path, rev_opt, kind) k ->
                          Cps.Option.map revision rev_opt
                          @> fun rev_opt -> (path, rev_opt, kind) |> k)
                       d
                       @> k)
          dialog
        @> fun dialog -> Stat dialog |> k
    | Contents dialog ->
        D.Dialog_aux.map_dialog ~query:(fun () k -> () |> k) ~response:(fun d k -> d |> k) dialog
        @> fun dialog -> Contents dialog |> k
    | Children dialog ->
        D.Dialog_aux.map_dialog ~query:(fun r k -> r |> k) ~response:(fun l k -> l |> k) dialog
        @> fun dialog -> Children dialog |> k
    | Revisions dialog ->
        D.Dialog_aux.map_dialog
          ~query:(fun r k -> map_range revision r @> k)
          ~response:(fun l k ->
                       Cps.List.map
                         (fun (r,ts) k ->
                            revision r @> fun r -> (r, ts) |> k)
                         l
                       @> k)
          dialog
        @> fun dialog -> Revisions dialog |> k
    | Search dialog ->
        D.Dialog_aux.map_dialog ~query:(fun x k -> x |> k) ~response:(fun l k -> l |> k) dialog
        @> fun dialog -> Search dialog |> k

  let map_write_op
      ~(transaction: 'transaction1 -> 'transaction2 Cps.t)
      ~(revision: 'revision1 -> 'revision2 Cps.t)
      (op: ('which, 'transaction1, 'revision1) generic_write_op)
      : ('which, 'transaction2, 'revision2) generic_write_op Cps.t =
    fun k -> match op with
    | Set dialog ->
        D.Dialog_aux.map_dialog ~query:(fun d k -> d |> k) ~response:transaction dialog
        @> fun dialog -> Set dialog |> k
    | Clear dialog ->
        D.Dialog_aux.map_dialog ~query:(fun () k -> () |> k) ~response:transaction dialog
        @> fun dialog -> Clear dialog |> k
    | Link dialog ->
        D.Dialog_aux.map_dialog ~query:(fun p k -> p |> k) ~response:transaction dialog
        @> fun dialog -> Link dialog |> k
    | Copy dialog  ->
        D.Dialog_aux.map_dialog
          ~query:(fun (p, rev) k ->
                    match rev with
                    | None -> (p, None) |> k
                    | Some rev -> revision rev @> fun rev -> (p, Some rev) |> k)
          ~response:transaction
          dialog
        @> fun dialog -> Copy dialog |> k

  let map_write_list_op
      ~(transaction: 'transaction1 -> 'transaction2 Cps.t)
      ~(revision: 'revision1 -> 'revision2 Cps.t)
      (l_op: ('which, 'transaction1, 'revision1) generic_write_op list)
      : ('which, 'transaction2, 'revision2) generic_write_op list Cps.t =
    fun k ->
      let wr acc op k =
        map_write_op ~transaction ~revision op @> fun op -> op::acc |> k
      in
      Cps.List.fold wr [] l_op k

  let rec options_conflict o1 o2 = match (o1,o2) with
    | Options_Local l1, Options_Local l2 -> l1 = l2
    | Options_Client (_,remote1,_), Options_Client (_,remote2,_) -> remote1 = remote2
    | Options_Debug (_,o1), o2
    | o1, Options_Debug (_,o2) -> options_conflict o1 o2
    | Options_Dispatcher (_, ol), o
    | o, Options_Dispatcher (_, ol) ->
        List.exists (fun o1 -> options_conflict o1 o) ol
    | _ -> false

  let path_to_string = Path.to_string
end
