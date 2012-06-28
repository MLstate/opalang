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

open Cps.Ops
module L = Badop_locator
module Node_property = Badop_structure.Node_property

(* In here we introduce parallelism by dispatching requests among several
   servers, using the Locator module to select a host given a path (we say
   servers even though they may just be a local instance of Badop: that
   behaviour is mostly for testing).

   Several difficulties have to be taken care of:
   1) Have a way to recover all children of a given node
   2) Handling links (and copies) correctly
   3) Time-consistency

   (1) is needed for requests like Children or Search. The proposed solution is
   to keep the full list of children for any given node at some place ; an
   optimisation could be to just keep ranges and the servers they belong to, and
   have the Children query span among these servers.
   - write operation should return a status telling if the node was created or
   updated. If created, then (recursively) add it to the parent (if hosted on a
   different server). Children run on the parent has then no difficulty.
   - in the meantime we can first check for existence before any write, but
   that's a huge performance loss (at least until queries are grouped).
   - a very first prototype could query all servers for Children and Search;
   another one could always write to the parents (... and then until the root)
   for every node write.
   - when we have the Node_properties (see (2) below), we could do it just for
   variable-number-of-children nodes (Multi and Hidden nodes from the high-level
   schema).

   (2) will be handled by means of Node_properties, pushed to the low-level
   database at initialisation. Then we can check any path for parent nodes that
   could be links (eg, in /mylist/0/tl/0/tl/0/hd, we know that the 'tl'
   intermediate nodes should be links). Do a (read Status) on each of these in
   order to get their real location until we get the final resolved path ; then
   that path can be used to locate the real host.

   (3) if we start a meta-transaction M by a read on server A, a transaction trA
   is started on A inside M. If sometime later we want to read on server B,
   still inside M, we need to extend M with trB on server B. Problem is, in some
   cases it could be inconsistent to start trB at the latest possible
   revision. That would happen if B has any dependency to a revision on A
   posterior to the start of trA.
   - this is not critical and won't be handled in the near future.
   - a simple solution might be to push the max of all local revisions + 1 as
   the new revision number to all local dbs. It's an over-approximation but
   should work
   - a better solution could be to remember the start-revision of local
   transactions on each server and propagate them at commit-time. Then each
   server knows its dependencies towards each other's revision; and when
   propagating a transaction we can ask for "last revision that doesn't depend
   on anything further than this list of revisions my meta-transaction is
   already started on"
   - doing this using a special part of the db (map other_server => last_revision)
   that we write to at each commit, and using the history on that needs to be
   investigated, it sounds like a neat solution. (of course, for that we would
   need to be able to set the merge-policy of such nodes to TakeMax (or actually
   TakeSup and relying on our backend's revisions to form a lattice)).
*)

module F (Bk: Badop.S) = struct

  type local_transaction =
    | Tr_notstarted of Bk.database * Bk.revision option * (exn -> unit)
    | Tr_started of Bk.transaction
    | Tr_changed of Bk.transaction

  type local_revision =
    | Rev_now
    | Rev_fixed of Bk.revision

  type database = Bk.database L.t
  type revision = local_revision L.t
  type transaction_status = Fresh | Changed | Prepared | Committed | Aborted
  type transaction = { (* meta-transaction *)
    loc: local_transaction L.t;
    status: transaction_status ref;
  }

  (* general todo for below: make asynchronous (need to implement multi-cont) *)
  let open_database options k = match options with
    | Badop.Options_Dispatcher (flat_replication, bk_options) ->
        let loc = L.create ~flat_replication bk_options in
        L.map loc
          (fun o k -> Bk.open_database o @> k)
        @> k
    | _ -> assert false

  let close_database xdb =
    L.sequential_iter xdb Bk.close_database;
    fun _ -> ()

  let status xdb k =
    L.map xdb Bk.status
    @> fun stloc ->
      Badop.Layer_multi("Dispatcher", L.to_list stloc) |> k

  module Tr = struct
    let start xdb errk k =
      L.map xdb (fun db k -> Tr_notstarted (db,None,errk) |> k)
      @> fun loc -> {
        loc = loc;
        status = ref Fresh;
      } |> k
    let start_at_revision xdb rev errk k =
      L.mapi xdb
        (fun key db k ->
           let rev_opt = match L.get_key rev key with Rev_now -> None | Rev_fixed r -> Some r in
           Tr_notstarted (db, rev_opt, errk) |> k)
      @> fun loc -> {
        loc = loc;
        status = ref Fresh;
      } |> k

    let on_started f default loc k = match loc with
      | Tr_started tr | Tr_changed tr -> f tr @> k
      | Tr_notstarted _ -> default |> k

    let on_changed f default loc k = match loc with
      | Tr_changed tr -> f tr @> k
      | Tr_started _ | Tr_notstarted _ -> default |> k

    let abort xtr k =
      xtr.status := Aborted;
      L.iter xtr.loc (on_started Bk.Tr.abort ()) @> k

    let prepare xtr k = match !(xtr.status) with
      | Fresh | Prepared -> (xtr, true) |> k
      | Committed | Aborted -> (xtr, false) |> k
      | Changed ->
          L.map_reduce xtr.loc
            (fun acc (loc,ok) -> loc, acc && ok) true
            (fun loc k -> match loc with
             | Tr_changed tr ->
                 Bk.Tr.prepare tr
                 @> fun (tr,ok) -> (Tr_changed tr, ok) |> k
             | loc -> (loc, true) |> k)
          @> fun (loc,ok) ->
            if ok then
              ({ loc = loc; status = ref Prepared }, true) |> k
            else
              abort xtr @> fun () -> (xtr, false) |> k

    let commit xtr k = match !(xtr.status) with (* two-phase commit *)
      | Fresh -> xtr.status := Committed; true |> k
      | Committed | Aborted -> false |> k
      | Changed | Prepared ->
          prepare xtr
          @> function
          | xtr, true ->
              L.iter xtr.loc
                (on_started (fun tr k -> Bk.Tr.commit tr @> fun r -> assert(r); () |> k) ())
              @> fun () -> xtr.status := Committed; true |> k
          | xtr, false -> abort xtr @> fun () -> false |> k
  end

  type 'which read_op = ('which,revision) Badop.generic_read_op
  type 'which write_op = ('which,transaction,revision) Badop.generic_write_op

  let get_tr (push: local_transaction -> unit) ltr k = match ltr with
    | Tr_started tr | Tr_changed tr -> tr |> k
    | Tr_notstarted (db,None,errk) ->
        (* FIXME: start at a revision guaranteed consistent with the transactions that xtr already contains *)
        Bk.Tr.start db errk
        @> fun tr -> push (Tr_started tr); tr |> k
    | Tr_notstarted (db,Some rev,errk) ->
        Bk.Tr.start_at_revision db rev errk
        @> fun tr -> push (Tr_started tr); tr |> k

  let get_local_rev key rev k = match L.get_key rev key with
    | Rev_now -> assert false
    | Rev_fixed r -> r |> k

  let set_local_rev loc key bkrev k =
    L.mapi loc
      (fun key' _ k -> if key' = key then Rev_fixed bkrev |> k else Rev_now |> k)
    @> k

  let read xtr path read_op k =
    L.at_path xtr.loc path
      (fun key ltr k ->
         get_tr (L.push_key xtr.loc key) ltr
         @> fun tr -> Badop.Aux.map_read_op read_op ~revision:(get_local_rev key)
         @> fun bk_read_op -> Bk.read tr path bk_read_op
         @> function
         | `Answer resp ->
             Badop.Aux.map_read_op resp ~revision:(set_local_rev xtr.loc key)
             @> fun ans -> `Answer ans |> k
         | `Absent -> `Absent |> k
         | `Linkto p -> `Linkto p |> k)
    @> k

  let raw_write loc path ?(loc_path=path) write_op k =
    L.mapi_path loc loc_path
      (fun key ltr k ->
         get_tr (L.push_key loc key) ltr
         @> fun tr ->
           Badop.Aux.map_write_op write_op
             ~revision:(get_local_rev key)
             ~transaction:(fun _xtr k -> (assert false: Bk.transaction) |> k)
           @> fun bk_write_op -> Bk.write tr path bk_write_op
             @> fun resp -> Tr_changed (Badop.Aux.result_transaction resp) |> k)
    @> k

  let write xtr path write_op k =
    match !(xtr.status) with
    | Prepared | Committed | Aborted ->
        (* we're not allowed to change that transaction anymore, mark it as
           aborted (but with a new ref, to keep the parent sane) *)
        Badop.Aux.respond_set_transaction write_op { xtr with status = ref Aborted } |> k
    | Fresh | Changed ->
        (fun k -> match Path.pop_last path with
         | None -> xtr.loc |> k
         | Some (_,parent) ->
             (* register the existence of the node to its parent: write Unit /
                remove to the same path but on the server hosting the parent *)
             let query = match write_op with
               | Badop.Clear _ -> Badop.Clear (Badop_lib.Dialog.query ())
               | _ -> Badop.Set (Badop_lib.Dialog.query (DataImpl.Unit))
             in raw_write xtr.loc path ~loc_path:parent query @> k)
        @> fun loc -> raw_write loc path write_op
        @> fun loc ->
          Badop.Aux.respond_set_transaction write_op { loc = loc; status = ref Changed } |> k


  let write_list xtr path_write_op_list k =
        let wr xtr (path, op) k =
      write xtr path op @> fun resp -> Badop.Aux.result_transaction resp |> k
    in
    Cps.List.fold wr xtr path_write_op_list k

  let node_properties xtr config k = 
    (* TODO xtr.config <- config; *)
    L.iter xtr (fun db -> Bk.node_properties db config) @> k

  module Debug = struct
    let revision_to_string rev =
      Base.String.concat_map ~left:"[" ~right:"]" ","
        (function Rev_fixed r -> Bk.Debug.revision_to_string r | Rev_now -> "x")
        (L.to_list rev)
  end
end
