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

module D = Badop_lib
module Node_property = Badop_structure.Node_property

type 'a answer = [ `Answer of 'a | `Absent | `Linkto of Badop.path ]
(** This module provides a simple db, no specific results to handle *)

type database = { session: Session.t; file: string; mutable node_config : Node_property.config }
type transaction = { db: database; tr: Transaction.t }

let (|>) x f = f x

let open_database options k =
  let options =
    match options with
    | Badop.Options_Local options -> options
    | _ -> assert false in
  let open_db ?readonly ?dot ?restore p =
    match options.Badop.revision with
    | Some rev -> Session.open_db ?readonly ?dot ?restore ~rev p
    | None -> Session.open_db ?readonly ?dot ?restore p
  in
  let open_db ?readonly ?dot p =
    match options.Badop.restore with
    | Some restore -> open_db ?readonly ?dot ~restore p
    | None -> open_db ?readonly ?dot p
  in
  let open_db ?readonly p =
    if options.Badop.dot then open_db ?readonly ~dot:true p
    else open_db ?readonly p
  in
  let open_db p =
    if options.Badop.readonly then open_db ~readonly:true p
    else open_db p
  in
  let path = options.Badop.path in

  open_db path |> fun (db,_) -> { session = db; file = path; node_config = [] } |> k

let close_database db k =
  Session.close_db db.session |> k

let status db k = Badop.Local db.file |> k

module Tr = struct
  let start db _errk k =
    { db = db; tr = Session.new_trans db.session } |> k

  let start_at_revision db rev _errk k =
    { db = db; tr = Session.new_trans ~read_only:(true, Some rev) db.session } |> k

  let prepare trans k =
    (* Executes [k] as soon as prepare finished, asynchronously, nonblocking.
       When prepare is postponed and stored on the FIFO,
       the continuation is stored as well. The exceptions from [k]
       are never caught here. *)
    if Transaction.modified trans.tr then
      Session.try_trans_prepare trans.db.session trans.tr
      (fun (tr, b) -> ({db = trans.db; tr = tr}, b) |> k)
    else
      (* Non-modifying trans, so nothing to do; commit will be void, too. *)
      ({db = trans.db; tr = trans.tr}, true) |> k

  let commit trans k =
    if Transaction.modified trans.tr then
      (* Assumption: [trans] is prepared by [execute_trans_prepare].
         Here some continuations of [prepare] may be executed, but only in case
         when some transactions are on the FIFO and are being prepared
         after the actual commit is completed. *)
      Session.really_commit trans.db.session trans.tr |> k
    else
      true |> k

  let abort trans k = Session.abort_or_rollback trans.db.session trans.tr |> k
end

type revision = Revision.t

(** All the operations that query the db *)
type 'which read_op = ('which,revision) Badop.generic_read_op

let read trans path op k = match op with
  | Badop.Stat (D.Query () as q) ->
      (try `Answer (Badop.Stat (D.Dialog_aux.respond q (Session.stat trans.tr path)))
       with Hldb.UnqualifiedPath -> `Absent) |> k
  | Badop.Contents (D.Query () as q) ->
      (try `Answer (Badop.Contents (D.Dialog_aux.respond q (Session.get trans.db.session trans.tr path)))
       with Hldb.UnqualifiedPath -> `Absent) |> k
  | Badop.Children (D.Query range as q) ->
      (try
         `Answer
           (Badop.Children
              (D.Dialog_aux.respond q
                 (Session.get_children trans.db.session trans.tr range path
                 |> List.map fst)))
       with Hldb.UnqualifiedPath -> `Absent) |> k
  | Badop.Revisions (D.Query range as q) ->
      (try
         `Answer
           (Badop.Revisions
              (D.Dialog_aux.respond q
                 ((if (range = (None, -1)
                       && (try ignore (Session.stat trans.tr path); true
                           with Hldb.UnqualifiedPath -> false))
                   then
                     (* current revision *)
                     [Session.get_rev trans.db.session]
                   else
                     (Session.get_all_rev_of_path trans.tr path
                     |> BaseList.filterbounds range Base.identity))
                 |> List.map (fun rev -> rev, Session.get_timestamp_from_rev trans.db.session rev))))
       with Hldb.UnqualifiedPath -> `Absent) |> k
  | Badop.Search (D.Query (words, _range_FIXME) as q) ->
      (try
         `Answer
           (Badop.Search
              (D.Dialog_aux.respond q
                 (Session.full_search trans.tr words path)))
         (* FIXME: limit number of results *)
       with Hldb.UnqualifiedPath -> `Absent) |> k
  | _ -> assert false (* _ (Response _) can't happen (ensured by typing) *)

(** All the operations that write to the db *)
type 'which write_op = ('which,transaction,revision) Badop.generic_write_op

let write trans path op k = match op with
  | Badop.Set (D.Query data as q) ->
      Badop.Set (D.Dialog_aux.respond q { trans with tr = Session.set trans.tr path data }) |> k
  | Badop.Clear (D.Query () as q) ->
      Badop.Clear
        (D.Dialog_aux.respond q
           (try
              { trans with tr = Session.remove trans.tr path }
            with Hldb.UnqualifiedPath -> trans)) |> k
  | Badop.Link (D.Query linkpath as q) ->
      Badop.Link
        (D.Dialog_aux.respond q
           { trans with tr = Session.set_link trans.tr path linkpath }) |> k
  | Badop.Copy (D.Query (copypath,copyrev) as q) ->
      Badop.Copy
        (D.Dialog_aux.respond q
           { trans with tr = Session.set_copy trans.db.session trans.tr path (copypath, copyrev) }) |> k
  | _ -> assert false (* _ (Response _) can't happen (ensured by typing) *)

let write_list trans path_op_list k =
  let wr trans (path, op) k =
    write trans path op (fun resp -> Badop.Aux.result_transaction resp |> k)
  in
  Cps.List.fold wr trans path_op_list k

let node_properties db config k =
  (match db.node_config with
  | [] ->
    #<If:BADOP_DEBUG$minlevel 10>
      Printf.printf "Set node config\n%s\n%!" (Node_property.StringOf.config config) #<End>;
    db.node_config <- config
  | nc ->
      if nc <> config then
        (#<If:BADOP_DEBUG$minlevel 5> Printf.eprintf "Try to set another config, refuse\n%!" #<End>;
        failwith "Badop local: Invalid config"));
  () |> k

module Debug = struct
  let revision_to_string = Revision.to_string
  let path_to_string = Path.to_string
end
