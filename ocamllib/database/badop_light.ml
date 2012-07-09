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

module F (DB : DbSig.DB) : Badop.S =
struct

  module D = Badop_lib
  module Node_property = Badop_structure.Node_property
  module Session_light = Session_light.Session_light_(DB)
  module Transaction_light = Session_light.Transaction_light

  type 'a answer = [ `Answer of 'a | `Absent | `Linkto of Badop.path ]
  (** This module provides a simple db, no specific results to handle *)

  type database = { session: Session_light.t; file: string; mutable node_config : Node_property.config }
  type transaction = { db: database; tr: Transaction_light.t }

  let (@>) f x = f x
  let (|>) x f = f x

  let open_database options k =
    let options =
      match options with
      | Badop.Options_Light options -> options
      | _ -> assert false in
    let open_db ?ondemand ?direct ?max_size p =
      Logger.log ~color:`red "DB-LIGHT : open_database: ondemand=%s" (Option.to_string string_of_bool options.Badop.ondemand);
      Logger.log ~color:`red "DB-LIGHT : open_database: direct=%s" (Option.to_string string_of_bool options.Badop.direct);
      Logger.log ~color:`red "DB-LIGHT : open_database: max_size=%s" (Option.to_string string_of_int options.Badop.max_size);
      Session_light.open_db p ?ondemand ?direct ?max_size
    in
    let open_db ?ondemand ?direct p =
      match options.Badop.max_size with
      | Some max_size -> open_db ?ondemand ?direct ~max_size(*:1024*) p
      | None -> open_db ?ondemand ?direct p
    in
    let open_db ?ondemand p =
      match options.Badop.direct with
      | Some direct -> open_db ?ondemand ~direct(*:true*) p
      | None -> open_db ?ondemand p
    in
    let open_db p =
      match options.Badop.ondemand with
      | Some ondemand -> open_db ~ondemand(*:true*) p
      | None -> open_db p
    in
    let path = options.Badop.lpath in
    open_db path |> fun (db,_) -> { session = db; file = path; node_config = [] } |> k

  let close_database db k =
    Session_light.close_db db.session |> k

  let status db k = Badop.Light db.file |> k

  let start_time = ref 0.0

  module Tr = struct
    let start db _errk k =
      (*Logger.debug "Badop_light.Tr.start";*)
      #<If:BADOP_DEBUG$minlevel 10>start_time := Unix.gettimeofday ()#<End>;
      { db = db; tr = Session_light.new_trans db.session } |> k

    let start_at_revision db _rev _errk k =
      { db = db; tr = Session_light.new_trans (*~read_only:(true, Some rev)*) db.session } |> k

    let prepare trans k =
      (*Logger.debug "Badop_light.Tr.prepare";*)
      (* Executes [k] as soon as prepare finished, asynchronously, nonblocking.
         When prepare is postponed and stored on the FIFO,
         the continuation is stored as well. The exceptions from [k]
         are never caught here. *)
      if Transaction_light.modified trans.tr then
        Session_light.try_trans_prepare trans.db.session trans.tr
        (fun (tr, b) -> ({db = trans.db; tr = tr}, b) |> k)
      else
        (* Non-modifying trans, so nothing to do; commit will be void, too. *)
        ({db = trans.db; tr = trans.tr}, true) |> k

    let commit trans k =
      (*Logger.debug "Badop_light.Tr.commit";*)
      if Transaction_light.modified trans.tr then
        (* Assumption: [trans] is prepared by [execute_trans_prepare].
           Here some continuations of [prepare] may be executed, but only in case
           when some transactions are on the FIFO and are being prepared
           after the actual commit is completed. *)
        Session_light.really_commit trans.db.session trans.tr
        |> (fun tf ->
              #<If:BADOP_DEBUG$minlevel 10>Logger.debug "DB-LIGHT : Badop_light.commit: time=%f\n%!"
                                                        ((Unix.gettimeofday()) -. !start_time)#<End>;
              tf |> k)
      else
        true |> k

    let abort trans k = Session_light.abort_or_rollback trans.db.session trans.tr |> k
  end

  type revision = Revision.t

  (** All the operations that query the db *)
  type 'which read_op = ('which,revision) Badop.generic_read_op

  let read trans path op k =
    match op with
    | Badop.Stat (D.Query () as q) ->
        (*Logger.debug "Badop_light.read Stat";*)
        (try `Answer (Badop.Stat (D.Dialog_aux.respond q (Session_light.stat trans.db.session trans.tr path)))
         with Session_light.UnqualifiedPath -> `Absent) |> k
    | Badop.Contents (D.Query () as q) ->
        (*Logger.debug "Badop_light.read Contents";*)
        (try `Answer (Badop.Contents (D.Dialog_aux.respond q (Session_light.get trans.db.session trans.tr path)))
         with Session_light.UnqualifiedPath -> `Absent) |> k
    | Badop.Children (D.Query range as q) ->
        (*Logger.debug "Badop_light.read Children";*)
        (try
           `Answer
             (Badop.Children
                (D.Dialog_aux.respond q
                   (Session_light.get_children trans.db.session trans.tr range path)))
         with Session_light.UnqualifiedPath -> `Absent) |> k
    | Badop.Revisions (D.Query _range as q) ->
        (*Logger.debug "Badop_light.read Revisions";*)
        (try
           `Answer
             (Badop.Revisions
                (D.Dialog_aux.respond q
                   ((* current revision *)
                     [Session_light.get_rev trans.db.session]
                   |> List.map (fun rev -> rev, Session_light.get_timestamp trans.db.session))))
         with Session_light.UnqualifiedPath -> `Absent) |> k
    | Badop.Search (D.Query (words, _range_FIXME) as q) ->
        (*Logger.debug "Badop_light.read Search";*)
        (try
           `Answer
             (Badop.Search
                (D.Dialog_aux.respond q
                   (Session_light.full_search trans.db.session trans.tr words path)))
           (* FIXME: limit number of results *)
         with Session_light.UnqualifiedPath -> `Absent) |> k
    | _ -> assert false (* _ (Response _) can't happen (ensured by typing) *)

  (** All the operations that write to the db *)
  type 'which write_op = ('which,transaction,revision) Badop.generic_write_op

  let write trans path op k =
    match op with
    | Badop.Set (D.Query data as q) ->
        (*Logger.debug "Badop_light.write Set";*)
        Badop.Set (D.Dialog_aux.respond q { trans with tr = Session_light.set trans.db.session trans.tr path data }) |> k
    | Badop.Clear (D.Query () as q) ->
        (*Logger.debug "Badop_light.write Clear";*)
        Badop.Clear
          (D.Dialog_aux.respond q
             (try
                { trans with tr = Session_light.remove trans.db.session trans.tr path }
              with Session_light.UnqualifiedPath -> trans)) |> k
    | Badop.Link (D.Query linkpath as q) ->
        (*Logger.debug "Badop_light.write Link";*)
        Badop.Link
          (D.Dialog_aux.respond q
             { trans with tr = Session_light.set_link trans.db.session trans.tr path linkpath }) |> k
    | Badop.Copy (D.Query (copypath,copyrev) as q) ->
        (*Logger.debug "Badop_light.write Copy";*)
        Badop.Copy
          (D.Dialog_aux.respond q
             { trans with tr = Session_light.set_copy trans.db.session trans.tr path (copypath, copyrev) }) |> k
    | _ -> assert false (* _ (Response _) can't happen (ensured by typing) *)

  let write_list trans path_op_list k =
    let wr trans (path, op) k =
      write trans path op (fun resp -> Badop.Aux.result_transaction resp |> k)
    in
    Cps.List.fold wr trans path_op_list k

  let node_properties db config k =
    (match db.node_config with
    | [] ->
      (*#<If:BADOP_DEBUG$minlevel 10>
        Printf.printf "Set node config\n%s\n%!" (Node_property.StringOf.config config) #<End>;*)
      db.node_config <- config
    | nc ->
        if nc <> config then
          ((*#<If:BADOP_DEBUG$minlevel 5> Printf.eprintf "Try to set another config, refuse\n%!" #<End>;*)
          failwith "Badop local: Invalid config"));
    () |> k

  module Debug = struct
    let revision_to_string = Revision.to_string
    let path_to_string = Path.to_string
  end

end

module WithDbm = F(DbmDB)
