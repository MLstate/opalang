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

(** The format to export the database schema *)
type schema_output =
  | Gml (** The format used internally, needed by database_import *)
  | Dot (** Export to dot *)
  | Aa (** Print an ascii-art tree *)

(** There are two kind of operations permitted :
  * those which need to open the db, and do a transaction on it, using badop interface
  * those which don't need *)
type command_open =
  | Schema of schema_output
  | Dump of string
  | Import of string * string option (** Xml file, Gml schema *)
  | Config

type command_close =
  | Backup of string

type command =
  | Open of command_open
  | Close of command_close
  | Unset

type options = {
  command: command;
  time: Time.t option;
  backend: (module Badop.S) * Badop.options;
}

let options =
  let default = {
    command = Unset;
    backend = Badop_meta.default;
    time = None;
  } in
  let set_command o c = match o.command with
    | Unset -> { o with command = c }
    | _ ->
        Printf.eprintf "Error: conflicting arguments (you can only choose one of --schema, --dump or --import)\n";
        raise Exit
  in
  try
    let wrap_parser parse =
      fun o -> ServerArg.wrap (parse o.backend) (fun bo -> { o with backend = bo }) in
    let p =
      ServerArg.filter default
        (ServerArg.make_parser ~final:true "Database manipulation tool" (
           (["--schema";"-s"],
            ServerArg.func
              (ServerArg.option (ServerArg.stringset ["gml", Gml; "dot", Dot; "aa", Aa]))
              (fun o out -> set_command o (Open (Schema (Option.default Aa out)))),
            "[gml|dot|aa]", "Print the database schema in the given format to stdout")
           ::
           (["--config";"-c"],
              (ServerArg.func ServerArg.unit (fun o () -> set_command o (Open Config))),
              "", "Print the database node configuration to stdout")
           ::
           (["--dump";"-d"], ServerArg.func ServerArg.string (fun o s -> set_command o (Open (Dump s))),
            "<file>", "Dump the contents of the database to this XML file")
           ::
           (["--import";"-i"],
            ServerArg.func
              (ServerArg.pair ServerArg.string (ServerArg.option ServerArg.string))
              (fun o (xml,schema_opt) -> set_command o (Open (Import (xml,schema_opt)))),
            "<file> [schema]", "Import the database data from the given XML file, using the given GML schema (or the one from the database if unspecified).")
           ::
           (["--time"], ServerArg.func ServerArg.int (fun o i -> { o with time = Some (Time.seconds i) }),
            "<timestamp>", "Use the database as it was at the given timestamp, instead of now (for -s and -d only)")
           ::
           (["--backup"], ServerArg.func ServerArg.string (fun o s -> set_command o (Close (Backup s))),
            "<dirpath>", "Do a backup of the database, stored at the given path. Local databases only")
           ::
           List.map
             (fun (arg,parse,params,help) -> arg, wrap_parser parse, params, help)
             Badop_meta.options_parser
         ))
    in
    if not (ServerArg.is_empty (ServerArg.get_argv ())) then
      (Printf.eprintf "Error: unknown command-line argument: %s\n" (ServerArg.argv_to_string ());
       raise Exit)
    else if p.command = Unset then
      (Printf.eprintf "Error: you need to specify a command (either --schema, --dump or --import)\n";
       raise Exit)
    else
      p
  with Exit -> exit 1

module Db = (val (fst options.backend) : Badop.S)

module SimpleCpsBackend = struct
  type 'a continuation = 'a -> unit
  let mkcont _ = fun k -> k
  let return x k = k x
end

module DbSerializer = Xml_dump.F(SimpleCpsBackend)
module DbImporter = Xml_import.F(Db)(SimpleCpsBackend)


open Cps.Ops

let read_schema_from_db tr k =
  let path_schema = Badop.Path.of_list [ Badop.Key.IntKey 2; Badop.Key.IntKey 0 ] in
  let path_schema_version = Badop.Path.of_list [ Badop.Key.IntKey 2; Badop.Key.IntKey (-1) ] in
  Db.read tr path_schema_version (Badop.Contents (Badop.Dialog.query ()))
  @> function
  | `Answer (Badop.Contents (Badop.Dialog.Response (Badop.Data.Int version))) ->
      (if version > Dbgraph.version then
         (Printf.eprintf
            "Error: unexpected schema version %d (this program was built with version %d)\n"
            version Dbgraph.version;
          exit 2);
       Db.read tr path_schema (Badop.Contents (Badop.Dialog.query ()))
       @> function
       | `Answer (Badop.Contents (Badop.Dialog.Response (Badop.Data.Binary gml_schema))) ->
           Some gml_schema |> k
       | `Answer _ | `Linkto _ ->
           prerr_endline "Error: inconsistency while trying to read database schema";
           exit 2
       | `Absent -> None |> k)
  | _ ->
      None |> k

let get_transaction_at_revision db k =
  match options.time with
  | None ->
      Db.Tr.start db
        (fun exc -> Logger.critical "Database error: %s" (Printexc.to_string exc); exit 4)
      @> k
  | Some revision_timestamp ->
      Db.Tr.start db
        (fun exc -> Logger.critical "Database error: %s" (Printexc.to_string exc); exit 4)
      @> fun tr ->
        Db.read tr Badop.Path.root (Badop.Revisions (Badop.Dialog.query (None,0)))
        @> function
        | `Answer (Badop.Revisions (Badop.Dialog.Response revision_list)) ->
            let rec find_last_before acc timestamp = function
              | (revision,ts)::r when ts < timestamp -> find_last_before (Some (revision,ts)) timestamp r
              | _ -> acc
            in
            (match find_last_before None revision_timestamp revision_list with
             | None -> Printf.eprintf "Sorry, couldn't find any revision before the given timestamp\n"; exit 3
             | Some (revision,ts) ->
                 Printf.eprintf "Using revision %s, of the %d/%d/%d at %d:%d:%d\n"
                   (Db.Debug.revision_to_string revision)
                   (Time.local_mday ts) (Time.local_mon ts) (Time.local_year ts)
                   (Time.local_hour ts) (Time.local_min ts) (Time.local_sec ts);
                 Db.Tr.start_at_revision db revision
                   (fun exc -> Logger.critical "Database error: %s" (Printexc.to_string exc); exit 4)
                 @> k)
        | _ ->
            Printf.eprintf "Error while looking for revisions of the database"; exit 2

let treat_open command =
  Db.open_database (snd options.backend)
  @> fun db ->
    at_exit (fun () -> Db.close_database db ignore);
    get_transaction_at_revision db
  @> fun tr ->
    match command with
    | Schema format ->
        (read_schema_from_db tr
         @> function
         | Some gml_schema ->
             (match format with
              | Gml ->
                  print_endline gml_schema
              | Dot ->
                  let schema = QmlDbGen.Schema.from_gml gml_schema in
                  QmlDbGen.Schema.to_dot schema stdout
              | Aa ->
                  let schema = Dbgraph.import_schema gml_schema in
                  let tree = Dbgraph.to_tree schema in
                  print_endline (Dbgraph.print_tree ~color:(Unix.isatty (Unix.descr_of_out_channel stdout)) tree))
         | None ->
             prerr_endline "Read failed: sorry, couldn't find a schema in this database";
             exit 2)
    | Dump file ->
        (DbSerializer.to_file (Db.read tr) file
         @> fun () ->
           Printf.eprintf "XML dump to %s done.\n" file)
    | Import (file,schema_file_opt) ->
        (let schema_from_file_opt = match schema_file_opt with
           | None -> None
           | Some f ->
               try Some (File.content f)
               with Unix.Unix_error _ ->
                 Printf.eprintf "Error: could not open the schema file \"%s\".\n" f; exit 2
         in
         (fun k ->
            read_schema_from_db tr
            @> fun schema_from_db_opt -> match schema_from_file_opt, schema_from_db_opt with
            | Some s, None | None, Some s -> Dbgraph.to_tree (Dbgraph.import_schema s) |> k
            | None, None ->
                prerr_endline "Error: no schema found either from the command-line or from the database.";
                prerr_endline "Either use an initialised database or provide a gml file.";
                exit 2
            | Some s1, Some s2 ->
                let t1 = Dbgraph.to_tree (Dbgraph.import_schema s1)
                and t2 = Dbgraph.to_tree (Dbgraph.import_schema s2)
                in
                if t1 <> t2 then
                  (prerr_endline "Error: you specified a schema that is different from the one in that database.";
                   prerr_endline "Either import into an empty database or use the schema from the existing one.";
                   exit 2)
                else t1 |> k)
           @> fun t ->
             Printf.eprintf "Starting import from %s.\n" file;
             DbImporter.from_file db t file
             @> fun () ->
               Printf.eprintf "XML import from %s done.\n" file)
    | Config ->
      (read_schema_from_db tr
         @> function
         | Some gml_schema ->
           let node_config = Badop_structure.Node_property.construct gml_schema in
           Printf.printf "Node configuration : %s\n%!" (Badop_structure.Node_property.StringOf.config node_config);
         | None ->
             prerr_endline "Read failed: sorry, couldn't find a schema in this database";
             exit 2)


let treat_close command =
  match command with
    | Backup path ->
      (let loc =
        match options.backend with
        | _, Badop.Options_Local opt -> opt.Badop.path
        | _, _ -> prerr_endline "Wrong option, missing location. --backup can be used only on local database."; exit 2 in

        Backup.do_backup path loc)

let _ =
  match options.command with
  | Open command -> treat_open command
  | Close command -> treat_close command
  | Unset -> assert false

let _ = Scheduler.run Scheduler.default
