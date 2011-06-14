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
(** This module provides a functor that casts a Badop interface into a
    record. This is intended as a workaround for the BSL's lack of functors,
    allowing run-time selection of an engine through an added function
    parameter *)

module D = Badop.Dialog

(* All options *)
type dbgen_options = { force_upgrade: bool; }

type engine_options = (module Badop.S) * Badop.options

##extern-type [normalize] database_options = { dbgen_options: dbgen_options; engine_options: engine_options; }

(* abstracted pseudo polymorphic types (the same inside of each database engine,
   we loose the safety if engines get mixed though) *)
type db

type tr

type rv

type t0 = {
  open_database: unit -> db Cps.t;
  close_database: db -> unit Cps.t;
  status: db -> Badop.status Cps.t;

  tr_start: db -> (exn -> unit) -> tr Cps.t;
  tr_start_at_revision: db -> rv -> (exn -> unit) -> tr Cps.t;
  tr_prepare: tr -> (tr * bool) Cps.t;
  tr_commit: tr -> bool Cps.t;
  tr_abort: tr -> unit Cps.t;

  read: tr -> Badop.path
        -> (D.query,rv) Badop.generic_read_op
        -> (D.response,rv) Badop.generic_read_op Badop.answer Cps.t;

  write: tr -> Badop.path
        -> (D.query,tr,rv) Badop.generic_write_op
        -> (D.response,tr,rv) Badop.generic_write_op Cps.t;

  node_properties: db -> Badop.Structure.Node_property.config -> unit Cps.t;

  options: dbgen_options;
}

##extern-type [normalize] t = t0

module Make_Badop_Record (Backend: Badop.S) :
sig
  val engine: database_options -> t
end = struct

    let engine o = {
      open_database = Obj.magic (fun () -> Backend.open_database (snd o.engine_options));
      close_database = Obj.magic Backend.close_database;
      status = Obj.magic Backend.status;
      tr_start = Obj.magic Backend.Tr.start;
      tr_start_at_revision = Obj.magic Backend.Tr.start_at_revision;
      tr_prepare = Obj.magic Backend.Tr.prepare;
      tr_commit = Obj.magic Backend.Tr.commit;
      tr_abort = Obj.magic Backend.Tr.abort;
      read = Obj.magic Backend.read;
      write = Obj.magic Backend.write;
      node_properties = Obj.magic Backend.node_properties;
      options = o.dbgen_options;
    }

end

let arguments = ref (ServerArg.extract_prefix "--db-")
let general_arguments_left = ref [] (* for testing that everything has been consumed at the end *)

(* -- Parsing the command-line options -- *)
module A = ServerArg

(* options parser *)

let db_options =
  let arg_parser_dbgen = [
    ["--db-force-upgrade"],
    A.func A.unit
      (fun o () -> { o with dbgen_options = { force_upgrade = true }
           (* { o.dbgen_options with force_upgrade = true } once there is more than 1 field *) }),
      "",
      "Attempt to upgrade an existing database if it differs slightly from the one expected by the application";
  ]
  in
  let dbgen_default = { force_upgrade = false }
  in
  let wrap_parser parse =
    fun o -> A.wrap (parse o.engine_options) (fun bo -> { o with engine_options = bo })
  in
  let make_arg_parser ?name default =
      arg_parser_dbgen @
        List.map
        (fun (arg,parse,params,help) -> arg, wrap_parser parse, params, help)
        (Badop_meta.options_parser_with_default ?name default.engine_options)
  in
  (* association list (backend_opts -> db name) used to check for conflicts *)
  let parsed_engine_options = ref []
  in
  fun ident engine_options ->
    let default = { dbgen_options = dbgen_default; engine_options; }
    in
    (* A first parse, for generic arguments (without the ':database_ident' suffix) *)
    let arg_parse_generic =
      make_arg_parser ?name:ident default
    in
    let parse_generic =
      A.make_parser ~nohelp:(ident <> None) "database options (generic)" arg_parse_generic
    in
    let options, rest_generic =
      try A.filter_functional !arguments default parse_generic
      with Exit -> exit 1
    in
    general_arguments_left := rest_generic :: !general_arguments_left
    ;
    (* A second parse, using the results of the first as default, for options specific to this engine *)
    let db_name = Option.default "database" ident in
    let arg_parse_specific =
      List.map
        (fun (sl,parse,args,help) -> List.map (fun s -> Printf.sprintf "%s:%s" s db_name) sl, parse, args, help)
        (make_arg_parser options)
    in
    let parse_specific =
      A.make_parser ~nohelp:(ident = None)
        (Printf.sprintf "options for database \"%s\"" db_name)
        arg_parse_specific
    in
    let options,rest =
      try A.filter_functional !arguments options parse_specific
      with Exit -> exit 1
    in
    arguments := rest (* consume the specific arguments, not the generic ones *)
    ;
    (* check for conflicting options with previous settings *)
    try
      let conflicting_db =
        Base.List.assoc_custom_equality ~eq:Badop.Aux.options_conflict
          (snd options.engine_options) !parsed_engine_options
      in
      Logger.critical
        "Error: conflicting configuration for databases \"%s\" and \"%s\": same location%s."
        conflicting_db (Option.default "database" ident)
        (match snd options.engine_options with
         | Badop.Options_Local { Badop.path = str; _ } -> Printf.sprintf " (%s)" str
         | Badop.Options_Client (_,(h,p), _) -> Printf.sprintf " (%s:%d)" (Unix.string_of_inet_addr h) p
         | _ -> "");
      exit 1
    with Not_found ->
        parsed_engine_options :=
          (snd options.engine_options, Option.default "database" ident) :: !parsed_engine_options;
        options

(* Run once after parsers have been applied for all databases *)
##register [opacapi] check_remaining_arguments: -> void
let check_remaining_arguments () =
  let rec intersec l1 l2 = match l1,l2 with
    | x1::r1, x2::r2 ->
        if x1 == x2 then x1 :: intersec r1 r2
        else if x1 < x2 then intersec r1 l2
        else intersec l1 r2
    | _ -> []
  in
  let tosl args = List.stable_sort compare (A.to_list args) in
  let args = tosl !arguments in
  let general_args_left = List.map tosl !general_arguments_left in
  let rem_args = List.fold_left intersec args general_args_left in
  if rem_args <> [] && not (List.mem "--help" rem_args) then
      (Printf.fprintf stderr
         "Error: bad db argument%s: %s\n"
         (match rem_args with _::_::_ -> "s" | _ -> "")
         (String.concat " " rem_args);
       exit 1)

##register [restricted: dbgen; opacapi] local_options: option(string), option(string) -> database_options
let local_options name file_opt =
  let m = (module Badop_local : Badop.S) in
  let o = Badop.Options_Local {
    Badop.
      path = (match file_opt with Some f -> f | None -> Badop_meta.default_file ?name ());
      revision = None;
      restore = None;
      dot = false;
      readonly = false;
  }
  in
  db_options name (m, o)

##register [restricted: dbgen; opacapi] light_options: option(string), option(string) -> database_options
let light_options name file_opt =
  let m = (module Badop_light : Badop.S) in
  let o = Badop.Options_Light {
    Badop.
      lpath = (match file_opt with Some f -> f | None -> Badop_meta.default_file ?name ());
  }
  in
  db_options name (m, o)

##register [restricted: dbgen; opacapi] client_options: option(string), option(string), option(int) -> database_options
let client_options ident host_opt port_opt =
  let m = (module Badop_client : Badop.S) in
  let default_host = Unix.inet_addr_loopback in
  let host = match host_opt with None -> default_host | Some host ->
    try (Unix.gethostbyname host).Unix.h_addr_list.(0) with Not_found -> default_host in
  let port = Option.default Badop_meta.default_port port_opt in
  let o = Badop.Options_Client (Scheduler.default, (host, port), fun () -> `abort) in
  db_options ident (m, o)

##register [restricted: dbgen; no-projection; opacapi] get: database_options -> t
let get options =
  let { engine_options = (backend, eopts); _ } = options in
  let eopts = match eopts with
    | Badop.Options_Client (sched,server,_on_disconnect) ->
        let on_disconnect () =
          (match ServerLib.field_of_name "server_event_db_error" with
           | Some record ->
               let event =
                 ServerLib.make_record
                   (ServerLib.add_field ServerLib.empty_record_constructor record ServerLib.void)
               in
               BslServer_event.send (Obj.magic event) (QmlCpsServerLib.cont_ml (fun _ -> ()))
           | None ->
               Logger.error "Database connection error before OPA runtime initialisation";
               exit 3)
          ;
          #<If:DATABASE_RECONNECT$minlevel 0>
            `retry (Time.seconds (int_of_string (Option.get DebugVariables.database_reconnect)))
          #<Else> `abort #<End>
        in
        Badop.Options_Client (sched,server,on_disconnect)
    | eopts -> eopts
  in
  let module Backend = (val backend : Badop.S) in
  let module Engine = Make_Badop_Record(Backend) in
  Engine.engine { options with engine_options = (backend, eopts) }
