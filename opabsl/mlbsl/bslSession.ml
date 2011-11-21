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
#<Debugvar:SESSION_DEBUG>

(**********************************************************)
(* Alias **************************************************)
open Cps.Ops

module String = Base.String
module List   = Base.List

module Client = BslPingRegister.Client
module Ping   = BslPingRegister.M
module JS     = JsonTypes

type json = JS.json

let unserialize_uu = BslPingRegister.unserialize_uu
let serialize_uu   = BslPingRegister.serialize_uu



(**********************************************************)
(* Utils **************************************************)
let debug level fmt =
  Logger.debug ("[LLSESS][%s] "^^fmt^^"%!") level

let info level fmt =
  Logger.info ("[LLSESS][%s] "^^fmt^^"%!") level

let error level fmt =
  Logger.error ("[LLSESS][%s] "^^fmt^^"%!") level

(**
   In case of an error during the handler of a message, we will print some
   debug info corresponding to the state, the context and the message received

   About the exc value, it can be quite anything. In the BSL, Ocaml exception are serialized using
   the Printexc.to_string and then put into an Opa record.
   We use the runtime debug print for loging the error.
*)
let session_error_snapshot exc msg st ctx =
  Logger.error "@[<2>[SESSION] %a@]" Llsession.pp_snapshot (exc, msg, st, ctx)

(**********************************************************)
(* Opa session/channel ************************************)
(** Type of response for a send on an hlnet channel used by shared
    session *)

(** - A channel can be Local (i.e. own by a local client : use
    [Ping] for send messages).
    - Or can be remote (i.e. own by a remote entity : use [Hlnet] for
    send messages).

    (lifted OpaNetwork.cid) *)
type cid' =
  | Local of string
  | Remote of string * Unix.inet_addr * int

(** Types for the hlnet communications. *)
type schan_query = string * json * bool
type schan_response = Success | Error | Release of cid'
type schan_client = (schan_query, schan_response) Hlnet.channel
type schan_server = (schan_response, schan_query) Hlnet.channel

let schan_channel_spec_client name =
  Hlnet.Aux.easy_spec ~name ~version:1
    ~serialise:(fun (name,json,bool) -> Json_utils.to_string (JS.Array [JS.String name; json; JS.Bool bool]))
    ~unserialise:(fun _channel -> function
                  | "Success" -> Some Success
                  | "Error" -> Some Error
                  | s when String.is_prefix "Release " s ->
                      (try
                         Scanf.sscanf s "Release L%S" (fun id -> Some (Release (Local id)))
                       with Scanf.Scan_failure _ | End_of_file  -> try
                         Scanf.sscanf s "Release R%S:%d/%S"
                           (fun addr port id -> Some (Release (Remote (id, Unix.inet_addr_of_string addr,port))))
                       with Scanf.Scan_failure _ | End_of_file | Failure "inet_addr_of_string" ->
                         info "JSON" "Bad release message on client session channel %s" name;
                         None)
                  | _ -> info "JSON" "Bad message on client session channel %s" name; None)

let schan_channel_spec_server name =
  Hlnet.Aux.easy_spec ~name ~version:1
    ~serialise:(function
                | Success -> "Success"
                | Error -> "Error"
                | Release (Local id) -> Printf.sprintf "Release L%S" id
                | Release (Remote (id,addr,port)) -> Printf.sprintf "Release R%S:%d/%S" (Unix.string_of_inet_addr addr) port id)
    ~unserialise:(fun _channel s ->
                    try
                      (match Json_utils.from_string s with
                       | JS.Array [JS.String name; json; JS.Bool bool] -> Some (name,json,bool)
                       | _ -> info "JSON" "Bad json contents on server session channel %s" name; None)
                    with e ->
                      info "JSON" "Invalid json received on server session channel %s (%s)"
                        name (Printexc.to_string e);
                      None)

(** Define the opa network for channel, contains :
    - clients
    - remote servers
    - remote clients *)
module OpaNetwork = struct

  type cid = cid' =
    | Local of string
    | Remote of string * Unix.inet_addr * int

  (** Define what entities the network consists. *)
  type entity =
    | Client of Client.key
        (** A client relied on this server. Communicate with [Ping].*)

    | RemoteServer of Unix.inet_addr * int * schan_client Cps.Lazy.t
        (** A remote server. Communicate with [Hlnet]. *)

    | RemoteClient of Unix.inet_addr * int * string * schan_client Cps.Lazy.t
        (** A remote client relied to a remote server. Communicate
            with [Hlnet] by the intermediary of remote server. *)

  (** Our serialized message is json. *)
  type serialized = json

  (** Define how send a channel message to an entity. *)
  let send entity cid serialized =
    match entity, cid with
    | Client entity, Local cid ->
        Ping.send
          (Client.SendCChan (cid, serialized))
          entity
    | RemoteServer (_, _, channel), Remote (cid, _, _)
    | RemoteClient (_, _, _, channel), Remote (cid, _, _) ->
        (Cps.Lazy.force channel @> function channel -> (
           Hlnet.send channel (cid, serialized, false)))
    | _ -> failwith ("A remote entity can't host a local channel or vice versa")

  (** Define how send a channel message with success or error handler
      to an entity. *)
  let send_then entity cid serialized herror hsuccess =
    match entity, cid with
    | Client entity, Local cid ->
        (* Lanch herror on client disconnection *)
        let event_key =
          Ping.register_event (Some entity)
            Ping.Disconnect
            (fun _ -> herror ())
        in
        (* herror was called remove disconnect handler *)
        let herror () =
          Ping.remove_event event_key;
          herror () in
        (* hsucces was called remove disconnect handler *)
        let hsuccess () =
          Ping.remove_event event_key;
          hsuccess () in
        Ping.send
          (Client.SendCChanThen (cid, serialized, herror, hsuccess))
          entity
    | RemoteServer (_, _, channel), Remote (cid, _, _)
    | RemoteClient (_, _, _, channel), Remote (cid, _, _) ->
        #<If>
          debug "NETWORK" "send_then : Lazy channel (%s) opening" cid
        #<End>;
        (Cps.Lazy.force channel @> function channel -> (
           #<If>
             debug "NETWORK" "send_then : Send a message to channel(%s)" cid
           #<End>;
           Hlnet.sendreceive channel (cid, serialized, true)) @>
           (function x ->
              #<If>
                debug "NETWORK" "send_then : Receive response of channel(%s)" cid
              #<End>;
              match x with
              | Error -> herror ()
              | Success -> hsuccess ()
              | Release _ -> assert false))
    | _ ->
        failwith ("A remote entity can't host a local channel or vice versa")


  let equal_entity e1 e2 =
    match e1,e2 with
    | Client e1, Client e2 -> e1 = e2
    | RemoteServer (i1, p1, _), RemoteServer (i2, p2, _) -> (i1, p1) = (i2, p2)
    | RemoteClient _, RemoteClient _ ->
        (* We never export explicitly for a remote client, see
           [WebChannel.redefined_export] *)
        assert false
    | _ -> false

  let compare_entity e1 e2 =
    match e1,e2 with
    | Client e1, Client e2 -> Pervasives.compare e1 e2
    | RemoteServer (i1, p1, _), RemoteServer (i2, p2, _) -> Pervasives.compare (i1, p1) (i2, p2)
    | Client _, RemoteServer _ ->  1
    | RemoteServer _, Client _ -> -1
    | RemoteClient _, _
    | _, RemoteClient _ ->
        (* We never export explicitly for a remote client, see
           [WebChannel.redefined_export] *)
        assert false

  let hash_entity = function
    | Client e -> Hashtbl.hash e
    | RemoteServer (i1, p1, _) -> Hashtbl.hash (i1, p1)
    | RemoteClient _ ->
        (* We never export explicitly for a remote client, see
           [WebChannel.redefined_export] *)
        assert false


  (** {6 Debug printing} *)

  let entity_to_string entity =
    match entity with
    | Client entity -> Client.key_to_string entity
    | RemoteClient (ip, port,_, _) ->
        Printf.sprintf "RemoteClient(%s:%d)"
          (Unix.string_of_inet_addr ip) port
    | RemoteServer (ip, port, _) ->
        Printf.sprintf "RemoteServer(%s:%d)"
          (Unix.string_of_inet_addr ip) port

  let cid_to_string = function
    | Local cid -> Printf.sprintf "Local(%s)" cid
    | Remote (cid, addr, port) -> Printf.sprintf "Remote(%s)@%s:%d"
        cid (Unix.string_of_inet_addr addr) port

end

(** Make the channel module, with the network defined on [OpaNetwork],
    and use the [BslScheduler] for define Channel scheduler params.  *)
module Channel = (val Channel.make BslScheduler.opa : Channel.M) (OpaNetwork)

(** Remove client channel when a client is disconnected from the ping
    register. *)
let _ = Ping.register_event None Ping.Disconnect
  (fun x -> Channel.remove_entity (OpaNetwork.Client x))


module SharedChannel = struct

  type options = {
    kind : [`client | `server ];
    dir_endpoint : Hlnet.endpoint;
    shared_endpoint : Hlnet.endpoint;
  }

  let default_port = Hlnet.port_of_endpoint Hlnet.default_endpoint

  let parse =
    ServerArg.make_parser "shared channel directory" [
      ["--chan-directory"], ServerArg.func ServerArg.string
        (fun o s ->
           match s with
           | "own" -> {o with kind = `server}
           | _ ->
               let l = String.slice ':' s in
               { o with
                 kind = `client;
                 dir_endpoint =
                   let addr, port =
                     match l with
                     | [addr] -> addr, default_port
                     | [addr; port] -> addr, int_of_string port
                     | _ -> failwith "Unexpected argument"
                   in
                   Hlnet.Tcp ((Unix.inet_addr_of_string addr), port);
               }
        ),
      "(own | <ip>[:<port>])",
      "Set the shared channel directory adress. By default it's own:<hlport>.";

      ["--chan-shared-port"], ServerArg.func ServerArg.int
        (fun o i -> { o with shared_endpoint = Hlnet.Tcp (Unix.inet_addr_any, i) }),
      "<int>",
      "Set the port for shared channel communications. By default it's <hlport>";
    ]

  let opt = fst (
    ServerArg.filter_functional (ServerArg.extract_prefix "--chan-") {
      kind = `server;
      dir_endpoint = Hlnet.Tcp (Unix.inet_addr_any, default_port);
      shared_endpoint = Hlnet.Tcp (Unix.inet_addr_any, default_port);
    } parse
  )

  let hash = Hashtbl.create 64
  let common construct kind (ip, port) =
    try
      Hashtbl.find hash (kind, (ip, port))
    with Not_found ->
      #<If>
        debug "SHARED" "No found remote %s entity %s:%d"
        kind (Unix.string_of_inet_addr ip) port;
      #<End>;
      let entity = construct (ip, port) in
      Hashtbl.add hash (kind, (ip, port)) entity;
      entity

  let directory =
    Cps.Lazy.make BslScheduler.push
      (fun k ->
         let rec aux k =
           let timeout_retry = 5000 in
           let err_cont __exn =
             #<If>
               debug "SHARED"
               "Catch %s => retry starting directory in %dms"
               (Printexc.to_string __exn)
               timeout_retry
             #<End>;
             BslScheduler.sleep timeout_retry (fun _ -> aux k)
           in
           Directory.make ~err_cont BslScheduler.opa
             opt.dir_endpoint "ssession" opt.kind @>
             (function (dir:(string, string * int) Directory.t) -> dir |> k)
         in aux k
      )

  let addr k =
    Cps.Lazy.force directory @> function directory ->
      let k x = k (x, Hlnet.port_of_endpoint opt.shared_endpoint) in
      Directory.my_public_addr directory k

  (* Fixme: it would be better to let Hlnet handle cleanup automatically, if possible *)
  let rec release_listener get_entity =
    (fun id _respond ->
       addr @> function (myip, myport) ->
         let id = match id with
         | Release (OpaNetwork.Remote (id, ip, port) as remote) ->
             if ip = myip && port = myport
             then Channel.LocalId (int_of_string id)
             else Channel.EntityId remote
         | Release (OpaNetwork.Local id) -> Channel.LocalId (int_of_string id)
         | Success | Error -> assert false (* these should only be sent as response to a query *)
         in
         Channel.release (get_entity ()) id)

  (* The _lc suffix stands for "lazy-cps version" *)
  let open_service_lc
      (* : ('o,Channel.identity) Hlnet.channel_spec -> *)
      (*   (Unix.inet_addr * int -> Channel.identity -> OpaNetwork.entity) -> (Unix.inet_addr * int) *)
      (*   -> ('o,Channel.identity) Hlnet.channel Cps.Lazy.lazy_t *)
      = fun service g (ip, port) ->
    Cps.Lazy.make BslScheduler.push (
    fun k ->
      Hlnet.open_channel BslScheduler.opa (Hlnet.Tcp (ip, port)) service
      @> function chan ->
        Hlnet.setup_respond chan
          (release_listener (fun () -> g (ip, port)));
        k chan
  )

  let open_server_lc x = open_service_lc (schan_channel_spec_client "sess/server") x

  let open_client_lc x = open_service_lc (schan_channel_spec_client "sess/client") x

  let rec get_remote_server x =
    (common (fun (ip, port) -> OpaNetwork.RemoteServer (ip, port, (open_server_lc get_remote_server (ip, port)))) "server") x

  let rec get_remote_client s x =
    (common (fun (ip, port) -> OpaNetwork.RemoteClient (ip, port, s, (open_client_lc (get_remote_client s) (ip, port)))) "client") x


  let client_listener (incomming:schan_server) =
    Hlnet.setup_respond incomming
      (fun (id, msg, callk) k ->
         try
           #<If>
             debug "SHARED"
             "Receive a message by hlnet for one of my client session %s" id
           #<End>;
           let chan = Channel.find (Channel.EntityId (OpaNetwork.Local id)) in
           if callk then
             let herror () = k Error in
             let hsuccess () = k Success in
             (* TODO forward something in context *)
             Channel.forward_then chan msg None herror hsuccess
           else Channel.forward chan msg None;
         with Channel.Unregistered -> if callk then k Error
      )

  let server_listener (incomming:schan_server) =
    Hlnet.setup_respond incomming
      (fun (id, msg, callk) k ->
         try
           #<If>
             debug "SHARED"
             "Receive a message by hlnet for one of my own chanel %s" id;
           #<End>;
           let chan = Channel.find (Channel.LocalId (int_of_string id)) in
           let context = Some (
             BslUtils.create_ctx
               (`server incomming)
               None) in
           if callk then
             let herror () = k Error in
             let hsuccess () = k Success in
             Channel.forward_then chan msg context herror hsuccess
           else Channel.forward chan msg context;
         with Channel.Unregistered ->
           #<If>
             debug "SHARED" "Channel %s was Unregistered" id;
           #<End>;
           if callk then k Error
      )

  type s_entity = [
  | `client of Client.key
  | `rclient of Unix.inet_addr * int * string
  | `rserver of Unix.inet_addr * int ]

  let serialize_entity = function
    | OpaNetwork.Client k -> `client k
    | OpaNetwork.RemoteServer (ip, port, _) -> `rserver (ip, port)
    | OpaNetwork.RemoteClient (ip, port, id, _) -> `rclient (ip, port, id)

  let unserialize_entity = function
    | `client k -> OpaNetwork.Client k
    | `rserver x -> get_remote_server x
    | `rclient (ip, port, id) -> get_remote_client id (ip, port)

  type export_query = s_entity option * Channel.identity * Channel.identity
  type export_response = unit
  type export_channel_client = (export_query, export_response) Hlnet.channel
  type export_channel_server = (export_response, export_query) Hlnet.channel
  let export_channel_spec_client : (export_query, export_response) Hlnet.channel_spec =
    Hlnet.Aux.magic_spec (Hlnet.make_service_id ~name:"sess/export" ~version:1)
  let export_channel_spec_server : (export_response, export_query) Hlnet.channel_spec =
    Hlnet.Aux.magic_spec (Hlnet.make_service_id ~name:"sess/export" ~version:1)

  (** Used for export chan for local client *)
  let export_listener (incomming: export_channel_server) =
    Hlnet.setup_respond incomming
      (fun (owner, chan, entity) k ->
         let owner = Option.map unserialize_entity owner in
         let entity = match entity with
         | Channel.EntityId (OpaNetwork.Remote (cl_id, _, _)) ->
             Channel.EntityId (OpaNetwork.Local cl_id)
         | _ -> assert false in
         let entity = Channel.find entity in
         let entity = Option.get (Channel.owner entity) in
         let chan =
           try
             Channel.find chan
           with Channel.Unregistered ->
             (match chan with
              | Channel.EntityId (OpaNetwork.Remote (s, ip, port)) ->
                  (let entity =
                     (match owner with
                     | Some (OpaNetwork.RemoteClient _)
                     | Some (OpaNetwork.Client _) -> get_remote_client s
                     | _ -> get_remote_server) (ip, port) in
                   match chan with
                   | Channel.EntityId chan -> Channel.register chan entity
                   | _ -> assert false)
              | _ -> assert false
             )
         in
         ignore(Channel.export chan entity);
         Channel.on_remove chan
           (fun () ->
              #<If>
                debug "SHARED" "Call export continuation for release"
              #<End>;
              k ())
      )

  let shared_directory =
    Cps.Lazy.make BslScheduler.push
      (fun k ->
         Cps.Lazy.force directory @>
           function (dir:(string, string * int) Directory.t) ->
             (* Accept services *)
             Hlnet.accept ~safe:true BslScheduler.opa
               opt.shared_endpoint (schan_channel_spec_server "sess/client")
               client_listener;
             Hlnet.accept ~safe:true BslScheduler.opa
               opt.shared_endpoint (schan_channel_spec_server "sess/server")
               server_listener;
             Hlnet.accept ~safe:true BslScheduler.opa
               opt.shared_endpoint export_channel_spec_server
               export_listener;
             dir |> k
      )

  let release (hl:(Channel.identity, unit) Hlnet.channel) cid =
    Hlnet.send hl cid

  let request_export (ip, port) (o,c,e) k =
    Hlnet.open_channel BslScheduler.opa (Hlnet.Tcp (ip, port)) export_channel_spec_client @>
      function (chan : export_channel_client) ->
        Hlnet.sendreceive chan (Option.map serialize_entity o,c,e) k

  let make_cps key state unserialize handler ondelete ctx more ronly k =
    Cps.Lazy.force shared_directory @> function directory ->
      (* Create a candidate local channel *)
      let mychan =
        Channel.make_cps state unserialize handler ondelete ctx more ronly in
      (* Create a shared_id from candidate *)
      let id =
        match Channel.export mychan (OpaNetwork.Client (Client.key "0" 0)) with
        | Channel.LocalId i -> i
        | _ -> assert false in
      (* Search if key is already registered *)
      let shared_port = Hlnet.port_of_endpoint opt.shared_endpoint in
      Directory.find_or_replace directory key ((string_of_int id), shared_port) @>
        function

          (* [key] is not already registered return my chan *)
          | None -> k mychan

          (* [key] already registered by another server open a hlchannel *)
          | Some ((cid, port), Directory.Other ip) ->
              begin
                let cid = OpaNetwork.Remote (cid, ip, port) in
                let entity = get_remote_server (ip, port) in
                Channel.register cid entity |> k
              end

          (* [key] already registered but by me, retreive local channel *)
          | Some ((cid, _), Directory.Me) ->
              Channel.find (Channel.LocalId (int_of_string cid)) |> k

end





(**********************************************************)
(* Web facade to access to opa session/channel ************)
(** A Web facade for access to channel : check json request, ... *)
module WebChannel : sig

  (** {6 Hight-level facade} *)
  (** [register cookie page request] Register a client channel
      contained on the [request] from a client's [page]. [cookie] it's
      the cookie that client sent. *)
  val register : string -> int -> json -> bool

  (** [register cookie page js_array] *)
  val registers : string -> int -> json -> bool

  (** [remove cookie page request] Remove a client channel
      contained on the [request] from a client's [page]. [cookie] it's
      the cookie that client sent. *)
  val remove : string -> int -> json -> bool

  (** [send cookie page request context] Send a message to channel
      contained on the [request] from a client's [page]. [cookie] it's
      the cookie that client sent. *)
  val send : string -> int -> json -> 'ctx option -> unit

  (** {6 Serialization} *)
  (** Serialize a channel to json. *)
  val serialize : OpaNetwork.entity -> ('msg,'ctx) Channel.t -> json

  val serialize_with_endpoint : OpaNetwork.entity -> ('msg,'ctx) Channel.t -> string -> int -> json

  (** Unserialize a channel from json. *)
  val unserialize : 'ctx option -> json -> ('msg,'ctx) Channel.t option Cps.t

end = struct
  exception Malformed

  let bad_formatted description request =
    error "WebChannel" "On %s, bad formatted request %s"
      description (Json_utils.to_string request);
    false

  let cook_of_id cl_id = String.sub cl_id (String.length cl_id - 32) 32

  (* Check if the channel identifier corresponding to the given
     cookie. *)
  let check_cl_id cl_id cookie =
    try
      (cook_of_id cl_id) = cookie
    with _ -> false

  (* Generic function for perform a request which contains a client
     channel. *)
  let check_and_perform_client_channel description action
      cookie page request =
    let cid = Client.key cookie page in
    (* Debug message *)
    #<If>
      debug "WebChannel" "Try %s for %s => (%s)"
      description (Client.key_to_string cid) (Json_utils.to_string request);
    #<End>;
    (* Json check *)
    match request with
    | JS.Record (("cl_id", JS.String cl_id)::addr_and_port) ->
        (* TODO - Check addr_and_port *)
        ignore addr_and_port;
        (* Channel check *)
        if not (check_cl_id cl_id cookie) then
          bad_formatted description request
        else (
          (* Perform action *)
          action (OpaNetwork.Local cl_id) (OpaNetwork.Client cid);
          #<If>
            debug "WebChannel" "Success %s for %s => (%s)"
            description (Client.key_to_string cid) (Json_utils.to_string request);
          #<End>;
          true )
    | _ -> bad_formatted description request

  let unserialize_identity json k =
    match json with
    | JS.Record (("srv_id", id)::endpoint) ->
        let id =
          match id with
          | JS.String id -> int_of_string id
          | JS.Int id -> id
          | _ -> raise Malformed
        in
        (match endpoint with
         | [] -> Channel.LocalId id |> k
         | [("addr", JS.String addr); ("port", JS.Int port)] ->
             let ip = Unix.inet_addr_of_string addr in
             SharedChannel.addr @>
               (function (myip, myport) ->
                  if myip = ip && myport = port then
                    Channel.LocalId id |> k
                  else
                    let cid = string_of_int id in
                    let cid = (OpaNetwork.Remote (cid, ip, port)) in
                    Channel.EntityId cid |>k)
         | _ -> raise Malformed
        )
    | JS.Record (("cl_id", JS.String id)::endpoint) ->
        (match endpoint with
         | [] -> Channel.EntityId (OpaNetwork.Local id) |> k
         | [("addr", JS.String addr); ("port", JS.Int port)] ->
             let ip = Unix.inet_addr_of_string addr in
             SharedChannel.addr @>
               (function (myip, myport) ->
                  if myip = ip && myport = port then
                    Channel.EntityId (OpaNetwork.Local id) |> k
                  else
                    let id = OpaNetwork.Remote (id, ip, port) in
                   Channel.EntityId id |> k)
         | _ -> raise Malformed
        )
    | _ -> raise Malformed


  let register =
    check_and_perform_client_channel "registering"
      Channel.register_strong

  let redefined_export channel entity =
    match entity with
    | OpaNetwork.RemoteClient (ip, port, s, _) ->
        (* We export for the server that own the client entity,
           And request to this server to export for the client. *)
        let sentity = SharedChannel.get_remote_server (ip, port) in
        let idchan = Channel.export channel sentity in
        let identity = Channel.EntityId (OpaNetwork.Remote (s, ip, port)) in
        (SharedChannel.addr @> function (myip, myport) ->
           (* Transform local identity to remote identity *)
           let idchan' = match idchan with
           | Channel.LocalId id ->
               let id = string_of_int id in
               Channel.EntityId (OpaNetwork.Remote (id, myip, myport))
           | Channel.EntityId (OpaNetwork.Local id) ->
               Channel.EntityId (OpaNetwork.Remote (id, myip, myport))
           | _ -> idchan in
           SharedChannel.request_export (ip, port) ((Channel.owner channel), idchan', identity)
             (fun () ->
                #<If$minlevel 50>
                  debug "SHARED" "Export continuation for release called"
                #<End>;
                Channel.release sentity idchan)
        );
        idchan
    | _ -> Channel.export channel entity

  let register_release ctx chan id =
    match ctx with
    | None ->
        #<If>
          debug "SHARED"
          "No context don't set release callback"
        #<End>
    | Some ctx ->
        match BslUtils.get_serverkey (Obj.magic ctx) with
        | None ->
            #<If>
              debug "SHARED"
              "No server context don't set release callback"
            #<End>
        | Some x ->
            #<If>
              debug "SHARED"
              "Server context set a release callback"
            #<End>;
            Channel.on_remove chan
              (fun () ->
                 #<If>
                   debug "SHARED"
                   "Channel %s can be released on server"
                   (Channel.identity_to_string id)
                 #<End>;
                 SharedChannel.release x id
              )

  let unserialize ctx json k = try (
    let k x = k (Some x) in
    match json with
    | JS.Record (("srv_id", id)::endpoint) ->
        let id =
          match id with
          | JS.String id -> int_of_string id
          | JS.Int id -> id
          | _ -> raise Malformed
        in
        (match endpoint with
         | [] -> Channel.find (Channel.LocalId id) |> k
         | [("addr", JS.String addr); ("port", JS.Int port)] ->
             let ip = Unix.inet_addr_of_string addr in
             SharedChannel.addr @>
               (function (myip, myport) ->
                  if myip = ip && myport = port then
                    Channel.find (Channel.LocalId id) |> k
                  else
                    let cid = string_of_int id in
                    let cid = (OpaNetwork.Remote (cid, ip, port)) in
                    let id = Channel.EntityId cid in
                    (try
                       Channel.find id
                     with Channel.Unregistered ->
                       (* This channel was never registered on this
                          server, register it *)
                       let entity = SharedChannel.get_remote_server (ip, port) in
                       let chan = Channel.register cid entity in
                       register_release ctx chan id;
                       chan) |> k
               )
         | _ -> raise Malformed
        )
    | JS.Record (("cl_id", JS.String cl_id)::endpoint) ->
        (match endpoint with
         | [] -> Channel.find (Channel.EntityId (OpaNetwork.Local cl_id)) |> k
         | [("addr", JS.String addr); ("port", JS.Int port)] ->
             let ip = Unix.inet_addr_of_string addr in
             SharedChannel.addr @>
               (function (myip, myport) ->
                  if myip = ip && myport = port then
                    Channel.find (Channel.EntityId (OpaNetwork.Local cl_id)) |> k
                  else
                    let id = OpaNetwork.Remote (cl_id, ip, port) in
                    let cid = Channel.EntityId id in
                    (try
                       Channel.find cid
                     with Channel.Unregistered ->
                       #<If>
                         debug "SHARED"
                         "Unserialize an unknow remote session (%s), register now"
                         (OpaNetwork.cid_to_string id)
                         #<End>;
                       let ip = Unix.inet_addr_of_string addr in
                       let entity = SharedChannel.get_remote_client cl_id (ip, port) in
                       let chan = Channel.register id entity in
                       register_release ctx chan cid;
                       chan
                    ) |> k
               )
         | _ -> raise Malformed
        )
    | _ -> raise Malformed
  ) with
      (* We catch all exceptions so we can answer the client and close the
         current connection *)
    | e -> Logger.error "unserialize error: (%s)" (Printexc.to_string e); (None |> k)


  let registers cookie page request =
    match request with
    | JS.Array lst ->
        let not_registered =
          List.fold_left
            (fun not_registered jschan ->
               match jschan with
               | JS.Record (("entity", entity)::("channel", chan)::[]) ->
                   debug "WebChannel" "EXPORT";
                   (unserialize_identity entity @> function identity ->
                     let entity = Channel.find identity in
                     match Channel.owner entity with
                     | None ->
                         #<If> debug "WebChannel" "Don't export channel for myself" #<End>;
                         ()
                     | Some entity ->
                         unserialize None chan @> function
                           | None -> error "WebChannel" "Error when try to export"
                           | Some chan ->
                               try
                                 ignore (redefined_export chan entity)
                               with Channel.Unregistered ->
                                 #<If>
                                   error "WebChannel" "Try to export an unregistered channel"
                                   #<End>;
                                 ()
                   );
                   not_registered
               | _ ->
                   debug "WebChannel" "REGISTER";
                   if not (check_and_perform_client_channel "registerings"
                             Channel.register_strong cookie page jschan) then
                     jschan::not_registered
                   else not_registered)
            [] lst
        in List.is_empty not_registered
    | _ -> bad_formatted "registerings" request


  let remove =
    check_and_perform_client_channel "removing"
      (fun cl_id _ -> Channel.remove (Channel.EntityId cl_id))


  let send __cookie __page request context =
    try
      #<If>
        let cid = Client.key __cookie __page in
        debug "WebChannel" "Try sending for %s => (%s)"
          (Client.key_to_string cid) (Json_utils.to_string request);
        #<End>;
        let cpschan, msg, then_ =
          match request with
          | JS.Record (("to", to_)::("message", msg)::then_) ->
              unserialize context to_, msg, then_
          | _ -> raise Malformed
        in
        cpschan @> function
          | None -> ignore (bad_formatted "sending channel" request)
          | Some chan -> (
              match then_ with
              | [] -> Channel.forward chan msg context
              | [("herror", herror); ("hsuccess", hsuccess)] ->
                  unserialize_uu herror
                  @> (function herror ->
                        unserialize_uu hsuccess
                        @> (function hsuccess ->
                              Channel.forward_then chan msg context herror hsuccess));
              | _ -> ignore (bad_formatted "sending2" request)
            )
    with
    | Malformed -> ignore (bad_formatted "sending" request)
    | Channel.Unregistered ->
        (* Execute herror if present *)
        (match request with
         | JS.Record (_::_::("herror",herror)::_) ->
             unserialize_uu herror @> (function herror -> herror ());
         | _ ->
             #<If>
               debug "WebChannel" "Unregistered channel %s" (Printexc.get_backtrace());
             #<End> )

  let serialize entity chan =
    let identity = redefined_export chan entity in
    match identity with
    | Channel.LocalId id ->
        JS.Record [("srv_id", JS.Int id)]
    | Channel.EntityId (OpaNetwork.Local id) ->
        JS.Record [("cl_id", JS.String id)]
    | Channel.EntityId (OpaNetwork.Remote (id, ip, port)) ->
        let kind =
          (match Option.get (Channel.owner (Channel.find identity)) with
           | OpaNetwork.RemoteServer _ -> "srv_id", JS.Int (int_of_string id)
           | OpaNetwork.RemoteClient _ -> "cl_id", JS.String id
           | _ -> assert false
          ) in
        let addr = Unix.string_of_inet_addr ip in
        JS.Record [kind; ("addr", JS.String addr); ("port", JS.Int port)]


  let serialize_with_endpoint entity chan addr port =
    let endpoint =
      [("addr", JS.String addr); ("port", JS.Int port)] in
    match redefined_export chan entity with
    | Channel.LocalId id ->
        JS.Record ([("srv_id", JS.Int id)] @ endpoint)
    | Channel.EntityId (OpaNetwork.Local id) ->
        JS.Record ([("cl_id", JS.String id)] @ endpoint)
    | Channel.EntityId (OpaNetwork.Remote (id, ip, port)) as identity ->
        let kind =
          (match Option.get (Channel.owner (Channel.find identity)) with
           | OpaNetwork.RemoteServer _ -> "srv_id", JS.Int (int_of_string id)
           | OpaNetwork.RemoteClient _ -> "cl_id", JS.String id
           | _ -> assert false
          ) in
        let addr = Unix.string_of_inet_addr ip in
        JS.Record [kind; ("addr", JS.String addr); ("port", JS.Int port)]

end




(* Projection continuation('a) => 'a -> void *)
let cont_to_fun k =
  fun x ->
    QmlCpsServerLib.return k x

let acont_to_fun f k =
  fun x ->
    QmlCpsServerLib.return k (f x)

let ocont_to_fun k =
  acont_to_fun ServerLib.wrap_option k

(* Projection 'a -> void => continuation('a) *)
let fun_to_cont f = QmlCpsServerLib.cont_ml f

let afun_to_cont p f =
  QmlCpsServerLib.cont_ml (fun x -> f (p x))

let ofun_to_cont f =
  afun_to_cont ServerLib.unwrap_option f

let vfun_to_cont f =
  afun_to_cont (fun (_:ServerLib.ty_void) -> ()) f

(* cps(-> unit) -> (unit -> unit)*)
let void_cps_to_unit f =
  fun _ -> f (QmlCpsServerLib.cont_ml (fun _ -> ()))

(**********************************************************)
(* BSL REGISTERING ****************************************)
##extern-type Session.private.native('a, 'b) = ('a, 'b) Channel.t

##extern-type Session.entity = OpaNetwork.entity

##extern-type middle('a, 'b) = ('a, 'b) Channel.t * json

(* HACK PART - This module convert was a big hack for that client cell
   works without cps client.
   NOT STABLE!!!.*)
##module Convert
  let get_request_params req = req.HttpServerTypes.request_message_body

  let wreq2json wreq =
    (Json_utils.from_string (Rcontent.get_content (get_request_params wreq)))

  (** Convert a request to a couple that contains a channel, and a
      serialized message for this channel (type named middle). *)
  ##register [cps-bypass] request_to_middle : WebInfo.private.native, continuation(opa[option(middle('msg, 'ctx))]) -> void
  let request_to_middle winfo k =
    let (|>) x k = QmlCpsServerLib.return k x in
    match wreq2json winfo.HttpServerTypes.request with
    | JS.Record (("to", to_)::("message", mess)::[]) ->
        (try
           WebChannel.unserialize None to_ @>
             (function
                | Some chan -> ServerLib.some (chan, mess) |> k
                | None -> ServerLib.none |> k)
         with e ->
           error "WEBCHANNEL" "Unserialization fail  : %s" (Printexc.to_string e);
           ServerLib.none |> k
        )
    | _ -> ServerLib.none |> k

  (** Get the unserialized message from middle *)
  ##register [cps-bypass] msg_from_middle : middle('msg, 'ctx), continuation(opa[option('msg)]) -> void
  let msg_from_middle (lchan, json) k =
    Channel.unserialize lchan json (ocont_to_fun k)

  (** Get channel from middle*)
  ##register chan_from_middle : middle('msg, 'ctx) -> Session.private.native('msg, 'ctx)
  let chan_from_middle (lchan, _) = lchan

  (** Get object stored at make *)
  ##register get_more : middle('msg, 'ctx) -> option('more)
  let get_more (lchan, _) =
    Option.map Obj.obj (Channel.get_more lchan)

##endmodule


(** {6 Initialization} *)
(** Set opa function for serialize and unserialize (-> void)
    function. Needed by [send_then]. *)
##register set_uu : ((-> void) -> RPC.Json.private.native), (RPC.Json.private.native -> (-> void)) -> void
let set_uu se us =
  let se x k = se x |> k in
  let us x k = us x |> k in
  BslPingRegister.set_uu se us

(** [cps-bypass] of set_uu. Don't use this on non-closure mode : SEGFAULT. *)
##register [cps-bypass] set_uu_cps : \
    ((continuation(opa[void]) -> void), continuation(RPC.Json.private.native) -> void), \
    (RPC.Json.private.native, continuation(opa[continuation(void) -> void]) -> void), \
    continuation(opa[void]) -> \
    void
let set_uu_cps se us k =
  (* SEGFAULT ON NON CLOSURE MODE *)
  let us x (k : (unit -> unit) -> unit) =
    let k f =
      let f () =
        ignore (QmlClosureRuntime.args_apply1 (Obj.magic f)
                  (QmlCpsServerLib.cont_ml (fun _ -> ())))
      in k f
    in
    let k = QmlCpsServerLib.cont_ml k in
    us x k
  in let se f k =
    let f k  =
      f (); QmlCpsServerLib.return k ServerLib.void in
    se f (QmlCpsServerLib.cont_ml k)
  in BslPingRegister.set_uu se us;
  QmlCpsServerLib.return k ServerLib.void

(** {6 Creation} *)

(* Arguments projection for cps-bypasses *)
let llmake_cps0 state unserialize handler ondelete ctx more concurrent =
  let unserialize c x f =
    unserialize c x (ofun_to_cont f) in
  let handler st msg ctx f =
    let error_cont exc k =
      session_error_snapshot exc msg st ctx ;
      QmlCpsServerLib.return k (ServerLib.some st)
    in
    let k = ofun_to_cont f in
    let k = QmlCpsServerLib.catch_ml error_cont k in
    handler st msg ctx k in
  let ondelete =
    Option.map
      (fun ondelete -> void_cps_to_unit ondelete)
      ondelete in
  let more = Option.map (Obj.repr) more in
  (Channel.make_cps state unserialize handler ondelete
     ctx more concurrent)

##register [cps-bypass] llmake_cps : \
    'st, \
    (option('ctx), RPC.Json.private.native, continuation(opa[option('msg)]) -> void), \
    ('st, 'msg, option('ctx), continuation(opa[option('st)]) -> void), \
    option(continuation(opa[void]) -> void), \
    option('ctx), \
    option('more), \
    bool, \
    continuation(Session.private.native('msg, 'ctx)) -> \
    void
let llmake_cps state unserialize handler ondelete ctx more concurrent k =
  QmlCpsServerLib.return k
    (llmake_cps0 state unserialize handler ondelete
       ctx more concurrent)

##register [cps-bypass] make_shared : \
    string, \
    'st, \
    (option('ctx), RPC.Json.private.native, continuation(opa[option('msg)]) -> void), \
    ('st, 'msg, option('ctx), continuation(opa[option('st)]) -> void), \
    option(continuation(opa[void]) -> void), \
    option('ctx), \
    option('more), \
    bool, \
    continuation(Session.private.native('msg, 'ctx)) -> \
    void
let make_shared key state unserialize handler ondelete ctx more concurrent k =
  let unserialize c x f =
    unserialize c x (ofun_to_cont f) in
  let handler st msg ctx f =
    let error_cont exc k =
      session_error_snapshot exc msg st ctx ;
      QmlCpsServerLib.return k (ServerLib.some st)
    in
    let k = ofun_to_cont f in
    let k = QmlCpsServerLib.catch_ml error_cont k in
    handler st msg ctx k in
  let ondelete =
    Option.map
      (fun ondelete -> void_cps_to_unit ondelete)
      ondelete in
  let more = Option.map (Obj.repr) more in
  let k x = QmlCpsServerLib.return k x in
  (* Hack - Force getting address, for make sure serialize(_and_share) function
     will not be asynchronous.
     TODO - Real fix : make llsend as cps-bypass *)
  SharedChannel.addr @> function _ ->
    SharedChannel.make_cps key state unserialize handler ondelete ctx more concurrent k



##register llmake : \
      'st, \
      (option('ctx), RPC.Json.private.native -> option('msg)), \
      ('st, 'msg, option('ctx) -> option('st)), \
      option(-> void), \
      option('ctx), \
      option('more), \
      bool -> \
      Session.private.native('msg, 'ctx)
let llmake s uns f ondelete ctx more concurrent =
  let ondelete =
    Option.map
      (fun ondelete ->
         (fun _ -> ondelete ())
      )
      ondelete in
  Channel.make s uns f ondelete ctx
    (Option.map (Obj.repr) more)
    concurrent

(** {6 Send} *)

##register llsend : Session.private.native('msg, 'ctx), ('msg -> RPC.Json.private.native), 'msg, option('ctx) -> void
let llsend ch ser msg ctx =
  Channel.send ch ser msg ctx

##register llsend_then : \
  Session.private.native('msg, 'ctx), \
  ('msg -> RPC.Json.private.native), \
  'msg, option('ctx), (-> void), (-> void) -> void
let llsend_then ch ser msg ctx herror hsuccess =
  Channel.send_then ch ser msg ctx herror hsuccess


(** {6 Serialization} *)

##register get_server_id : Session.private.native('msg, 'ctx) -> option(string)
let get_server_id t =
  (* This case is on toplelvel javascript serialization. *)
  (* Use a client identity which can't be collected. *)
  let universal = OpaNetwork.Client (Client.key "_internal_" (-1)) in
  match Channel.export t universal with
  | Channel.LocalId id -> Some (string_of_int id)
  | Channel.EntityId _ -> None

##register export : Session.private.native('msg, 'ctx), opa[ThreadContext.client] -> RPC.Json.private.native
let export chan ctx =
  let entity = OpaNetwork.Client (Obj.magic ctx) in
  WebChannel.serialize entity chan


##register serialize_for_entity : Session.private.native('msg, 'ctx), Session.entity -> RPC.Json.private.native
let serialize_for_entity _ = assert false

##register [cps-bypass] serialize_for_entity_cps : Session.private.native('msg, 'ctx), Session.entity, continuation(RPC.Json.private.native) -> void
let serialize_for_entity_cps chan entity k =
  match entity with
  | OpaNetwork.Client _ ->
      QmlCpsServerLib.return k (WebChannel.serialize entity chan)

  | OpaNetwork.RemoteClient _ | OpaNetwork.RemoteServer _ ->
      (* We must share serialized session *)
      SharedChannel.addr @> function (ip, port) ->
        let addr = Unix.string_of_inet_addr ip in
        let json = WebChannel.serialize_with_endpoint entity chan addr port in
        QmlCpsServerLib.return k json




##register unserialize : option('ctx), RPC.Json.private.native -> option(Session.private.native('msg, 'ctx))
let unserialize _ = assert false

##register [cps-bypass] unserialize_cps : option('ctx), RPC.Json.private.native, continuation(opa[option(Session.private.native('msg, 'ctx))]) -> void
let unserialize_cps c x k = (WebChannel.unserialize c x (ocont_to_fun k))


(** {Other utils} *)

##register on_remove : Session.private.native('msg, 'ctx), (-> void) -> void
let on_remove = Channel.on_remove

##register [cps-bypass] on_remove_cps : Session.private.native('msg, 'ctx), (continuation(opa[void]) -> void), continuation(opa[void]) -> void
let on_remove_cps chan f k =
  Channel.on_remove chan (void_cps_to_unit f);
  QmlCpsServerLib.return k ServerLib.void

(*
   Important note: We only export equality check as the order is not stable from client to server -- and not even stable in time inside the client
*)
##register equals_channel : Session.private.native('msg, 'ctx), Session.private.native('msg, 'ctx) -> bool
let equals_channel a b = Channel.compare a b = 0

##register compare_channels : Session.private.native('msg, 'ctx), Session.private.native('msg, 'ctx) -> int
let compare_channels a b = Channel.compare a b (*Server-side comparison of channels always works*)

##register get_more : Session.private.native('msg, 'ctx) -> option('more)
let get_more chan =
  Option.map Obj.obj (Channel.get_more chan)

##register owner : Session.private.native('msg, 'ctx) -> option(Session.entity)
let owner x = try Channel.owner x with Channel.Unregistered -> None

##register is_client : option(Session.entity) -> bool
let is_client = function
  | Some (OpaNetwork.RemoteClient _) -> true
  | Some (OpaNetwork.Client _) -> true
  | _ -> false

##register is_remote : Session.private.native('msg, 'ctx) -> bool
let is_remote chan =
  match Channel.owner chan with
  | Some (OpaNetwork.RemoteServer _)
  | Some (OpaNetwork.RemoteClient _) -> true
  | _ -> false

##register is_local : Session.private.native('msg, 'ctx) -> bool
let is_local chan =
  try Option.is_none (Channel.owner chan) with Channel.Unregistered -> false

##register get_endpoint : Session.private.native('msg, 'ctx) -> option(endpoint)
let get_endpoint chan =
  match Channel.identify chan with
  | Channel.EntityId (OpaNetwork.Remote (_, ip, port)) -> Some (Hlnet.Tcp (ip, port))
  | _ -> None

##register get_server_entity : endpoint -> Session.entity
let get_server_entity = function
  | Hlnet.Tcp (ip, port)
  | Hlnet.Ssl (ip, port, _)
  (* | Hlnet.Udp (ip, port) *) -> SharedChannel.get_remote_server (ip, port)

##register [cps-bypass] my_endpoint : continuation(endpoint) -> void
let my_endpoint k =
  SharedChannel.addr (function (ip, port) -> QmlCpsServerLib.return k (Hlnet.Tcp (ip, port)))



##register force_gc : -> void
let force_gc = Gc.full_major

##register size : -> int
let size = Channel.size
