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
#<Debugvar:PING_DEBUG>

module JS = JsonTypes
module RD = Requestdef
module HS = HttpServer
module HSC = HttpServerCore
module HST = HttpServerTypes

type json = JS.json

let (@>) f k = f k

let (|>) = InfixOperator.(|>)

module type CLIENT = sig
  type key
  type msg
  val serialize : msg -> json Cps.t
  val key_to_string : key -> string
end

module type SCHEDULER = sig
  type async_key
  val sleep : int -> (unit -> unit) -> async_key
  val abort : async_key -> unit
end


let ping_debug level fmt =
  Logger.debug ("[PING][%s] "^^fmt^^"%!") level

let ping_info level fmt =
  Logger.info ("[PING][%s] "^^fmt^^"%!") level

let ping_error level fmt =
  Logger.error ("[PING][%s] "^^fmt^^"%!") level

let send_txt_response winfo txt =
  winfo.HST.cont (HS.make_response_modified_since
                (Time.now ())
                winfo.HST.request
                Requestdef.SC_OK
                "text/plain, charset=utf-8"
                (Http_common.Result txt))

let send_json_response winfo json =
  let txt = Json_utils.to_string json in
  #<If>
    ping_debug "SEND" "Sending json (%s)" txt;
  #<End>;
  send_txt_response winfo txt

let send_error winfo txt =
  winfo.HST.cont (HS.make_response ~req:winfo.HST.request Requestdef.SC_Unauthorized
                "text/plain" (Http_common.Result txt))

let disconnection_state_delay = 120 * 1000
let ping_delay_client_msecond_rush = 3 * 1000
let ping_delay_client_msecond_normal = 30 * 1000

let max_package_size = 10

module Make (S : SCHEDULER) (C : CLIENT) = struct

  (** Connection events*)
  type event =
    | Connect
    | Disconnect

  (** Type of a ping(/pang) loop response. *)
  type response =
    | Msgs of json list
    | Pong
    | Break
    | Result of int * string

  (** Event to string (for debugging)*)
  let event_to_string = function
    | Connect         -> "Connect"
    | Disconnect      -> "Disconnect"

  (** Make a json response which may be interpreted by client ping
      loop. *)
  let response_to_json = function
    | Msgs l -> JS.Record [("type", JS.String "msgs"); ("body", JS.Array l)]
    | Pong -> JS.Record [("type", JS.String "pong")]
    | Break -> JS.Record [("type", JS.String "break")]
    | Result (id, result) -> JS.Record [("type", JS.String "result");
                                        ("body", JS.String result);
                                        ("id", JS.Int id);]

  let send_response winfo response =
    send_json_response winfo (response_to_json response)

  (** Manage communications with clients *)
  module Entry : sig

    type t =
      | Ajax_call of HST.web_info * int * S.async_key
      | Messages of json Queue.t

    (** Bind a [cid] and an entry. The [cid]
        must not be already binded (assertion).*)
    val add : C.key -> t -> unit

    (** Find the entry corresponding to a cid. If the given [cid] is
        not binded throw [Not_found].*)
    val find : C.key -> t

    (** Remove the binding for a [cid]. *)
    val remove : C.key -> unit

    (** Replace the binding of [cid], if it not binded add this one.*)
    val replace : C.key -> t -> unit

    (** Pong the client identified by the given parameter if the given
        number coresponding. *)
    val pong : ?pong_callback:(C.key -> unit) -> C.key -> int -> unit

    (** Sending the json message on the given client connection
        identifier. *)
    val send : C.msg -> C.key -> unit

    (** Remove the client connection from this manager*)
    val remove : C.key -> unit

    (** Update the status of the client connection and update the
        [web_info] that allows to send a message to the corresponding
        client with [send]. *)
    val ping : ?crush:bool -> ?find_delay:(C.key -> int) -> ?pong_callback:(C.key -> unit) ->
      C.key -> HST.web_info -> int -> unit

    (** Like [ping] but allows to return to this specific pang with
        [return]. *)
    val pang : C.key -> HST.web_info -> int -> unit

    (** Return a result of a pang. *)
    val return : C.key -> int -> string -> unit

  end = struct

    type t =
      | Ajax_call of HST.web_info * int * S.async_key
      | Messages of json Queue.t

    let entry_tbl = Hashtbl.create 1024

    (* Client identifier to all PANG result *)
    let pang_tbl : (C.key, string IntMap.t) Hashtbl.t =
      Hashtbl.create 512

    let find key =
      Hashtbl.find entry_tbl key

    let remove key =
      Hashtbl.remove entry_tbl key

    let replace key =
      Hashtbl.replace entry_tbl key

    let add key entry =
      assert (not (Hashtbl.mem entry_tbl key));
      Hashtbl.add entry_tbl key entry

    (** Pop [max_package_size] on the queue and send it on winfo. *)
    let send_with_winfo _key winfo queue =
      let rec aux i =
        let t = Queue.pop queue in
        if Queue.is_empty queue then
          [t]
        else
          t::(aux (i+1))
      in
      let to_send = aux 0 in
      #<If>
        let l = (List.length to_send) in
        ping_debug "PING" "Send %d/%d messages to %s"
          l (Queue.length queue + l) (C.key_to_string _key);
      #<End>;
      send_response winfo (Msgs to_send)

    let send mess key =
      C.serialize mess @> function json ->
        #<If>
          let json = (Json_utils.to_string json) in
          ping_debug "PING" "Try send to %s => (%s)" (C.key_to_string key) json;
          #<End>;
          try
            match find key with
            | Ajax_call(winfo, _, sk) ->
                #<If>
                  ping_debug "PING" "Send to %s => (%s)"
                  (C.key_to_string key) (Json_utils.to_string json);
                #<End>;
                send_response winfo (Msgs [json]);
                S.abort sk;
                remove key
            | Messages lst ->
                Queue.push json lst;
                #<If>
                  let json = (Json_utils.to_string json) in
                  ping_debug "PING" "Store (%d) for %s => (%s)"
                    (Queue.length lst) (C.key_to_string key) json;
                  #<End>;
                  ()
          with Not_found ->
            (* TODOK1 : Check if cid is registered on Connection*)
            let lst = Queue.create () in
            Queue.push json lst;
            #<If>
              ping_debug "PING" "Store (%d) for %s => (%s)"
              (Queue.length lst) (C.key_to_string key) (Json_utils.to_string json);
            #<End>;
            add key (Messages lst)

    let pong ?(pong_callback=fun _ -> ()) key nb =
      try
        match find key with
        | Ajax_call (winfo, nb2, _) when nb = nb2 ->
            #<If>
              ping_debug "PING" "Sending a pong to %s" (C.key_to_string key);
            #<End>;
            send_response winfo Pong;
            remove key;
            pong_callback key
        | _ ->
            #<If>
              ping_debug "PING"
              "PONG (%d) for %s not sended, request is already consumed"
              nb (C.key_to_string key);
            #<End>;
            ()
      with Not_found ->
        #<If>
          ping_debug "PING"
          "PONG (%d) for %s not sended, request is already consumed"
          nb (C.key_to_string key);
        #<End>;
        ()

    let ping ?(crush=false) ?(find_delay=fun _ -> ping_delay_client_msecond_normal)
        ?(pong_callback=fun _ -> ()) key winfo nb =
      let iping () =
        let sleep_pong () =
          #<If>
            ping_debug "PING""A PONG (%d) is programmed for %s"
            nb (C.key_to_string key);
          #<End>;
          S.sleep
            (find_delay key)
            (fun () -> pong ~pong_callback key nb)
        in
        try
          match find key with
          | Ajax_call (owinfo, n, sk) ->
              if crush then (
                S.abort sk;
                replace key (Ajax_call (winfo, nb, sleep_pong ()));
                send_response owinfo Break
              ) else (
                ping_error "PING"
                  "PING(%d) not registered PING(%d) already present"
                  n nb;
                send_error winfo "Already present"
              )
          | Messages q ->
              if Queue.is_empty q then (
                replace key (Ajax_call (winfo, nb, sleep_pong ()))
              ) else (
                send_with_winfo key winfo q;
                if (Queue.is_empty q) then remove key
              )
        with Not_found -> add key (Ajax_call (winfo, nb, sleep_pong ()))
      in
      if Hashtbl.mem pang_tbl key then
        let map = Hashtbl.find pang_tbl key in
        if not (IntMap.is_empty map) then (
          let n, result = IntMap.min map in
          Hashtbl.replace pang_tbl key (IntMap.remove n map);
          send_response winfo (Result (n, result))
        ) else iping ()
      else iping ()

    let pang key winfo nb =
      if not (Hashtbl.mem pang_tbl key) then
        Hashtbl.add pang_tbl key IntMap.empty;
      ping ~crush:true key winfo nb

    let return key nb result =
      #<If>
        ping_debug "PING" "PANG(%d) result sent to %s => (%s)"
        nb (C.key_to_string key) result;
      #<End>;
      (* For add result to the pang table *)
      let add_to_pang_tbl () =
        #<If>
          ping_debug "PING" "PANG(%d) wait client %s request for send result"
          nb (C.key_to_string key);
        #<End>;
        let map = Hashtbl.find pang_tbl key in
        let map =
          if IntMap.mem nb map then (
            ping_error "PING"
              "PANG(%d) result is already present" nb;
            invalid_arg "return"
          ) else
            IntMap.add nb result map
        in
        Hashtbl.replace pang_tbl key map;
      in
      try
        match find key with
        | Ajax_call (winfo, _, sk) ->
            S.abort sk;
            remove key;
            send_response winfo (Result (nb, result))
        | _ -> add_to_pang_tbl ()
      with Not_found -> add_to_pang_tbl ()

  end

  (** Manage the status of connection with client *)
  module Connection : sig

    (** Type of key for callback*)
    type event_key

    (** Register a callback that will be executed when a corresponding
        event will be launched. The first parameters indicates for
        which client the callback will be registered, if it's None
        then the callback will be executed for all client. *)
    val register_event : C.key option -> event -> (C.key -> unit) -> event_key

    (** Remove callback event registered with the given
        [event_key]. *)
    val remove_event : event_key -> unit

    (** Create connexion of a client. *)
    val create : C.key -> unit

    (** Delete connexion of a client. *)
    val delete : C.key -> unit

    (** Return [true] if the client connection identifier exists on
        the manager. *)
    val mem : C.key -> bool

    (** Iter on client connection identifier. *)
    val iter : (C.key -> unit) -> unit

    (** Update the status of the client connection. If the status is
        not updating during [disconnection_state_delay] the client
        connection will deleted. *)
    val ping : C.key -> HST.web_info -> int -> unit

    (** Like ping but allows to reply with [Entry.return] *)
    val pang : C.key -> HST.web_info -> int -> unit

    (** Broadcast the json message. *)
    val broadcast : C.msg -> unit

    (** Returns the number of connections. *)
    val size : unit -> int

  end = struct

    type event_key = (C.key option * event * int)

    module EventMap = BaseMap.Make(
      struct
        type t = event
        let compare = Pervasives.compare
      end)

    (* Client identifier to last ping number. *)
    let state_tbl : (C.key, (int * S.async_key * int (* delay *))) Hashtbl.t =
      Hashtbl.create 512

    (* Client identifier to event map that contains list of
       callback *)
    let event_tbl : (C.key option, int * ((C.key -> unit) IntMap.t) EventMap.t) Hashtbl.t =
      Hashtbl.create 512

    let register_event cid event callback =
      let k, emap =
        try Hashtbl.find event_tbl cid with Not_found -> 0, EventMap.empty in
      let nk = k+1 in
      let emap =
        let imap = Option.default IntMap.empty (EventMap.find_opt event emap) in
        EventMap.add event (IntMap.add nk callback imap) emap
      in Hashtbl.replace event_tbl cid (nk, emap);
      (cid, event, nk)

    let remove_event (cid, event, k) =
      try
        let _, emap = Hashtbl.find event_tbl cid in
        let imap = EventMap.find event emap in
        let imap = IntMap.remove k imap in
        let emap = EventMap.add event imap emap in
        Hashtbl.replace event_tbl cid (k, emap)
      with Not_found -> ()

    let get_callbacks cid event =
      let emap =
        try
          snd(Hashtbl.find event_tbl cid)
        with Not_found -> EventMap.empty in
      try
        EventMap.find event emap
      with Not_found -> IntMap.empty

    let raise_event cid event =
      #<If>
        ping_debug "PING" "Event (%s) for client %s was raised"
        (event_to_string event) (C.key_to_string cid);
      #<End>;
      (* Execute specialized callbacks *)
      let imap = get_callbacks (Some cid) event in
      #<If>
        ping_debug "PING"
        "Event (%s) %d specialized callbacks was registered"
        (event_to_string event) (IntMap.size imap)
        #<End>;
      IntMap.iter (fun _ cb -> cb cid) imap;

      (* Execute gloabal callbacks *)
      let imap = get_callbacks None event in
      #<If>
        ping_debug "PING"
        "Event (%s) %d global callbacks was registered"
        (event_to_string event) (IntMap.size imap)
        #<End>;
      IntMap.iter (fun _ cb -> cb cid) imap

    let remove_events cid =
      #<If>
        ping_debug "PING" "Remove callbacks events for client %s"
        (C.key_to_string cid);
      #<End>;
      Hashtbl.remove event_tbl (Some cid)

    let delete key =
      #<If>
        ping_debug "PING" "Remove the client %s" (C.key_to_string key);
      #<End>;
      raise_event key Disconnect;
      Entry.remove key;
      Hashtbl.remove state_tbl key;
      remove_events key

    let update key (nb:int) =
      let s =
        S.sleep disconnection_state_delay
          (fun () ->
             try
               let (n, _, _) = Hashtbl.find state_tbl key in
               if n=nb then delete key
             with Not_found -> delete key
          ) in
      try
        let (_, old_s, d) = Hashtbl.find state_tbl key in
        S.abort old_s; (* Abort the previous sleep *)
        Hashtbl.replace state_tbl key (nb, s, d)
      with
      | Not_found ->
          Hashtbl.add state_tbl key (nb, s, ping_delay_client_msecond_rush)

    let create key = update key 0

    let find_delay key =
      try
        let (_, _, d) = Hashtbl.find state_tbl key in d
      with
      | Not_found ->
          #<If>
            ping_debug "PING" "Delay not found for client %s" (C.key_to_string key);
          #<End>;
          ping_delay_client_msecond_normal

    let end_of_rush_delay key =
      try
        let (n, s, _) = Hashtbl.find state_tbl key in
        Hashtbl.replace state_tbl key (n, s, ping_delay_client_msecond_normal)
      with
      | Not_found ->
          #<If>
            ping_debug "PING" "End of rush delay: not found for client %s" (C.key_to_string key);
          #<End>;
          ()


    let mem cid =
      #<If>
        ping_debug "PING" "State table : size (%d)"
        (Hashtbl.length state_tbl);
      #<End>;
      Hashtbl.mem state_tbl cid

    let iter f =
      Hashtbl.iter (fun cid _ -> f cid) state_tbl

    let ping key winfo nb =
      #<If>
        ping_debug "PING"
        "PING(%d) received from %s" nb (C.key_to_string key);
      #<End>;
      update key nb;
      Entry.ping ~crush:(nb = 1) ~find_delay:find_delay ~pong_callback:end_of_rush_delay key winfo nb


    let pang key winfo nb =
      #<If>
        ping_debug "PING"
        "PANG (%d) received from %s" nb (C.key_to_string key);
      #<End>;
      update key nb;
      Entry.pang key winfo nb

    let broadcast mess =
      #<If>
        ping_debug "PING" "Broadcasting to clients"
      #<End>;
      iter (Entry.send mess)

    let size () = Hashtbl.length state_tbl

  end

  type event_key = Connection.event_key

  (** {6 Exported functions}*)

  let register_event = Connection.register_event

  let remove_event = Connection.remove_event

  let send = Entry.send

  let broadcast = Connection.broadcast

  let ping = Connection.ping

  let pang = Connection.pang

  let return = Entry.return

  let mem = Connection.mem

  let delete = Connection.delete

  let create = Connection.create

  let size = Connection.size

end
