(*
    Copyright © 2011, 2012 MLstate

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
module Hashtbl = BaseHashtbl

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

let send_txt_response winfo txt code =
   HS.make_response_modified_since
     (Time.now ())
     winfo.HST.request
     code
     "text/plain; charset=utf-8"
     (Http_common.Result txt)
     winfo.HST.cont

let send_json_response winfo json code =
  let txt = Json_utils.to_string json in
  #<If>
    ping_debug "SEND" "Sending json (%s)" txt;
  #<End>;
  send_txt_response winfo txt code

let send_unmodified winfo txt =
  HS.make_response ~req:winfo.HST.request Requestdef.SC_NotModified
    "text/plain" (Http_common.Result txt) winfo.HST.cont

let disconnection_state_delay = ref (120 * 1000)
let inactive_state_delay = ref None
let ping_delay_client_msecond_rush = 3 * 1000
let ping_delay_client_msecond_normal = 30 * 1000

let max_package_size = 10

module Make (S : SCHEDULER) (C : CLIENT) = struct

  (** Connection events*)
  type event =
    | Connect
    | Disconnect
    | Inactive

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
    | Inactive        -> "Inactive"

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
    send_json_response winfo (response_to_json response) Requestdef.SC_OK

  let send_error winfo reason =
    send_txt_response winfo reason Requestdef.SC_ResetContent

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

    val send_error : C.key -> string -> unit

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
                send_response winfo Break
              )
          | Messages q ->
              if Queue.is_empty q then (
                replace key (Ajax_call (winfo, nb, sleep_pong ()))
              ) else (
                send_with_winfo key winfo q;
                if (Queue.is_empty q) then remove key
              )
        with Not_found ->
          add key (Ajax_call (winfo, nb, sleep_pong ()))
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

    let send_error key msg =
      try
        match find key with
        | Ajax_call (winfo, _, sk) ->
            S.abort sk;
            remove key;
          send_error winfo msg
        | _ -> remove key
      with Not_found -> ()

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
    val pang : C.key -> HST.web_info -> int -> bool -> unit

    (** Broadcast the json message. *)
    val broadcast : C.msg -> unit

    (** Returns the number of connections. *)
    val size : unit -> int

    (** Sending the json message on the given client connection
        identifier. *)
    val send : C.msg -> C.key -> unit

    (** Return a result of a pang. *)
    val return : C.key -> int -> string -> unit

    val update_activity : ?nb:int -> ?is_ping:bool -> ?is_active:bool ->
    ?winfo:HST.web_info -> C.key -> bool

    val set_inactive_delay : C.key option -> Time.t option -> unit

  end = struct

    type event_key = (C.key option * event * int)

    module EventMap = BaseMap.Make(
      struct
        type t = event
        let compare = Pervasives.compare
      end)

    (* Client identifier to last ping number. *)
    let state_tbl : (C.key,(int *
      S.async_key * int * (* Disconnection key and delay *)
      S.async_key option * int option (* Inactivity key and delay *)
    )) Hashtbl.t =
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
      Entry.send_error key "You've been disconnected by the server";
      (* remove key; *)
      (try
        let (_, old_s, _,old_s2,_) = Hashtbl.find state_tbl key in
        S.abort old_s;
        Option.iter S.abort old_s2;
        Hashtbl.remove state_tbl key;
      with
      | Not_found -> ());
      remove_events key

    let update_activity ?(nb=(-1)) ?(is_ping=false) ?(is_active=false) ?winfo key=
      let will_disconnect t nb key =
        S.sleep t
          (fun () ->
            try
              let (n, _, _, _, _) = Hashtbl.find state_tbl key in
              if n=nb then delete key
            with Not_found -> delete key
          ) in
      let will_raise_timeout t key =
        S.sleep t (fun () -> raise_event key Inactive ) in
      match Hashtbl.find_opt state_tbl key with
        Some((old_nb, old_s, d, old_s2, d2)) ->
          let s =
            if is_ping
            then (S.abort old_s; will_disconnect !disconnection_state_delay nb key)
            else old_s in
          let s2 =
            if is_active
            then (Option.iter S.abort old_s2;
            match Option.default (Option.default 0 !inactive_state_delay) d2  with
            | 0 -> None
            | n -> Some(will_raise_timeout n key))
            else old_s2 in
          Hashtbl.replace state_tbl key ((if nb = -1 then old_nb else nb), s, d, s2, d2);
          true
      | None ->
          ignore(winfo);
          let s = will_disconnect !disconnection_state_delay nb key in
          let s2 =
            match Option.default 0 !inactive_state_delay with
            | 0 -> None
            | n -> Some(will_raise_timeout n key) in
          Hashtbl.add state_tbl key (nb, s, ping_delay_client_msecond_rush, s2, None);
          true

    let create key = ignore(update_activity ~is_ping:true ~nb:1 key)

    let find_delay key =
      try
        let (_, _, d, _, _) = Hashtbl.find state_tbl key in d
      with
      | Not_found ->
          #<If>
            ping_debug "PING" "Delay not found for client %s" (C.key_to_string key);
          #<End>;
          ping_delay_client_msecond_normal

    let set_inactive_delay key_opt time_opt =
      match key_opt with
      | None -> inactive_state_delay:=Option.map Time.in_milliseconds time_opt
      | Some key ->
          let will_raise_timeout t key =
            S.sleep t (fun () -> raise_event key Inactive ) in
          try
            let (n,s,d,s2,_) = Hashtbl.find state_tbl key in
            let time = Option.map Time.in_milliseconds time_opt in
            Option.iter S.abort s2;
            Hashtbl.replace state_tbl key (n, s, d, Option.map (fun t -> will_raise_timeout t key) time, time)
          with Not_found -> ()

    let end_of_rush_delay key =
      try
        let (n, s, _, s2, d2) = Hashtbl.find state_tbl key in
        Hashtbl.replace state_tbl key (n, s, ping_delay_client_msecond_normal, s2, d2)
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
      if update_activity ~is_ping:true ~nb key ~winfo
      then Entry.ping ~crush:(nb=1) ~find_delay:find_delay ~pong_callback:end_of_rush_delay key winfo nb

    let pang key winfo nb is_active =
      #<If>
        ping_debug "PING"
        "PANG (%d) received from %s" nb (C.key_to_string key);
      #<End>;
      if update_activity ~is_ping:true ~nb ~is_active key ~winfo
      then Entry.pang key winfo nb

    let broadcast mess =
      #<If>
        ping_debug "PING" "Broadcasting to clients"
      #<End>;
      iter (Entry.send mess)

    let size () = Hashtbl.length state_tbl

    let send msg key =
      ignore(update_activity ~is_active:true key);
      Entry.send msg key

    let return key nb result =
      ignore(update_activity ~is_active:true key);
      Entry.return key nb result
  end

  type event_key = Connection.event_key

  (** {6 Exported functions}*)

  let register_event = Connection.register_event

  let remove_event = Connection.remove_event

  let send = Connection.send

  let broadcast = Connection.broadcast

  let ping = Connection.ping

  let pang = Connection.pang

  let return = Entry.return

  let mem = Connection.mem

  let delete = Connection.delete

  let create = Connection.create

  let size = Connection.size

  let update_activity = Connection.update_activity

  let set_inactive_delay = Connection.set_inactive_delay

end
