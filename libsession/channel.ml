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

(** Primitives for defining channels, sending messages, manage gc and
    exportations, etc... *)

module type NETWORK = sig
  (** Type of a indetifier of a entity channel *)
  type cid

  (** Type of an entity of network. *)
  type entity

  (** Type of a serialized message. *)
  type serialized

  (** {6 Sending functions}*)

  (** [send identity cid message] Should be allows to send a
      serialized [message] to a [channel] identified by [cid] owned by
      [entity]. *)
  val send : entity -> cid -> serialized -> unit

  (** [send_then identity cid message herror hsuccess] *)
  val send_then : entity -> cid -> serialized ->
    (unit -> unit) -> (unit -> unit) -> unit

  (** {6 Hash functions} *)

  (** Hash an entity (see [Hashtbl.HashedType])*)
  val hash_entity : entity -> int

  (** Equality on entity (see [Hashtbl.HashedType])*)
  val equal_entity : entity -> entity -> bool

  (** {6 Just for debug} *)

  val entity_to_string : entity -> string

  val cid_to_string : cid -> string
end

(** A channel is like a session but it can be distributed so this
    functor required a description of the [NETWORK] and utilities for
    sending to a remote session.*)
module type M = functor (N : NETWORK) -> sig
  (** An exception throwed by some function when channel was not
      registered. *)
  exception Unregistered

  (** {6 Defined types}*)

  (** Type of a channel *)
  type ('msg, 'ctx) t

  (** Type used for identify a channel. *)
  type identity =
    | LocalId of int
    | EntityId of N.cid

  (** {6 Create channel } *)
  (** [make state unserialize handler on_delete context more ronly]
      Make a local channel. A channel can be seen as a session that we
      can communicate and that can receive message from a network.
      Therefore a creation of a channel is very close to a creation of
      a session except that :

      - We can provide a [unserialize] function which should allows to
      unserialize a message from the serialized format of the
      network. [unserialize] take also an optionnal context, this
      context is provided by the sender.

      - [ondelete] channel unlike that [ondelete] session take an
      optional [identity] parameter. This one corresponding to the
      channel identity if this one was instiated (see identify)

      - Optional [more] information can be attached to a local
      channel.

      For more explication see [Session.make] *)
  val make : 'st ->
    ('ctx option -> N.serialized -> 'msg option) ->
    ('st -> 'msg -> 'ctx option -> 'st option) ->
    (identity option -> unit) option ->
    'ctx option -> Obj.t option -> bool ->
    ('msg,'ctx) t

  (** Like [make] but instead of take synchronous functions that take
      asynchronous functions that should be writted on continuation
      passing style. *)
  val make_cps :
    'st ->
    ('ctx option -> N.serialized -> ('msg option -> unit) -> unit) ->
    ('st -> 'msg -> 'ctx option -> ('st option -> unit) -> unit) ->
    (identity option -> unit) option ->
    'ctx option -> Obj.t option -> bool ->
    ('msg,'ctx) t

  (** {6 Sending functions} *)
  (** [send channel serialize message context] Like [Session.send] but
      we can provide a [serialize] function that will used if the
      channel is not local. *)
  val send : ('msg,'ctx) t ->
    ('msg -> N.serialized) ->
    'msg -> 'ctx option ->
    unit

  (** Like [send] but with an error handler will be executed if
      message was not delivered. On entity channel the guarantee of
      error handler execution can depends of [N.send_then]
      behavior. *)
  val send_then : ('msg,'ctx) t ->
    ('msg -> N.serialized) ->
    'msg -> 'ctx option ->
    (unit -> unit) ->
    (unit -> unit) ->
    unit

  (** Like [send] but with a serialized message. *)
  val forward : ('msg,'ctx) t ->
    N.serialized -> 'ctx option ->
    unit

  (** Like [send_error] but with a serialized message. *)
  val forward_then : ('msg,'ctx) t ->
    N.serialized -> 'ctx option ->
    (unit -> unit) ->
    (unit -> unit) ->
    unit

  (** {6 Identifing/Search/Removing} *)

  (** Get the [identity] of given channel. If you call [identify] on a
      local channel for the first time this one will be instiated and
      an identity it assigned. This for allows to communicate this
      channel. We can retrieve this channel thanks [find]. *)
  val identify : ('msg,'ctx) t -> identity

  (** Register an entity channel relied to a network entity.
      @throw Unregistered When channel is already registered and don't
      register given channel.  *)
  val register : N.cid -> N.entity -> ('msg,'ctx) t

  (** Like [register] but keep a strong reference to channel. Use it
      if you do not want the registered channel can be collected by
      the GC. *)
  val register_strong : N.cid -> N.entity -> ('msg, 'ctx) t

  (** Find a channel from an [identity].
      @throw Unregistered When [identity] corresponding to any
      channel.  *)
  val find : identity -> ('msg,'ctx) t

  (** Remove channel from an [identity]. *)
  val remove : identity -> unit

  (** Remove all channel owned (registered) by an entity. *)
  val remove_entity : N.entity -> unit

  (** {6 Export} *)

  (** Export a [channel] for a network [entity], that inhibits garbage collection
      of this channel until [release] is called. *)
  val export : ('msg,'ctx) t -> N.entity -> identity

  (** Release an exportation for a network [entity] of a channel
      identified by [identity]. *)
  val release : N.entity -> identity -> unit

  (** Release all exported channel for the given [entity]. *)
  val release_all : N.entity -> unit

  (** Channels comparison function. *)
  val compare : ('msg, 'ctx) t -> ('msg, 'ctx) t -> int

  (** Returns the owner of a channel. [None] indicates that the
      channel is local.
      @throw Unregistered If channel is unknown *)
  val owner : ('msg, 'ctx) t -> N.entity option

  (** Add a callback that will be executed when channel is removed. *)
  val on_remove : ('msg, 'ctx) t -> (unit -> unit) -> unit

  (** Get a string representation of an [identity]. *)
  val identity_to_string : identity -> string

  (** {6 Deprecated/Hack} *)
  (** Fail on non local channel*)
  val get_more : ('msg, 'ctx) t -> Obj.t option

  val unserialize : ('msg, 'ctx) t -> N.serialized -> ('msg option -> unit) -> unit

end

(** [make scheduler] Create the functor [M] from a [scheduler]*)

let make scheduler =

  let module Llsession = (val Llsession.make scheduler : Llsession.M) in

  let module Implem (N : NETWORK) = struct

  module U = SessionUtils

  let (@>) = U.(@>)

  exception Unregistered

  type identity =
    | LocalId of int
    | EntityId of N.cid

  (* ****************************************************************)
  (* MANAGE LOCAL CHANNEL *******************************************)
  module Local : sig
    (** {6 Type that identify a local channel} *)

    (** Type of a local channel. *)
    type ('msg,'ctx) t

    (** External identifier can be serialized and communicated. *)
    type sid = int

    (** {6 Create a local channel }*)

    val make_cps : 'st ->
      ('ctx option -> N.serialized -> ('msg option -> unit) -> unit) ->
      ('st -> 'msg -> 'ctx option -> ('st option -> unit) -> unit) ->
      (identity option -> unit) option ->
      'ctx option ->
      Obj.t option ->
      bool ->
      ('msg,'ctx) t

    val make : 'st ->
      ('ctx option -> N.serialized -> 'msg option) ->
      ('st -> 'msg -> 'ctx option -> 'st option) ->
      (identity option -> unit) option ->
      'ctx option ->
      Obj.t option ->
      bool ->
      ('msg, 'ctx) t


    (** {6 Send functions}*)

    val send : ('msg,'ctx) t -> 'msg -> 'ctx option -> unit

    val send_then : ('msg,'ctx) t -> 'msg -> 'ctx option -> (unit -> unit) -> (unit -> unit) -> unit

    val forward : ('msg,'ctx) t -> N.serialized -> 'ctx option -> unit

    val forward_then : ('msg,'ctx) t -> N.serialized -> 'ctx option -> (unit -> unit) -> (unit -> unit) -> unit

    (** {6 Utilities}*)

    val instantiate : ?strong:bool -> ('msg, 'ctx) t -> sid

    val relax : ('msg, 'ctx) t -> unit

    val find : sid -> ('msg, 'ctx) t

    val remove : ('msg, 'ctx) t -> unit

    val mem : ('msg, 'ctx) t -> bool

    val on_remove : ('msg, 'ctx) t -> (unit -> unit) -> unit

    val get_more : ('msg, 'ctx) t -> Obj.t option

    val unserialize : ('msg, 'ctx) t -> N.serialized -> ('msg option -> unit) -> unit

  end = struct
    type ('msg,'ctx) t = {
      lid : int;
      session : ('msg,'ctx) Llsession.t;
      more : Obj.t option;
      unserialize : 'ctx option -> N.serialized -> ('msg option -> unit) -> unit;
    }

    type sid = int

    (* INTERNAL DATA STRUCTURE ******************)
    type t2 = (unit, unit) t

    let to_t2 (t:('a, 'b) t) = ((Obj.magic t):t2)

    module W = WeakHashtbl.Make
      (struct
         type t = sid
         let equal = (=)
         let hash = Hashtbl.hash
       end)
      (struct
         type t = t2
         let compare a b = compare a.lid b.lid
         let hash a = a.lid
       end)

    (* schan -> lchan *)
    let weak = W.create 1024

    (* lchan.lid -> schan *)
    let rhtbl : (int, sid) Hashtbl.t = Hashtbl.create 1024

    (* lchan.lid -> remove callbacks *)
    let rcbhtbl : (int, unit -> unit) Hashtbl.t = Hashtbl.create 1024

    (* set for id generation without collisions *)
    let idset = ref IntSet.empty


    (* UTILS ************************************)
    (** Generate a fresh and random identifier for local channel *)
    let generate_id =
      #<If:DIFFING>
        let id = ref 0 in
        (fun () ->
           incr(id);
           !id
        )
      #<Else>
        (fun () ->
           let id = U.generate_without_conflicts (fun id -> IntSet.mem id !idset) in
           idset := IntSet.add id !idset;
           id
        )
      #<End>

    let remove_by_id lchan_lid =
      idset := IntSet.remove lchan_lid !idset;
      #<If>
        U.debug "CHANNEL" "Remove channel %d : %d rcb"
        lchan_lid (try List.length (Hashtbl.find_all rcbhtbl lchan_lid) with Not_found -> 0);
      #<End>;
      let _ =
        let rec aux () =
          (Hashtbl.find rcbhtbl lchan_lid) ();
          Hashtbl.remove rcbhtbl lchan_lid;
          aux () in
        try aux () with Not_found -> ()
      in
      if Hashtbl.mem rhtbl lchan_lid then (
        let schan = Hashtbl.find rhtbl lchan_lid in
        Hashtbl.remove rhtbl lchan_lid;
        W.remove weak schan;
      ) else ()

    let common_make session_make
        state unserialize handler ondelete context more ronly =
      let lid = generate_id () in
      #<If>
        U.debug "CHANNEL" "Create channel %d" lid;
      #<End>;
      let ondelete () =
        let identity =
          try
            Some (LocalId (Hashtbl.find rhtbl lid))
          with Not_found -> None in
        remove_by_id lid;
        Option.iter (fun f -> f identity) ondelete in
      {
        lid = lid;
        session = session_make state context handler (Some ondelete) ronly;
        more = more;
        unserialize = unserialize;
      }

    let instantiate ?(strong=false) lchan =
      try
        Hashtbl.find rhtbl lchan.lid
      with Not_found ->
        begin
          let schan =
            U.generate_without_conflicts
              (fun id -> W.mem weak id) in
          #<If>
            U.debug "CHANNEL" "Instantiating channel %d as %d"
            lchan.lid schan;
          #<End>;
          (if strong then W.strong else W.add) weak schan (Obj.magic lchan);
          Hashtbl.add rhtbl lchan.lid schan;
          schan
        end

    let relax lchan =
      #<If>
        U.debug "CHANNEL" "Relax channel %d"
        lchan.lid;
      #<End>;
      let schan = Hashtbl.find rhtbl lchan.lid in
      W.relax weak schan

    let find schan =
      try
        Obj.magic (W.find weak schan)
      with Not_found ->
        #<If>
          U.debug "CHANNEL" ""
        #<End>;
        raise Unregistered

    let remove lchan = remove_by_id lchan.lid

    let mem lchan = IntSet.mem lchan.lid  !idset

    let get_more channel = channel.more

    let unserialize channel msg k = channel.unserialize None msg k

    (* CREATION/INITIALISATION ******************)
    let make_cps x =
      common_make Llsession.make_cps x

    let make x unserialize =
      let unserialize ctx msg k =  k (unserialize ctx msg) in
      common_make Llsession.make x unserialize

    (* SENDING **********************************)
    let send lchan message context =
      Llsession.send
        lchan.session
        message context

    let send_then lchan message context herror hsuccess =
      Llsession.send_then
        lchan.session
        message context
        herror hsuccess

    let forward lchan serialized context =
      (lchan.unserialize context serialized) @>
        function
          | Some message -> send lchan message context
          | None -> U.error "CHANNEL"
              "An error occurs on unserialization for a local channel %d"
                lchan.lid

    let forward_then lchan serialized context herror hsuccess =
      (lchan.unserialize context serialized) @>
        function
          | Some message -> send_then lchan message context herror hsuccess
          | None -> U.error "CHANNEL"
              "An error occurs on unserialization for a local channel %d"
                lchan.lid

    let on_remove chan rcb =
      if not (mem chan) then (
        Scheduler.push scheduler rcb
      ) else (
        #<If>
          U.debug "CHANNEL" "Add on_remove callback for channel %d"
          chan.lid;
        #<End>;
        (* Just add use find_all for retrive list of callbacks *)
        Hashtbl.add rcbhtbl chan.lid rcb
      )

  end


  (* ****************************************************************)
  (* MANAGE NETWORK ENTITY CHANNEL **********************************)
  module Entity : sig

    type t

    val send : t -> N.serialized -> unit

    val send_then : t -> N.serialized -> (unit -> unit) -> (unit -> unit) -> unit

    val find : N.cid -> t

    val remove : t -> unit

    val remove_entity : N.entity -> unit

    val get_cid : t -> N.cid

    val register : strong:bool -> N.cid -> N.entity -> t

    val find_entity : t -> N.entity

    val on_remove : t -> (unit -> unit) -> unit

    val strong : t -> N.cid

    val relax : N.cid -> unit

  end = struct

    type t

    type t2 = t

    module W = WeakHashtbl.Make
      (struct
         type t = N.cid
         let equal = (=)
         let hash = Hashtbl.hash
       end)
      (struct
         type t = t2
         let compare = compare
         let hash = Hashtbl.hash
       end)

    let (weak : W.t) = W.create 512

    let (entity_channels : (N.cid, N.entity * (unit -> unit)list) Hashtbl.t) = Hashtbl.create 512

    let find cid =
      try
        W.find weak cid
      with Not_found -> raise Unregistered

    let get_cid (t : t) = ((Obj.magic t) : N.cid)

    let strong (t : t) =
      let cid = Obj.obj (Obj.dup (Obj.repr t)) in
      W.strong weak cid t;
      cid

    let relax cid = W.relax weak cid

    let find_entity chan =
      let chan = get_cid chan in
      try
        fst (Hashtbl.find entity_channels chan)
      with Not_found -> raise Unregistered


    let remove chan =
      let cid = get_cid chan in
      let cbs =
        try snd (Hashtbl.find entity_channels cid)
        with Not_found -> [] in
      #<If>
        U.debug "CHANNEL" "Removing entity channel %s : %d rcb"
        (N.cid_to_string cid) (List.length cbs);
      #<End>;
      List.iter (fun cb -> Scheduler.push scheduler cb) cbs;
      Hashtbl.remove entity_channels cid


    let register ~strong cid entity =
      (* Duplicate because we want t can be collected *)
      let (t : t) = Obj.obj (Obj.dup (Obj.repr cid)) in
      Scheduler.finalise scheduler remove t;
      if W.mem weak cid then raise Unregistered
      else (
        #<If>
          U.debug "CHANNEL" "Registering entity channel %s of %s"
          (N.cid_to_string cid) (N.entity_to_string entity);
        #<End>;
        Hashtbl.add entity_channels cid (entity, []);
        (if strong then W.strong else W.add) weak cid t;
        t
      )

    let remove_entity entity =
      #<If>
        U.debug "CHANNEL" "Removing all entity channel of %s"
        (N.entity_to_string entity);
      #<End>;
      Hashtbl.iter
        (fun cid (v,_) ->
           if v = entity then (
             remove (find cid)
           )
        ) entity_channels

    let send chan serialized =
      let cid = get_cid chan in
      try
        let entity = find_entity chan in
        #<If>
          U.debug "CHANNEL" "Sending a message to entity channel %s of %s"
          (N.cid_to_string cid) (N.entity_to_string entity);
        #<End>;
        N.send entity cid serialized
      with Unregistered ->
        #<If>
          U.error "CHANNEL"
          "On sending entity channel %s is not found" (N.cid_to_string cid);
        #<End>;
        ()

    let send_then chan serialized herror hsuccess =
      let cid = get_cid chan in
      try
        let entity = find_entity chan in
        #<If>
          U.debug "CHANNEL"
          "Sending with herror message to entity channel %s of %s"
          (N.cid_to_string cid) (N.entity_to_string entity);
        #<End>;
        N.send_then entity cid serialized herror hsuccess
      with Unregistered ->
        #<If>
          U.error "CHANNEL"
          "On sending entity channel %s is not found, execute herror"
          (N.cid_to_string cid);
        #<End>;
        herror ()

    let on_remove chan rcb =
      let cid = get_cid chan in
      #<If>
        U.debug "CHANNEL" "Add on_remove callback for entity channel %s "
        (N.cid_to_string cid);
      #<End>;
      try
        let e, cbs = Hashtbl.find entity_channels cid in
        Hashtbl.replace entity_channels cid (e, rcb::cbs)
      with Not_found -> Scheduler.push scheduler rcb
  end


  (* ****************************************************************)
  (* MANAGE SUM OF CHANNELS *****************************************)
  type ('msg,'ctx) t =
    | Local of ('msg, 'ctx) Local.t
    | Entity of Entity.t

  let on_remove channel on_remove =
    match channel with
    | Local chan -> Local.on_remove chan on_remove
    | Entity chan -> Entity.on_remove chan on_remove



  let make_cps state unserialize handler ondelete ctx more ronly =
    Local (Local.make_cps state unserialize handler ondelete ctx more ronly)

  let make state unserialize handler ondelete ctx more ronly =
    Local (Local.make state unserialize handler ondelete ctx more ronly)

  let send channel serialize message ctx =
    match channel with
    | Local chan -> Local.send chan message ctx
    | Entity chan -> Entity.send chan (serialize message)

  let send_then channel serialize message ctx herror hsuccess =
    match channel with
    | Local chan -> Local.send_then chan message ctx herror hsuccess
    | Entity chan -> Entity.send_then chan (serialize message) herror hsuccess

  let forward channel serialized context =
    match channel with
    | Local chan -> Local.forward chan serialized context
    | Entity chan -> Entity.send chan serialized

  let forward_then channel serialized context herror hsuccess =
    match channel with
    | Local chan -> Local.forward_then chan serialized context herror hsuccess
    | Entity chan -> Entity.send_then chan serialized herror hsuccess

  let find identity =
    match identity with
    | LocalId sid -> Local (Local.find sid)
    | EntityId cid -> Entity (Entity.find cid)

  let remove identity =
    match identity with
    | LocalId sid -> Local.remove (Local.find sid)
    | EntityId cid -> Entity.remove (Entity.find cid)

  let identify channel =
    match channel with
    | Local chan -> LocalId (Local.instantiate chan)
    | Entity chan -> EntityId (Entity.get_cid chan)

  let compare c1 c2 =
    match c1, c2 with
    | Local _, Entity _ -> -1
    | Entity _, Local _ -> 1
    | _, _ -> Pervasives.compare c1 c2

  let owner = function
    | Local _ -> None
    | Entity chan -> Some (Entity.find_entity chan)

  let register c e = Entity (Entity.register ~strong:false c e)

  let register_strong c e = Entity (Entity.register ~strong:true c e)

  let get_more channel =
    match channel with
    | Local chan -> Local.get_more chan
    | _ -> invalid_arg "get_more : Channel is not local"

  let unserialize channel =
    match channel with
    | Local chan -> Local.unserialize chan
    | _ -> invalid_arg "get_more : Channel is not local"



  (* Keep a strong reference on channel *)
  module IdentitySet = BaseSet.Make (
    struct
      type t = identity
      let compare = Pervasives.compare
    end)

  module EntityHash = BaseHashtbl.Make (
    struct
      type t = N.entity
      let equal = N.equal_entity
      let hash = N.hash_entity
    end)

  let exported : IdentitySet.t EntityHash.t = EntityHash.create 512

  let is_exported k =
    EntityHash.fold (fun _ set b -> b || (IdentitySet.mem k set)) exported false

  let identity_to_string = function
    | LocalId i -> Printf.sprintf "ID:Local(%d)" i
    | EntityId cid -> Printf.sprintf "ID:Entity(%s)" (N.cid_to_string cid)

  let print_exportations () =
    EntityHash.iter
      (fun e set ->
         U.debug "CHANNEL-EXPORT" "Entity %s export :"
           (N.entity_to_string e);
         IdentitySet.iter
           (fun id ->
              U.debug "CHANNEL-EXPORT" "  -> %s" (identity_to_string id))
           set)
      exported;
    U.debug "CHANNEL-EXPORT" "%!"

  let release e k =
    #<If>
      U.debug "CHANNEL-EXPORT" "Release channel %s for entity %s"
      (identity_to_string k) (N.entity_to_string e);
    #<End>;
    (try
       let set = EntityHash.find exported e in
       let set = IdentitySet.remove k set in
       if IdentitySet.is_empty set  then
         EntityHash.remove exported e
       else EntityHash.replace exported e set
     with Not_found -> ());
    #<If$minlevel 50>
      let count k =
        EntityHash.fold (fun _ set b -> if IdentitySet.mem k set then b+1 else 0)
          exported 0 in
      U.debug "CHANNEL-EXPORT" "Channel %s has %d exportations : %b"
      (identity_to_string k) (count k) (is_exported k);
      print_exportations ();
    #<End>;
    if not (is_exported k) then
      match k with
      | LocalId sid ->
          (try Local.relax (Local.find sid) with Unregistered -> ())
      | EntityId cid -> Entity.relax cid

  let export t e =
    let id = match t with
    | Local chan ->
        let sid = (Local.instantiate ~strong:true chan) in
        #<If>
          U.debug "CHANNEL" "Exporting local channel for entity %s as %d"
          (N.entity_to_string e) sid;
        #<End>;
        LocalId sid
    | Entity chan ->
        let id = Entity.strong chan in
        #<If>
          U.debug "CHANNEL" "Exporting entity channel for entity %s as %s"
          (N.entity_to_string e) (N.cid_to_string id);
        #<End>;
        EntityId id
    in
    let set =
      try
        EntityHash.find exported e
      with Not_found -> IdentitySet.empty in
    let set = IdentitySet.add id set in
        EntityHash.replace exported e set;
    #<If>
      print_exportations ();
    #<End>;
    on_remove t (fun _ -> release e id (*TODO : Inform e when t is removed*) );
    id

  let release_all e =
    #<If>
      U.debug "CHANNEL" "Release all exported channel for entity %s"
      (N.entity_to_string e);
    #<End>;
    let set =
      try
        EntityHash.find exported e
      with Not_found -> IdentitySet.empty in
    EntityHash.remove exported e;
    IdentitySet.iter
      (fun k ->
         if not (is_exported k) then
           match k with
           | LocalId sid -> Local.relax (Local.find sid)
           | EntityId eid -> Entity.relax eid)
      set;
    #<If>
      print_exportations ();
    #<End>

  let remove_entity entity =
    release_all entity;
    Entity.remove_entity entity

  end in (module Implem : M)
