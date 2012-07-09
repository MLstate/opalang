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

(* depends *)
module List = BaseList

#<Debugvar:DEBUG_DB$flag "io">


let debug fmt =
  #<If> Printf.fprintf stdout ("[35m[FileIo][0m"^^fmt^^"\n%!")
  #<Else> Printf.ifprintf stderr ("[35m[FileIo][0m"^^fmt^^"\n%!")
  #<End>

let print fmt =
  Printf.fprintf stdout ("[35m[FileIo] [0m"^^fmt^^"\n%!")

let error fmt =
  Printf.fprintf stderr ("[35m[FileIo] [31m"^^fmt^^"[0m\n%!")

(* -- *)

module Make = functor (WR:IoSig.S) ->
struct

  (* keep it for the export of the exception EOF *)
  include WR

  exception Corruption

  type t = {
    mutable is_open : bool;
    location : string;
    node_filename : string;
    uid_filename : string;
    uid_rev_filename : string;
    timestamp_filename : string;
    trans_filename : string;
    flags_filename : string;
    db_state_filename : string;
    config_filename : string;
    lock_filename : string;

    mutable node_chan : WR.chan;

    mutable uid_chan : WR.chan;

    mutable uid_rev_chan : WR.chan;

    mutable trans_chan : WR.chan;

    mutable timestamp_chan : WR.chan;

    mutable flags_chan : WR.chan;

    mutable db_state_chan : WR.chan;

    mutable config_chan : WR.chan;
  }

  type mode = Create | Append | ReadOnly
  type file = Node | Uid | Uid_rev | Timestamp | Flags | Trans | Db_state | Config | Lock
  type write = WChar of char | WInt of int | WString of string  | WFloat of float | WInt32 of int32 | WInt64 of int64
  type read = RChar | RInt | RString  | RFloat | RInt32 | RInt64


  let node_file = "_node_file"
  let uid_file  = "_uid_file"
  let uid_rev_file = "_uid_rev_file"
  let timestamp_file = "_timestamps_file"
  let flags_file = "_flags_file"
  let trans_file = "_trans_file"
  let db_state_file= "_db_state"
  let config_file= "_config"
  let lock_file = "_lock"

  let create_filename ?(suffix="") file kind =
    Printf.sprintf "%s%s%s" file kind suffix

  let make_channel = function
    | Create -> WR.make_chan_create
    | Append -> WR.make_chan_append
    | ReadOnly -> WR.make_chan_readonly

  let make ?(_extension) mode file =
    let cf = create_filename file in
    let cw f =
      (match mode with
       | Create ->
           if Sys.file_exists f then
             (#<If> Logger.warning "New db, purge: deleting file %s" f #<End>;
              Sys.remove f);
           make_channel Create f
       | mode -> make_channel mode f)
    in

    let node_filename = cf node_file in
    let uid_filename = cf uid_file in
    let uid_rev_filename = cf uid_rev_file in
    let timestamp_filename = cf timestamp_file in
    let trans_filename = cf trans_file in
    let flags_filename = cf flags_file in
    let db_state_filename = cf db_state_file in
    let config_filename = cf config_file in

    let node_chan = cw node_filename in
    let uid_chan = cw uid_filename in
    let uid_rev_chan = cw uid_rev_filename in
    let trans_chan = cw trans_filename in
    let timestamp_chan = cw timestamp_filename in
    let flags_chan = cw flags_filename in
    let db_state_chan = cw db_state_filename in
    let config_chan = cw config_filename in

    {
      is_open = true;
      location = file;
      node_filename = node_filename;
      uid_filename = uid_filename;
      uid_rev_filename = uid_rev_filename;
      timestamp_filename = timestamp_filename;
      trans_filename = trans_filename;
      flags_filename = flags_filename;
      db_state_filename = db_state_filename;
      config_filename = config_filename;
      lock_filename = cf lock_file;

      node_chan = node_chan;

      uid_chan = uid_chan;

      uid_rev_chan = uid_rev_chan;

      trans_chan = trans_chan;

      timestamp_chan = timestamp_chan;

      flags_chan = flags_chan;

      db_state_chan = db_state_chan;

      config_chan = config_chan;
    }


  let get_name entity = function
    | Node -> entity.node_filename
    | Uid -> entity.uid_filename
    | Uid_rev -> entity.uid_rev_filename
    | Timestamp -> entity.timestamp_filename
    | Trans -> entity.trans_filename
    | Flags -> entity.flags_filename
    | Db_state -> entity.db_state_filename
    | Config -> entity.config_filename
    | Lock -> entity.lock_filename

  let get_channel entity file =
    match file with
    | Node -> entity.node_chan
    | Uid -> entity.uid_chan
    | Uid_rev -> entity.uid_rev_chan
    | Timestamp -> entity.timestamp_chan
    | Trans -> entity.trans_chan
    | Flags -> entity.flags_chan
    | Db_state -> entity.db_state_chan
    | Config -> entity.config_chan
    | Lock -> assert false

  let get_location entity = entity.location

  (* todo rename *)
  let empty_file entity file =
    let foc = get_channel entity file in
    match file with
    | Db_state
    | Trans
    | Config ->
        WR.erase_file foc
    | _ -> assert false


  let output entity file =
    let f = get_channel entity file in
    WR.output f

  let add_int entity file i =
    let f = get_channel entity file in
    WR.add_int f i

  let add_char entity file c  =
    let f = get_channel entity file in
    WR.add_char f c

  let add_string entity file c  =
    let f = get_channel entity file in
    WR.add_string f c

  let add_float entity file c  =
    let f = get_channel entity file in
    WR.add_float f c

  let add_int32 entity file i =
    let f = get_channel entity file in
    WR.add_int32 f i

  let add_int64 entity file i =
    let f = get_channel entity file in
    WR.add_int64 f i

  let add entity file ?(output=false) lst =
    let f = get_channel entity file in
    List.iter (function
                 WChar c -> WR.add_char f c
               | WInt i -> WR.add_int f i
               | WString s -> WR.add_string f s
               | WFloat s -> WR.add_float f s
               | WInt32 i -> WR.add_int32 f i
               | WInt64 i -> WR.add_int64 f i
              ) lst;
    if output then WR.output f

  let seek_out entity file pos =
    let f = get_channel entity file in
    WR.seek_out f pos

  let position_out entity file =
    let f = get_channel entity file in
    WR.position_out f

  let read_int entity file =
    let f = get_channel entity file in
    WR.read_int f

  let read_char entity file =
    let f = get_channel entity file in
    WR.read_char f

  let read_string entity file =
    let f = get_channel entity file in
    WR.read_string f

  let read_float entity file =
    let f = get_channel entity file in
    WR.read_float f

  let read_int32 entity file =
    let f = get_channel entity file in
    WR.read_int32 f

  let read_int64 entity file =
    let f = get_channel entity file in
    WR.read_int64 f

  let read entity file lst =
    let f = get_channel entity file in
    List.tail_map (function
                     RChar -> WChar (WR.read_char f)
                   | RInt -> WInt (WR.read_int f)
                   | RString -> WString (WR.read_string f)
                   | RFloat -> WFloat (WR.read_float f)
                   | RInt32 -> WInt32 (WR.read_int32 f)
                   | RInt64 -> WInt64 (WR.read_int64 f)
                  ) lst

  let position_in entity file =
    let f = get_channel entity file in
    WR.position_in f

  let length entity file =
    let f = get_channel entity file in
    WR.length f

  let seek_in entity file =
    let f = get_channel entity file in
    WR.seek_in f


  let get_list_chan entity =
    [entity.node_chan; entity.uid_chan; entity.uid_rev_chan; entity.timestamp_chan; entity.flags_chan; entity.config_chan; entity.db_state_chan; entity.trans_chan]

  let get_reset_list entity =
    [entity.node_chan; entity.uid_chan; entity.uid_rev_chan; (*entity.timestamp_chan;*) (*entity.trans_chan*)]


  let close entity =
    entity.is_open <- false;
    List.iter WR.close (get_list_chan entity)

  let is_open entity = entity.is_open

  let reset_files entity =
    List.iter WR.reset_file (get_reset_list entity)

  let set_size entity file =
    WR.truncate_file (get_channel entity file)


  let copy_file entity file ?location ext =
    let chan = get_channel entity file in
    let get_name_ = function
      | Node -> node_file
      | Uid -> uid_file
      | Uid_rev -> uid_rev_file
      | Timestamp -> timestamp_file
      | Flags -> flags_file
      | Trans -> trans_file
      | Db_state -> db_state_file
      | Config -> config_file
      | Lock -> assert false in
    (* create new name *)
    let new_name = get_name_ file in
    let new_name = (new_name ^ ext) in
    let new_name = Option.default_map (entity.location ^ new_name) (fun x -> x ^ new_name) location in

    (* flush the file *)
    WR.output chan;
    let name = get_name entity file in
    (* copy the file *)
    let res = File.copy ~force:true name new_name in
    if res <> 0 then failwith "No cp"



  (* fichiers a unique ecriture / lecture *)
  let single_write filename lst =
    let f = make_channel Append filename in
    List.iter (function
                 WChar c -> WR.add_char f c
               | WInt i -> WR.add_int f i
               | WString s -> WR.add_string f s
               | WFloat s -> WR.add_float f s
               | WInt32 i -> WR.add_int32 f i
               | WInt64 i -> WR.add_int64 f i
              ) lst;
    WR.output f;
    WR.close f

  let single_read filename lst =
    let f = make_channel Append filename in
    let res = List.tail_map (function
                               RChar -> WChar (WR.read_char f)
                             | RInt -> WInt (WR.read_int f)
                             | RString -> WString (WR.read_string f)
                             | RFloat -> WFloat (WR.read_float f)
                             | RInt32 -> WInt32 (WR.read_int32 f)
                             | RInt64 -> WInt64 (WR.read_int64 f)
                            ) lst in
    WR.close f;
    res

  let get_single_length filename =
    let f = make_channel Append filename in
    let res = WR.length f in
    WR.close f;
    res


  (* fichiers a usage "unique" *)
  type unik = { u_filename : string; unik_chan : WR.chan }
  let create_unik ?(mode=Create) filename =
    { u_filename = filename;
      unik_chan = make_channel mode filename;
    }

  let add_unik kentity ?(output=false) lst =
    let f = kentity.unik_chan  in
    List.iter (function
                 WChar c -> WR.add_char f c
               | WInt i -> WR.add_int f i
               | WString s -> WR.add_string f s
               | WFloat s -> WR.add_float f s
               | WInt32 i -> WR.add_int32 f i
               | WInt64 i -> WR.add_int64 f i
              ) lst;
    if output then WR.output f

  let read_unik kentity lst =
    let f = kentity.unik_chan in
    List.tail_map (function
                     RChar -> WChar (WR.read_char f)
                   | RInt -> WInt (WR.read_int f)
                   | RString -> WString (WR.read_string f)
                   | RFloat -> WFloat (WR.read_float f)
                   | RInt32 -> WInt32 (WR.read_int32 f)
                   | RInt64 -> WInt64 (WR.read_int64 f)
                  ) lst

  let read_unik_int kentity =
    let f = kentity.unik_chan in
    WR.read_int f

  let read_unik_char kentity =
    let f = kentity.unik_chan in
    WR.read_char f

  let read_unik_string kentity =
    let f = kentity.unik_chan in
    WR.read_string f

  let read_unik_float kentity =
    let f = kentity.unik_chan in
    WR.read_float f

  let read_unik_int32 kentity =
    let f = kentity.unik_chan in
    WR.read_int32 f

  let read_unik_int64 kentity =
    let f = kentity.unik_chan in
    WR.read_int64 f


  let output_unik kentity =
    WR.output kentity.unik_chan

  let close_unik kentity =
    WR.output kentity.unik_chan;
    WR.close kentity.unik_chan

  let position_out_unik kentity = 
    WR.position_out kentity.unik_chan

  let seek_out_unik kentity = 
    WR.seek_out kentity.unik_chan


  (* move unik file to permanent one *)
  let mv entity kfile file =
    let chan = get_channel entity file in
    (* je vide l'autre fichier *)
(*    WR.erase_file chan;*)
    (* je ferme le fichier temporaire *)
    close_unik kfile;
    (* et je move *)
    let res = File.mv ~force:true kfile.u_filename (get_name entity file) in
    if res <> 0 then failwith "No mv";
    WR.reload chan


  module Channel =
  struct
    type channel = WR.chan
    let get = get_channel
    include WR
    let add f = function
      | WChar c -> add_char f c
      | WInt i -> add_int f i
      | WString s -> add_string f s
      | WFloat s -> add_float f s
      | WInt32 i -> add_int32 f i
      | WInt64 i -> add_int64 f i
  end
end
