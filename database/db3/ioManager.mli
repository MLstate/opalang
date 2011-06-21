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
type t

val create : [< `append | `create | `readonly ] -> string -> t
val close : t -> unit

(** Return database location *)
val get_location : t -> string
val is_uidfile_existing : string -> bool
val is_open : t -> bool
val output_dot : t -> int (* rev *) -> string (* content *) -> unit

(** Seek some file to 0 (in & out).
 * Used by databae orruption manager *)
val rebirth : t -> unit

(* Lock File *)
val lock_file : t -> bool

(** write config file *)
val write_config : t -> DbTypes.configfile -> unit
(** write only the revision of the last snapshot in Config file *)
val write_config_last_snapshot : t -> DbTypes.rev -> unit
val read_config : t -> DbTypes.configfile
val write_version : t -> int -> unit
val read_version : t -> int

val write_flags : t -> unit
val overwrite_flags:
  ?uidr:DbTypes.position ->
  ?node:DbTypes.position ->
  ?trans:DbTypes.position ->
  ?tms:DbTypes.position ->
  uid:DbTypes.uid ->
  rev:DbTypes.rev ->
  t ->
  unit

val write_timestamp : t -> Time.t -> unit
val read_timestamp : t -> int -> Time.t

val write_uid_rev : t -> DbTypes.uidrevfile -> unit
val read_uid_rev : ?rev:DbTypes.rev -> t -> DbTypes.uidrevfile

val write_dbstate :
  ?reset:bool ->
  t ->
  uidmap: DbTypes.uidmap ->
  index: DbTypes.index ->
  unit

val read_dbstate : t -> DbTypes.dbstate

val write_nodes : t -> ?last_uid:DbTypes.uid -> DbTypes.nodefile -> unit
val overwrite_nodes : t -> DbTypes.nodefile -> unit
val read_nodes : t -> DbTypes.nodefile
val read_specific_node : t -> DbTypes.uid -> Node.t

(*
  FIXME: option ?
*)
val write_trans : t -> DbTypes.trans option -> unit
(*val read_trans : t -> trans*)
val read_transs : t -> DbTypes.transfile

val get_filemanager : t -> DbIo.t

(* do a backup of db's files *)
val do_backup : t -> unit

(* Return last uid rev available. Given flags, restore file or not. *)
val restore_uidrev : restore:bool -> t -> DbTypes.uidrevfile option

(* Clean temporary hashtables, used for opening the db *)
val cleanup_hashtbls : unit -> unit
