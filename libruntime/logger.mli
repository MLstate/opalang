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

(** Log messages at runtime.

    Usage:
    - by default, logs will be output to stderr
    - by default, access logs will be output to "access.log"
    - by default, error logs will be output to "error.log"
    P.S: contrary to it's name, error log is not necessarily for errors

    You can change the default locations by setting them in your program (the most early possible)

    An application server should use log_access and log_error,
    and others should use log and lazy_log.

    @author Frederic Ye *)

(** {6 Loggers} *)

(** An entry priority, based on Syslog priority *)
type priority =
  | Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Info
  | Debug

(** One log entry *)
type entry = {
  time : float; (** GMT time of log *)
  priority : priority; (** priority *)
  message : string; (** message *)
  color : Ansi.color; (** color if printed in a tty *)
}

(** Type of a log destination *)
type destination =
  | Blackhole (** a blackhole where nothing will be output *)
  | Ram (** keep the log list in RAM *)
  | Channel of out_channel (** write log to a channel *)
  (* | Syslog of Syslog.t *)

(** A named_destination is just a destination, but with a name *)
type named_destination = string * destination Lazy.t

(** Those functions are just here to make life easier,
    you can always provide the out_channel or syslog by yourself *)

(** A file destination that will never be rotated or dated *)
val make_file_destination : string -> named_destination

(** A piped destination to another program *)
val make_piped_destination : string -> named_destination

(* val make_syslog_destination : string -> string -> destination *)

(** A rotating destination. Which means
    - if rotate-logs is set,
    uses the given rotatelogs_bin_path to rotatelogs,
    with the interval given by rotate-interval
    - if date_logs is set, and not rotate-logs
    dates the logs at every launch
    - otherwise, logs in filename.log in the current directory *)
val make_rotating_destination : ?days:int -> string -> named_destination

(** Logger structure *)
type t = {
  mutable destinations : named_destination list; (** list of named destinations *)
  mutable ram : entry list; (** list of entry to keep in ram *)
  mutable filter : int; (** level after this number will not be produced *)
  mutable lasttime : float; (** last time a log was output *)
}

(** Initialize an empty logger.
    Sets the default filter level to 6 (log everything except debug), and no destination *)
val empty_logger : unit -> t

(** Make a logger *)
val make_logger : named_destination list -> int -> t

(** Add a destination to a logger *)
val add_destination : t -> named_destination -> unit

(** Clear the destinations of a logger *)
val clear_destinations : t -> unit

(** Set the filter level of a logger *)
val set_filter : t -> int -> unit

(** Get the name of default logger destinations *)
val get_default_logger_destinations : unit -> string list

(** Set the default logger, used by log and lazy_log *)
val set_default_logger : t -> unit

(** Get the name of default access destinations *)
val get_access_logger_destinations : unit -> string list

(** Set the access logger, used by log_access *)
val set_access_logger : t -> unit

(** Get the name of error logger destinations *)
val get_error_logger_destinations : unit -> string list

(** Set the error logger, used by log_error, and derived functions (see below) *)
val set_error_logger : t -> unit



(** {6 Log functions} *)

(** All logging should be done with Logger.{debug,info,notice,warning,error,critical,alert,emergency} without
    any exotic options like ~long ; Logger.log should only be used for debug or when outputting to non-standard logs.

    The option --verbose enables to choose the level above which messages won't be shown on the console:
    --verbose 8 shows all, including debug, --verbose 0 disables even critical messages (but these are still logged).
    The maximum level of logging to the files, on the other hand, can be chosen in the API.
    The default level, 6, shows all up to notices (ie not 'info' nor 'debug')

    --verbose is recommended instead of MLSTATE_SHOW_LOGS or --display-logs (which is left for backwards-compat)
*)

(** Log a message. _Only use for debug_ on the default logger: prefer the
    specialised functions below
    (debug/info/notice/warning/error/critical/alert/emergency). *)
val log :
  ?logger:t ->
  ?priority:priority -> ?color:Ansi.color ->
  ('a, Format.formatter, unit, unit) format4 -> 'a

val no_logger : t

(** Log a lazy message into the default_logger *)
val lazy_log :
  ?logger:t ->
  ?priority:priority -> ?color:Ansi.color -> string Lazy.t -> unit

(** Log a message into the access_logger *)
val log_access :
  ?logger:t ->
  ?priority:priority -> ?color:Ansi.color -> ('a, Format.formatter, unit, unit) format4 -> 'a

(** Log a message into the error_logger *)
val log_error :
  ?logger:t ->
  ?priority:priority -> ?color:Ansi.color -> ('a, Format.formatter, unit, unit) format4 -> 'a

(** Predefined log functions that log to error_logger, and to stderr depending
    on the verbosity level.
    Recommanded for all servers. By order of increasing importance *)
val debug : ('a, Format.formatter, unit, unit) format4 -> 'a
val info : ('a, Format.formatter, unit, unit) format4 -> 'a
val notice : ('a, Format.formatter, unit, unit) format4 -> 'a
val warning : ('a, Format.formatter, unit, unit) format4 -> 'a
val error : ('a, Format.formatter, unit, unit) format4 -> 'a
(** Starting with priority [critical], the server is expected to exit right away. *)
val critical : ('a, Format.formatter, unit, unit) format4 -> 'a
val alert : ('a, Format.formatter, unit, unit) format4 -> 'a
val emergency : ('a, Format.formatter, unit, unit) format4 -> 'a

(** [critical], [alert], [emergency]: predefined log functions
    that log in error_logger and on the console.
    Usually the program should exit after any of these and the message
    should help the operator to fix the commandline arguments, etc.
    Alert level may be useful, e.g., when DB corruption or security breach
    is suspected; emergency, when the computer is on fire. *)
