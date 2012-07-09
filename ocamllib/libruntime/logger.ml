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
(*
   @author Frederic Ye *)

(* ksprintf : take the implementation from LangPrint *)
let sprintf fmt =
  Format.kfprintf (fun _ -> Format.flush_str_formatter ()) Format.str_formatter fmt

let ksprintf k fmt =
  let k _ =
    let s = Format.flush_str_formatter () in
    k s
  in
  Format.kfprintf k Format.str_formatter fmt

(* An entry priority *)
type priority =
  | Emergency
  | Alert
  | Critical
  | Error
  | Warning
  | Notice
  | Info
  | Debug

(* Convert an entry priority to a string *)
let priority_to_string = function
  | Emergency -> "emerg"
  | Alert -> "alert"
  | Critical -> "crit"
  | Error -> "error"
  | Warning -> "warn"
  | Notice -> "notice"
  | Info -> "info"
  | Debug -> "debug"

(* Convert an entry priority to a level in order to filter more easily *)
let priority_to_level = function
  | Emergency -> 0
  | Alert -> 1
  | Critical -> 2
  | Error -> 3
  | Warning -> 4
  | Notice -> 5
  | Info -> 6
  | Debug -> 7

(* Some references for rotating logs
   TODO: should be improved later *)
let logs_path = ref None (* logs_path option *)
let with_rotatelogs = ref None (* (rotatelogs_bin_path * days) option  *)
let date_logs = ref false

type entry = {
  time : float; (* time of log *)
  priority : priority; (* priority *)
  message : string; (* message *)
  color : Ansi.color; (* color if printed in a tty *)
}

type destination =
  | Blackhole (* a blackhole where nothing will be output *)
  | Ram (* keep the log list in RAM *)
  | Channel of out_channel (* write log to a channel *)
      (* | Syslog of Syslog.t *)
type named_destination = string * destination Lazy.t

let make_file_destination file = file, lazy (
  try
    let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 file in
    (* at_exit (fun _ -> close_out oc); *)
    Channel oc
  with Sys_error _ -> Blackhole) (* FIXME: Display a message to warn that this file can't be accessed *)
let make_piped_destination cmd = cmd, lazy (
  try
    let pipe = Unix.open_process_out cmd in
    (* at_exit (fun _ -> ignore(Unix.close_process_out pipe)); *)
    Channel pipe
  with Sys_error _ -> Blackhole) (* FIXME: Display a message to warn that this pipe can't be accessed *)
(* let make_syslog_destination facility program_name =  *)
(*   let syslog = Syslog.openlog ~flags:[] ~facility:(Syslog.facility_of_string facility) program_name in *)
(*   Syslog syslog *)
let make_rotating_destination ?(days) filename =
  match !with_rotatelogs with
  | Some (rpath, rdays) ->
      (* We here multiply by 86400 because rotaelogs rotationtime is in seconds *)
      let pipe = Printf.sprintf "%s -l \"%s%s.%%Y.%%m.%%d.log\" %d"
	rpath (Option.default "" !logs_path) filename ((Option.default rdays days) * 86400) in
      make_piped_destination pipe
  | _ ->
      if !date_logs then
	let log_suffix =
	  let tm = Unix.gettimeofday () in
	  let lc = Unix.localtime tm in
	  Printf.sprintf ".%04d.%02d.%02d"
	    (lc.Unix.tm_year + 1900)
	    (lc.Unix.tm_mon + 1)
	    lc.Unix.tm_mday
	in
	let file = Printf.sprintf "%s%s%s.log" (Option.default "" !logs_path) filename log_suffix in
	make_file_destination file
      else
	let file = Printf.sprintf "%s%s.log" (Option.default "" !logs_path) filename in
	make_file_destination file

(* Checks if the destination is a tty *)
let is_tty = function
  | Blackhole -> false
  | Ram -> false
  | Channel c -> Unix.isatty (Unix.descr_of_out_channel c)
  (* | Syslog _ -> false *)

type t = {
  mutable destinations : named_destination list; (* list of destinations *)
  mutable ram : entry list; (* list of entry to keep in ram *)
  mutable filter : int; (* level after this filter number will not be produced *)
  mutable lasttime : float; (* last time a log was output *)
}

let default_filer_level = 6

let empty_logger () = {
  destinations = [];
  ram = [];
  filter = default_filer_level;
  lasttime = Unix.gettimeofday ();
}

let make_logger dsts filter = {
  destinations = dsts;
  ram = [];
  filter = filter;
  lasttime = Unix.gettimeofday ();
}

let add_destination logger dst =
  logger.destinations <- dst :: logger.destinations

let clear_destinations logger =
  logger.destinations <- []

let set_filter logger filter =
  logger.filter <- filter

let get_ram logger = logger.ram
let get_filter logger = logger.filter

let no_logger = empty_logger ()

(* Create a reference to a logger that will write to stderr *)
let default_logger = ref (
  let logger = empty_logger () in
  { logger with destinations = ["STDERR", lazy (Channel stderr)]}
)

let get_default_logger_destinations () =
  List.map (fst) (!default_logger).destinations

let set_default_logger logger =
(*   default_logger_changed := true; *)
  default_logger := logger

(* Create a reference to a logger that will write to "access.log" *)
let access_logger = ref (lazy (
  let logger = empty_logger () in
  let dst = make_rotating_destination "access" in
  { logger with destinations = [dst]}))

let get_access_logger_destinations () =
  List.map (fst) (Lazy.force !access_logger).destinations

let set_access_logger logger =
  access_logger := lazy logger

(* Create a reference to a logger that will write to "error.log" *)
let error_logger = ref (lazy (
  let logger = empty_logger () in
  let dst = make_rotating_destination "error" in
  { logger with destinations = [dst]}))

let get_error_logger_destinations () =
  List.map (fst) (Lazy.force !error_logger).destinations

let set_error_logger logger =
  error_logger := lazy logger

(* Convert a log entry into a string *
   @param long display a long message (date + priority)
   @dest the log destination
   @entry the log entry *)
let string_of_entry long dest entry =
  let message =
  if is_tty dest || (try Sys.getenv "MLSTATE_FORCE_COLOR" = "1" with Not_found -> false) then (* FIXME: use debugvar *)
      Ansi.print entry.color entry.message
    else entry.message in
  if (#<If:TESTING> false #<Else> long #<End>) then
    let lc = Unix.localtime entry.time in
    let csec = int_of_float ((fst (modf entry.time)) *. 100.0) in
    let time = Printf.sprintf "%02d/%02d/%02d %02d:%02d:%02d.%02d" lc.Unix.tm_mday (lc.Unix.tm_mon + 1) (lc.Unix.tm_year mod 100) lc.Unix.tm_hour lc.Unix.tm_min lc.Unix.tm_sec csec in
    Printf.sprintf "%s [%s]: %s\n" time (priority_to_string entry.priority) message
  else Printf.sprintf "%s\n" message

let make_entry priority color s = {
  time = Unix.gettimeofday ();
  priority = priority;
  message = s;
  color = color;
}

(* let priority_to_syslog_level = function *)
(*   | Emergency -> `LOG_EMERG *)
(*   | Alert -> `LOG_ALERT *)
(*   | Critical -> `LOG_CRIT *)
(*   | Error -> `LOG_ERR *)
(*   | Warning -> `LOG_WARNING *)
(*   | Notice -> `LOG_NOTICE *)
(*   | Info -> `LOG_INFO *)
(*   | Debug -> `LOG_DEBUG *)

let do_log logger long priority color s =
  if (priority_to_level priority) > logger.filter then ()
  else
    let entry = make_entry priority color s in
    let log_entry dst =
      try
	let sentry = string_of_entry long dst entry in
	begin match dst with
	| Blackhole -> ()
	| Ram -> logger.ram <- entry :: logger.ram
	| Channel oc -> output_string oc sentry; flush oc
	    (* | Syslog s -> Syslog.syslog s (priority_to_syslog_level priority) sentry *)
	end;
	logger.lasttime <- entry.time;
      with
        (* channel no longer valid, e.g., because we are in at_exit code *)
        Sys_error _ -> ()
    in
    #<If:SHOW_LOGS> (* If SHOW_LOGS, only log one time in stderr *)
      let dst = Channel stderr in
      log_entry dst
    #<Else>
      List.iter (
	fun (_, dst) ->
	  log_entry (Lazy.force dst)
      ) logger.destinations
    #<End>


let log ?(logger=(!default_logger)) ?(priority=Info) ?(color=`black) fmt =
  ksprintf (
    fun s ->
      do_log logger true priority color s
  ) fmt

let lazy_log ?(logger = !default_logger) ?(priority=Info) ?(color=`black) s =
  do_log logger true priority color (Lazy.force s)

let log_access ?(logger=(Lazy.force !access_logger)) ?(priority=Info) ?(color=`black) fmt =
  ksprintf (
    fun s ->
      (* long = false because access log is usually CLF format *)
      do_log logger false priority color s
  ) fmt

let log_error ?(logger=(Lazy.force !error_logger)) ?(priority=Info) ?(color=`black) fmt =
  ksprintf (
    fun s ->
      do_log (!default_logger) false priority color s;
      do_log logger true priority color s
  ) fmt

let emergency fmt =
  log_error ~logger:(Lazy.force !error_logger) ~priority:Emergency ~color:`red fmt

let alert fmt =
  log_error ~logger:(Lazy.force !error_logger) ~priority:Alert ~color:`red fmt

let critical fmt =
  log_error ~logger:(Lazy.force !error_logger) ~priority:Critical ~color:`red fmt

let error fmt =
  (*log_error ~logger:(Lazy.force !error_logger) ~priority:Error ~color:`red "%s" (Printexc.get_backtrace());*)
  log_error ~logger:(Lazy.force !error_logger) ~priority:Error ~color:`red fmt

let warning fmt =
  log_error ~logger:(Lazy.force !error_logger) ~priority:Warning ~color:`yellow fmt

let notice fmt =
  log_error ~logger:(Lazy.force !error_logger) ~priority:Notice ~color:`black fmt

let info fmt =
  log_error ~logger:(Lazy.force !error_logger) ~priority:Info ~color:`cyan fmt

let debug fmt =
  log_error ~logger:(Lazy.force !error_logger) ~priority:Debug ~color:`blue fmt


(* ------------------------------------------------------------ *)
(* Command-line configuration                                   *)
(* ------------------------------------------------------------ *)

type log_opts = { verbose: int; display: bool; path: string; date: bool; rotate: string option; rotate_interval: int option }

let _ = (* Initial parse *)
  let log_opts =
    { verbose = #<If:SHOW_LOGS> 8 #<Else> 6 #<End>
    ; display = false
    ; path = Sys.getcwd()
    ; date = false
    ; rotate = None
    ; rotate_interval = None
    } in
  let parse = ServerArg.make_parser "logging options" [
    ["--verbose"],
      ServerArg.func ServerArg.int (fun o level -> { o with verbose = level }),
      "<level>", Printf.sprintf "Set the verbosity level (0-8, default %d)" log_opts.verbose;
    ["--display-logs"], ServerArg.func ServerArg.unit (fun o () -> { o with display = true }),
    "", "Display a part (default, access and error) of the server logs directly on the terminal instead of outputting them into log files";
    ["--logs-path"], ServerArg.func ServerArg.string (fun o s -> logs_path := Some (Printf.sprintf "%s/" s); { o with path = s }),
    "<string>", "Set the path for storing logs";
    ["--date-logs"], ServerArg.func ServerArg.unit (fun o () -> date_logs := true; { o with date = true }),
    "", "(simple alternative to rotate-logs) Use the date of the system to name the logs at every launch";
    ["--rotate-logs"], ServerArg.func ServerArg.string (fun o x -> { o with rotate = Some x }),
    "<path>", "Use the utility \"Apache rotatelogs\" at location  <path> to log accesses and errors: a new log file will be created every 1 day by default";
    ["--rotate-interval"], ServerArg.func ServerArg.int (fun o x -> { o with rotate_interval = Some x }),
    "<int>", "Rotate logs will be rotated every <int> days";
  ]
  in
  let log_opts = try ServerArg.filter log_opts parse with Exit -> exit 1 in
  if Option.is_some log_opts.rotate
    && ((Mlstate_platform.platform_dependent
      ~unix:(fun () -> (* Sys.command ("which rotatelogs") = 0 *)
	if Sys.file_exists (Option.get log_opts.rotate) then true
	  (* When rotatelogs is not found,
	     we date the logs, but we don't consider the user gave --date-logs in log_opts *)
	else (log ~priority:Warning ~color:`yellow "%s NOT FOUND: logs will just be dated." (Option.get log_opts.rotate); date_logs := true; false)
      )
      ~windows:(fun () -> false) ()) ()) then
      begin
	with_rotatelogs := Some ((Option.get log_opts.rotate), (Option.default 1 log_opts.rotate_interval));
      end
  ;
  (!default_logger).filter <- log_opts.verbose - 1
  ;
  (* Display logs overrides default, access and error logger *)
  if log_opts.display then (
    let log_out = make_logger ["STDOUT", lazy (Channel stdout)] default_filer_level
    and log_err = make_logger ["STDERR", lazy (Channel stderr)] default_filer_level in
    set_default_logger log_out;
    set_access_logger log_out;
    set_error_logger log_err;
  )
