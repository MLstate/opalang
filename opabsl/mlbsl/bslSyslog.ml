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

##register debug:   string, 'a -> void
##register info:    string, string -> void
##register notice:  string, string -> void
##register warning: string, string -> void
##register error:   string, string -> void
##register fatal\ critical:   string, string -> void

let error       topic x = Logger.error     "%s %s" topic x
let warning     topic x = Logger.warning   "%s %s" topic x
let notice      topic x = Logger.notice    "%s %s" topic x
let info        topic x = Logger.info      "%s %s" topic x
let debug       topic x = Logger.debug     "%s %s" topic (DebugPrint.simple_print x)
let emergency   topic x = Logger.emergency "%s %s" topic x
let alert       topic x = Logger.alert     "%s %s" topic x
let critical    topic x = Logger.critical  "%s %s" topic x

(**
   int is in microseconds
   event are log in event.log
   [event time src properties json_content]
*)
##register event: int,string,string,string -> void
let event =
  let logger =
    let file = Logger.make_rotating_destination ~days:1 "event" in
    let dest = Logger.empty_logger () in
    let _ = Logger.add_destination dest file in
    dest
  in
  fun time src properties content -> Logger.log_access ~logger ~priority:Logger.Emergency "%d %s %s %s" time src properties content (* time display disactivated *)

(* Put this here because it's mostly used by logger.opa *)
##extern-type Logger.out_channel = out_channel

let field_some = ServerLib.static_field_of_name "some"
let field_none = ServerLib.static_field_of_name "none"

##register open_out : string -> opa[option(Logger.out_channel)]
let open_out file =
  try
    let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o666 file in
    ServerLib.make_record(ServerLib.add_field ServerLib.empty_record_constructor field_some oc)
  with | Sys_error _s ->
    ServerLib.make_simple_record field_none (* TODO: report error *)

##register open_pipe : string -> opa[option(Logger.out_channel)]
let open_pipe cmd =
  try
    let pipe = Unix.open_process_out cmd in
    ServerLib.make_record(ServerLib.add_field ServerLib.empty_record_constructor field_some pipe)
  with | Sys_error _s ->
    ServerLib.make_simple_record field_none (* TODO: report error *)

##register close_out : Logger.out_channel -> void
let close_out oc =
  try close_out oc
  with | Sys_error _s -> ()

##register output : Logger.out_channel, string -> void
let output oc str =
  try
    output_string oc str;
    flush oc
  with | Sys_error _s -> () (* No longer valid, eg. during at_exit *)

##register is_tty : Logger.out_channel -> opa[bool]
let is_tty oc = ServerLib.wrap_bool (Unix.isatty (Unix.descr_of_out_channel oc))

##register get_stderr : -> Logger.out_channel
let get_stderr _ = stderr

##register get_stdout : -> Logger.out_channel
let get_stdout _ = stdout

##register get_cwd : -> string
let get_cwd _ = Sys.getcwd()

##register os_type : -> string
let os_type _ = Sys.os_type

##register now : -> float
let now _ = Unix.gettimeofday ()

(* Had to do this because of dependency problems in stdlib *)
##register log_time : float -> string
let log_time t =
    let lc = Unix.localtime t in
    let csec = int_of_float ((fst (modf t)) *. 100.0) in
    Printf.sprintf "%02d/%02d/%02d %02d:%02d:%02d.%02d" lc.Unix.tm_mday (lc.Unix.tm_mon + 1) (lc.Unix.tm_year mod 100) lc.Unix.tm_hour lc.Unix.tm_min lc.Unix.tm_sec csec

##register log_suffix : -> string
let log_suffix _ =
  let tm = Unix.gettimeofday () in
  let lc = Unix.localtime tm in
  Printf.sprintf ".%04d.%02d.%02d" (lc.Unix.tm_year + 1900) (lc.Unix.tm_mon + 1) lc.Unix.tm_mday

