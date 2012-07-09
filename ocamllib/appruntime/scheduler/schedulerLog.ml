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
#<Debugvar:SCHEDULER_DEBUG>

let int_of_fd = Epoll.Debug.int_of_filedescr

let level = ref (-1)

let print_level () =
  let rec aux s n =
    if n <= 0 then s
    else if n >= 10 then (Printf.sprintf "%s ...(level %d)" s !level)
    else aux (Printf.sprintf "%s\t" s) (n-1)
  in
  aux "" !level

let incr_level () = level := !level + 1; ()
let decr_level () = level := !level - 1; ()

let sprintf = Printf.sprintf

let dest = [Logger.make_rotating_destination "scheduler"]
let logger =  Logger.make_logger dest 10

let log priority color fmt =
  let _log fmt = Logger.log_error ~priority ~color ~logger fmt in
  let _nolog fmt = Format.ifprintf Format.std_formatter fmt in
  #<If>
    _log fmt
  #<Else>
    _nolog fmt
  #<End>

let string_of_connection conn =
  sprintf "%d" (int_of_fd (NetAddr.get_fd conn))

let string_addr_of_connection conn =
  try
    Unix.string_of_inet_addr (NetAddr.get_addr conn)
  with
  | Invalid_argument _ -> "file"

let label _s = sprintf "%s" (print_level ())

let info_conn m ?(s="") conn  =
  Logger.debug "%s [%s] %s %s" (label "sched") m (string_of_connection conn) s

let info_new_conn m conn =
  info_conn m ~s:(string_addr_of_connection conn) conn

let job_info _key ?(s="") _m  =
  #<If$minlevel 2>
    log Logger.Info `magenta "%s [%s] %d %s" (label "job") _m _key s
  #<Else>
    ignore s;
  #<End>

let ker_info ?(s="") _m  =
  #<If$minlevel 2>
    log Logger.Info `magenta "%s [%s] %s" (label "ker") _m s
  #<Else>
    ignore s;
  #<End>

let info ?(s="") m =
  Logger.info "%s [%s] %s" (label "sched") m s

let warning fct conn fmt =
  Logger.warning "%s [%s] %s %s" (label "sched") fct (string_of_connection conn) fmt

let error_conn fct conn fmt =
  Logger.error "%s [%s] %s %s" (label "sched") fct (string_of_connection conn) fmt

let scheduler_debug_and_not_show_logs scheduler_debug =
  DebugVariables.defined scheduler_debug
  && DebugVariables.undefined DebugVariables.show_logs

let error fct msg = Logger.error ("[%s] %s") fct msg

let critical fct msg =  Logger.critical ("[%s] %s") fct msg

let warning_without_connection fct fmt =
  Logger.warning "%s [%s] %s" (label "sched") fct fmt
