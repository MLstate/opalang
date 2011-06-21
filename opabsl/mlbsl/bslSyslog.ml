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
##register debug:   string, 'a -> void
##register info:    string, 'a -> void
##register notice:  string, 'a -> void
##register log\ notice:     string, 'a -> void
##register warning: string, 'a -> void
##register error:   string, 'a -> void
##register fatal\ critical:   string, 'a -> void

let error       topic x = Logger.error     "%s %s" topic (DebugPrint.simple_print x)
let warning     topic x = Logger.warning   "%s %s" topic (DebugPrint.simple_print x)
let notice      topic x = Logger.notice    "%s %s" topic (DebugPrint.simple_print x)
let info        topic x = Logger.info      "%s %s" topic (DebugPrint.simple_print x)
let debug       topic x = Logger.debug     "%s %s" topic (DebugPrint.simple_print x)
let emergency   topic x = Logger.emergency "%s %s" topic (DebugPrint.simple_print x)
let alert       topic x = Logger.alert     "%s %s" topic (DebugPrint.simple_print x)
let critical    topic x = Logger.critical  "%s %s" topic (DebugPrint.simple_print x)

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
  fun time src properties content -> Logger.log_access ~logger ~priority:Logger.Emergency "%d %s %s %s" time src properties content (* time display desactivated *)
