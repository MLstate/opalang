(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

##register debug:   string, 'a -> void
##register info:    string, 'a -> void
##register notice:  string, 'a -> void
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
