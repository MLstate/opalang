(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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

