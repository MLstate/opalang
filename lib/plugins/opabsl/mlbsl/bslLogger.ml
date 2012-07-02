(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

##extern-type Logger.out_channel = out_channel
##extern-type Logger.date = float

##register backend : -> string
let backend _ = "caml"

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

##register now : -> Logger.date
let now _ = Unix.gettimeofday ()

(* Had to do this because of dependency problems in stdlib *)
##register log_time : Logger.date -> string
let log_time t =
    let lc = Unix.localtime t in
    let csec = int_of_float ((fst (modf t)) *. 100.0) in
    Printf.sprintf "%02d/%02d/%02d %02d:%02d:%02d.%02d" lc.Unix.tm_mday (lc.Unix.tm_mon + 1) (lc.Unix.tm_year mod 100) lc.Unix.tm_hour lc.Unix.tm_min lc.Unix.tm_sec csec

##register log_suffix : -> string
let log_suffix _ =
  let tm = Unix.gettimeofday () in
  let lc = Unix.localtime tm in
  Printf.sprintf ".%04d.%02d.%02d" (lc.Unix.tm_year + 1900) (lc.Unix.tm_mon + 1) lc.Unix.tm_mday

