(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(**
   Binding with module Unix
   <!> Not for casual user
*)

##extern-type caml_list('a) = 'a list

(* ugly duplication, need to have dependencies between plugin *)
let f1 = ServerLib.static_field_of_name "f1"
let f2 = ServerLib.static_field_of_name "f2"
let opa_tuple_2 (a, b) =
  let record =
    let acc = ServerLib.empty_record_constructor in
    let acc = ServerLib.add_field acc f1 a in
    let acc = ServerLib.add_field acc f2 b in
    ServerLib.make_record acc
  in
  BslUtils.wrap_opa_tuple_2 record

##opa-type System.process_status

##extern-type System.wait_flag = Unix.wait_flag

##register wnohang \ `Unix.WNOHANG` : System.wait_flag
##register wuntraced \ `Unix.WUNTRACED` : System.wait_flag

##register fork \ `Unix.fork` : -> int

let field_WEXITED = ServerLib.static_field_of_name "WEXITED"
let field_WSIGNALED = ServerLib.static_field_of_name "WSIGNALED"
let field_WSTOPPED = ServerLib.static_field_of_name "WSTOPPED"

let make_opa_status field i =
  let record = ServerLib.empty_record_constructor in
  let record = ServerLib.add_field record field (ServerLib.wrap_int i) in
  let record = ServerLib.make_record record in
  wrap_opa_system_process_status record

let opa_status = function
  | Unix.WEXITED i -> make_opa_status field_WEXITED i
  | Unix.WSIGNALED i -> make_opa_status field_WSIGNALED i
  | Unix.WSTOPPED i -> make_opa_status field_WSTOPPED i

let return_pid_status (pid, status) =
  let pid = ServerLib.wrap_int pid in
  let status = opa_status status in
  opa_tuple_2 (pid, status)

##register wait : -> opa[tuple_2(int, System.process_status)]
let wait () =
  let pid_status = Unix.wait () in
  return_pid_status pid_status

##register waitpid_flags : caml_list(System.wait_flag), int -> opa[tuple_2(int, System.process_status)]
let waitpid_flags flags pid =
  let pid_status = Unix.waitpid flags pid in
  return_pid_status pid_status

##register waitpid : int -> opa[tuple_2(int, System.process_status)]
let waitpid pid =
  let pid_status = Unix.waitpid [] pid in
  return_pid_status pid_status
