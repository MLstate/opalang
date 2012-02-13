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


module U = Unix
module S = Sys

exception InvalidCommand

exception CommandError

let system_call cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
    (try
       while true do
         Buffer.add_channel buf ic 1
       done
     with End_of_file -> ());
    let ret = Unix.close_process (ic, oc) in
    let msg = Base.String.rtrim ~is_space:(fun x -> if x = '\n' then true else false) (Buffer.contents buf) in
      msg, ret

let get_return_code status =
  match status with
    | Unix.WSIGNALED x -> failwith (Printf.sprintf "Process killed by signal %d" x)
    | Unix.WSTOPPED x -> failwith (Printf.sprintf "Process stopped by signal %d" x)
    | Unix.WEXITED x -> x

let command_fct cmd fct =
  let t, s = system_call (Printf.sprintf "%s %s" cmd fct) in
  let r = get_return_code s in
    if r = 0 then t
    else raise CommandError

let which cmd = command_fct "which" cmd
let wget cmd = command_fct "wget" cmd
let tar cmd = command_fct "tar" cmd
let ps cmd = command_fct "ps" cmd
let mktemp cmd = command_fct "mktemp" cmd
let cp cmd = command_fct "cp" cmd
