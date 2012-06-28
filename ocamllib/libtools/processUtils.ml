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
