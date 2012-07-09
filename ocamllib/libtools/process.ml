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

module U = Unix
module S = Sys

open Cps.Ops


type t = {
  exec_name: string;
  exec_options: string list;
  pid: int;
  stdin : U.file_descr option;
  stdout: U.file_descr option;
  stderr: U.file_descr option;
}

type chans = {
  p_stdin  : Unix.file_descr;
  p_stdout : Unix.file_descr;
  p_stderr : Unix.file_descr;
}

let get_pid sp = sp.pid

let recover_from_pid ~exec_name ~options pid =
  {exec_name = exec_name;
   exec_options = options;
   pid = pid;
   stdin = None;
   stdout = None;
   stderr = None;
  }

let start ~exec_name ~options =
  (* let command = exec_name::options in *)
  let child_stdin, proc_stdin = U.pipe() in
  let proc_stdout, child_stdout = U.pipe() in
  let proc_stderr, child_stderr = U.pipe() in
  let reader, writer = U.pipe () in
  match U.fork() with
  | 0 -> (
      (* setsid() prevents from dying after a ctrl+C in the father's terminal *)
      ignore(U.setsid());
      let pid =
        U.create_process exec_name (Array.of_list options) child_stdin child_stdout child_stderr in
      U.close reader;
      let output = U.out_channel_of_descr writer in
      (* \n mandatory because we do use input_line on the other side *)
      Printf.fprintf output "%d\n" (pid);
      flush output;
      U.close writer;  (* this will happen anyway *)
      exit 0 (* exit zombieland *)
    )
  | x -> (
      U.close writer;
      let input = U.in_channel_of_descr reader in
      (* if the msg does not contain a \n, the following will block *)
      let line = input_line input in
      U.close reader;
      ignore (U.waitpid [] x);
      {exec_name = exec_name;
       exec_options = options;
       pid = (int_of_string line);
       stdin = Some child_stdin;
       stdout = Some child_stdout;
       stderr = Some child_stderr;
      }, {p_stdin = proc_stdin;
          p_stdout = proc_stdout;
          p_stderr = proc_stderr})

let send_signal ?(maxattempts=3) ?(interval=Time.seconds 2) ~signal p sched cont =
  let rec job n cont =
    if n <= 0 then false |> cont
    else
      let finished = try
        U.kill p.pid signal; true
      with U.Unix_error (_, _, _) -> false
      in
      if finished then true |> cont
      else ignore(Scheduler.sleep sched interval (fun () -> job (n-1) @> cont)) 
  in
  job maxattempts cont

let stop =
  send_signal ~signal:S.sigint

let kill =
  send_signal ~signal:S.sigkill
