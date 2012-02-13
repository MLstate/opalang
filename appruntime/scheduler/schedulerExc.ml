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

(*
   This module provide a layer to execute functions

   @author Cedric Soulas
*)

(*
 It catches:
  - errors (eg. nothing to read, connection closed...)
  - system break (ctrl-c)
  - other unexpected exceptions

 The general contract is
      - to never crash on executing
        external *and* internal functions
        inside the scheduler
      - to provide, when an error occure,
        a usefull log message for debugging purpose
      - to provide, when the scheduler debug mode is off,
        a least the right backtrace in the console
*)

(* =================== *)
(*  Private functions  *)
(* =================== *)

let print_exc log e =
  let backtrace =
    if Printexc.backtrace_status() then "\n" ^ Printexc.get_backtrace() else ""
  in
  log (Printf.sprintf "%s%s" (Printexc.to_string e)  backtrace)

(* ================== *)
(*  Public functions  *)
(* ================== *)

(* Execute simple callback that return unit        *)
(* Used by the scheduler in 2 cases :              *)
(*  - for timeout external callbacks               *)
(*  - for internal callbacks provided              *)
(*     in the private action function              *)
let execute f log =
  try
    f ()
  with
  | Sys.Break -> raise Sys.Break
  | e ->
      print_exc log e

(* Execute external error continuation  *)
let execute_err f e log =
  print_exc log e;
  execute (fun () -> f e) log

let execute_io f err_cont cont log =
  try
    let res = f () in
    execute (fun () -> cont res) log
  with
  | e -> execute_err err_cont e log

let setup_signal_handlers () =
  let clean_quit_handler signal name =
    Sys.set_signal signal (
      Sys.Signal_handle
        (fun _ ->
           Logger.warning "%s received, exiting." name;
           exit 130)
    )
  in
  clean_quit_handler Sys.sigint "SIGINT";
  clean_quit_handler Sys.sighup "SIGHUP";
  clean_quit_handler Sys.sigterm "SIGTERM";
  clean_quit_handler Sys.sigquit "SIGQUIT";
  List.iter (fun s -> Sys.set_signal s Sys.Signal_ignore)
    [Sys.sigpipe]

let _ =
  Mlstate_platform.on_unixes setup_signal_handlers

(* Execute the Epoll.wait operation                                 *)
(* or other system call related functions                           *)
(* Because of a system call, the Failure is raised, not a Sys.Break *)
let rec execute_wait f =
  try
    f ()
  with
  | Failure "Interrupted system call" ->
      Logger.notice "Signal intercepted";
      execute_wait f
