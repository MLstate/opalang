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

(** Module to manage a single process 

    @author Mathieu Baudet
    @author Hugo Venturini
*)

(** Type of a process *)
type t


type chans = {
  p_stdin  : Unix.file_descr;
  p_stdout : Unix.file_descr;
  p_stderr : Unix.file_descr;
}


(** Launches the given executable with the given options. The child process is launched after a double fork mechanism which implies that there will be no zombie process if the child dies before the father and the latter does not wait for it to finish.
    @return the running process and channels to communicate with it
*)
val start : exec_name:string -> options:string list -> t * chans

(** Creates a [t] from a given pid *)
val recover_from_pid : exec_name:string -> options:string list -> int -> t

(** [send_signal ?maxattempt:x ?interval:t ~signal:s sched p cont] schedules in [sched] the sending to process [p] of the signal [s] (see the [Sys] module) every [t] seconds until success but not more than [x] times. It then calls continuation [cont] with the result ([true] on success, [false] otherwise.
    @param maxattempt number of attempts to send the signal, default is 3
    @param interval time between to attempt to send the signal, default is 2 sec.
*)
val send_signal : ?maxattempts:int -> ?interval:Time.t ->
  signal:int -> t -> Scheduler.t -> (bool -> unit) -> unit

(** Calls [send_signal] with [~signal:Sys.sigint] *)
val stop : ?maxattempts:int -> ?interval:Time.t -> t -> Scheduler.t  ->
  (bool -> unit) -> unit

(** Calls [send_signal] with [~signal:Sys.sigkill] *)
val kill : ?maxattempts:int -> ?interval:Time.t ->  t -> Scheduler.t ->
  (bool -> unit) -> unit

(** @return the pid of the given processus *)
val get_pid : t -> int
