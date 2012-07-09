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
