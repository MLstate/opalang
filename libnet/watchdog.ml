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

type options = {
  opt_memory_limit : float;
  opt_fd_limit: int;
  opt_freq: float;
}

type t = {
  memory_limit : float;
  fd_limit: int;
  freq: Time.t;
}


let name = "WatchDog"

let version = "0.0"

let default_options =
  {
    opt_memory_limit= 1000.;
    opt_fd_limit= 1024;
    opt_freq=5.;
  }

let spec_args _name = []

let make _name opts _sched =
  {
    memory_limit=opts.opt_memory_limit;
    fd_limit=opts.opt_fd_limit;
    freq=Time.seconds_float opts.opt_freq;
  }

let get_ports _t _sched = ["",`None]

let get_description _t _sched = `Watchdog

let memory_consumption () =
  (* let minor_words,promoted_words,major_words = Gc.counters () in *)
  (* let words = (minor_words +. major_words -. promoted_words) in *)
  let stat = Gc.stat () in
  let words = float_of_int(stat.Gc.heap_words + stat.Gc.stack_size) in
  (* let words = float_of_string(Sys.command ("ps --no-headers -o size "^(string_of_int(Unix.getpid ())))) in *)
  words /.(128.*.1024.)
    (* 1/131072 = 8 /(1024*1024)  (8: word size on 64) *)
    (* let fd = Unix.openfile "/proc/self/stat" [Unix.O_RDONLY] 0o640 in *)
    (* let conn = Scheduler.make_connection sched (Scheduler.File fd) in *)
    (* let  *)


let fd_consumption sched = Scheduler.nb_of_connection sched


let run t sched =
  Logger.info "[WatchDog] start";
  let too_much () =
    let mc = memory_consumption () in
    let fdc = fd_consumption sched in
    Logger.info "[WatchDog] Check resource consumption : memory %d/%d / file_desc %d/%d" (int_of_float mc) (int_of_float t.memory_limit) fdc
  t.fd_limit;
    (* VirtualResourceManager.print_stat (Scheduler.get_vrm sched); *)
    (* Logger.info "[Safeguard] memory %d/%d" (int_of_float mc) (int_of_float t.memory_limit); *)
    (* Logger.info "[Safeguard] file_desc %d/%d" fdc t.fd_limit; *)
    (mc >= t.memory_limit) or (fdc >= t.fd_limit)
  in

  let check () =
    (* Gc.compact (); *)
    if too_much ()
    then
      begin
        Logger.info "[WatchDog] Start Gc";
        Cookie2.gc_cookies (Time.now ());
        ResourceTracker.Default.collect ();
        Gc.compact ();
        while (too_much ())
        do
          Logger.info "[WatchDog] kill a group";
          (* let group = VirtualResourceManager.get_bigger_group vrm in *)
          (* VirtualResourceGroup.iter (fun rid -> *)
          (*   if !rid >= 0 *)
          (*   then *)
          (*     match VirtualResourceManager.get vrm !rid with *)
          (*     | None -> () *)
          (*     | Some r' -> *)
          (*         VirtualResource.kill *)
          (*           (VirtualResource.unblack_resource r') `Killed) group; *)
          Cookie2.gc_cookies (Time.now());
          ResourceTracker.Default.collect ();
          Gc.compact ();
        done
      end
  in
  Scheduler.timer sched t.freq check;
  t


let close _t _sched = ()
