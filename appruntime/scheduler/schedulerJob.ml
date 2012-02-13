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
  This module is a generic automaton to work
  both on K.Operation.t and K.Priority.t.

  @author Cedric Soulas
*)

(*
  It manages jobs based on 4 states:
  - deciding
  - managing an error
  - executing
  - finalizing

  The automaton:
  --------------------------------------------------------
  (1) (decide : 'dec -> ('exe, 'err, 'fin) job_decide)
            => (finalize : 'fin -> unit)
        or  => (error : 'err -> unit)
        or  => (execute : 'exe -> unit)
                --async--> (callack : 'exe -> 'dec) ==> (1)
  --------------------------------------------------------
*)

#<Debugvar:SCHEDULER_DEBUG>

module K = SchedulerKer
module L = SchedulerLog
module E = SchedulerExc

type ('exe, 'err, 'fin) decide = Execute of 'exe | Error of 'err | Finalize of 'fin

type order =
  | Do    (* try to do the job immediately          *)
  | Poll  (* try polling to do the job              *)
  | Wait  (* already polled, just wait for an event *)

let make
    (operation : K.Operation.t)
    (priority : K.Priority.t)
    (counter: K.Counter.t)
    ?(force_polling=false)
    (fd : Unix.file_descr)
    (direction : K.Operation.direction)
    (timeout : Time.t option)
    (job_decide : 'dec -> ('exe, 'err, 'fin) decide)
    (job_execute : 'exe -> 'dec)
    (job_error : 'err -> unit)
    (job_finalize : 'fin -> unit)
    (init : 'dec)

    =

  let key = K.Counter.get_key counter in

  let log = L.job_info key in

  let remove order =
    match order with
    | Do | Poll -> ()
    | Wait ->
        begin
          match timeout with
          | Some _ -> K.Priority.remove priority key
          | None -> ()
        end;
        K.Operation.remove operation key;
        K.Counter.release_key counter key
  in

  let order, decr_sync =
    if force_polling then
      Poll, fun () -> ()
    else begin
        begin
          try
            K.Counter.incr_sync counter;
            Do, (fun () -> K.Counter.decr_sync counter)
          with
          | K.Counter.Sync_limit -> Poll, (fun () -> ())
        end
    end
  in

  let error order (v:'err) =
    remove order;
    job_error v;
  in

  let finalize order (v:'fin) =
    log "FIN";
    remove order;
    L.incr_level ();
    job_finalize v;
    begin
      match order with
      | Do -> () (* not yet incr *)
      | Poll -> decr_sync ()
      | Wait -> () (* yet decr *)
    end;
    L.decr_level ();
  in

  let add_timeout order =
    match timeout with
    | Some time ->
        log "PRI" ~s:(string_of_int (Time.in_milliseconds time));
        K.Priority.add priority key time (
          fun () -> log "TIM"; error order K.Priority.Timeout;
        );
    | None -> ()
  in

  let rec decide order (v:'dec) =
    match (job_decide v) with
    | Finalize v -> finalize order v
    | Execute v -> execute order v
    | Error v -> log "ERD"; error order v

  and execute order (v:'exe) =
    match order with
    | Do ->
        log "EXE";
        callback Poll v ()
    | Poll ->
        log "EXE" ~s:"!";
        add_timeout Wait;
        K.Operation.add operation fd direction key (callback Wait v) (error Wait)
    | Wait ->
        log "EXE" ~s:"+";
        if K.Operation.mem_key operation key then
          K.Operation.replace operation key (callback Wait v) (error Wait)
        else
          ()

  and callback order (v:'exe) () =
    log "CAL";
    #<If> L.incr_level () #<End>;
    let exe () =
      let v = job_execute v in
      #<If> L.decr_level () #<End>;
      v
    in
    let error e =
      log "ERX";
      #<If> L.decr_level () #<End>;
      error order e
    in
    let cont_error_log s =
      #<If> L.decr_level () #<End>;
      L.error "io callback" s
    in
    E.execute_io
      (exe)
      (error)
      (decide order)
      cont_error_log
  in

  begin
    match direction with
    | K.Operation.In ->
        if K.Operation.mem operation fd K.Operation.In then
          raise K.Operation.Busy_direction
        else ()
    | K.Operation.Out -> ()
  end;

  decide order init;

  key
