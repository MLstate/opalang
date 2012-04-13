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

(*
  This module manage asynchronous operations.
  See scheduler.mli for more information.

  @author Cedric Soulas
*)

#<Debugvar:SCHEDULER_DEBUG>

module C = Connection
module L = SchedulerLog
module K = SchedulerKer
module E = SchedulerExc
module Job = SchedulerJob
module NA = NetAddr

type t = {
  stats       : NetStats.t;
  mutable operation   : K.Operation.t;
  priority    : K.Priority.t;
  descriptor  : K.Descriptor.t;
  compute     : K.Compute.t;
  finalise    : K.Finalise.t;
  counter     : K.Counter.t;
  mutable at_exit     : K.Compute.t option; (* none => already transfer to compute field, some => pending exiting computation *)
}

type async_key = K.Priority.key

type connection_info = {
  addr    : NetAddr.t; (* the connection type *)
  conn_id : int        (* the unique id of the connection *)
}

exception StopTimer
exception Busy_direction = K.Operation.Busy_direction
exception Timeout = K.Priority.Timeout
exception Connection_closed
exception Syscall
exception LimitExceeded
exception Empty
exception Unbound_key

(* ============================== *)
(*          Connections           *)
(* ============================== *)

let make_connection sched ?(register=true) addr =
  #<If> L.info_new_conn "make_connection" addr #<End>;
  let id =
    if register then
      K.Descriptor.add sched.descriptor (NA.get_fd addr)
    else
      -1
  in
  {
    addr = addr;
    conn_id = id;
  }

let nb_of_connection sched = K.Descriptor.length sched.descriptor

let check_connection sched conn =
  let fd = NA.get_fd conn.addr in
  match K.Descriptor.mem sched.descriptor fd conn.conn_id with
  | K.Descriptor.Alive -> true
  | K.Descriptor.Replaced | K.Descriptor.Closed -> false

let remove_connection sched conn =
  if check_connection sched conn then begin
    let fd = NA.get_fd conn.addr in
    #<If> L.info_conn "remove_connection" conn.addr #<End>;
    K.Descriptor.remove sched.descriptor fd;
    K.Operation.remove_id sched.operation fd;
    C.close conn.addr;
  end

let get_connection_addr conn = conn.addr

let get_connection_fd conn = NA.get_fd conn.addr

let get_connection_inet_addr conn =
  NA.get_addr (get_connection_addr conn)

let get_connection_secured_from_normal conn secured_fd =
  let addr = NA.secured_from_normal secured_fd conn.addr in
  {conn with addr = addr}

(* ============================== *)
(*       In / Out Operations      *)
(* ============================== *)

let get_err_cont sched conn err_cont =
  Option.default
    (function
     | Connection_closed ->
         #<If> L.info_conn "connection_closed" conn.addr #<End>;
         remove_connection sched conn
     | Syscall ->
         #<If> L.info_conn "syscall" conn.addr #<End>;
         remove_connection sched conn
     | Timeout | _ -> ()
    ) err_cont

let write sched conn ?(block_size=32768) ?timeout buf ?(len=0) ?err_cont finalize =
  #<If> L.info_conn "write" conn.addr #<End>;
  let error = get_err_cont sched conn err_cont in
  let buflen = if len = 0 then String.length buf else len in
  let decide (nwrite, pos) =
    if nwrite = 0 then
      Job.Error Connection_closed
    else
      let to_write = min block_size (buflen - pos) in
      if to_write > 0 then Job.Execute (pos, to_write, buf)
      else Job.Finalize buflen
  in
  let execute (pos, to_write, buf) =
    try
      let n = C.write conn.addr ~pos buf to_write in
      NetStats.register_send ~size:to_write ~conn:conn.addr sched.stats;
      n, pos + n
    with
    | C.Busy ->
        #<If> L.info_conn "write" ~s:"busy" conn.addr #<End>;
        -1, pos
  in
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    (NA.get_fd conn.addr)
    K.Operation.Out
    timeout
    decide
    execute
    error
    finalize
    (-1, 0)
  in
  ()

let write_to sched conn addr ?(block_size=4096) ?timeout buf ?err_cont finalize =
  #<If> L.info_conn "write_to" conn.addr #<End>;
  let error = get_err_cont sched conn err_cont in
  let buflen = String.length buf in
  let decide (nwrite, pos) =
    if nwrite = 0 then
      Job.Error Connection_closed
    else
      let to_write = min block_size (buflen - pos) in
      if to_write > 0 then Job.Execute (pos, to_write, buf)
      else Job.Finalize buflen
  in
  let execute (pos, to_write, buf) =
    try
      let n = C.write_to conn.addr addr ~pos buf to_write in
      NetStats.register_send ~size:to_write ~conn:conn.addr sched.stats;
      n, pos + n
    with
    | C.Busy ->
        #<If> L.info_conn "write_to" ~s:"busy" conn.addr #<End>;
        -1, pos
    | e -> raise e
  in
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    (NA.get_fd conn.addr)
    K.Operation.Out
    timeout
    decide
    execute
    error
    finalize
    (-1, 0)
  in
  ()

let read_more sched conn ?read_max ?(block_size=32768) ?timeout buf ?(size_max=(-1)) ?err_cont finalize =
  (* TODO: read_max and size_max (windows) unused *)
  #<If> L.info_conn "read_more" conn.addr #<End>;
  let _ = read_max in
  let _ = size_max in
  let decide (nb_read, buf) =
    if nb_read = -1 then Job.Execute buf
    else if nb_read = 0 then Job.Error Connection_closed
    else Job.Finalize (nb_read, buf)
  in
  let execute buf =
    try
      let nread, buf = C.read_more conn.addr buf block_size in
      NetStats.register_recv ~size:nread ~conn:conn.addr sched.stats;
      nread, buf
    with
    | C.Busy ->
        #<If> L.info_conn "read" ~s:"busy" conn.addr #<End>;
        (-1, buf)
    | e ->  raise e
  in
  let error = get_err_cont sched conn err_cont in
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    (NA.get_fd conn.addr)
    K.Operation.In
    timeout
    decide
    execute
    error
    finalize
    (-1, buf)
  in
  ()

let read_content sched conn ?read_max ?(block_size=32768) ?timeout content ?(size_max=(-1)) ?err_cont finalize =
  (* TODO: read_max and size_max (windows) unused *)
  #<If> L.info_conn "read_content" conn.addr #<End>;
  let _ = read_max in
  let _ = size_max in
  let decide (nb_read, content) =
    if nb_read = -1 then Job.Execute content
    else if nb_read = 0 then Job.Error Connection_closed
    else Job.Finalize (nb_read, content)
  in
  let execute content =
    try
      let nread, buf = C.read_content conn.addr content block_size in
      NetStats.register_recv ~size:nread ~conn:conn.addr sched.stats;
      nread, buf
    with
    | C.Busy ->
        #<If> L.info_conn "read" ~s:"busy" conn.addr #<End>;
        (-1, content)
    | e ->  raise e
  in
  let error = get_err_cont sched conn err_cont in
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    (NA.get_fd conn.addr)
    K.Operation.In
    timeout
    decide
    execute
    error
    finalize
    (-1, content)
  in
  ()

let read_more2 sched conn ?read_max ?timeout buf ?(size_max=(-1)) ?err_cont finalize =
  (* TODO: read_max and size_max (windows) unused *)
  #<If> L.info_conn "read_more2" conn.addr #<End>;
  let _ = read_max in
  let _ = size_max in
  let decide (nb_read, buf) =
    if nb_read = -1 then Job.Execute buf
    else if nb_read = 0 then Job.Error Connection_closed
    else Job.Finalize (nb_read, buf)
  in
  let execute buf =
    try
      let nread, buf = C.read_more2 conn.addr buf in
      NetStats.register_recv ~size:nread ~conn:conn.addr sched.stats;
      nread, buf
    with
    | C.Busy ->
        #<If> L.info_conn "read" ~s:"busy" conn.addr #<End>;
        (-1, buf)
    | e ->  raise e
  in
  let error = get_err_cont sched conn err_cont in
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    (NA.get_fd conn.addr)
    K.Operation.In
    timeout
    decide
    execute
    error
    finalize
    (-1, buf)
  in
  ()

let read_more4 sched conn ?read_max ?timeout buf ?(size_max=(-1)) ?err_cont finalize =
  (* TODO: read_max and size_max (windows) unused *)
  #<If> L.info_conn "read_more4" conn.addr #<End>;
  let _ = read_max in
  let _ = size_max in
  let decide (nb_read, buf) =
    if nb_read = -1 then Job.Execute buf
    else if nb_read = 0 then Job.Error Connection_closed
    else Job.Finalize (nb_read, buf)
  in
  let execute buf =
    try
      let nread, buf = C.read_more4 conn.addr buf in
      NetStats.register_recv ~size:nread ~conn:conn.addr sched.stats;
      nread, buf
    with
    | C.Busy ->
        #<If> L.info_conn "read" ~s:"busy" conn.addr #<End>;
        (-1, buf)
    | e ->  raise e
  in
  let error = get_err_cont sched conn err_cont in
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    (NA.get_fd conn.addr)
    K.Operation.In
    timeout
    decide
    execute
    error
    finalize
    (-1, buf)
  in
  ()

let read sched conn ?timeout ?err_cont finalize =
  #<If> L.info_conn "read" conn.addr #<End>;
  let decide (nb_read, str) =
    if nb_read = -1 then Job.Execute ()
    else if nb_read = 0 then Job.Error Connection_closed
    else Job.Finalize (nb_read, str)
  in
  let execute () =
    try
      let nread, buf = C.read conn.addr in
      NetStats.register_recv ~size:nread ~conn:conn.addr sched.stats;
      nread, buf
    with
    | C.Busy ->
        #<If> L.info_conn "read" ~s:"busy" conn.addr #<End>;
        (-1, "")
  in
  let error = get_err_cont sched conn err_cont in
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    (NA.get_fd conn.addr)
    K.Operation.In
    timeout
    decide
    execute
    error
    finalize
    (-1, "")
  in
  ()

let read_from sched conn ?timeout ?err_cont finalize =
  #<If> L.info_conn "read_from" conn.addr #<End>;
  let no_result = (-1, Unix.ADDR_UNIX "[no source]", "") (* FIXME: what should be the non-existant value for the address? *) in
  let decide (nb_read, addr, str) =
    if nb_read = -1 then Job.Execute ()
    else if nb_read = 0 then Job.Error Connection_closed
    else Job.Finalize (nb_read, addr, str)
  in
  let execute () =
    try
      let nread, addr, buf = C.read_from conn.addr in
      NetStats.register_recv ~size:nread ~conn:conn.addr sched.stats;
      nread, addr, buf
    with
    | C.Busy ->
        #<If> L.info_conn "read_from" ~s:"busy" conn.addr #<End>;
        no_result
    | e ->  raise e
  in
  let error = get_err_cont sched conn err_cont in
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    (NA.get_fd conn.addr)
    K.Operation.In
    timeout
    decide
    execute
    error
    finalize
    no_result
  in
  ()

let read_until sched conn read_cond ?(block_size=32768) ?timeout ?err_cont finalize =
  #<If> L.info_conn "read_until" conn.addr #<End>;
  let rec aux_more (_, buff) =
    let nb_read = FBuffer.length buff in
    let str = FBuffer.contents buff in
    if read_cond (nb_read, str) then
      finalize (nb_read, str)
    else
      read_more sched conn ?timeout ~block_size ?err_cont buff aux_more
  in
  let aux (nb_read, str) =
    if read_cond (nb_read, str) then
      finalize (nb_read, str)
    else begin
      let buff = FBuffer.make (block_size*2) in
      let buff = FBuffer.add buff str in
      read_more sched conn ?timeout ~block_size ?err_cont buff aux_more
    end
  in
  read sched conn ?timeout ?err_cont aux

let read_min sched conn read_min  =
  #<If> L.info_conn "read_min" conn.addr #<End>;
  let read_cond (nb_read, _) = nb_read >= read_min in
  read_until sched conn read_cond

let read_lines sched conn =
  #<If> L.info_conn "read_lines" conn.addr #<End>;
  let read_cond (_, str) =
    let l = String.length str in
    str.[l - 2] = '\r' && str.[l - 1] = '\n'
  in
  read_until sched conn read_cond

let read_all sched conn ?(read_max=Some max_int) ?(block_size=32768) ?timeout ?(buf=FBuffer.make 0) ?(size_max=(-1)) ?err_cont finalize =
  let decide (nb_read, nb_part_read, buf) =
    #<If> L.info_conn "read_all" conn.addr #<End>;
    let to_read = min block_size (Option.default max_int read_max - nb_read) in
    let buf_len = FBuffer.length buf in
    if size_max > 0 && buf_len > size_max then
      raise LimitExceeded
    else
      if (nb_part_read = -1)
        || (read_max <> None && nb_part_read > 0 && nb_read < Option.default max_int read_max)
      then begin
        #<If> L.info_conn "read_all" ~s:(Printf.sprintf "%d" nb_read) conn.addr #<End>;
        Job.Execute (nb_read, to_read, buf)
      end else begin
        #<If> L.info_conn "read_all" ~s:"finalize" conn.addr #<End>;
        Job.Finalize (nb_read, buf)
      end
  in
  let execute (nb_read_before, to_read, buf) =
    try
      let (nb_read, buf) = C.read_more conn.addr buf to_read in
      NetStats.register_recv ~size:nb_read ~conn:conn.addr sched.stats;
      nb_read_before + nb_read, nb_read, buf
    with
    | C.Busy ->
        #<If> L.info_conn "read_all" ~s:"busy" conn.addr #<End>;
        nb_read_before, -1, buf
  in
  let finalize v =
    finalize v;
    remove_connection sched conn
  in
  let error = Option.default (fun _ -> ()) err_cont in
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    (NA.get_fd conn.addr)
    K.Operation.In
    timeout
    decide
    execute
    error
    finalize
    (0, -1, buf)
  in
  ()

let listen sched conn ?timeout ?err_cont execute =
  #<If> L.info_conn "listen" conn.addr #<End>;
  let decide () = Job.Execute () in
  (* Can't use the get_err_cont because it remove the
     listen connection when the client disconnect *)
  let error = Option.default (fun _ -> ()) err_cont in
  let finalize () = assert false in (* never finalize *)
  Job.make
    sched.operation
    sched.priority
    sched.counter
    ~force_polling:true
    (NA.get_fd conn.addr)
    K.Operation.In
    timeout
    decide
    execute
    error
    finalize
    ()

let poll direction _label sched conn ?timeout ?err_cont finalize =
  #<If> L.info_conn _label conn.addr #<End>;
  let error = get_err_cont sched conn err_cont in
  let decide is_first =
    if is_first then Job.Execute ()
    else Job.Finalize ()
  in
  let execute () = false in (* directly finalize *)
  let _ =
  Job.make
    sched.operation
    sched.priority
    sched.counter
    ~force_polling:true
    (NA.get_fd conn.addr)
    direction
    timeout
    decide
    execute
    error
    finalize
    true
  in
  ()

let listen_once = poll K.Operation.In "listen_once"
let connect = poll K.Operation.Out "connect"

(* ============================== *)
(*       Async calculations       *)
(* ============================== *)

let sleep sched time timer_fun =
  #<If> L.info "sleep" ~s:(Printf.sprintf "%d" (Time.in_milliseconds time)) #<End>;
  let key = K.Counter.get_next_int sched.counter in
  K.Priority.add sched.priority key time timer_fun;
  key

let timer sched time timer_fun =
  let rec f() =
    try
      timer_fun ();
      ignore(sleep sched time f)
    with
      | StopTimer -> ()
      | Sys.Break as e -> raise e
      | e -> (* maybe move it in the E. module *)
          let message = Printf.sprintf "Timer function exception: (%s)" (Printexc.to_string e) in
          L.error "timer" message;
  in
  ignore(sleep sched time f)

let push sched f =
  K.Compute.push sched.compute f

(* ============================== *)
(*             Wait               *)
(* ============================== *)

(* Private function *)
let incr_level sched =
  #<If> L.incr_level () #<End>;
  K.Counter.incr_level sched.counter

(* Private function *)
let decr_level sched =
  #<If> L.decr_level () #<End>;
  K.Counter.decr_level sched.counter

let abort sched key =
  if K.Priority.mem sched.priority key then begin
    #<If> L.info "abort priority" ~s:(Printf.sprintf "%d" key) #<End>;
    K.Priority.remove sched.priority key
  end;
  if K.Operation.mem_key sched.operation key then begin
    #<If> L.info "abort operation" ~s:(Printf.sprintf "%d" key) #<End>;
    K.Operation.remove sched.operation key
  end

let do_wait sched ~block =

  if not (K.Finalise.is_empty sched.finalise) then begin
    #<If> L.info "finalise" ~s:(Printf.sprintf "%d callbacks" (K.Finalise.length sched.finalise)) #<End>;
    K.Finalise.process_all sched.finalise
  end;

  if not (K.Compute.is_empty sched.compute) then begin
    #<If> L.info "compute" #<End>;
    K.Compute.process sched.compute
  end;
  let tout = K.Priority.process sched.priority in (* in milliseconds *)

  let nothing_to_do =
    K.Compute.is_empty sched.compute
    && K.Finalise.is_empty sched.finalise
  in

  if (not (K.Operation.is_empty sched.operation)) || (nothing_to_do && tout != -1) then
    begin
    let block = block && nothing_to_do in
    let tout = if block then tout else 0 in
    #<If> L.info "priority" ~s:(Printf.sprintf "%d ms" tout) #<End>;
    let ids = K.Operation.wait sched.operation tout in
    #<If> L.info "operation" ~s:(Printf.sprintf "%d event(s)" (Array.length ids)) #<End>;

    let errors = K.Operation.process_all sched.operation ids in
    let nb_errors = List.length errors in
    if nb_errors != 0 then begin
      #<If> L.info "operation" ~s:(Printf.sprintf "%d errors" nb_errors) #<End>;
      List.iter
        (fun id -> K.Operation.process_id_error sched.operation id Connection_closed)
        errors
    end;
  end

let is_empty sched =
  (K.Operation.is_empty sched.operation) && (K.Priority.is_empty sched.priority) &&
    (K.Compute.is_empty sched.compute) && (K.Finalise.is_empty sched.finalise)

let wait sched ~block=
  if not (is_empty sched) then
    begin
      incr_level sched;
      do_wait sched ~block;
      decr_level sched
    end;
  not (is_empty sched)

let flush ?(f = fun _ -> ()) sched =
  while not (is_empty sched) do
    #<If> L.info "flush" #<End>;
    incr_level sched;
    do_wait sched ~block:true;
    decr_level sched;
    f()
  done

let loop_until sched condition =
  while (not (condition ())) do
    if (is_empty sched) then
      raise Empty
    else begin
      #<If> L.info "loop_until" #<End>;
      incr_level sched;
      do_wait sched ~block:false;
      decr_level sched
    end
  done

let finalise sched f v = K.Finalise.add sched.finalise f v

(* ============================== *)
(*             Misc               *)
(* ============================== *)

(* This function is intended to be in Runtime when introduced *)
let invoke_on_exit _stats () =
  #<If:SERVER_STATS>
    Logger.log "Server statistics:\n%s" (NetStats.to_string _stats)
  #<End>

let empty_sched stats =
  {
    stats        = stats;
    operation    = K.Operation.make ();
    priority     = K.Priority.make ();
    descriptor   = K.Descriptor.make ();
    compute      = K.Compute.make ();
    finalise     = K.Finalise.make ();
    counter      = K.Counter.make ();
    at_exit      = Some(K.Compute.make ());
  }

(* at_exit handling *)
let do_at_exit_time_limit = 10.0 (* take at most N s for do_at_exit *)
let at_exit sched f =
  (* if we already have done the transfer *)
  let here = match sched.at_exit with
  | None -> sched.compute
  | Some(here) -> here
  in K.Compute.push here f

let do_at_exit sched time_limit =
  let log = Logger.debug "Processing Scheduler.at_exit %s" in
  match sched.at_exit with
  | None -> log "Warning: at_exit called twice"
  | Some(sched_at_exit) ->
    sched.at_exit <- None;
    if not(K.Compute.is_empty sched_at_exit) then (
      log (Printf.sprintf "(at most %1.2f seconds)" time_limit);
    (* NEED A REVIEW TO KNOWN WHY A EMTPY SCHEDULER HERE IS NOT WORKING
       PROBABLY BECAUSE THE SCHEDULER IS A GLOBAL VALUE AT EVERY PLACE BUT HERE
       => every K.Truc must proposed an eraser and must be called here *)
      K.Compute.clear sched.compute;
      K.Compute.rev_transfer ~src:sched_at_exit ~dest:sched.compute;
      K.Priority.clear sched.priority;
      sched.operation <- K.Operation.make ();

      let time_limit = Unix.gettimeofday () +. time_limit in
      while not(is_empty sched) &&  (Unix.gettimeofday()<time_limit) do
        do_wait sched ~block:false;
      done;
      if not(is_empty sched)
      then log "stopped because too long"
      else log "finished"
    )
(* end at_exit handling *)


let make ?(is_server=false) () =
  #<If> L.info "make" #<End>;
  let _ = is_server in
  let stats = NetStats.make () in
  let sched = empty_sched stats in
  Pervasives.at_exit (invoke_on_exit stats);
  Pervasives.at_exit (fun () -> do_at_exit sched do_at_exit_time_limit(* time limited *));
  sched

let set_max_compute_successive = SchedulerKer.Compute.set_max_successive

let run sched =
  let rec aux () =
    try
      flush sched
    with
    | Failure "Interrupted system call" -> aux()
    | e -> E.print_exc (L.error "run") e; aux ()
  in
  aux ()

let default = make ~is_server:true ()

(* Print a more helpful message when encountering Unix errors *)
let _ =
  Printexc.register_printer
    (function
     | Unix.Unix_error (err,fct,param) ->
         Some (Printf.sprintf "System error: %s (at %s(%s))"
                 (Unix.error_message err) fct param)
     | _ -> None)
