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
  The low level module for [Scheduler] and [SchedulerJob].
  This module register and process jobs depending on special events or condition.
  See schedulerKer.mli for more information.

  @author Cedric Soulas
*)

#<Debugvar:SCHEDULER_DEBUG>

module E = SchedulerExc
module L = SchedulerLog
module MP = Mlstate_platform

module Const =
struct
  let epoll_size = 1000
  let epoll_max_events = 1000
  let fd_hash_size = 1000
  let tout_cb_size = 1000
  let client_hash_size = 1024
  let init_keys_size = 1024
  let priority_max_successive = 1
  let compute_max_successive = 10000
  let counter_max_reentrant_level = 0
  let counter_max_sync = 0
  let max_keys_number = Sys.max_array_length
end

module Operation =
struct

  type direction = In | Out

  type id = Unix.file_descr

  type callback = {
    key: id * direction;
    cb: unit -> unit;
    cb_error: exn -> unit
  }

  type key = int

  module RA = ResArray

  type t = {
    op_in : (key Queue.t) RA.t;
    op_out : (key Queue.t) RA.t;
    op_cb : callback RA.t;
    mutable op_n : int;
    mutable guard : int;
    epoll : Epoll.epoll_descriptor;
  }

  (* Private module *)
  module S =
  struct

    let null_cb = Obj.magic None
    let null_q = Obj.magic None

    let make_q () = RA.make Const.client_hash_size null_q

    let make_cb () = RA.make Const.init_keys_size null_cb

    let op_dir operation = function
      | In -> operation.op_in
      | Out -> operation.op_out

    (* unix.ml : type Unix.file_descr = int *)
    let get_q operation id direction =
      RA.get (op_dir operation direction) (Obj.magic id)

    let set_q operation id direction q =
      RA.set (op_dir operation direction) (Obj.magic id) q

    let remove_q operation id direction =
      RA.set (op_dir operation direction) (Obj.magic id) null_q

    let get_cb operation key =
      RA.get operation.op_cb key

    let set_cb operation key cb =
      RA.set operation.op_cb key cb

    let remove_cb operation key =
      RA.set operation.op_cb key null_cb

    (* **Physical** test with null_x *)
    let mem operation id direction =
      try
        (get_q operation (Obj.magic id) direction) != null_q
      with
        RA.UnknownCell -> false

    let mem_key operation key =
      try
        (RA.get operation.op_cb key) != null_cb
      with
        RA.UnknownCell -> false

  end

  exception Busy_direction
  exception Not_found of (id * direction)
  exception Unbound_key of key
  exception Existent_key of key

  let direction_to_string = function
    | In -> "In"
    | Out -> "Out"

  let raise_operation_not_found id direction =
 (*    Logger.error "Operation not found: (%d, %s)" (Epoll.Debug.int_of_filedescr id) (direction_to_string direction); *)
    raise (Not_found (id, direction))

  let make () =
    let unix () = Epoll.create Const.epoll_size and dummy () = Obj.magic 42 in
    {op_in = S.make_q ();
     op_out = S.make_q ();
     op_cb = S.make_cb ();
     op_n = 0;
     guard = 0;
     epoll = MP.platform_dependent ~unix ~cygwin:dummy ~windows:dummy () ();
    }

  let mem = S.mem
  let mem_key = S.mem_key

  let length operation = operation.op_n

  let is_empty operation = (length operation = 0)

 (*  Private function  *)
  let register operation fd direction =
    let exist d = mem operation fd d in
    match direction with
    | In ->
        if exist Out then (
          Epoll.listen_in_out operation.epoll fd
        ) else (
          Epoll.listen_in_only operation.epoll true fd)
    | Out ->
        if exist In then (
          Epoll.listen_in_out operation.epoll fd
        ) else (
          Epoll.listen_out_only operation.epoll true fd)

 (*  Private function  *)
  let unregister operation fd direction =
    let exist d = mem operation fd d in
    match direction with
    | In ->
        if exist Out then (
          Epoll.listen_out_only operation.epoll false fd
        ) else (
          Epoll.del operation.epoll fd )
    | Out ->
        if exist In then (
          Epoll.listen_in_only operation.epoll false fd
          ) else (
          Epoll.del operation.epoll fd)

  (* Private function *)
  (* Return an non-empty queue, raise Operation_not_found otherwise *)
  let find_queue operation id direction =
    if (mem operation id direction) then
      begin
        let q = S.get_q operation id direction in
        if Queue.is_empty q then
          raise_operation_not_found id direction
        else
          q
      end
    else
      raise_operation_not_found id direction

  (* Private function *)
  let find_key operation id direction =
    let q = find_queue operation id direction in
    Queue.peek q

  (* Private function *)
  let find_op_cb operation key =
    if mem_key operation key then
      S.get_cb operation key
    else
      raise (Unbound_key key)

  (* Private function *)
  let find_op_key operation key =
    let c = (find_op_cb operation key) in
    c.key

  (* Private function *)
  let find_cb operation key =
    let c = (find_op_cb operation key) in
    c.cb

  (* Private function *)
  let find_cb_error operation key =
    let c = (find_op_cb operation key) in
    c.cb_error

  let add operation id direction key callback callback_error =
    begin
      if operation.guard <> 0 then begin
          Logger.critical "A routine has been broken (code %d)" operation.guard;
          exit 1
      end else
        ()
    end;
    operation.guard <- operation.guard + 1;
    begin
      if mem_key operation key then
        raise (Existent_key key)
      else
        let c = {
          key = id, direction;
          cb = callback;
          cb_error =  callback_error }
        in
        S.set_cb operation key c
    end;
    if mem operation id direction then begin
      let q = S.get_q operation id direction in
      Queue.add key q;
    end else begin
      register operation id direction;
      let q = Queue.create () in
      Queue.add key q;
      operation.op_n <- operation.op_n + 1;
      S.set_q operation id direction q;
    end;
    operation.guard <- operation.guard - 1

  (* Private function *)
  let remove_candidate operation id direction =
    let q = find_queue operation id direction in
    let _ = Queue.pop q in
    if Queue.is_empty q then
      begin
        operation.op_n <- operation.op_n - 1;
        S.remove_q operation id direction;
        unregister operation id direction
      end
    else
      ()

  (* Private function *)
  (* Remove, on top of the queue, keys associated to operation removed *)
  (* Raise Not_found, via get_queue, if no operation was found *)
  let rec clean operation id direction =
    try
      let key = find_key operation id direction in
      if mem_key operation key then
        ()
      else begin
        (* The operation was removed, try to find another *)
        remove_candidate operation id direction;
        clean operation id direction
      end
    with
    | Not_found _ -> ()
    | e -> raise e

  let remove operation key =
    if mem_key operation key then begin
      let id, direction = find_op_key operation key in
      S.remove_cb operation key;
      clean operation id direction
    end else begin
      raise (Unbound_key key)
    end

  let replace operation key callback callback_error =
    if mem_key operation key then
      let op_key = find_op_key operation key in
      (* Simpe remove because public remove also clean *)
      S.remove_cb operation key;
      let cb = {
        key = op_key;
        cb = callback;
        cb_error = callback_error }
      in
      S.set_cb operation key cb
    else
      raise (Unbound_key key)

  let remove_id operation id =
    if mem operation id In || mem operation id Out then begin
      begin match MP.mlstate_platform with
      | MP.Unix ->
          (try Epoll.del operation.epoll id
           with _e ->
             Logger.error "Remove error: %s" (Printexc.to_string _e)
          );
      | _ -> ()
      end
    end;
    begin
      let rm direction =
        if mem operation id direction then
          let q = S.get_q operation id direction in
          Queue.iter (fun key -> S.remove_cb operation key) q;
          operation.op_n <- operation.op_n - 1;
          S.remove_q operation id direction;
        else
          ()
      in
      rm In;
      rm Out
    end

  let wait operation tout =
    E.execute_wait (fun () -> Epoll.wait ~tout operation.epoll Const.epoll_max_events)

  let process operation id direction =
    let key = find_key operation id direction in
    let callback = find_cb operation key in
    E.execute callback (L.error "operation process")

  let process_error operation id direction e =
    let key = find_key operation id direction in
    let callback = find_cb_error operation key in
    E.execute (fun () -> callback e) (L.error "operation error process")

  let process_id_error operation id e =
    if mem operation id In then
      process_error operation id In e;
    if mem operation id Out then
      process_error operation id Out e

  let process_all operation fds =
    let exec errors (fd, event_list) =
      let callback d =
        #<If> L.ker_info "process" ~s:(Printf.sprintf "%d, %s" (L.int_of_fd fd) (direction_to_string d)) #<End>;
        #<If> L.incr_level () #<End>;
        (try process operation fd d
         with Not_found(_) -> Logger.warning "SchedulerKer.Operation.Not_found");
        #<If> L.decr_level () #<End>
      in
      let has_err = List.fold_left
        (fun has_err event ->
           match event with
           | Epoll.In  | Epoll.Out -> has_err
           | Epoll.Err | Epoll.Hup -> true
           | Epoll.Unsupported e -> L.error "operation process" (Printf.sprintf "Unsupported event mask: %d" (Epoll.Debug.int_of_events e)); true
        ) false event_list
      in
      if has_err then
        fd::errors
      else begin
        List.iter
          (function
             | Epoll.In -> callback In
             | Epoll.Out -> callback Out
             | _ -> assert false (* errors checked above *)
          ) event_list;
        errors
      end
    in
    Array.fold_left exec [] fds
end

module Priority =
struct

  type ref_cb = (unit -> unit) ref

  module OrderedFD =
  struct
    type t = Time.t * ref_cb
    let compare (t1, _) (t2, _) = compare t1 t2
  end

  module BHeap = Heap.Binary(OrderedFD)

  type key = int
  type t = {
    heap : BHeap.t; (* timeout for jobs (valid and invalid) *)
    tout_cb : (key, ref_cb) Hashtbl.t;
    mutable n : int; (* the remaining active jobs *)
  }

  exception Timeout
  exception Busy_direction
  exception Existent_key of key

  let nothing_todo = (fun () -> ())

  let make () =
    { heap = BHeap.empty();
      tout_cb = Hashtbl.create Const.tout_cb_size;
      n = 0;
    }

  let clear t =
    begin
      Hashtbl.clear t.tout_cb;
      while not(BHeap.is_empty t.heap) do ignore(BHeap.remove t.heap) done;
      t.n <- 0
    end

  let length priority = priority.n

  let is_empty priority = length priority == 0

  let mem priority key = Hashtbl.mem priority.tout_cb key

  let remove priority key =
    if mem priority key then (
      priority.n <- priority.n - 1;
      let ref_cb = Hashtbl.find priority.tout_cb key in
      ref_cb := nothing_todo;
      Hashtbl.remove priority.tout_cb key
    ) else
      ()

  let add priority key tout callback =
    if Hashtbl.mem priority.tout_cb key then
      raise (Existent_key key)
    else
      priority.n <- priority.n + 1;
      let tout_date = Time.add (Time.now ()) tout in
      let callback e =
        remove priority key;
        callback e
      in
      let ref_cb = ref callback in
      Hashtbl.add priority.tout_cb key ref_cb;
      let _ = BHeap.insert priority.heap (tout_date, ref_cb) in
      ()

  (* TODO, nb_successive should be use in conjunction with a maximum time for
     successive jobs set *)
  let process priority =
    let rec aux nb_successive =
      let nb_successive=nb_successive+1 in
      match BHeap.minimum priority.heap with
      | Some (tout_date, ref_cb) ->
          let t = Time.difference (Time.now ()) tout_date in
          if (nb_successive <= Const.priority_max_successive) && not (Time.is_positive t) then begin
            let _ = BHeap.remove priority.heap in
            let todo = !ref_cb in
            if todo==nothing_todo then aux (nb_successive-1)
            (* everything above is considered as no op *)
            else (
              begin
                E.execute
                  todo
                  (L.error "priority process");
              end;
              aux nb_successive
            )
            (* +2 ms  to have a small upper value *)
            (* otherwise we could be called just to soon *)
          end else
            let t = Time.max Time.zero (Time.add t (Time.milliseconds 2)) in
            Time.in_milliseconds t
        | None -> -1 (* special epoll parameter: infinite wait *)
    in aux 0
end


module Descriptor =
struct

  type t = (Unix.file_descr, int) Hashtbl.t
  type id = Unix.file_descr
  type key = int
  type mem_response = Alive | Replaced | Closed

  let make () = Hashtbl.create Const.fd_hash_size
  let length = Hashtbl.length
  let is_empty descriptor = length descriptor = 0

  let remove  = Hashtbl.remove

  let add descriptor id =
    let key = Random.bits () in
    if Hashtbl.mem descriptor id then
      remove descriptor id
    else
      ();
    Hashtbl.add descriptor id key;
    key

  let mem descriptor id key =
    if Hashtbl.mem descriptor id then
      if (Hashtbl.find descriptor id) = key then
        Alive
      else
        Replaced
    else
      Closed
end


module Compute =
struct

  type t = (unit -> unit) Queue.t

  let make = Queue.create

  let length = Queue.length

  let is_empty = Queue.is_empty

  let push t f = Queue.push f t

  let rec process t =
    let rec aux i =
      if i = 0 then ()
      else (
        if Queue.is_empty t then ()
        else
          let f = Queue.take t in
          E.execute f (L.error "compute");
          aux (pred i)
      )
    in aux Const.compute_max_successive

  let clear (t:t) = Queue.clear t

  let rec rev_append_list (t:t) l =
    if Queue.is_empty t then l
    else rev_append_list t ((Queue.take t)::l)

  let push_seq l t = List.iter (push t) l

  let rec rev_transfer ~(src:t) ~(dest:t) =
    let l = rev_append_list src [] in
    push_seq l dest

end

module Finalise =
struct

  type t = {
    mutable fin_cb : (unit -> unit) list;
    mutable length : int
  }

  let make () = {
    fin_cb = [];
    length = 0
  }

  let length finalise = finalise.length

  let is_empty finalise = length finalise = 0

  let add finalise cb v =
    let f v =
      let cb = fun () -> cb v in
      finalise.length <- finalise.length + 1;
      finalise.fin_cb <- cb::finalise.fin_cb
    in
    Gc.finalise f v

  let process_all finalise =
    List.iter (fun cb -> E.execute cb (L.error "finalise")) finalise.fin_cb;
    finalise.fin_cb <- [];
    finalise.length <- 0;

end

module Counter =
struct

  type t = {
    mutable reentrant_level  : int;
    mutable sync_counter: int;
    mutable keys: int list;
    mutable keys_size: int;
    mutable next_int: int
  }

  exception Sync_limit

  (* Private function *)
  let init_keys min max =
    let rec aux n l =
      if n < min then
        l
      else
        aux (n-1) (n::l)
    in
    aux max []

  let make () =
    {
      reentrant_level = -1;
      sync_counter = 0;
      keys = init_keys 0 Const.init_keys_size;
      keys_size = Const.init_keys_size;
      next_int = Const.max_keys_number (* start from the limit of keys *)
    }

  let get_next_int counter =
    begin
      (* The counter starts at max_keys_number,
         then it increases and reach max_int,
         then continue to increase from min_int to 0.
         At that point, we have to jump over the current
         "keys" max number.
      *)
      if counter.next_int = -1 then
        counter.next_int <- Const.max_keys_number
      else
        counter.next_int <- counter.next_int + 1
    end;
    counter.next_int

  let get_key counter =
    match counter.keys with
    | key::t ->
        counter.keys <- t;
        key
    | [] ->
        let s = counter.keys_size * 2 in
        Logger.debug "Adjusting kernel size to %d" s;
        let key = counter.keys_size + 1 in
        counter.keys <- init_keys (counter.keys_size + 2) s;
        counter.keys_size <- s;
        key

  let release_key counter key =
    counter.keys <- key::(counter.keys)

  let incr_level counter =
    if counter.reentrant_level + 1 > Const.counter_max_reentrant_level then begin
      Logger.critical "Reentrant routine";
      failwith "Reentrant routine";
    end else begin
      counter.reentrant_level <- counter.reentrant_level + 1
    end

  let decr_level counter =
    counter.reentrant_level <- counter.reentrant_level - 1

  let incr_sync counter =
    if counter.sync_counter + 1 > Const.counter_max_sync then
      raise Sync_limit
    else begin
      counter.sync_counter <- counter.sync_counter + 1
    end

  let decr_sync counter =
    counter.sync_counter <- counter.sync_counter - 1

end
