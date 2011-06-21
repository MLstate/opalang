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

module CR = QmlClosureRuntime

type continuation_options =
    {
      movable: bool;
      atomic: bool;
      _lazy: bool
    }

type stack_infos =
    { caller_info : continuation_info
    ; call_position : string
    ; callee_name : string
    ; call_arguments : Obj.t } (* it is an opa tuple *)

and continuation_info =
    { options : continuation_options ;
      (** options for this continuation. Often, the options are shared between several continuation,
          using syntax {[let c = { previous_c with payload = new_payload }]} *)
      thread_context : Obj.t option ;
      (** passing \@thread_context around.
          The thread_context is set in Opa (or Qml) using \@with_thread_context,
          the type is a Qml(Opa) record, the qml typer checking that the utilisation is correct
          if it has never been set with a \@with_thread_context, it is None
      *)
      transaction_context : Obj.t option;
      (** Has a behaviour similar to thread_context, but is reserved for use by
          database and transaction handling mechanisms
      *)
      exn_handler : Obj.t continuation option;
      (** continuation that can be triggered by \@raise *)

      stack_infos : stack_infos option;
    }

(* the representation of continuation is not flattened because the main operations
 * on continuation are allocations and return, that only need the payload field
 * this representation makes allocation much cheaper *)
and 'a continuation =
    {
      payload: CR.t ; (* the closure should have type 'a -> unit *)
      (** the effective function of the continuation *)

      continuation_info : continuation_info
      (** contains any information associated to the current execution *)
    }

type 'a func0 = 'a continuation -> unit
type ('a, 'b) func = 'a -> 'b func0
type ('a, 'b, 'c) func2 = 'a -> 'b -> 'c func0
type ('a, 'b, 'c, 'd) func3 = 'a -> 'b -> 'c -> 'd func0
type ('a, 'b, 'c, 'd, 'e) func4 = 'a -> 'b -> 'c -> 'd -> 'e func0
type ('a, 'b, 'c, 'd, 'e, 'f) func5 = 'a -> 'b -> 'c -> 'd -> 'e -> 'f func0

let debug = (* I'm a temporary handler, remove me *)
  if (try Sys.getenv "MLSTATE_CPS_DEBUG" <> "0" with Not_found -> false)
  then (fun fmt -> Format.eprintf ("[34m[cps][0m "^^fmt^^"\n%!"))
  else (fun fmt -> Format.ifprintf Format.err_formatter fmt)

let magic_cont = (Obj.magic : _ continuation -> _ continuation)

external make_func: ('a -> 'b continuation -> unit) -> ('a, 'b) func = "%identity"

let default_options = {movable = true; atomic = false; _lazy = false}
let default_thread_context = None
let default_transaction_context = None
let default_exn_handler = None
let default_stack_infos = None
let default_continuation_info =
  { options = default_options
  ; thread_context = default_thread_context
  ; transaction_context = default_transaction_context
  ; exn_handler = default_exn_handler
  ; stack_infos = default_stack_infos }

(* inlining : these function are called every 2 lines in the generated code *)
let make_cont options f =
  {payload = f; continuation_info = {default_continuation_info with options = options}}
let make_cont_ml options f =
  make_cont options (CR.create_no_ident1 f)
let cont f =
  {payload = f; continuation_info = default_continuation_info}
let cont_ml f =
  cont (CR.create_no_ident1 f)
let ccont b f =
  { b with payload = f }
let ccont_ml b f =
  ccont b (CR.create_no_ident1 f)

let update_cont cont parent name position args =
  {cont with
     continuation_info =
        { cont.continuation_info with
            stack_infos =
              Some
                { caller_info =
                    (match parent with
                     | None -> default_continuation_info (* a little wierd *)
                     | Some cont -> cont.continuation_info)
                ; callee_name = name
                ; call_position = position
                ; call_arguments = Obj.repr args } } }

let generic_trace_printer ?(first_line="*** Stack trace:") printer (cont : _ continuation) =
  Printf.eprintf "%s\n" first_line;
  let rec aux i infos =
    match infos.stack_infos with
    | None -> () (* not calling printer, because this 'infos' a the dummy one introduced above *)
    | Some stack_infos ->
        printer i infos stack_infos;
        aux (i+1) stack_infos.caller_info in
  aux 0 cont.continuation_info

let trace_printer ?(args= #<If:CPS_STACK_TRACE$contains "arg">true#<Else>false#<End>)
                  ?(thread_context= #<If:CPS_STACK_TRACE$contains "th">true#<Else>false#<End>)
                  ?(transaction_context= #<If:CPS_STACK_TRACE$contains "tr">true#<Else>false#<End>) () =
  fun index infos stack_infos ->
    Printf.eprintf "%3d: %20s called at %s%s%s%s\n"
      index
      stack_infos.callee_name
      stack_infos.call_position
      (if args then " with args=" ^ DebugPrint.print stack_infos.call_arguments else "")
      (if thread_context then
         match infos.thread_context with
         | None -> ""
         | Some thread_context -> " with thread_context=" ^ DebugPrint.print thread_context
       else "")
      (if transaction_context then
         match infos.transaction_context with
         | None -> ""
         | Some transaction_context -> " with transaction_context=" ^ DebugPrint.print transaction_context
       else "")
let print_trace_fl first_line cont = generic_trace_printer ~first_line (trace_printer ()) cont
let print_trace cont = generic_trace_printer (trace_printer ()) cont

let thread_context b : _ option = Obj.magic (b.continuation_info.thread_context : Obj.t option)
let with_thread_context tc b = { b with continuation_info = {b.continuation_info with thread_context = Some (Obj.repr tc) } }

(* Identical to thread_context handlers *)
let transaction_context b : _ option = Obj.magic (b.continuation_info.transaction_context : Obj.t option)
let with_transaction_context tc b = { b with continuation_info = {b.continuation_info with transaction_context = Some (Obj.repr tc) } }

(**
   Runtime backtrace generation. Uses only constant space and time.
*)
let (bt_add, bt_take) =
  let max_backtrace_size = 25 in
  let cyclic_queue = Array.create max_backtrace_size "" in (*LIFO queue so far*)
  let cursor = ref 0 in
  let cursor_valid () = !cursor >= 0 && !cursor < max_backtrace_size in
  let bt_add s =
    if s <> Array.get cyclic_queue !cursor then begin (* TODO: turn this off for full backtrace *)
      assert (cursor_valid ());
      Array.set cyclic_queue !cursor s;
      cursor := (!cursor + 1) mod max_backtrace_size
    end
  in
  let bt_take () =
    assert (cursor_valid ());
    cursor := (!cursor + max_backtrace_size - 1) mod max_backtrace_size;
    let s = Array.get cyclic_queue !cursor in
    Array.set cyclic_queue !cursor "";
    s
  in
  (bt_add, bt_take)

let fun_args2string f_string larg =
  let larg = [larg] in (* TODO: a tmp hack *)
  let f arg =
    let s = DebugPrint.print arg in
    String.sub s 0 (min (String.length s) 1000)
  in
  let lval = Base.String.concat_map ", " f larg in
  Base.sprintf "%s(%s, ...)" f_string lval

let display_backtrace s =
  let bt_pos = ref (bt_take ()) in
  if !bt_pos <> "" then begin
    Printf.fprintf stderr "%s at %s\n" s !bt_pos;
    bt_pos := bt_take ();
    while !bt_pos <> "" do
      Printf.fprintf stderr "Called from %s\n" !bt_pos;
      bt_pos := bt_take ()
    done
  end

let report_exception exc k =
  Printf.eprintf "Error: uncaught OPA exception %s\n" (DebugPrint.print exc);
  (*display_backtrace "Raised"*)
  print_trace k

let handler_cont k = match k.continuation_info.exn_handler with
  | None -> cont_ml (fun exc -> report_exception exc k)
  | Some h -> magic_cont h
let catch_ml h k = { k with continuation_info = {k.continuation_info with exn_handler = Some (ccont_ml k (fun x -> h (Obj.obj x) k)) } }
let catch h k = { k with continuation_info = {k.continuation_info with exn_handler = Some (ccont_ml k (fun x -> CR.args_apply2 h (Obj.obj x) k)) } }
(*
  Runtime error : The scheduler has nothing to do
  This event can have serveral different meaning depending on the situation.
  + At end of program : the exception is simply ignored, execution is finished
  + Waiting (blocking_wait) for a barrier : internal error, a release barrier should have been done
*)
exception Nothing_to_do

type 'a barrier_status =
  | Computed of 'a
  | Exception of Obj.t
  | Waiting of 'a continuation list ref

type 'a barrier = {
  mutable status : 'a barrier_status;
  (**
     The status of the barrier. Mutable, manipulated internally by
     + [release_barrier]
     + [fail_barrier]
  *)

  ident : string;
  (**
     A human readable identifier for identifying barriers.
     Used for debug.
  *)

  nb : int;
  (**
     A uniq identifier for identifying barriers.
  *)
}

type task = {
  action: unit continuation;
  waiting: unit barrier
}

type 'a future = 'a barrier

let print_barrier_status f = function
  | Computed _ -> Format.fprintf f "Computed"
  | Exception _ -> Format.fprintf f "Exception"
  | Waiting l -> Format.fprintf f "Waiting <%d>" (List.length !l)

let print_barrier f b =
  Format.fprintf f "{ident : %s; nb : %d; status %a}"
    b.ident b.nb print_barrier_status b.status

let print_task i {action = action; waiting = waiting} =
  print_trace_fl (Printf.sprintf "** Scheduler task %d" i) action;
  Format.eprintf "@[<2>%d - Waiting:@ %a@]\n" i print_barrier waiting

let nb_barrier = ref 0
let make_barrier ident =
  incr nb_barrier;
  {
    nb = !nb_barrier;
    ident = ident;
    status = Waiting (ref [])
  }

let make_task (f:unit continuation) : task = {
  action = f;
  waiting = make_barrier "make_task"
}

let task_of_fun = make_task

let push f = Scheduler.push Scheduler.default f

let push_cont k x =
  push (fun () -> CR.args_apply1 k.payload x)

(* With the explicit flush of the scheduler at end of the toplevel
   initialization, there is not need to schedule in apply or return
   Moreover, this breaks the tail-rec optimization of ocaml code. *)
let nb_step_apply = 10000
let max_blocking_step = 1000000
(* cannot embbed the reference for typing problem *)
let applied_step = ref nb_step_apply
let wait_release = ref false

let check_stack_step = pred (1 lsl 10) (* <!> should be a 2^^n -1 *)
let stack_limit = 5000000
let stack_usage = BaseSys.stack_usage

let return k x =
  #<Ifstatic:CPS_STACK_SIZE .*>
    Printf.printf "stack-usage: %d\n%!" (stack_usage ()) ;
  #<End>
  let applied_step_contents = !applied_step in
  if
    (*
      The stack should not be checked and nothing should be pushed
    *)
    (applied_step_contents land check_stack_step <> 0)
    ||
      (*
        The stack is ok, and we should not push
      *)
      ( (stack_usage () <= stack_limit) && ( applied_step_contents <> 0 ) )

  then (
    decr applied_step;
    CR.args_apply1 k.payload x
  )
  else (
    applied_step:=nb_step_apply;
    push_cont k x
  )

let execute k x = CR.args_apply1 k.payload x

let apply f v k =
  push (fun () -> f v k)
  (* dont schedule here *)

(* used for nary application, f is the partial application of f' on every arg
   but the final continuation *)
let apply0 f k =
  push (fun () -> execute (ccont k f) ())
 (* dont schedule here *)
let apply0_ml f k = apply0 (CR.create_no_ident1 f) k

let wait_barrier v k =
  debug "Wait_barrier";
  let payload x = push_cont k x in
  match v.status with
  | Computed x -> payload x
  | Exception exc ->
      (match k.continuation_info.exn_handler with
         | None -> report_exception exc k
         | Some h ->
             push_cont (ccont_ml k (fun () -> CR.args_apply1 h.payload exc)) ())
  | Waiting l ->
      l := ccont_ml k payload :: !l

(* used for a non concurrency toplevel value introduction,
   and uncps
*)
let nb_block = ref 0
let block_stack = ref []
let save_block_stack _ =
  block_stack := !applied_step::!block_stack;
  applied_step := max_blocking_step
let resume_block_stack _ =
  match !block_stack with
  | hd::tl ->
      applied_step := hd ;
      block_stack := tl
  | [] ->
      (* we assume that a save_block_stack is always executed before a resume_block_stack *)
      assert false

let before_blocking_wait _ =
  save_block_stack () ;
  wait_release := true

let blocking_wait (v : 'a barrier) : 'a =
  resume_block_stack () ;
  wait_release := false;
  match v.status with
  | Computed a -> a
  | Exception _ -> failwith "exception outside of cps context"
      (* TODO Obj.dump *) (* a bypass cannot raise exc, or non concurrency mode and exc at toplevel *)
  | Waiting _ ->
      failwith
        (Printf.sprintf
           "Barrier (%s : %d) was not released, don't wait anymore"
           v.ident v.nb)

let before_wait = save_block_stack

let toplevel_wait (v : 'a barrier) : 'a =
  resume_block_stack () ;
  match v.status with
  | Computed a -> a
  | Exception _ ->
      failwith "toplevel_wait: exception at non-cps toplevel"
  | Waiting _ ->
      (*
        This should really not happens, because we give to the scheduler a function
        for checking if the barrier was released.
      *)
      failwith "toplevel_wait: internal error"

let release_barrier (v:'a barrier) (x:'a) =
  debug "release_barrier";
  match v.status with
  | Exception _
  | Computed _ ->
      failwith "Internal inconsistency: this barrier has been released twice."
  | Waiting l ->
      v.status <- Computed x;
      List.iter (fun f -> CR.args_apply1 f.payload x) !l

let fail_barrier (v:'a barrier) (exc:'a) =
  match v.status with
  | Exception _
  | Computed _ ->
      failwith "Internal inconsistency: this barrier has been released twice."
  | Waiting l ->
      v.status <- Exception (Obj.repr exc);
      List.iter (fun f -> return (handler_cont f) exc) !l

let is_released v =
  match v.status with
  | Waiting _ ->
      let _ =
        #<If:CPS_BLOCKING_WAIT>
          Printf.eprintf "LOOP-UNTIL: is_released(%s)=false\n%!" v.ident
        #<End>
      in
      false
  | _ -> true

let max_loop_time = 40.0
let time_limit () = Unix.gettimeofday () +.  max_loop_time
let false_may_fail_on_time_limit v time_limit =
  let t = Unix.gettimeofday() in
  if t < time_limit then false
  else failwith
    (Printf.sprintf
       "Barrier (%s : %d) was not released after %1.2f seconds, don't wait anymore"
       v.ident v.nb (t-.time_limit)
    )

let looping_wait (v : 'a barrier) : 'a =
  (* a first 'fast' shot without call to time *)
  let rec one_shot i =
    if i <> 0 then
      if not(is_released v) then (
        ignore (Scheduler.wait Scheduler.default ~block:false);
        one_shot (i-1)
      )
  in
  one_shot 1;
  (* a second slower loop wait for long computation *)
  if not(is_released v) then (
    let loop () =
      let time_limit = time_limit () in
      let is_released () =
        is_released v
        || (false_may_fail_on_time_limit v time_limit)
      in
      Scheduler.loop_until Scheduler.default is_released
    in
    loop ();
  );
  toplevel_wait v

type black_future
external black_future : 'a future -> black_future = "%identity"
external unblack_future : black_future -> 'a future = "%identity"
let black_make_barrier str = black_future (make_barrier str)
let black_release_barrier v = release_barrier (unblack_future v)
let black_blocking_wait v = blocking_wait (unblack_future v)
let black_toplevel_wait v = toplevel_wait (unblack_future v)

external magic_func : ('a, 'b) func -> ('c, 'd) func = "%identity"
external magic_func0 : (_) func0 -> (_) func0 = "%identity"
external magic_func1 : (_,_) func -> (_,_) func = "%identity" (* same as magic_func, created for homogeneity *)
external magic_func2 : (_,_,_) func2 -> (_,_,_) func2 = "%identity"
external magic_func3 : (_,_,_,_) func3 -> (_,_,_,_) func3 = "%identity"
external magic_func4 : (_,_,_,_,_) func4 -> (_,_,_,_,_) func4 = "%identity"
external magic_func5 : (_,_,_,_,_,_) func5 -> (_,_,_,_,_,_) func5 = "%identity"
external magic_func_more : _ -> _ = "%identity"


let spawn (f:(unit, 'a) func) =
  let barrier = make_barrier "spawn" in
  let k =
    { payload = CR.create_no_ident1 (fun x -> release_barrier barrier x);
      continuation_info =
        {default_continuation_info with
           exn_handler = Some (cont_ml (fun exc -> fail_barrier barrier (Obj.obj exc)))}
    } in
  let action =
    { payload = CR.create_no_ident1 (fun () -> f () k);
      continuation_info = default_continuation_info } in
  push_cont action ();
  barrier

let wait v k = wait_barrier v k

let uncps ident k f =
  debug "uncps";
  let b = make_barrier ident in
  let c = ccont_ml k (fun z -> release_barrier b z) in
  before_blocking_wait ();
  let _ = CR.args_apply1 f c in
  looping_wait b

let uncps_ml ident k (f:'b continuation -> unit) =
  uncps ident k (CR.create_no_ident1 f)

let callcc_directive f k =
  let unit_cont = cont_ml (fun _ -> ()) in
  f k unit_cont

(* === *)

type ('a, 'b) pair = {f_0: 'a; f_1: 'b}
let pair x y = {f_0=x; f_1=y}

let callback_of_fun f =
  let barrier = make_barrier "callback_of_fun" in
  pair (fun x -> release_barrier barrier (f x)) barrier

let callback_post () =
  let barrier = make_barrier "callback_post" in
  pair (fun x -> release_barrier barrier x) barrier

let wrap_async f x cb =
  let barrier = make_barrier "wrap_async" in
  f x (fun result_of_f ->
    let result_of_cb = cb result_of_f in
    release_barrier barrier result_of_cb; result_of_cb);
  barrier


(* registering a debug printer *)
let () =
  let module D = DebugPrint in
  let check_continuation_options = D.tuple3 ~f1:D.bool ~f2:D.bool ~f3:D.bool in
  let check_payload = D.true_ in (* not checking that we really have a qml closure
                                  * on purpose
                                  * if not, i still want to print the continuation
                                  * as a continuation (even with random content) *)
  let check_thread_context x = D.option x in
  let check_transaction_context x = D.option x in
  let check_exn_handler x = D.option x in (* don't do recursive checks *)
  let check_stack_infos =
    D.tuple_n [ D.true_
              ; D.string
              ; D.string
              ; D.true_ ] in
  let check_continuation_info =
    D.tuple_n [ check_continuation_options
              ; check_thread_context
              ; check_transaction_context
              ; check_exn_handler
              ; check_stack_infos ] in
  let check =
    D.tuple_n [ check_payload
              ; check_continuation_info ] in
  let unsafe_print {payload = payload; continuation_info = continuation_info} =
    Printf.sprintf "{payload = %s%s}"
      (DebugPrint.print payload)
      (if continuation_info = default_continuation_info then "" else
         Printf.sprintf "continuation_info = {%s%s%s%s}"
           (if continuation_info.options = default_options then ""
            else
              let options = continuation_info.options in
              Printf.sprintf "options = {movable=%B; atomic=%B; _lazy=%B}; " options.movable options.atomic options._lazy)
           (match continuation_info.thread_context with
            | None -> ""
            | Some thread_context ->
                Printf.sprintf "thread_context = %s; " (DebugPrint.print thread_context))
           (match continuation_info.transaction_context with
            | None -> ""
            | Some transaction_context ->
                Printf.sprintf "transaction_context = %s; " (DebugPrint.print transaction_context))
           (match continuation_info.exn_handler with
            | None -> ""
            | Some exn_handler ->
                Printf.sprintf "exn_handler = %s; " (DebugPrint.print exn_handler))) in
  let print_opt x =
    if check (Obj.repr x) then
      Some (unsafe_print (Obj.magic x : _ continuation))
    else
      None in
  D.register {D.f = print_opt}


(* Useful exported functions *)

module Ops = struct
  let (@>) f k = f k
  let (|>) x k = return k x
end
open Ops

let rec fold_list f acc l k = match l with
  | [] -> acc |> k
  | hd::tl -> f acc hd @> ccont_ml k @> fun acc -> fold_list f acc tl @> k

let fold_array f acc arr k =
  let s = Array.length arr in
  let rec aux acc i k =
    if i >= s then acc |> k
    else f acc arr.(i) @> ccont_ml k @> fun acc -> aux acc (succ i) @> k
  in
  aux acc 0 @> k
