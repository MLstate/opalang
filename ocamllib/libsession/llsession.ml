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
#<Debugvar:SESSION_DEBUG>

module type M = sig

  (** {6 Type definitions} *)

  (** Type of a session parameterized by type of message and type of
      context that the message handler can be handle.*)
  type ('msg, 'ctx) t

  (** {6 Creations}*)
  (** [make state context handler ondelete ronly] Make a channel with
      original [state].

      [handler] is a function that take on arguments the current
      [state] of the session, a message and a [context]. It should
      returns an optionnal [state]. If it returns [None] the session
      is killed and [ondelete] is executed, else the session state is
      setted with the returned state (unless if [ronly == true], see
      below for more explication).

      If the given [context] is [Some _] then the [handler] will be
      always call with this creation context. Else the considered
      [context] will be that the context provided by the [send]
      function.

      Parameter [ronly] indicates that the state is never writed. Then
      the value returned by session [handler] just indicate if session
      should be killed ([None]) or not ([Some _]) (i.e. the state is
      constant and its value is the parameter [state] provided on
      creation). {b That allows to execute handler on concurrent way
      (if it manipulate imperative structure like references, hashtbl,
      etc. handler must be manage itself concurrency).} *)
  val make : 'state ->
    'context option ->
    ('state -> 'message -> 'context option -> 'state option) ->
    (unit -> unit) option ->
    bool -> ('message, 'context) t

  (** Like [make] but functionnal parameters ([handler] and
      [ondelete]) are asynchronous and writed on "cps" (i.e. Take a
      continuation as last parameter instead of returns a value) *)
  val make_cps : 'state ->
    'context option ->
    ('state -> 'message -> 'context option -> ('state option -> unit) -> unit) ->
    (unit -> unit) option ->
    bool -> ('message, 'context) t


  (** {6 Communications} *)

  (** [send session message context] Send a [message] to a [session]
      with a [context]. This [message] will be handle when it possible
      (or maybe never if the session is killed before). Provided
      [context] will be forward to the message handler only if the
      [session] was create with a [None] context (see [make]).  *)
  val send : ('message, 'context) t -> 'message -> 'context option -> unit

  (** [send_then session message context herror hsuccess] Like [send]
      but if message was not delivered [herror] will be executed, else
      execute [hsuccess]. *)
  val send_then : ('message, 'context) t -> 'message -> 'context option ->
    (unit -> unit) -> (unit -> unit) -> unit

end

(**
   A pprinter for debuging sessions.
*)
let pp_snapshot fmt (exc, __msg, __st, __ctx) =
  let depth = 5 in
  let pp x = DebugPrint.pp ~depth x in
  #<If$minlevel 200>
    Format.fprintf fmt (
      "Uncaught exception: \"%a\"@\n@[<2>The following message is skipped. Session snapshot:@\n"^^
      "@[<2>msg: %a@]@\n"^^
      "@[<2>st: %a@]@\n"^^
      "@[<2>ctx: %a@]@]"
    )
      pp exc
      pp __msg
      pp __st
      pp __ctx
   #<Else>
    Format.fprintf fmt (
      "Uncaught exception: \"%a\"@\n@[<2>The following message is skipped.:@\n"^^
      "If you want (msg, st, ctx) debug printing set debug variable session_debug >= 200"
    )
      pp exc
   #<End>

let make scheduler =

  let module Implem = struct

  module U = SessionUtils

  let (@>) = U.(@>)

  type ('msg,'ctx) t = {
    mutable killed : bool;
    mutable lock : bool;
    action : 'msg -> 'ctx option -> (bool -> unit) -> unit;
    messages : ('msg * 'ctx option * ((unit -> unit) * (unit -> unit)) option) Queue.t;
    ondelete : (unit -> unit) option;
    owner_ctx : 'ctx option;
    ronly : bool;
  }

  (*
    This is the location in this code where the closure corresponding
    to the message handler of the session is created.
    The state is allocated there (let state = ref st).
    The parameter [f] corresponds to the original handler from outside.
    The resulting closure is the action field of the session. It encapsulates the state of the session.
    The boolean applied to k mean that we want to continue the live of the session.

    The handler given by the user of the session (e.g. BslSession) assures that there is
    a continuation of error, called if an Ocaml exception (or even an Opa exception)
    is raised during the execution of the Opa handler.
    The action is to print the exception, and to continue to process to the live of the session,
    without killing it.
    This error continuation call the continuation given there, after the @> operator.
    However, if an Ocaml exception is raised without beeing caught by Opa, that means
    that a bypass is not correctly projected. In this case, we print an invitation
    to correct the code.
  *)
  let memo f st owner ronly =
    let state = ref st in
    fun msg sender k ->
      let ctx =
        match owner with
        | None -> sender
        | Some _ -> owner in
      (try
         f !state msg ctx @>
           function
             | Some stt ->
                 if not ronly then state := stt;
                 k true
             | None -> k false
       with
       | e ->
           U.error "CRITICAL" (
             "%a@\n"^^
             "This exception was not caught by Opa, and might soon kill this session in case of a scheduling@\n"^^
             "Hint: track the exception to where it was raised in the BSL and add the tag [raise] to the bypass"
           )
             pp_snapshot (Printexc.to_string e, msg, !state, ctx)
           ;
           k true
      )

  let push_error handlers =
    Option.iter (fun f -> Scheduler.push scheduler (fst f)) handlers

  let push_success handlers =
    Option.iter (fun f -> Scheduler.push scheduler (snd f)) handlers

  let finalize session =
    Queue.iter
      (fun (_, _, handlers) -> push_error handlers)
      session.messages;
    Queue.clear session.messages;
    Option.iter (fun f -> f ()) session.ondelete;
    session.killed <- true

  let isend session message context handlers =
    #<If$minlevel 150>
      U.debug "SESSION" "Send a message to session"
    #<End>;
    if session.killed then (
      push_error handlers
    ) else if session.ronly then (
      Scheduler.push scheduler (fun () ->
                push_success handlers;
                session.action message context @> function
                  | false -> finalize session
                  | true -> ()
             )
    ) else (
      #<If$minlevel 150>
        U.debug "SESSION" "Message added to queue"
      #<End>;
      Queue.add (message, context, handlers) session.messages;
      if session.lock then (
        #<If$minlevel 150>
          U.debug "SESSION" "Launch handling of message"
        #<End>;
        session.lock <- false;
        let rec process_message () =
          let queue = session.messages in
          if Queue.is_empty queue then
            session.lock <- true
          else
            let (msg, ctx, handlers) = Queue.pop session.messages in
            push_success handlers;
            session.action msg ctx @> function
              | false -> finalize session
              | true -> Scheduler.push scheduler process_message
        in
        process_message ()
      )
    )

  let send session message context =
    isend session message context None

  let send_then session message context herror hsuccess =
    isend session message context (Some (herror, hsuccess))


  (* Asynchronous interface *)
  let make_cps state context handler ondelete ronly =
    let session = {
      lock = true;
      action = memo handler state context ronly;
      messages = Queue.create ();
      ondelete = ondelete;
      owner_ctx = context;
      ronly = ronly;
      killed = false;
    } in Scheduler.finalise scheduler finalize session; session

  (* Synchronous inteface (use asynchronous interface) *)
  let make state context handler ondelete ronly =
    let handler state message context k =
      k (handler state message context)
    in
    make_cps state context handler ondelete ronly
end in (module Implem : M)
