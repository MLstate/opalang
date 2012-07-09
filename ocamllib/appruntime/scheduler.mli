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

exception Syscall (* temporary, could probably be removed for new scheduler *)

(**
   Module to manage asynchronous operations.

   @author Henri Binsztok
   @author Laurent Le Brun
   @author Frederic Ye
   @author Cedric Soulas
   @author Hugo Venturini
*)

(**
   This module provides functions to register
   - asynchronous operations : [listen], [read] and [write] over sockets and file descriptors.
   - asynchronous calculations : [sleep], [timer] and [yield].

   The scheduler is asynchronous and non-preemptive {e i.e.} operations and calculation are registers
   for sometime later in the future and they cannot interrupt one another.
   The general mechanism is to register operations ([read], [write], [listen]) and calculation
   ([sleep], [timer], [yield]) and to call for their execution ([wait]).
*)

(** {6 Types} *)

type t
(** The type of schedulers *)

type connection_info = { (* Temporarily exported for debug *)
  addr    : NetAddr.t; (* the connection type *)
  conn_id : int        (* the unique id of the connection *)
}
(** The type of connections *)

type async_key
(** The type of a key associated to an asynchronous job. See [listen] and [sleep]. *)

(** {6 Exceptions} *)

exception Timeout
  (** Raised when a timeout expires. *)

exception Connection_closed
  (**  Raised when trying to connect through a closed connection. *)

exception Busy_direction
  (**  Raised when another read event is already waiting for data on that connection *)

exception StopTimer
  (** Raised to stop the [timer] *)

exception Empty
  (** Raised when there is nothing queued in the scheduler *)

exception Unbound_key
  (** Raised when an key of type [async_key] is unbound *)

(** {6 Control of the scheduler} *)

val make :  ?is_server:bool -> unit -> t
(** @return a fresh [Scheduler.t]
    @param is_server (optional argument) Setting this parameter to true adds a server startup message. Default: [false]
*)

val default : t
(** A default scheduler *)

val wait : t -> block:bool -> bool
(** [wait sched block] Performs the following action in that order:
    - Executes functions corresponding to operations which reached a deadline.
    - Execute the next pending operation.

    @param block Boolean stating whether the waiting blocks or not
    {e i.e.} if [block] then it waits until any incoming operation,
    otherwise it returns right after the end of execution.

    @return [true] if there still are any pending events or any opened connections, [false] otherwise.
*)

val is_empty : t -> bool
(** @return [true] if and only if there is nothing queued in the scheduler, {e e.g.} no connections
    open and no events or timeouts pending. This method is needed since [wait]
    processes one event and may return [false] whether something was done or
    not. *)

(** {6 Manipulation of connections} *)

val make_connection :  t -> ?register:bool -> NetAddr.t -> connection_info
(** @return a fresh connection.
    @param register States whether the new connection should be registered in the scheduler provided or not. Default: [true]
*)

val remove_connection :
  t -> connection_info -> unit
(**
   Removes the given connection.
*)

val check_connection :
  t -> connection_info -> bool
(**
   @return [true] if the given connection is registered in the given scheduler, [false] otherwise.
*)

val get_connection_addr: connection_info -> NetAddr.t
(**
   @return the address of the given connection.
*)

val get_connection_inet_addr : connection_info -> Unix.inet_addr

val get_connection_fd : connection_info -> Unix.file_descr

val nb_of_connection : t -> int
(**
   @return the number of registered connections in the given scheduler
*)

val get_connection_secured_from_normal : connection_info -> Ssl.socket -> connection_info
(**
   Creates a secured connection from a given connection and a given socket.
*)

(** {6 Asynchronous operation over [connection_info]} *)

(**
   An operation is register in a given scheduler and is set over a connection. It must
   provide a continuation, among other parameters specific to each operation e.g. a string to write.

   A {b continuation} is a function which returns [unit] and which will be executed when
   the corresponding operation is triggered.

   An continuation to handle errors can be provide. If an error happen, this error continuation will be used *instead* of the normal continuation.

   Beside [listen], any operation can be registered with a timeout and/or an error continuation.
   This allows you a fine-grained handling of continuations over time. But {b with great power comes
   great responsabilities}. The policy is the following: if you don't provide any error continuation,
   we'll handle the connection you provided at registration, fine. But if you decide to provide an error
   continuation, you will have to explicitly remove the connection (see [remove_connection] above).
*)

val listen :
  t ->
  connection_info ->
  ?timeout:Time.t ->
  ?err_cont:(exn -> unit) ->
  (unit -> unit)
  -> async_key
(**
    Listen for events onto the given connection.
   @return the associated key that can be used with [abort].
*)

val listen_once :
  t ->
  connection_info ->
  ?timeout:Time.t ->
  ?err_cont:(exn -> unit) ->
  (unit -> unit)
  -> unit
(**
    Listen for one event coming on a given connection.
*)

val connect :
  t ->
  connection_info ->
  ?timeout:Time.t ->
  ?err_cont:(exn -> unit) ->
  (unit -> unit)
  -> unit
(**
    Connect on a given connection.
*)

val read_more :
  t ->
  connection_info ->
  ?read_max:int ->
  ?block_size:int ->
  ?timeout:Time.t ->
  FBuffer.t ->
  ?size_max:int ->
  ?err_cont:(exn -> unit) ->
  (int * FBuffer.t -> unit) -> unit
(** Reads more data over a [connection_info], appending those new data in the providing buffer
    @raise Busy_direction exception is raised when another callback is already waiting
    for datas on that [connection_info] *)

val read_content :
  t ->
  connection_info ->
  ?read_max:int ->
  ?block_size:int ->
  ?timeout:Time.t ->
  Rcontent.content ->
  ?size_max:int ->
  ?err_cont:(exn -> unit) ->
  (int * Rcontent.content -> unit) -> unit

val read_more2 :
  t ->
  connection_info ->
  ?read_max:int ->
  ?timeout:Time.t ->
  Buffer.t ->
  ?size_max:int ->
  ?err_cont:(exn -> unit) ->
  (int * Buffer.t -> unit) -> unit

val read_more4 :
  t ->
  connection_info ->
  ?read_max:int ->
  ?timeout:Time.t ->
  Buf.t ->
  ?size_max:int ->
  ?err_cont:(exn -> unit) ->
  (int * Buf.t -> unit) -> unit

val read :
  t ->
  connection_info ->
  ?timeout:Time.t ->
  ?err_cont:(exn -> unit) ->
  (int * string -> unit) -> unit
(** Reads over a [connection_info], a maximum of 4096 characters.
    @raise Busy_direction exception is raised when another callback is already waiting
    for datas on that [connection_info] *)

val read_from :
  t ->
  connection_info ->
  ?timeout:Time.t ->
  ?err_cont:(exn -> unit) ->
  (int * Unix.sockaddr * string -> unit) -> unit
(** Reads over a [connection_info], a maximum of 4096 characters. The difference with
    [read] is that the callback is additionally informed of the address of the sender
    of the message. This is useful for un-connected UDP communication.
    @raise Busy_direction exception is raised when another callback is already waiting
    for data on that [connection_info] *)

val read_until :
  t ->
  connection_info ->
  (int * string -> bool) ->
  ?block_size:int ->
  ?timeout:Time.t ->
  ?err_cont:(exn -> unit) ->
  (int * string -> unit) -> unit
(** [read_until sched conn read_cond cont] reads over a [connection_info],
    until read_cond returns true from the provided couple (number of characters, data) read.
    It can be used, for example, to read a minimum number of characters or for your callback
    to be called only if the data ends with certain characters.
    @raise Busy_direction exception is raised when another callback is already waiting
    for datas on that [connection_info] *)

val read_min :
  t ->
  connection_info ->
  int ->
  ?block_size:int ->
  ?timeout:Time.t ->
  ?err_cont:(exn -> unit) ->
  (int * string -> unit) -> unit
(** [read_min sched conn read_min cont] reads over a [connection_info],
    a minimum of [read_min] characters.
    @raise Busy_direction exception is raised when another callback is already waiting
    for datas on that [connection_info] *)

val read_lines :
  t ->
  connection_info ->
  ?block_size:int ->
  ?timeout:Time.t ->
  ?err_cont:(exn -> unit) ->
  (int * string -> unit) -> unit
(** [read_lines sched conn cont] reads over a [connection_info],
    and call you callback only if last characters are "\r\n",
    it means only when the data is a line or a set of lines.
    @raise Busy_direction exception is raised when another callback is already waiting
    for datas on that [connection_info] *)

val read_all :
  t ->
  connection_info ->
  ?read_max:int option->
  ?block_size:int ->
  ?timeout:Time.t ->
  ?buf:FBuffer.t ->
  ?size_max:int ->
  ?err_cont:(exn -> unit) ->
  (int * FBuffer.t -> unit) -> unit
(** Reads until the connection is closed.
    It means your callback will be called only at the end of the connection.
    See [read] for further information.
    @raise Connection_closed exception is raised when the connection on which it was reading is closed.
*)

val write :
  t ->
  connection_info ->
  ?block_size:int ->
  ?timeout:Time.t ->
  string ->
  ?len:int ->
  ?err_cont:(exn -> unit) ->
  (int -> unit) -> unit
(** Writes over a [connection_info]
    Several writes on the same connection are permited. In this case,
    datas will be written in the same order as the [write] declarations.
*)

val write_to :
  t ->
  connection_info ->
  Unix.sockaddr ->
  ?block_size:int ->
  ?timeout:Time.t ->
  string ->
  ?err_cont:(exn -> unit) ->
  (int -> unit) -> unit
(** Writes over a [connection_info]
    Several writes on the same connection are permited. In this case,
    datas will be written in the same order as the [write] declarations.
*)

(** {6 Asynchronous calculation} *)

val sleep : t -> Time.t -> (unit -> unit) -> async_key
  (**
      [sleep sched x f] schedules the execution of [f] in time interval [x] in [sched]
      @return the associated key that can be used with [abort].
  *)

val abort : t -> async_key -> unit
  (**
     [abort sched key] abort the pending job associated the [key].
     @raise Unbound_key if the key is unbound
  *)

val timer : t -> Time.t -> (unit -> unit) -> unit
  (** [timer sched x f] schedules the execution of [f] every time interval [x] in [sched].

      If you don't define an explicit stop to the timer, it will be call forever (and ever).
      Although you might have excellent reasons not to stop the timer, feel free to copy the
      following PATTERN:
{v
          let f () =
             if <condition> then <do_somthing>
             else raise Scheduler.StopTimer;
          in Scheduler.timer sched x f;

v}
  *)


val push : t -> (unit -> unit) -> unit
  (** [push sched f] Push in [sched] the task [f]. *)

val at_exit : t ->  (unit -> unit) -> unit
(** [at_exit sched f] Push in [sched] the task [f] to be done at the end of the program. *)

val flush : ?f:(unit -> unit) -> t -> unit
  (** Execute all asynchronous calculation and operation. Optionally given [f] will be evaluated after
      performing any scheduled operation. *)

val loop_until : t -> (unit -> bool) -> unit
  (** [flush_until sched cond] Execute all asynchronous calculation and operation until [cond ()] is true
      @raise Empty if cond not satisfied and there is nothing queued in the scheduler.
      @raise Reentrant_routine if another [wait], [flush] or [flush_until] is yet in progress.
  *)

val finalise : t -> ('a -> unit) -> 'a -> unit
  (**
     [finalise sched f v] registers f as a finalisation function for v.
     It is permited, and only with this finalisation function, to use the
     scheduler inside the finalisation function.
     WARNING: contrary to [Gc.finalise], the order of calls
     to finalisation functions is not guarantee.
  *)

val run : t -> unit
  (** Like [flush] but catch all exn *)

val set_max_compute_successive : int -> unit
  (** Set the maximum of compute the scheduler can do before scheduling *)
