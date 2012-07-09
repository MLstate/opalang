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

(**
   The low level module for [Scheduler] and [SchedulerJob].
   This module register and process jobs depending on special events or conditions.

   @author Cedric Soulas
*)

(**
   This module handle IO operations.
   An IO operation is associated to a [id] (a file descriptor) and a [direction] ([In] to read, and [Out] to write).
   Several operations can be queued for the same ([id], [direction]) couple.
   The first operation, ready to be processed, is called a candidate.
   A poll system (called with [wait]) is used to retrieve wich
   ([id], [direction]) are ready to be processed (with [process], [process_id] or [process_all]).
   @inline doc
*)
module Operation :
sig

  type t
  type key = int
  type id = Unix.file_descr
  type direction = In | Out

  exception Busy_direction
    (**
       This exception is not used by the module itself, but can be used
       to restrict operation queuing
    *)
  exception Not_found of (id * direction)
    (**
       Raised when ([id], [direction]) is not found
    *)

  exception Unbound_key of key
  exception Existent_key of key

  val make : unit -> t
    (**
       @return a fresh [t].
    *)

  (** {6 Test existence} *)

  val length : t -> int
    (**
        @return the number of queue associated to a ([id], [direction]) couple.
    *)
  val is_empty : t -> bool
    (**
       [is_empty o] Is equivalent to [length o = 0]
    *)
  val mem : t -> id -> direction -> bool
    (**
       @return true if a queue exist for the ([id], [direction]) couple.
    *)

  val mem_key : t -> key -> bool
    (**
       @return true if an operation is associated to [key].
    *)

  (** {6 Add / remove operation} *)

  val add : t -> id -> direction -> key -> (unit -> unit) -> (exn -> unit) -> unit
    (**
       [add o id d key callback callback_error] add a new operation for the [id] on the direction [d].
       If another operation exists on that id and direction, the [callback] is queued,
       otherwise, this operation is candidate for processing.
       See [remove] for more information.
       Raise [Existent_key] is [key] already exists
    *)

  val replace : t -> key -> (unit -> unit) -> (exn -> unit) -> unit
    (**
       [replace o key callback callback_error] replace the current callback
       of the operation associated to [key] with a new [callback], for the [id] on direction [d].
       Raise an [Unbound_key] if the [key] is unbound

    *)
  val remove_id : t -> id -> unit
    (**
       [remove_id o id] remove all operations associated to the id [id].
    *)

  val remove : t -> key -> unit
    (**
       [remove o key] remove the operation associated to [key].
    *)

  val wait :
    t -> int ->
    (id * Epoll.supported_event list) array
      (**
         [wait o t] wait for a maximum of [t] milliseconds.
         @return a maximum of 1000 ([id], [Epoll.supported_event list]) representing
         operations ready to be processed.
         See [process_all] for more information.
      *)

  (** {6 Process operations} *)

  val process : t -> id -> direction -> unit
    (**
       [process o id d] process the current candidate operation ([id], [d]).
       It *doesn't* remove this candidate from the queue.
       Raise Operation_not_found (id, d) if no operation is candidate.
    *)
  val process_error : t -> id -> direction -> exn -> unit
    (**
       [process_error o id d] process the error callback of the current candidate ([id], [d]).
       It *doesn't* remove this candidate from the queue.
       Raise Operation_not_found (id, d) if no operation is candidate.
    *)
  val process_id_error : t -> id -> exn -> unit
    (**
       [process_id_error o id e], process in both direction, if it exists,
       process the error callback, with [e] argument, of the first candidate of the queue.
       It *doesn't* remove those candidate from the queue.
       This function use [process_error].
    *)
  val process_all : t -> (id * Epoll.supported_event list) array -> id list
    (**
       [process_all o a] process the array [a] of ([id], [Epoll.supported_event list]).
       The [Epoll.supported_event list] describes the list of events ready to be processed for the id [id].
       @return the list of [id] associated to an error like connection hang up or closed, as describe in the [Epoll.supported_event list].
       This function use [process].
    *)

  (** {6 Misc.} *)
  val direction_to_string : direction -> string
end

(**
   This module handle callbacks to be processed after a certain amout of time.
   "After" means after, not necessarily exactly on timeout.
   Each callback registered is associated to a [key] (see [add]).
   The couple (callback, key) is called a job.
   The most priority job can be processed with the [process] function.
   @inline doc
*)
module Priority :
sig

  exception Timeout
    (**
       This exception is not used by the module itself,
       but can be used on timeout event.
    *)
  type t
  type key = int

  exception Existent_key of key

  val make : unit -> t
    (**
       @return a fresh [t].
    *)

  (** {6 Test existence} *)

  val length : t -> int
    (**
       @return the number of jobs yet to be processed.
    *)
  val is_empty : t -> bool
    (**
       @return true if no jobs are registered, false otherwise.
       [is_empty p] is equivalent to [length p = 0].
    *)

  (** {6 Add / remove jobs} *)

  val add : t -> key -> Time.t -> (unit -> unit) -> unit
    (**
       [add p key t callback] add a callback, associated to a [key], to be executed throught the [process] function after time intrval [t]
       See [execute] for more information.
       Raise an [Existent_key] if the [key] already exists.
    *)
  val mem : t -> key -> bool
    (**
       [mem p key] return [true] if the job idenfied by the [key] exists.
    *)
  val remove : t -> key -> unit
    (**
       [remove p key] remove the job idenfied by the key [key].
       It does nothing if [key] is not bound in [p].
       See [add] for more information.
    *)

  val clear : t -> unit
   (**
       [clear p ] remove all jobs
    *)

  (** {6 Process a job} *)

  val process : t -> int
    (**
       Process and *remove* the next priority job. A maximum of *one* job is processed.
       The next priority job is dependent of timeouts set for the jobs.
       @return 0 if one or several other jobs have reach their tiemouts and are ready to be processed,
       -1 if no job was processed,
       the timeout (in milliseconds) of the most priority job otherwise.
    *)

end

(**
   This module handle a set of file descriptors.
   @inline doc
*)
module Descriptor :
sig
  type t
  type id = Unix.file_descr
  type key = int
  type mem_response = Alive | Replaced | Closed
      (**
         See [mem] for more information
      *)

  val make : unit -> t

 (** {6 Test existence} *)

  val length : t -> int
    (**
       @return the number of [Alive] descriptors.
       See [mem] for more information.
    *)
  val is_empty : t -> bool
    (**
       [is_empty d] is equivalent to [length d = 0]
    *)
  val mem : t -> id -> key -> mem_response
    (**
       @return a [mem_response] for a given ([id], [key]).
       Alive means the [key] for that [id] is still present.
       Replaced means [id] exist, but is not associated to [key] anymore.
       Closed means [id] doesn't exist anymore.
    *)

  (** {6 Add / remove descriptors} *)

  val add : t -> id -> key
    (**
       [add d key] generate and store a new [key] for the [id].
       If a key already exists for that id,
       that key is removed without warning.
       @return the generated key.
    *)
  val remove : t -> id -> unit
    (**
       [remove d id] remove the key associated to [id].
    *)
end

(**
    This module handle a set of task to execute. Is implemented with a
    very simple structure and it is ideal for simple operation.
    @inline doc
*)
module Compute :
sig

  type t

  (** Create an empty scheduler. *)
  val make : unit -> t

  (** {6 Test existence} *)

  (** Returns the number of task into a scheduler. *)
  val length : t -> int

  (** Test if the scheduler is empty *)
  val is_empty : t -> bool

  (** {6 Push tasks}*)

  (** Push one jobs to execute into a scheduler*)
  val push : t -> (unit -> unit) -> unit

  (** Process some jobs was pushed on a scheduler. *)
  val process : t -> unit

  (** Remove all jobs of a scheduler. *)
  val clear : t -> unit

  (** Transfer all jobs from a scheduler to another *)
  val rev_transfer : src:t -> dest:t -> unit

  val set_max_successive : int -> unit
  (** Set the maximum of compute the scheduler can do before scheduling *)

end

(**
   It is not permited to use [Gc.finalise f v] directly because [f] can
   occures at any time.
   This module store those finalise callback, to be processed at a safe
   moment.
   @inline doc
*)
module Finalise :
sig
  type t

  val make : unit -> t
    (**
       @return a fresh [t]
    *)
  val length : t -> int
    (**
       @return the number of finalisation function ready
       to be processed
    *)
  val is_empty : t -> bool
    (**
       [is_empty f] is equivalent to [length f = 0]
    *)
  val add : t -> ('a -> unit) -> 'a -> unit
    (**
       [add f v] registers [f] as a finalisation function for [v].
       Waiting finalisation functions are executed with [process_all].
    *)
  val process_all : t -> unit
    (**
       Process all waiting finalisation functions.
       WARNING: contrary to Gc.finalise, the order of calls
       to finalisation functions is not guarantee.
    *)
end

(**
   This module handle the number of reentrant routines
   and the number of synchronous computations authorized to
   guarantee a good qualities of service.
   It also manage a set of unique key.
   @inline doc
*)
module Counter :
sig

  type t

  exception Sync_limit

  val make : unit -> t
    (** Return a fresh counter of type [t] *)


  (** {6 Key getter} *)

  val get_next_int : t -> int
    (**
       Return an int and increase the counter.
       [get_key] and [get_next_key] never return the same key.
       You can only use this key to store data in a structure that hash the key,
       like hash tables.
       Be aware that when max_int is reached, the counter will restart from min_int.
    *)

  val get_key : t -> int
    (**
        Return an unused int to be used as a key.
        You have to release the key with [release_key]
        as soon as you can because the number of those keys is limited.
        If the limit is reach, the number of key is automaticaly doubled.
        This function is usefull to store callbacks in an resizable array.
    *)

  val release_key : t -> int -> unit
    (** Release a previous used key *)

  val incr_level : t -> unit
    (**
       Increment the reentrant level counter.
       Raise [Reentrant_routine] if already incremented.
    *)

  val decr_level : t -> unit
    (**
       Decrement the reentrant level counter.
    *)


  val incr_sync : t -> unit
    (**
       Increment the synchronous counter.
       Raise [Sync_limit] if, according to the counter value, the computation have to be asynchronous.
    *)

  val decr_sync : t -> unit
    (**
       Decrement the synchronous counter.
    *)

end
