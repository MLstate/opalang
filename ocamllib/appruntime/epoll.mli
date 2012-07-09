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
   Bindings with Epoll

   @see "http://linux.die.net/man/2/epoll_ctl" or man pages of epoll
   @see "binding_epoll.c" for the c-implementation of external values
   @author Laurent Le Brun
   @author Cedric Soulas
   @author Mathieu Barbin (documentation and masking implementation with abstract types)
*)

(** {6 Error reporting} *)

(**
   Error management is complex dealing with a C binding because there is no exception in C.
   However, the C libraries often provides a function to get the last code of error (or directly
   accessing the [errno] variable), and a function to get a printable message from its code.

   We export these functions in this interface as deprecated functions, because they are potentially
   still used in existing code.

   But in ocaml, we prefer having an exception with the error message in the exception
   it self, using a exception constructor.

   In the implementation of functions of this module calling C functions,
   we check every returned code, and if an error occurs, we raise the exception [Error]
   with the current error code and its corresponding message.
*)

(** The abstract type of an error *)
type error

(** Get the C code corresponding to the error *)
val error_code : error -> int

(** Get a printable message corresponding to the error *)
val error_message : error -> string

(** The exception raised in case of error. Functions of this module which possibly raise this exceptions are taged *)
exception Error of error

(** Low-level : Return a printable message corresponding to the last error.
    @deprecated Please do not use it in new code *)
val last_error_message : unit -> string

(** Low-level : Get an int corresponding to the type of the last error.
    meaning of values : see c documentation of epoll
    @deprecated Please do not use it in new code *)
val last_error_code : unit -> int

(** {6 Epoll private descriptors} *)

(**
   An epoll descriptors is a structure for making epoll requests, related to [Unix.file_descr]
*)

(** The abstract type of epoll file descriptors *)
type epoll_descriptor

(**
   Open an epoll descriptor by requesting the kernel to allocate an event backing
   store dimensioned for [size] descriptors.

   The size is not the maximum size of the backing store but just a {b hint} to the kernel
   about how to dimension internal structures.

   The returned file descriptor will be used for all the subsequent calls to the epoll interface.

   The file descriptor returned by [create] must be closed by using [close].

   @raise Error if the c call failed.
*)
val create : int -> epoll_descriptor

(** Closing a [epoll_descriptor] previously created with [create]
    {b THIS FUNCTION IS YET IMPLEMENTED BUT NOT TESTED}
    @raise Error if the c call failed *)
val close : epoll_descriptor -> unit

(** {6 Epoll events} *)

(**
   Events are used to tell what kind of conditions should be waited by Epoll. (read access, write access, etc...)

   Events are represented by C-flags in C, and can be combined with a {b lor}.
   For more control about events to be passed to the C functions, the type events is abstract.

   Events are documented there : http://linux.die.net/man/2/epoll_ctl

   They are C MACRO, like {b EPOLLIN}, {b EPOLLOUT} and their value cannot be determined
   staticaly (too dangerous). So the events flags are build in C using the macro, and returned
   to caml, available as a value of the abstract type [events].

   Currently only 4 events are published in this interface. If you need more, please
   update the implementation carrefully.

   The interface events which are currently used, a function
   to combine them, and functions to know, given a combined events if a event is part of it.
*)

(** The abstract types of epoll events *)
type event_mask
type supported_event = In | Out | Hup | Err | Unsupported of event_mask
type event
val combine : event list -> event_mask

(** This event is used if you want to discard every other events (using [Epoll.modif]) but
    if you does not want to disard the [file_descr] from the [fd] handled by the [epoll_descriptor] *)
(* It should be forbiden *)
(* val event_empty : events *)

val event_mask_to_list : event_mask -> supported_event list


(** {6 Epoll requests} *)

(**
   Functions [add], [del], [modif] are based on the function [epoll_ctl(2)] of the epoll library.

   Theses functions control an [epoll_descriptor], by requesting that the corresponding operations
   (add, modif, del) be performed on the target [Unix.file_descr].

   The [events] describes the kind of events which should be catched by epoll, occuring to the [Unix.file_descr].

   Hack : weblib is also used with [dummy_connection] which are recognized by the fact that the associated
   [Unix.file_descr] has a negative value. In this case, the functions [add], [del] and [modif] ignore their
   arguments (nothin is done, not any c-call).

   @see "http://linux.die.net/man/2/epoll_ctl" for the documentation of [epoll_ctl(2)]
*)

(** Tell an [epoll_descriptor] that it {b should no more handle} any events occuring to a [Unix.file_descr].
    This call remove the [file_descr] from the descr handled by the [epoll_descriptor].
    It means that both In an Out filters are removed
    Error cases may be related to the following problems :
    + [fd] is not handled by [epoll_descriptor].
    + [fd] is not a valid file descriptor, etc...
    @raise Error if the c call failed. *)
val del : epoll_descriptor -> Unix.file_descr -> unit

(**
   Tell an [epoll_descriptor] to listen both In and Out events ono a [Unix.file_descr]
   By default, it assumes that the file descriptor has already been registered
*)
val listen_in_out : epoll_descriptor -> ?is_new_fd:bool -> Unix.file_descr -> unit

(**
   Tell an [epoll_descriptor] to listen only In event on a [Unix.file_descr]
   If the Out event was filtered, I will now be ignored
*)
val listen_in_only : epoll_descriptor -> bool -> Unix.file_descr -> unit

(**
   Tell an [epoll_descriptor] to listen only Out event on a [Unix.file_descr]
   If the In event was filtered, I will now be ignored
*)
val listen_out_only : epoll_descriptor -> bool -> Unix.file_descr -> unit

(**
   [wait ?tout epd maxevents ]
   Wait for events handled by the epoll descriptor [epd] for a maximum time of [tout] milliseconds.
   The returned array will contain the events that will be available for the caller.
   Up to [maxevents] are returned by wait. The maxevents parameter must be greater than zero.

   Specifying a timeout of [-1] makes [epoll_wait(2)] wait indefinitely,
   while specifying a timeout equal to [0] makes [epoll_wait(2)] to return immediately even
   if no events are available.
*)
val wait : ?tout:int -> epoll_descriptor -> int -> (Unix.file_descr * supported_event list) array

(** {6 Debug} *)

(**
   For printing [epoll_descriptors], [events], or [Unix.file_descr], we provide functions
   to access the int hidden by the implementation.
   Do not use it in standard code, only for unit tests
*)
module Debug :
sig
  val int_of_filedescr : Unix.file_descr -> int
  val filedescr_of_int : int -> Unix.file_descr
  val int_of_events : event_mask -> int
  val int_of_epoll_descriptor : epoll_descriptor -> int
  (* val test : unit -> unit *)
end

