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
   Http dialog component
   @author Cedric Soulas
*)

(**
   This module reply to an http client request via the provided dialog function.
   This module is a RuntimeSig.COMPONENT.
*)

(** {6 Types and functions to comply with RuntimeSig.COMPONENT} *)

type t
  (** Internal type *)

type port = {
  set_dialog : t -> unit
}
    (**
       Port to be used by http server via the runtime layer.
       See [RuntimeType.Ports.t].
    *)


type dialog  = Scheduler.t -> HttpServerTypes.web_info -> unit

type options = {
  opt_allowed : Unix.inet_addr -> bool;
  dialog_name : string;
  dialog : unit -> dialog;
  opt_url_prefix : string;
}
    (**
       Type to define the http dialog.
       See [make].
    *)
val name : string
val version : string

val default_options : options
  (**
     Default options of the http dialog.
     [options_with_dialog] can be used to define a dialog.
     Default options are:
     opt_allowed always return true,
     dialog_name is "default",
     dialog is null,
     opt_url_prefix is empty.
  *)

val spec_args : string -> (options ServerArg.arg_parser) list
  (**
     Return the command line parser specification.
  *)

val make : string -> options -> Scheduler.t -> t
  (**
     [make name options sched] make a fresh [t] from provided name and options.
     The name is used to prefix the command line options.
  *)

val run :
  t ->
  Scheduler.t ->
  t
    (**
       Run the http dialog.
       It forces the evaluation of the provided dialog function.
    *)

val get_ports :
  t ->
  Scheduler.t ->
  (string * [> ]) list
  (**
     This function return an empty port declaration (the http dialog accepts no port),
    to comply with [RuntimeSig.COMPONENT].
  *)

val get_description :
  t ->
  Scheduler.t ->
  [> `HttpDialog of t]
    (**
       Return an http dialog description, to comply with [RuntimeSig.COMPONENT].
    *)

val close :
  t ->
  Scheduler.t ->
  unit
    (**
       This function does nothing, but it complies with [RuntimeSig.COMPONENT].
    *)

(** {6 Misc.} *)

val options_with_dialog : (unit -> dialog) -> options
  (**
     Return a [default_options] but with the provided dialog.
  *)

val body : t -> Scheduler.t -> HttpServerTypes.web_info -> unit
  (**
     Reply to an http client request via the provided [HttpServerTypes.web_info].
  *)

val content : t -> (string -> int -> int -> unit)
  (**
     Deprecated.
     It was used for upload progress information.
  *)

val is_allowed : t -> Scheduler.connection_info -> bool
  (**
     Deprecated.
     It was used to check if [Scheduler.connection_info] is allowed to receive a response.
  *)
