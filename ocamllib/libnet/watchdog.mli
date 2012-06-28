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
  opt_freq:float;
}

type t = {
  memory_limit : float;
  fd_limit: int;
  freq:Time.t;
}

val name : string

val version : string

val memory_consumption : unit -> float

val fd_consumption : Scheduler.t -> int

val make : string -> options -> Scheduler.t -> t

val run : t -> Scheduler.t -> t

val close : t -> Scheduler.t -> unit

val get_description : t -> Scheduler.t -> RuntimeType.Description.t

val get_ports : t -> Scheduler.t -> RuntimeType.Ports.t

val spec_args : string -> options ServerArg.arg_parser list

val default_options : options
