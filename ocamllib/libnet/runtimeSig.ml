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
   Module type COMPONENT accepted by the Runtime layer.
   Runtime merge several COMPONENT in one APPLICATION.
   @author Cedric Soulas
*)

module type COMPONENT =
sig
  type options
  type t

  val name : string
  val version : string

  val default_options : options
    (** Default options *)

  val spec_args : string -> (options ServerArg.arg_parser) list
   (**
      [spec_args name] return the command line parser specification for type options.
      The name can be used to prefix the command line to avoid
      command line conflicts when adding several identical component.
      Example: [HttpServerOptions.spec_args].
   *)

  val make : string -> options -> Scheduler.t -> t
    (**
       [make name opt sched] make a named component of type [t]
       from a set of options [opt].
       See [run].
    *)

  val get_ports : t -> Scheduler.t -> RuntimeType.Ports.t
    (**
       Return the list of ports needed by the component.
       See [RuntimeType.Ports].
    *)

  val get_description : t -> Scheduler.t -> RuntimeType.Description.t
    (**
       Return the description of the component, to be used as
       a port by another component.
       See [RuntimeType.Description]
    *)

  val run : t -> Scheduler.t -> t
    (**
       Run the component of type [t].
       This function is called when all components have been made
       via their [make] function.
    *)

  val close : t -> Scheduler.t -> unit
    (**
       Close the component.
       This function is called at the end of the program.
    *)
end

(**
   Module type APPLICATION produced by the Runtime layer from a set of COMPONENT.
*)
module type APPLICATION =
sig
  type options
  type t

  val names : string list
  val versions : string StringMap.t

  val get_options : unit -> options
  val make : options -> Scheduler.t -> t
    (**
       Make the application.
       It calls on all components of the application:
       [COMPONENT.make], [COMPONENT.get_ports] and [COMPONENT.get_description].
       Ports and descriptions are added in [RuntimeType.Ports] and [RuntimeType.Description].
       See [Runtime.make] implementation.
    **)

  val run : t -> Scheduler.t -> t
    (**
       Run all components.
       This function is called by Runtime when all components have been made
       via their [make] function.
    *)

  val close : t -> Scheduler.t -> unit
    (**
       Close all components at the end of the program.
    *)

end
