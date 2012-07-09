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
   Ports and Description module for the Runtime layer.
   @author Cedric Soulas
*)

(**
    Ports module to store ports of RuntimeSig.COMPONENT
    @inline doc
*)
module Ports :
sig

  type t =
      (string *
         [ `Connection of Network.port
         | `Http_dialog of Http_dialog.port
         | `HttpDialog of HttpDialog.port
         | `Logger
         | `None
         ]) list

  val add : Scheduler.t -> t -> unit
    (**
       [add sched l] add a list of named ports.
    *)

  val init : Scheduler.t ->  unit
    (**
       Initialize all added port trying to find the appropriate description in Description module
       (see [Description.add]) and using, if existing, the [X.port] (see [t]).
    *)
end

(**
    Description module to store descriptions of RuntimeSig.COMPONENT,
    ready to be used as Port
   @inline doc
*)
module Description :
sig

  type t =
      [
      | `Connection
      | `Http_dialog of Http_dialog.t
      | `HttpDialog of HttpDialog.t
      | `Logger
      | `HttpServer
      | `FtpServer
      | `SmtpServer
      | `Watchdog
      ]

  val add : string -> t -> unit
    (**
       [add name port] Add a new named [Description.t], ready to be used, in a Port.
    *)
end
