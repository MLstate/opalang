(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
