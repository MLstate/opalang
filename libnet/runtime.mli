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
   Runtime layer to merge and start a set of RuntimeSig.COMPONENT on the same scheduler.
   @author Cedric Soulas
*)

val add_httpServer : string -> HttpServer.options -> unit
  (** Add an http server *)

val add_httpDialog : string -> HttpDialog.options -> unit
  (** Add an http dialog *)

val add_ftpServer : string -> FtpServer.options -> unit
  (** Add an ftp server *)

val add_smtpServer : string -> SmtpServer.options -> unit
  (** Add an smtp server *)

val add_watchdog : string -> Watchdog.options -> unit
  (** Add a watchdog *)

val start : unit -> unit
  (**
      Make all RuntimeSig.COMPONENT parsing the command line and start them on the same scheduler
      RuntimeMain.cmx/cmo can be linked (at the end) to make the native, instead of calling this start function.
  *)
