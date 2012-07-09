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
