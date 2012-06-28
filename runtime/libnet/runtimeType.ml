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

(**
   Ports and Description module for the Runtime layer.
   @author Cedric Soulas
*)

module rec Ports :
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

  val init : Scheduler.t ->  unit

end =
struct

  type t =
   (string *
       [ `Connection of Network.port
       | `Http_dialog of Http_dialog.port
       | `HttpDialog of HttpDialog.port
       | `Logger
       | `None
       ]) list

  let ports = ref []

  let init_port sched (name, port) =
    match port with
    | `Connection c ->
        let module N = Network in
        let abort_listen = Network.listen sched c.N.port_spec c.N.secure_mode c.N.conn_incoming in
        let _ = abort_listen in
        ()
    | `Http_dialog hd ->
        let e = Description.get name in
        let dialog = match e with
          | `Http_dialog dialog -> dialog
          | _ -> assert false
        in
        hd.Http_dialog.set_dialog dialog
    | `HttpDialog hd ->
        let e = Description.get name in
        let dialog = match e with
          | `HttpDialog dialog -> dialog
          | _ -> assert false
        in
        hd.HttpDialog.set_dialog dialog
    | `Logger ->()
    | `None -> ()
    | _ -> assert false

  let add _sched l =
    ports := l@(!ports)

  let init sched =
    List.iter (init_port sched) !ports

end
and Description :
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

  val get : string -> t

  val add : string -> t -> unit

end =
struct

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

  exception Not_found
  let (output: (string, t) Hashtbl.t) = Hashtbl.create 5

  let add k e =
    Hashtbl.add output k e

  let get k =
    if Hashtbl.mem output k then
      Hashtbl.find output k
    else begin
      Logger.error "Unbound port '%s'" k;
      raise Not_found
    end
end
