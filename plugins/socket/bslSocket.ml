(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under
    the terms of the GNU Affero General Public License, version 3, as
    published by the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT
    ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public
    License for more details.

    You should have received a copy of the GNU Affero General Public
    License along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

module C = QmlCpsServerLib
open C.Ops

##property [mli]
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation
##extern-type SSL.secure_type = SslAS.secure_type
##property [endmli]

##extern-type Socket.connection = Scheduler.connection_info

let private_connect ?(secure_mode = Network.Unsecured) (addr: string) port
                    ?err_cont (cont: Scheduler.connection_info -> unit) =
  let inet_addr = Network.inet_addr_of_name addr in
  let port_spec = Network.make_port_spec ~socket_type:Network.TCP
                                         ~protocol:"raw" inet_addr port in
  Network.connect Scheduler.default port_spec secure_mode ?err_cont cont

##register [cps-bypass] connect: string, int,\
                                 continuation(Socket.connection) -> void
let connect addr port cont = private_connect addr port (fun x -> x |> cont)

##register [cps-bypass] connect_with_err_cont2: string, int,\
                                 continuation(outcome(Socket.connection,string)) -> void
let connect_with_err_cont2 addr port cont =
  let inet_addr = Network.inet_addr_of_name addr in
  let port_spec = Network.make_port_spec ~socket_type:Network.TCP ~protocol:"raw" inet_addr port in
  Network.connect Scheduler.default port_spec Network.Unsecured
                  ~err_cont:(fun exn -> BslUtils.create_outcome (`failure (Printexc.to_string exn)) |> cont)
                  (fun conn -> BslUtils.create_outcome (`success conn) |> cont)

(* Patch: for simplicity we turn the exception into a string.  Note that it's
 * not a real continuation, you should exit the program in the err_cont.
 *)
##register [cps-bypass] connect_with_err_cont: string, int,\
                                 continuation(string), continuation(Socket.connection) -> void
let connect_with_err_cont addr port err_cont cont =
  private_connect addr port ~err_cont:(fun exn -> Printexc.to_string exn |> err_cont) (fun x -> x |> cont)




##register [cps-bypass] secure_connect: string, int, SSL.secure_type,\
                                        continuation(Socket.connection) -> void
let secure_connect addr port secure_type cont =
  private_connect ~secure_mode:(Network.Secured secure_type) addr port
                  (fun x -> x |> cont)

##register close: Socket.connection -> void
let close connection_info =
  Scheduler.remove_connection Scheduler.default connection_info

##register [cps-bypass] write: Socket.connection, string,\
                               continuation(int) -> void
let write connection_info data k =
  Scheduler.write Scheduler.default connection_info data (fun i -> i |> k)

##register [cps-bypass] write_len: Socket.connection, string, int,\
                               continuation(int) -> void
let write_len connection_info data len k =
  Scheduler.write Scheduler.default connection_info data ~len (fun i -> i |> k)

##register [cps-bypass] read : Socket.connection, continuation(string) -> void
let read connection_info k =
  Scheduler.read Scheduler.default connection_info (fun (_, str) -> str |> k)
