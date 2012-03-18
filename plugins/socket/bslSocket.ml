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
open OpabslgenMLRuntime

##opa-type outcome('a, 'b)

##property [mli]
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation
##extern-type SSL.secure_type = SslAS.secure_type
##property [endmli]

##extern-type Socket.connection = Scheduler.connection_info

let create_outcome outcome k =
  QmlCpsServerLib.return k (wrap_opa_outcome (BslUtils.unwrap_opa_outcome (BslUtils.create_outcome outcome)))

let private_connect ?(secure_mode = Network.Unsecured) (addr: string) port
                    (cont: Scheduler.connection_info -> unit) =
  let inet_addr = Network.inet_addr_of_name addr in
  let port_spec = Network.make_port_spec ~socket_type:Network.TCP
                                         ~protocol:"raw" inet_addr port in
  Network.connect Scheduler.default port_spec secure_mode cont

##register [cps-bypass] connect: string, int,\
                                 continuation(Socket.connection) -> void
let connect addr port cont = private_connect addr port (fun x -> x |> cont)

##register [cps-bypass] secure_connect: string, int, SSL.secure_type,\
                                        continuation(Socket.connection) -> void
let secure_connect addr port secure_type cont =
  private_connect ~secure_mode:(Network.Secured secure_type) addr port
                  (fun x -> x |> cont)

let private_connect_with_err_cont ?(secure_mode = Network.Unsecured) addr port cont =
  let inet_addr = Network.inet_addr_of_name addr in
  let port_spec = Network.make_port_spec ~socket_type:Network.TCP ~protocol:"raw" inet_addr port in
  Network.connect Scheduler.default port_spec secure_mode
                  ~err_cont:(fun exn -> create_outcome (`failure (Printexc.to_string exn)) cont)
                  (fun conn -> create_outcome (`success conn) cont)

##register [cps-bypass] connect_with_err_cont: string, int,\
                                 continuation(outcome(Socket.connection,string)) -> void
let connect_with_err_cont addr port cont = private_connect_with_err_cont addr port cont

##register [cps-bypass] secure_connect_with_err_cont: string, int, SSL.secure_type,\
                                 continuation(outcome(Socket.connection,string)) -> void
let secure_connect_with_err_cont addr port secure_type cont =
  private_connect_with_err_cont ~secure_mode:(Network.Secured secure_type) addr port cont

##register close: Socket.connection -> void
let close connection_info =
  Scheduler.remove_connection Scheduler.default connection_info

##register [cps-bypass] write: Socket.connection, string,\
                               continuation(int) -> void
let write connection_info data k =
  Scheduler.write Scheduler.default connection_info data (fun i -> i |> k)

##register [cps-bypass] write_with_err_cont: Socket.connection, int, string,\
                               continuation(outcome(int,string)) -> void
let write_with_err_cont connection_info timeout data cont =
  Scheduler.write Scheduler.default connection_info ~timeout:(Time.milliseconds timeout) data
                  ~err_cont:(fun exn -> create_outcome (`failure (Printexc.to_string exn)) cont)
                  (fun cnt -> create_outcome (`success cnt) cont)

##register [cps-bypass] write_len: Socket.connection, string, int,\
                               continuation(int) -> void
let write_len connection_info data len k =
  Scheduler.write Scheduler.default connection_info data ~len (fun i -> i |> k)

##register [cps-bypass] write_len_with_err_cont: Socket.connection, int, string, int,\
                               continuation(outcome(int,string)) -> void
let write_len_with_err_cont connection_info timeout data len cont =
  Scheduler.write Scheduler.default connection_info ~timeout:(Time.milliseconds timeout) data ~len
                  ~err_cont:(fun exn -> create_outcome (`failure (Printexc.to_string exn)) cont)
                  (fun cnt -> create_outcome (`success cnt) cont)

##register [cps-bypass] read : Socket.connection, continuation(string) -> void
let read connection_info k =
  Scheduler.read Scheduler.default connection_info (fun (_, str) -> str |> k)

##register [cps-bypass] read_with_err_cont : Socket.connection, int, continuation(outcome(string,string)) -> void
let read_with_err_cont connection_info timeout cont =
  Scheduler.read Scheduler.default connection_info ~timeout:(Time.milliseconds timeout)
                 ~err_cont:(fun exn -> create_outcome (`failure (Printexc.to_string exn)) cont)
                 (fun (_,str) -> create_outcome (`success str) cont)

##register conn_id : Socket.connection -> int
let conn_id conn = conn.Scheduler.conn_id
