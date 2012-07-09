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
(*
    @author Louis Gesbert
**)

type options = { bind: Unix.inet_addr; port: int; backend: (module Badop.S) * Badop.options; daemonize: bool; pidfile: string option }

let options =
  let default = {
    bind = Unix.inet_addr_any;
    port = 4849;
    backend = Badop_meta.default;
    daemonize = false;
    pidfile = None;
  } in
  try
    let wrap_parser parse =
      fun o -> ServerArg.wrap (parse o.backend) (fun bo -> { o with backend = bo }) in
    let p =
      ServerArg.filter default
        (ServerArg.make_parser ~final:true "Database server" (
           (["--port";"-p"], ServerArg.func ServerArg.int (fun o p -> {o with port = p}),
            "<int>", Printf.sprintf "Set the port the server should listen on (default %d)" default.port)
           ::
           (["--bind";"-b"],
            ServerArg.func ServerArg.parse_addr
              (fun o (addr,portopt) ->
                 {o with bind = addr; port = Option.default o.port portopt}),
            "<addr>[:<port>]", Printf.sprintf "Bind the server to the given local address")
           ::
             (if Config.os = Config.Mac then []
             else [
           (["--daemon";"-d"],
              ServerArg.func (ServerArg.option ServerArg.string)
                (fun o pidfile -> {o with daemonize = true; pidfile}),
            "[pidfile]", Printf.sprintf "Run in the background (does not exist on MacOS)")
             ])
           @
           List.map
             (fun (arg,parse,params,help) -> arg, wrap_parser parse, params, help)
             Badop_meta.options_parser
         ))
    in
    if ServerArg.is_empty (ServerArg.get_argv ()) then p
    else
      (Printf.eprintf "Error: unknown command-line argument: %s\n" (ServerArg.argv_to_string ());
       raise Exit)
  with Exit -> exit 1

let sched = Scheduler.default
let endpoint = Hlnet.Tcp (options.bind, options.port)

module Db = (val (fst options.backend) : Badop.S)
module Server = Badop_server.F(Db)
let _ =
  Server.start sched endpoint (snd options.backend)
    (fun db ->
       at_exit (fun () -> Server.stop db ignore);
       if options.daemonize then
         let child_pid = Unix.fork() in
         if child_pid <> 0 then
           (Option.iter
              (fun pidfile -> let c = open_out pidfile in Printf.fprintf c "%d" child_pid; close_out c)
              options.pidfile;
            BaseUnix._exit 0)
         else ignore (Unix.setsid()))

let _ = Scheduler.run sched
