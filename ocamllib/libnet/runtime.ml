(*
    Copyright © 2011, 2012 MLstate

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

module P = RuntimeType.Ports
module D = RuntimeType.Description
module SA = ServerArg

let make_application (type options) name opt comp =
  let module Comp = (val comp : RuntimeSig.COMPONENT with type options = options) in
  let module App =
      struct
        type options = Comp.options
        type t = Comp.t

        let full_name =
            if name <> "" then
              Printf.sprintf "%s %s (%s)" Comp.name Comp.version name
            else
              Printf.sprintf "%s %s" Comp.name Comp.version

        let names = [full_name]
        let versions = StringMap.singleton Comp.name Comp.version

        let make opt sched =
          let comp = Comp.make name opt sched in
          let ports = Comp.get_ports comp sched in
          let description = Comp.get_description comp sched in
          P.add sched ports;
          D.add name description;
          comp

        let get_options () =
          let parse = SA.make_parser full_name (Comp.spec_args name) in
          SA.filter (opt:Comp.options) parse

        let run = Comp.run
        let close = Comp.close
      end
  in
  (module App : RuntimeSig.APPLICATION)

let merge_applications app1 app2 =
  let module App1 = (val app1 : RuntimeSig.APPLICATION) in
  let module App2 = (val app2 : RuntimeSig.APPLICATION) in
  let module App =
      struct
        type options = App1.options * App2.options
        type t = App1.t * App2.t

        let merge_maps map1 map2 =
          StringMap.fold
            (fun k v map -> StringMap.add k v map) map2 map1

        let names = App1.names @ App2.names
        let versions = merge_maps App1.versions App2.versions
        let get_options () = App1.get_options (), App2.get_options ()

        let make (opt1, opt2) sch =
          let app1 = App1.make opt1 sch in
          let app2 = App2.make opt2 sch in
          (app1, app2)

        let run (app1, app2) sch =
          let app1 = App1.run app1 sch in
          let app2 = App2.run app2 sch in
          (app1, app2)

        let close (app1, app2) sch =
          App1.close app1 sch;
          App2.close app2 sch
      end
  in
  (module App : RuntimeSig.APPLICATION)

let app = ref None

let add_component comp name opt =
  match !app with
  | Some a ->
      app := Some (merge_applications a (make_application name opt comp))
  | None ->
      app := Some (make_application name opt comp)

let add_httpServer = add_component (module HttpServer : RuntimeSig.COMPONENT with type options = HttpServer.options)
let add_httpDialog = add_component (module HttpDialog : RuntimeSig.COMPONENT with type options = HttpDialog.options)

let add_ftpServer = add_component (module FtpServer : RuntimeSig.COMPONENT with type options = FtpServer.options)

let add_smtpServer = add_component (module SmtpServer : RuntimeSig.COMPONENT with type options = SmtpServer.options)

let add_watchdog = add_component (module Watchdog : RuntimeSig.COMPONENT with type options = Watchdog.options)

let start () =
  let sched = Scheduler.default in
  let get_fun =
    match !app with
    | None -> fun () -> (fun _ -> ()), (fun _ -> ())
    | Some a ->
        let module App = (val a : RuntimeSig.APPLICATION) in
        let options = App.get_options () in
        fun () ->
          begin
            let app = App.make options sched in
            RuntimeType.Ports.init sched;
            (fun () -> let _ = App.run app sched in ()),
            (fun () -> App.close app sched)
          end
  in
  let args = (SA.get_argv ()) in
  if List.mem "--help" (SA.to_list args) then exit 0;
  if not (SA.is_empty args) then begin
    Printf.eprintf "Unknown option `%s'.\n" (SA.argv_to_string ());
    Printf.eprintf "Try `--help' for more information.\n";
    exit 1;
  end;
  let run, close = get_fun () in
  run ();
  Scheduler.run sched;
  close ()
