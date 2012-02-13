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

module P = RuntimeType.Ports
module D = RuntimeType.Description

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
          let parse = ServerArg.make_parser full_name (Comp.spec_args name) in
          ServerArg.filter (opt:Comp.options) parse

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
  let run, close =
    match !app with
    | None -> (fun _ -> ()), (fun _ -> ())
    | Some a ->
        let module App = (val a : RuntimeSig.APPLICATION) in
        let options = App.get_options () in
        let args = (ServerArg.get_argv ()) in
        if List.mem "--help" (ServerArg.to_list args) then exit 0;
        let app = App.make options sched in
        RuntimeType.Ports.init sched;
        let run () = let _ = App.run app sched in () in
        let close () = App.close app sched in
        run, close
  in
  run ();
  Scheduler.run sched;
  close ()
