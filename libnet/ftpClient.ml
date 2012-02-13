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
(* ftpClient: Simple read of file from FTP server.
*)
#<Debugvar:PROTOCOL_DEBUG>

module FCC = FtpClientCore
module List = Base.List
module String = Base.String
module Rc = Rcontent
let protocol = FCC.protocol

exception Bad_address of string

let _log sep code reason = Logger.warning "%d%s%s" code sep reason

let get_match str n = try (Some (Str.matched_group n str)) with Not_found -> None
let find_matches str cnt = let rec aux n l = if n > cnt then l else aux (n+1) (l@[get_match str n]) in aux 1 []

let ftpre =
  Str.regexp
    "\\(ftp://\\)?\\(\\([a-zA-Z0-9_-\\.]+\\)\\(:\\([a-zA-Z0-9_-\\.]+\\)\\)?@\\)?\\([a-zA-Z0-9_-\\.]+\\):?\\([0-9]+\\)?/\\(.*\\)"
let split_ftp user pass port s =
  if Str.string_match ftpre s 0
  then
    match (get_match s 3, get_match s 5, get_match s 6, get_match s 7, get_match s 8) with
    | (user_opt, pass_opt, Some domain, port_opt, Some path) ->
        let port =
          match port_opt with
          | Some portstr -> (try int_of_string portstr with Failure "int_of_string" -> port)
          | None -> port in
        let user = Option.default user user_opt in
        let pass = Option.default pass pass_opt in
        (user, pass, domain, port, path)
    | _ -> raise (Bad_address s)
  else
    raise (Bad_address s)

let get_ip name = (Unix.gethostbyname name).Unix.h_addr_list.(0)
let my_ip () = Unix.string_of_inet_addr(get_ip (Unix.gethostname()))

let dre = Str.regexp_string "."
let dataport () =
  match Str.split dre (my_ip ()) with
  | [h1;h2;h3;h4] ->
      let dp = Random.int (65534-49152) + 49152 in
      let p1, p2 = (dp land 0xff00) lsr 8, dp land 0xff in
      let addr = Unix.inet_addr_of_string (Printf.sprintf "%s.%s.%s.%s" h1 h2 h3 h4) in
      #<If$minlevel 2>Logger.debug "dataport: addr=%s\n" (Unix.string_of_inet_addr addr)#<End>;
      let port_spec = Network.make_port_spec ~protocol addr dp in
      port_spec, Printf.sprintf "%s,%s,%s,%s,%d,%d" h1 h2 h3 h4 p1 p2
  | _ -> assert false

let receive_ftp_file sched url
                     ?(user="anonymous") ?(password="change.me@example.com") ?(ct=Rc.CT_BUFFER) ?(port=21)
                     ?(datablocksize=4096) ?(hint=4096)
                     cont =
  let user, password, domain, port, filespec = split_ftp user password port url in
  let pathname = Filename.dirname filespec in
  let filename = Filename.basename filespec in
  let dataportspec, dataportstr = dataport () in
  let datasecuremode = Network.Unsecured in
  #<If>Logger.debug "receive_ftp_file: domain=%s port=%d pathname=%s filename=%s dataportstr=%s\n"
          domain port pathname filename dataportstr#<End>;
  let ftp = { FCC.content_type=ct; content_hint=hint;
              user=user; password=password;
              domain=domain; pathname=pathname; filename=filename;
              dataportstr=dataportstr; datasecuremode=datasecuremode;
              dataportspec=dataportspec; datablocksize=datablocksize;
            } in
  let state = {
    FCC.log = _log " ";
    elog = _log "-";
    cont = (function
            | FCC.Error msg ->
                (Logger.error "ERROR: %s\n" msg;
                 cont (FCC.Error msg))
            | FCC.Ok res -> cont (FCC.Ok res));
  } in
  let client = { FCC.runtime = { FCC.rt_plim = 128;
                                 rt_buf = Buffer.create 0;
                                 rt_proto = { FCC.rt_block_size = 4096;
                                              rt_backtrace = true;
                                              rt_server_write_timeout = Time.seconds 36;
                                              rt_payload = ();
                                            };
                               };
                 err_cont = None;
                 extra_params = (state,ftp) } in
  FCC.connect client sched domain port

