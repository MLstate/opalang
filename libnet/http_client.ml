(*
    Copyright © 2011, 2012 MLstate

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
(*
   @author Laurent Le Brun
   @author Cédric Soulas
   @author Frederic Ye
   @author David Rajchenbach-Teller
**)

#<Debugvar:TESTING>
#<Debugvar:HTTP_CLIENT_DEBUG>

open Http_common

let http : NetAddr.protocol = HttpTools.http
let http_version = "HTTP/1.0"

let client_name = Printf.sprintf "Opa-webclient/%s" version

let parse_response str =
  try
    let pos, res = Request.parse_request_full_response str in
    `Success (res, String.sub str pos (String.length str - pos))
  with Trx_runtime.SyntaxError (pos, str) -> `Failure (Printf.sprintf "Http_client: parse response error: %s (pos:%d)" str pos)

exception Timeout

let place_request (sched: Scheduler.t) ~hostname ~port ~path
    ?client_certificate ?verify_params
    ?(secure=false) ~request_kind ?(auth="")
    ?(more_headers=[]) ?(data="")
    ?(client_name=client_name)
    ?(timeout=Time.seconds 36)
    ?err_cont ~success ~failure () =
  let err_cont =
    match err_cont with
    | Some err_cont -> err_cont
    | None -> (fun _ -> failure `Timeout)
  in
  try
  (* check *)
  let has_port =
    String.contains hostname ':'
  in
  let path =
    if path = "" then (
      Logger.warning "[Http_client.get] the Request_URI canNOT be null.";
      "/"
      (* Quote from http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html :
         (...)
         Note that the absolute path cannot be empty; if none is present in the original URI, it MUST be given as "/"
         (...)
      *)
    ) else path
  in
  match
    try `Success (Network.inet_addr_of_name hostname) with Network.Unknown_machine s -> `Unknown_machine s
  with
  | (`Unknown_machine _s) as e -> failure e
  |  `Success machine ->
      let secure_mode =
        if secure then Network.Secured (client_certificate, verify_params)
        else           Network.Unsecured
      in
      let port_spec = Network.make_port_spec ~protocol:http machine port in
      let command = Printf.sprintf "%s %s %s%s%sHost: %s%sUser-Agent: %s%s%s%s%s"
        request_kind
        path
        http_version
        Base.crlf
        (if auth = "" then "" else (Printf.sprintf "Authorization: %s%s" auth Base.crlf))
        (if port = 80 then hostname else Printf.sprintf "%s:%d" hostname port)
        Base.crlf
        client_name
        Base.crlf
        (List.fold_left (
	   fun acc h -> Printf.sprintf "%s%s%s" acc h Base.crlf
	 ) "" more_headers)
	Base.crlf
        data
      in
      let start conn =
        Scheduler.write ~timeout ~err_cont sched conn command (
          fun _ -> Scheduler.read_all ~timeout ~err_cont sched conn (
            fun (_, buf) ->
              #<If:TESTING $minlevel 0>
              Logger.info "[http_client] received\n %s" command;
              #<End>;
              match parse_response (FBuffer.contents buf) with
              | `Success (((_, status), header), body) ->
                  begin
                    match Requestdef.ResponseHeader.get_string `Content_Length header with
                    | Some s ->
                        begin
                          let len = String.length body in
                          match try Some (int_of_string s == len) with Failure _ -> None
                          with
                          | Some true  -> success (status, header, body)
                          | Some false ->
                              if (len = 0) && ("HEAD" = request_kind) then
                                success (status, header, body)
                              else
                                failure (`Cannot_parse_response (Printf.sprintf "(incorrect size %s, expected %d)" s (String.length body)))
                          | None       -> failure (`Cannot_parse_response (Printf.sprintf "(invalid size %S, expected an integer)" s))
                        end
                    | _ -> success (status, header, body)
                  end
              | `Failure s -> failure (`Cannot_parse_response s)
          )
        )
      in
      #<If:TESTING $minlevel 0>
        Printf.printf "%s\n" command;
      #<End>;
      #<If:HTTP_CLIENT_DEBUG>
      Logger.info "[http_client] %s" command;
      #<End>;
      if has_port then
        Logger.warning "[Http_client] hostname contains ':' but it shouldn't, please check";
      Network.connect sched port_spec secure_mode ~err_cont start
  with
  | exn -> err_cont exn

let default_failure = function
  | `Unknown_machine m       -> Logger.error "Unknown machine %s" m
  | `Cannot_parse_response s -> Logger.error "Cannot parse response %s" s
  | `Timeout                 -> Logger.error "Timeout exceeded"

let get (sched: Scheduler.t) hostname port path
    ?client_certificate ?verify_params
    ?(secure=false) ?(auth="")
    ?(more_headers=[]) ?err_cont ?(failure=default_failure) cont =
  place_request sched ~hostname ~port ~path
    ~request_kind:"GET"
    ?client_certificate ?verify_params
    ~secure ~auth
    ~more_headers
    ~client_name:client_name
    ?err_cont
    ~success:(fun (_, x, y) -> cont (x, y))
    ?failure
    ()

let post (sched: Scheduler.t) hostname port path
    ?client_certificate ?verify_params
    ?(secure=false) ?(auth="") mime_type
    ?(length=(-1)) ?err_cont ?(failure=default_failure) data cont =
  let length = if length = (-1) then String.length data else length in
  let more_headers = [
    Printf.sprintf "Content-Length: %d" length;
    Printf.sprintf "Content-Type: %s" mime_type;
  ] in
  place_request sched ~hostname ~port ~path
    ~request_kind:"POST"
    ?client_certificate ?verify_params
    ~secure ~auth
    ~more_headers ~data
    ~client_name:client_name
    ?err_cont
    ~success:(fun (_, x, y) -> cont (x, y))
    ?failure
    ()
