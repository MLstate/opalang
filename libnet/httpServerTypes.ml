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

type msg = HttpServerCore_parse.msg

type uri = string
type header = msg
type mime = msg
type post_body = mime list * Rcontent.content

type response = { sl : msg ; headers : header list ; body : Rcontent.content }

type multipart_post_request = { uri:uri;
                                request_headers:header list;
                                request_body : post_body list;
                                tmpfiles : string list;
                              }

type get = uri * header list
type post =
  | Multipart of multipart_post_request
  | Simple of uri * header list * Rcontent.content
type put = uri * header list * Rcontent.content
type optional = uri * header list

type request_line = { _method : msg ; request_uri : string ; http_version : string }

type server_info = { server_url:string; server_ip_or_name:string; server_port:int; server_secured:bool; server_id:int }

type handle_request = { hr_delcookies : bool;
                        hr_cachetype : string;
                        hr_server_info : server_info;
                        hr_is_secure : bool;
                        hr_timestamp : Time.t;
                        hr_timestamp_tm : Unix.tm;
                        hr_inet_addr_str : string;
                        hr_user_agent : string;
                        hr_referer : string;
                        hr_ec : string;
                        hr_ic : string;
                        hr_dt2 : Time.t;
                      }

type request = { request_scheduler: Scheduler.t;
                 request_line : request_line;
                 request_header : header list;
                 request_message_body : Rcontent.content;
                 request_post_body : post_body list;
                 server_info : server_info;
                 is_multipart : bool;
                 handle_request : handle_request;
               }

type web_info = { cont : response -> unit;
                  request : request;
                  connection : Scheduler.connection_info;
                  certificate : Ssl.certificate option;
                }

type remote_logs = {port : int; hostname : string; appkey : string}

let current_time_string = ref ""
let time_diff = ref ""
