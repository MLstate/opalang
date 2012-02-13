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

type request_line = { _method : msg ; request_uri : string ; http_version : string }

type server_info = { server_url:string; server_ip_or_name:string; server_port:int; server_secured:bool; server_id:int }

type handle_request = { hr_delcookies : bool;
                        hr_cachetype : string;
                        hr_server_info : server_info;
                        hr_is_secure : bool;
                        hr_timestamp : Time.t;
                        hr_timestamp_tm : Time.tm;
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

val current_time_string : string ref
val time_diff : string ref
