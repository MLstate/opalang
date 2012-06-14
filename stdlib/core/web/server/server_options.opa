/*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

// Part of the move to options being in Opa rather than behind the BSL.

type Server.private.opt_time =
   {infinity}
 / {seconds : int}

type Server.private.remote_logs = {
  hostname : string;
  port : int;
  appkey : string;
}

type Server.private.options = {
  ssl_cert : string;
  ssl_key : string;
  ssl_pass : string;
  pid_file : option(string);
  dialog : string;
  print_log_info : bool;
  print_server_info : bool;
  long_cookies : bool;
  cookie_expire_time_short : int;
  cookie_expire_time_long : int;
  dt1 : Server.private.opt_time;
  dt2 : Server.private.opt_time;
  max_external_cookies : int;
  rotate_cookies : bool;
  server_send_buffer_size: int;
  cookie_gc_period: int;
  cookie_accept_client_values: bool;
  cookie_pool_size_min: int;
  cookie_pool_size_max: int;
  cookie_timer_interval: int;
  cookie_rate_max: float;
  cookie_period_max: int;
  cookie_rate_ultimate: float;
  cookie_period_ultimate: int;
  cookies_filename: string;
  server_wait_for_request_timeout: float;
  server_wait_for_request_initial_timeout: float;
  server_write_timeout: float;
  maximum_content_length: int;
  maximum_number_of_headers: int;
  remote_logs: option(Server.private.remote_logs);
  backtrace: bool;
  name : string;
  addr : string;
  port : int;
  block_size : int;
  all_methods: bool;
  dos_prevention : bool;
}

Server_options = {{

  @private CL = CommandLine

  default_http_options = {
    ssl_cert = "";
    ssl_key = "";
    ssl_pass = "";
    pid_file = {none};
    dialog = "default";
    print_log_info = true;
    print_server_info = true;
    long_cookies = true;
    cookie_expire_time_short = 5;
    cookie_expire_time_long = 50;
    dt1 = {seconds=10*60*60*24};
    dt2 = {infinity};
    max_external_cookies = 10;
    rotate_cookies = true;
    server_send_buffer_size = 1024;
    cookie_gc_period = 100;
    cookie_accept_client_values = false;
    cookie_pool_size_min = 100;
    cookie_pool_size_max = 10000;
    cookie_timer_interval = 1;
    cookie_rate_max = 5.0;
    cookie_period_max = 5;
    cookie_rate_ultimate = 10.0;
    cookie_period_ultimate = 100;
    cookies_filename = "";
    server_wait_for_request_timeout = 36.0;
    server_wait_for_request_initial_timeout = 36.0;
    server_write_timeout = 3600.0;
    maximum_content_length = (50*1024*1024);
    maximum_number_of_headers = 200;
    remote_logs = {none};
    backtrace = true;
    name = "httpServerPort";
    addr = "0.0.0.0";
    port = 8080;
    block_size = 4096;
    all_methods = true;
    dos_prevention = true;
  } : Server.private.options

  default_https_options = {
    default_http_options with
      port = 4343
      ssl_cert = "cert.pem"
      ssl_key = "privkey.pem"
      ssl_pass = "change this password"
  }

  opt_time(s:string) : Server.private.opt_time =
    match s with
    | "inf" | "INF" | "Inf" | "infinity" | "Infinity" | "INFINITY" | "none" | "None" | "NONE" -> {infinity}
    | s ->
      match Parser.try_parse(Rule.natural, s) with
      | {some=seconds} -> {~seconds}
      | {none} ->
         do Log.fatal("Server_options.opt_time", "Argument '{s}' not valid time (<int> | \"inf\")")
         {infinity}
      end

  string_of_opt_time(t:Server.private.opt_time) : string = match t with | {infinity} -> "inf" | {~seconds} -> "{seconds}"

  spec_args(name, init) =
    optname(opt) = if name == "" then "--{opt}" else "--{name}-{opt}"
    ned(dflt) = if dflt == "" then "" else "(default: {dflt})"
    raw_parsers = [
      (["caml","node.js"],
      CL.string(List.append([optname("addr")],if name == "http" then ["-a"] else []),
                "Sets the IP address on which the server should run (default:{init.addr})",
                "<string>"
               )(addr, p ->
                 if false // TODO: inet_addr_of_string
                 then do Log.fatal("Server_options","Bad address: {addr}") p
                 else {p with ~addr}
                )),

      (["caml","node.js"],
      CL.int(List.append([optname("port")],if name == "http" then ["-p"] else []),
             "Sets the port on which the server should run (default:{init.port})",
             "<int>"
            )(port, p ->
              if port > 0xffff
              then do Log.fatal("Server_options","Bad port number: {port}") p
              else {p with ~port}
             )),

      (["caml"],
      CL.bool([optname("long-cookies")],
                "Use long cookies (default: {init.long_cookies})",
                "<bool>"
               )(long_cookies, p -> {p with ~long_cookies})),

      (["caml"],
      CL.int([optname("cookie-expire-short")],
                "Cookie expire time (short) seconds (default:{init.cookie_expire_time_short})",
                "<int>"
               )(i, p -> {p with cookie_expire_time_short=i})),

      (["caml"],
      CL.int([optname("cookie-expire-long")],
                "Cookie expire time (long) seconds (default:{init.cookie_expire_time_long})",
                 "<int>"
               )(i, p -> {p with cookie_expire_time_long=i})),

      (["caml","node.js"],
      CL.string([optname("long-cookie-expire-variable")],
                "Long cookie variable expire time seconds (default:{string_of_opt_time(init.dt1)})",
                "<int>|\"inf\""
               )(s, p -> {p with dt1=opt_time(s)})),

      (["caml","node.js"],
      CL.string([optname("long-cookie-expire-fixed")],
                "Long cookie fixed expire time seconds (default:{string_of_opt_time(init.dt2)})",
                "<int>|\"inf\""
               )(s, p -> {p with dt2=opt_time(s)})),

      (["caml"],
      CL.int([optname("max-external-cookies")],
                "Maximum number of concurrent external cookies per internal cookie (default:{init.max_external_cookies})",
                "<int>"
               )(i, p -> {p with max_external_cookies=i})),

      (["caml"],
      CL.switch([optname("no-rotate-cookies")],
                "Switch off cookie rotation"
               )(p -> {p with rotate_cookies=false})),

      (["caml"],
      CL.int([optname("server-send-buffer-size")],
                "Server send buffer size (default:{init.server_send_buffer_size})",
                "<int>"
               )(i, p -> {p with server_send_buffer_size=i})),

      (["caml","node.js"],
      CL.int([optname("cookie-gc-period")],
                "Cookie GC period in requests (default:{init.cookie_gc_period})",
                "<int>"
               )(i, p -> {p with cookie_gc_period=i})),

      (["caml","node.js"],
      CL.switch([optname("cookie-accept-client-values")],
                "WARNING: Only with long cookies. Accept cookie values provided by the client instead of generating new one when they aren't found on the server cookie table (default:{init.cookie_accept_client_values})"
               )(p -> {p with cookie_accept_client_values=true})),

      (["caml","node.js"],
      CL.int([optname("cookie-pool-size-min")],
                "Cookie pool size minimum (default:{init.cookie_pool_size_min})",
                "<int>"
               )(i, p -> {p with cookie_pool_size_min=i})),

      (["caml","node.js"],
      CL.int([optname("cookie-pool-size-max")],
                "Cookie pool size maximum (default: {init.cookie_pool_size_max})",
                "<int>"
               )(i, p -> {p with cookie_pool_size_max=i})),

      (["caml","node.js"],
      CL.int([optname("cookie-timer-interval")],
                "Cookie timer interval (seconds) (default: {init.cookie_timer_interval})",
                "<int>"
               )(i, p -> {p with cookie_timer_interval=i})),

      (["caml","node.js"],
      CL.float([optname("cookie-rate-max")],
                "Cookie connection rate max (default: {init.cookie_rate_max})",
                "<float>"
               )(f, p -> {p with cookie_rate_max=f})),

      (["caml"],
      CL.int([optname("cookie-period-max")],
                "Cookie rotation period above max rate (default: {init.cookie_period_max})",
                "<int>"
               )(i, p -> {p with cookie_period_max=i})),

      (["caml"],
      CL.float([optname("cookie-rate-ultimate")],
                "Cookie connection rate ultimate (default: {init.cookie_rate_ultimate})",
                "<float>"
               )(f, p -> {p with cookie_rate_ultimate=f})),

      (["caml"],
      CL.int([optname("cookie-period-ultimate")],
                "Cookie rotation period above ultimate rate (default: {init.cookie_period_ultimate})",
                "<int>"
               )(i, p -> {p with cookie_period_ultimate=i})),

      (["caml","node.js"],
      CL.string([optname("cookies-filename")],
                "Cookies filename (empty=disabled) (default: {init.cookies_filename})",
                "<filename>"
               )(s, p -> {p with cookies_filename=s})),

      (["caml"],
      CL.float([optname("wait-for-request-timeout")],
                "Timeout while waiting for requests (default: {init.server_wait_for_request_timeout})",
                "<float>"
               )(f, p -> {p with server_wait_for_request_timeout=f})),

      (["caml"],
      CL.float([optname("wait-for-request-initial-timeout")],
                "Initial timeout while waiting for requests (default: {init.server_wait_for_request_initial_timeout})",
                "<float>"
               )(f, p -> {p with server_wait_for_request_initial_timeout=f})),

      (["caml"],
      CL.float([optname("write-timeout")],
                "Timeout while writing data (default: {init.server_write_timeout})",
                "<float>"
               )(f, p -> {p with server_write_timeout=f})),

      (["caml"],
      CL.string([optname("remote-logs")],
                "Log access to a remote server (WARNING: this is experimental) (default: no log server)",
                "<hostname:port/appkey>"
               )(s, p ->
                 match String.explode(":",s) with
                 | [hostname,port_appkey] ->
                    match String.explode("/",port_appkey) with
                    | [port,appkey] ->
                      match Parser.try_parse(Rule.natural,port) with
                      | {some=port} -> {p with remote_logs={some=~{hostname; port; appkey}}}
                      | {none} -> do Log.fatal("Server_options.spec_args","Invalid port for --remote-logs: {port}") p
                      end
                    | _ -> do Log.fatal("Server_options.spec_args","Bad option \"{s}\" for --remote-logs") p
                    end
                  | _ -> do Log.fatal("Server_options.spec_args","Bad option \"{s}\" for --remote-logs") p
                  end)),

      (["caml"],
      CL.int([optname("maximum-content-length")],
                "Maximum request content length (default: {init.maximum_content_length})",
                "<int>"
               )(i, p -> {p with maximum_content_length=i})),

      (["caml"],
      CL.int([optname("maximum-number-of-headers")],
                "Maximum number of request headers (default: {init.maximum_number_of_headers})",
                "<int>"
               )(i, p -> {p with maximum_number_of_headers=i})),

      (["caml"],
      CL.switch([optname("no-print-log-info")],
                "Disable access and error logs"
               )(p -> {p with print_log_info=false})),

      (["caml"],
      CL.switch([optname("no-print-server-info")],
                "Disable server information printout"
               )(p -> {p with print_server_info=false})),

      (["caml"],
      CL.switch([optname("no-flood-prevention")],
                "Disable the built-in protection against Denial-of-Service attacks"
               )(p -> {p with dos_prevention=false})),

      (["caml"],
      CL.switch([optname("no-backtrace")],
                "Disable backtrace printout for server exceptions"
               )(p -> {p with backtrace=false})),

      (["caml"],
      CL.bool([optname("all-methods")],
                "Use all HTTP methods (default:{init.all_methods}) - Only GET, HEAD and POST if false.",
                "<bool>"
               )(b, p -> {p with all_methods=b})),

      (["caml","node.js"],
      CL.string([optname("ssl-cert")],
                "Location of your SSL certificate (requires ssl-key) (default: {init.ssl_cert})",
                "<file>"
               )(s, p -> {p with ssl_cert=s})),

      (["caml","node.js"],
      CL.string([optname("ssl-key")],
                "Location of your SSL key (requires ssl-cert) (default: {init.ssl_key})",
                "<file>"
               )(s, p -> {p with ssl_key=s})),

      (["caml","node.js"],
      CL.string([optname("ssl-pass")],
                "Password of your SSL certificate (requires ssl-cert and ssl-key options) (default: {init.ssl_pass})",
                "<string>"
               )(s, p -> {p with ssl_pass=s})),

      (["caml"],
      CL.string([optname("dialog")],
                "Name of the http dialog to use  (default: {init.dialog})",
                "<string>"
               )(s, p -> {p with dialog=s})),

      (["caml","node.js"], // TODO: implement pid_file for node.js
      CL.string([optname("pidfile")],
                "File to dump server's pid. Server exits on error.",
                "<file>"
               )(s, p -> {p with pid_file={some=s}})),
    ]
    args = {
      title="{String.capitalize(name)} Server"
      ~init
      anonymous=[]
      parsers=CL.select_backend(raw_parsers)
    }
    opts = CL.filter(args)
    do jlog("{name} options = {opts}")
    opts

}}

