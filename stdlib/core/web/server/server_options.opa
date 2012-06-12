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

type Server.private.options = {
  port : int;
  addr : string
  ssl_cert : string
  ssl_key : string
  ssl_pass : string
}

Server_options = {{

  default_http_options = {
    port = 8080;
    addr = "0.0.0.0"
    ssl_cert = ""
    ssl_key = ""
    ssl_pass = ""
  }

  default_https_options = {
    default_http_options with
      port = 4343
      ssl_cert = "cert.pem"
      ssl_key = "privkey.pem"
      ssl_pass = "change this password"
  }

  spec_args(name, init) =
    optname(opt) = if name ==  "" then "--{opt}" else "--{name}-{opt}"
    ned(dflt) = if dflt == "" then "" else "(default: {dflt})"
    args = {
      title = "{String.capitalize(name)} Server"
      ~init
      anonymous = []
      parsers = // TODO complete this list
        [{CommandLine.default_parser with
            names = List.append([optname("addr")],if name == "http" then ["-a"] else [])
            description = "Sets the IP address on which the server should run (default:{init.addr})"
            param_doc = "<string>"
            on_param(p) = parser addr={Rule.consume} ->
                            if false // TODO: inet_addr_of_string
                            then do Log.fatal("Server_options","Bad address: {addr}") {no_params=p}
                            else {no_params={p with ~addr}}
         },
         {CommandLine.default_parser with
            names = List.append([optname("port")],if name == "http" then ["-p"] else [])
            description = "Sets the port on which the server should run (default:{init.port})"
            param_doc = "<int>"
            on_param(p) = parser port={Rule.natural} ->
                            if port > 0xffff
                            then do Log.fatal("Server_options","Bad port number: {port}") {no_params=p}
                            else {no_params={p with ~port}}
         },
         {CommandLine.default_parser with
            names = [optname("ssl-cert")]
            description = "Location of your SSL certificate (requires ssl-key) {ned(init.ssl_cert)}"
            param_doc = "<file>"
            on_param(p) = parser ssl_cert={Rule.consume} -> {no_params={p with ~ssl_cert}}
         },
         {CommandLine.default_parser with
            names = [optname("ssl-key")]
            description = "Location of your SSL key (requires ssl-cert) {ned(init.ssl_key)}"
            param_doc = "<file>"
            on_param(p) = parser ssl_key={Rule.consume} -> {no_params={p with ~ssl_key}}
         },
         {CommandLine.default_parser with
            names = [optname("ssl-pass")]
            description = "Password for your SSL certificate (requires ssl-cert and ssl-key options)"
            param_doc = "<file>"
            on_param(p) = parser ssl_pass={Rule.consume} -> {no_params={p with ~ssl_pass}}
         },
        ]
    }
    CommandLine.filter(args)

}}

