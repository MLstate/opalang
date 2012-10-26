/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Logging support for Protocols driver
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * Module [ProtocolsLog] contains support for debugging and logging for the Protocols driver.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/
//package stdlib.apis.protocols
package protocols

type Protocols.params = {
  pool_max:int;
  log:bool;
}

ProtocolsCommon = {{

  @private PL = ProtocolsLog

  @private init_params = ({
    pool_max=2;
    log=false;
  }:Protocols.params)

  @private params = Mutable.make(init_params:Protocols.params)
  @private params_done = Mutable.make(false)

  @private
  get_params = ->
    do if not(params_done.get())
       then params.set(CommandLine.filter({
         title = "Protocols parameters";
         init = params.get() : Protocols.params;
         anonymous = [];
         parsers = [
           {CommandLine.default_parser with
              names = ["--protocols-socket-pool", "--protocolssocketpool", "--pp", "-pp"]
              description = "Number of sockets in socket pool (>=2 enables socket pool)"
              param_doc = "<int>"
              on_param(p) = parser n={Rule.natural} -> {no_params={p with pool_max=Int.max(n,1)}}
           },
           {CommandLine.default_parser with
              names = ["--protocols-log", "--protocolslog", "--pl", "-pl"]
              description = "Enable ProtocolsLog logging"
              param_doc = "<bool>"
              on_param(p) = parser b={Rule.bool} -> {no_params={p with log=b}}
           },
           {CommandLine.default_parser with
              names = ["--protocols-log-type", "--protocolslogtype", "--pt", "-pt"]
              description = "Type of logging: stdout, stderr, logger, none"
              param_doc = "<string>"
              on_param(p) = parser s={Rule.consume} ->
                logtype =
                  ((match s with
                    | "stdout" -> {stdout}
                    | "stderr" -> {stderr}
                    | "logger" -> {logger}
                    | "none" | "nolog" -> {nolog}
                    | _ -> PL.fatal("Protocols.get_params","Unknown Protocols log type string {s}",-1)):Protocols.logtype)
                do PL.logtype.set(logtype)
                {no_params=p}
           }
         ];
       }))
  params_done.set(true)

}}


