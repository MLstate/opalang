/*
    Copyright © 2011, 2012, 2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin mail

import stdlib.web.mail
import stdlib.crypto

type RAIServer.raiserver = external
type RAIServer.raiclient = external

type RAIServer.options = {
  bool debug,
  bool disconnectOnTimeout,
  bool secureConnection,
  string key,
  string cert,
  int timeout
}

type V8.Error = {
  string name,
  string message,
  string stack
}

type RAIServer.raicredentials = {
  string key,
  string cert,
  // list(string) ca // TODO: implement, it's actually an array
}

type RAIServer.server_callback =
    {RAIServer.raiclient connect}
 or {V8.Error error}

type RAIServer.client_callback =
    {string command, string payload}
 or {binary data}
 or {V8.Error error}
 or {timeout}
 or {ready}
 or {tls}

module RAIServer {

  /**
   * {1} Listening.
   */

  function RAIServer.raiserver make_server(int port) {
    %% BslRAI.RAIServer.make_server %%(port)
  }

  function RAIServer.raiserver make_server_options(int port, string hostname, RAIServer.options options) {
    %% BslRAI.RAIServer.make_server_options %%(port, hostname, options)
  }

  function void server_callback(RAIServer.raiserver raiserver,
                                (RAIServer.raiserver, RAIServer.server_callback -> void) callback) {
    %% BslRAI.RAIServer.server_callback %%(raiserver, callback)
  }

  function void server_remove_listeners(RAIServer.raiserver raiserver) {
    %% BslRAI.RAIServer.server_remove_listeners %%(raiserver)
  }

  function void close_server(RAIServer.raiserver raiserver, (void -> void) callback) {
    %% BslRAI.RAIServer.close_server %%(raiserver, callback)
  }

  function void send(RAIServer.raiclient raiclient, string msg) {
    %% BslRAI.RAIServer.send %%(raiclient, msg)
  }

  function bool secureConnection(RAIServer.raiclient raiclient) {
    %% BslRAI.RAIServer.secureConnection %%(raiclient)
  }

  function void client_callback(RAIServer.raiserver raiserver,  RAIServer.raiclient raiclient,
                                (RAIServer.raiserver,  RAIServer.raiclient, RAIServer.client_callback -> void) callback) {
    %% BslRAI.RAIServer.client_callback %%(raiserver, raiclient, callback)
  }

  function void client_remove_listeners(RAIServer.raiclient raiclient) {
    %% BslRAI.RAIServer.client_remove_listeners %%(raiclient)
  }

  function void client_start_tls(RAIServer.raiclient raiclient) {
    %% BslRAI.RAIServer.client_start_tls %%(raiclient)
  }

  function void client_start_tls_credentials(RAIServer.raiclient raiclient, RAIServer.raicredentials credentials) {
    %% BslRAI.RAIServer.client_start_tls_credentials %%(raiclient, credentials)
  }

  function void client_start_data(RAIServer.raiclient raiclient) {
    %% BslRAI.RAIServer.client_start_data %%(raiclient)
  }

  function void client_start_data_sequence(RAIServer.raiclient raiclient, string endSequence) {
    %% BslRAI.RAIServer.client_start_data_sequence %%(raiclient, endSequence)
  }

  function void client_end(RAIServer.raiclient raiclient) {
    %% BslRAI.RAIServer.client_end %%(raiclient)
  }

}

