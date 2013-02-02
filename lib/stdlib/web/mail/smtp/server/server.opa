/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin mail

type SmtpServer.failure =
  {unavailable}
/ {aborted}
/ {not_found}
/ {forbidden}
/ {error : string}

type SmtpServer.result =
  { success }
/ { failure : SmtpServer.failure }

type SmtpServer.handler = string, list(string), string -> SmtpServer.result

SmtpServer = {{

  @private init_server = %% BslMail.SmtpServer.init_server %%

  start(ip : ip, port : int, ssl : option(SSL.secure_type), handler : SmtpServer.handler) =

    // Partially from http://www.greenend.org.uk/rjk/2000/05/21/smtp-replies.html
    handler_wrapper(f,t,c) =
      match handler(f,t,c) with
      | {success} -> (250,"done!")
      | {failure = {unavailable}} -> (450,"Requested mail action not taken: mailbox unavailable")
      | {failure = {aborted}} -> (451,"Requested action aborted: local error in processing")
      | {failure = {not_found}} -> (550,"Requested action not taken: mailbox unavailable")
      | {failure = {forbidden}} -> (553,"Requested action not taken: mailbox name not allowed")
      | {failure = {error = txt}} -> (503,txt)

    init_server(port, IPv4.string_of_ip(ip), ssl, handler_wrapper)

}}
