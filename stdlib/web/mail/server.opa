/*
    Copyright © 2011, 2012 MLstate

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
*/

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

  @private init_server = %% BslMail.Mailserver.init_server %%

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

    st = ssl ? SSL.make_secure_type({none},{none})

    init_server(port, IPv4.string_of_ip(ip), st, handler_wrapper)

}}
