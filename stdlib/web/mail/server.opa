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
