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

type SmtpServer.failure = {dont_exist}
                        / {undisponible}
                        / {stopped}
                        / {unfound}
                        / {forbidden}
                        / {error : string}

type SmtpServer.result = { success }
                       / { failure : SmtpServer.failure}

type SmtpServer.handler = string, list(string), string -> SmtpServer.result

SmtpServer= {{

  @private init_server = %% BslMail.Mailserver.init_server %%

  start(ip : ip ,port : int, ssl : option(SSL.secure_type), handler : SmtpServer.handler)=
    new_handler(f,t,c) = match handler(f,t,c) with
                         | {success} -> (250,"done!")
                         | {failure = {undisponible}} -> (450,"undone because mail box not disponible")
                         | {failure = {stopped}} -> (451,"treatment error")
                         | {failure = {unfound}} -> (550,"No access to mail box => unfound")
                         | {failure = {dont_exist}} -> (553,"mail box's name is not allowed")
                         | {failure = {forbidden}} -> (553,"mail box's name is not allowed")
                         | {failure = {error = txt}} -> (503,txt)

    st = match ssl with
      | {~some} -> some
      | {none} -> SSL.make_secure_type({none},{none})

    init_server(port, IPv4.string_of_ip(ip), st, new_handler)

}}
