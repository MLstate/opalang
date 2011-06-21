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
(* smtpServerStandalone:
 * Test SMTP server.
 *)

module SCCp = SmtpServerCore_parse
module SST = SmtpServerType
module SSC = SmtpServerCore
module SS = SmtpServer

#<Debugvar:PROTOCOL_DEBUG>

let _ =
  let opts = SS.default_options in
  Runtime.add_smtpServer "smtp-server" opts;
  Runtime.start ()
