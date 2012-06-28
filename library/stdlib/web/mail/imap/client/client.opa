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

import-plugin mail

type Email.imap_command =
    { ImapSelect : string }
  / { ImapExamine : string }
  / { ImapNoop }
  / { ImapFetch : (bool, string, string) }
  / { ImapStore : (bool, string, string, string) }
  / { ImapSearch : (bool, string) }
  / { ImapSearchCs : (bool, string, string) }
  / { ImapCopy : (bool, string, string) }
  / { ImapList : (string, string) }
  / { ImapCreate : string }
  / { ImapDelete : string }
  / { ImapRename : (string, string) }
  / { ImapStatus : (string, string) }
  / { ImapAppend : (string, string, string, string) }
  / { ImapExpunge }

//type Email.imap_status = {
//  flags : string
//  exists : int
//  recent : int
//  oks : string list
//  rwstatus : string
//}

type Email.imap_result =
    { Ok : string }
  / { No : string }
  / { Bad : string }
  / { SelectResult : (string,int,int,list(string),string) }
  / { ExamineResult : (string,int,int,list(string),string) }
  // { NoopResult : Email.imap_status }
  / { NoopResult : (string,int,int,list(string),string) }
  / { SearchResult : list(int) }
  / { FetchResult : list((int, string, string)) }
  / { StoreResult : list((int, string)) }
  / { ListResult : list((string, string, string)) }
  / { StatusResult : list((string, string)) }
  / { ExpungeResult : list(int) }
  / { Error : string }

Imap = {{

  command = %% BslMail.ImapClient.command %% : int , string, option(SSL.secure_type), string , string, list(Email.imap_command), (list(Email.imap_result) -> void) -> void

}}

