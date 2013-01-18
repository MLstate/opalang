/*
    Copyright © 2011, 2012, 2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin mail

type Imap.command =
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

//type Imap.status = {
//  flags : string
//  exists : int
//  recent : int
//  oks : string list
//  rwstatus : string
//}

type Imap.result =
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

  command = %% BslMail.ImapClient.command %% : int , string, option(SSL.secure_type), string , string, list(Imap.command), (list(Imap.result) -> void) -> void

}}

