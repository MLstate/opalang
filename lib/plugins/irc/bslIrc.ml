(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
let create_bot = IrcBot.create_bot
let write_raw conn str = Scheduler.write (Scheduler.default) conn str
                                          (fun _ -> ())
let write_msg conn msg = write_raw conn (IrcBotCore.string_of_msg msg)

##extern-type Irc.connection = Scheduler.connection_info
##extern-type Irc.msg = IrcBotCore.msg

##register create_bot: string, string, string, string, string, string, int,\
                       (Irc.connection -> void),\
		       (Irc.connection, string, string, string -> void) -> void
##register write_raw: Irc.connection, string -> void
##register write_msg: Irc.connection, Irc.msg -> void
