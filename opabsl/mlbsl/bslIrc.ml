(*
    Copyright © 2011 MLstate

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
