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
let create_bot = IrcBot.create_bot
let write_raw conn str = Scheduler.write (Scheduler.default) conn str
                                          (fun _ -> ())
let write_msg conn msg = write_raw conn (IrcBotCore.string_of_msg msg)

##extern-type Irc.connection = Scheduler.connection_info
##extern-type Irc.msg = IrcBotCore.msg

##register create_bot: string, string, string, string, string, string, int,\
                       (Irc.connection -> void) -> void
##register write_raw: Irc.connection, string -> void
##register write_msg: Irc.connection, Irc.msg -> void
