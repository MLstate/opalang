/*
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
*/
package stdlib.apis.irc
import-plugin irc

type Irc.connection = external
type Irc.msg = external

Irc = {{
    create_bot = (%%bslirc.create_bot%%)
    write_raw = (%%bslirc.write_raw%%)
    write_msg = (%%bslirc.write_msg%%)
}}
