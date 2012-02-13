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
(*
 * A private type of *non-functional* buffer
 *
 * Note: It's not serializable, nor is it meant to be.
 *)


##extern-type Buffer2_private.buffer = Buffer.t
##register create \ `Buffer.create` : int -> Buffer2_private.buffer

##register add \ `Buffer.add_string` : Buffer2_private.buffer, string -> void

##register addln: Buffer2_private.buffer, string -> void
let addln buf s =
  Buffer.add_string buf s;
  Buffer.add_string buf "\n"

##register contents \ `Buffer.contents` : Buffer2_private.buffer -> string

##register is_empty: Buffer2_private.buffer -> bool
let is_empty buf = Buffer.length buf = 0

##register reset: Buffer2_private.buffer, int -> void
let reset buf _i = Buffer.clear buf
