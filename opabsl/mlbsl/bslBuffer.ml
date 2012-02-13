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
(**
 * A low-level module of mutable string buffers.
 *
 * Values of type [buffer] are not and should not be serialized, so any use of buffer must be strictly controlled
 *
 *
 * @author David Rajchenbach-Teller
 *)

##extern-type Buffer_private.buffer = Buffer.t

##register create\ `Buffer.create`: int -> Buffer_private.buffer
##register append\ `Buffer.add_string`: Buffer_private.buffer, string -> void
##register contents\ `Buffer.contents`: Buffer_private.buffer -> string
