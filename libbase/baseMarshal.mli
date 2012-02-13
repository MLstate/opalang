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
type extern_flags = Marshal.extern_flags
val to_channel : Pervasives.out_channel -> 'a -> extern_flags list -> unit
val to_string : 'a -> extern_flags list -> string
val to_buffer : string -> int -> int -> 'a -> extern_flags list -> int
val from_channel : Pervasives.in_channel -> 'a
val from_string : string -> int -> 'a
val header_size : int
val data_size : string -> int -> int
val total_size : string -> int -> int

(** marshal/unmarshal from a filename
    these functions remove any function from the serialized structure
    so that you can serialize them and pretend to unserialize them
    with a different binary
    (but of course trying to use these functions anyway will result in
    an error)
*)
val marshal_no_fun : out_channel -> 'a -> unit
val unmarshal_no_fun : in_channel -> 'a
