(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
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
