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
val put_len : char -> char -> char -> char -> int -> string
val get_len_ic : char -> char -> char -> char -> char -> in_channel -> int
val encode_key : Keys.t -> string
val decode_key : string -> int -> int * Keys.t
val decode_key_ic : in_channel -> Keys.t
val encode_path : Path.t -> string
val decode_path : string -> int -> int * Path.t
val encode_dataimpl : DataImpl.data -> string
val decode_dataimpl : string -> int -> int * DataImpl.data
val encode_datas : Datas.ns -> string
val decode_datas : string -> int -> int * Datas.ns
val encode_node : Node_light.t -> string
val decode_node : string -> int -> int * Node_light.t
val encode_kld : Keys.t list * Datas.ns -> string
val decode_kld : string -> int -> int * (Keys.t list * Datas.ns)
val encode_kln : Keys.t list * Node_light.t -> string
val decode_kln : string -> int -> int * (Keys.t list * Node_light.t)
