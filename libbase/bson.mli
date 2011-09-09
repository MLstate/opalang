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

(**
   Bson:  Module to allow the creation of BSON values in strings.

   You use module Append to create the values:

     let buf = Append.init ?hint () in
     Append.int buf 123;
     <etc...>
     Append.finish buf

   Use Iterator to scan BSON values:

     let iter = Iterator.init buf;
     <or...>
     let iter = Iterator.from_buffer str;
     let code = Iterator.next iter in
     let key = Iterator.key iter in
     match code with
     | c when c = el_int ->
       let i = Iterator.int iter in
       <...>

   There is a second Iterator module, IteratorSS which is founded on
   BaseStringSlice which may be more efficient on deeply-nested
   BSON objects (sub is a unit-time operation but note that the
   returned sub-string will not be a copy, updating the sub-string
   will cause the parent string to be altered, too).

**)

module type S_sig =
  sig
    type t
    val empty : t
    val length : t -> int
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val create : int -> t
    val make : int -> char -> t
    val copy : t -> t
    val sub : t -> int -> int -> t
    val fill : t -> int -> int -> char -> unit
    val blit : t -> int -> t -> int -> int -> unit
    val concat : t -> t list -> t
    val iter : (char -> unit) -> t -> unit
    val escaped : t -> t
    val index : t -> char -> int
    val rindex : t -> char -> int
    val index_from : t -> int -> char -> int
    val rindex_from : t -> int -> char -> int
    val contains : t -> char -> bool
    val contains_from : t -> int -> char -> bool
    val rcontains_from : t -> int -> char -> bool
    val uppercase : t -> t
    val lowercase : t -> t
    val capitalize : t -> t
    val uncapitalize : t -> t
    val compare : t -> t -> int
    val unsafe_get : t -> int -> char
    val unsafe_set : t -> int -> char -> unit
    val unsafe_blit : t -> int -> t -> int -> int -> unit
    val unsafe_fill : t -> int -> int -> char -> unit
    val to_string : t -> string
    val of_string : string -> t
    val export : t -> string * int * int
    val import : string * int * int -> t
    val widen : t -> unit
    val normalize : t -> t
    val real_size : t -> int
    val set_size : t -> int -> t
    val rebase : t -> unit
    val unsafe_sub : t -> int -> int -> t
  end

module S : S_sig with type t = string
module SS : S_sig with type t = BaseStringSlice.t

val el_eoo : char
val el_double : char
val el_string : char
val el_object : char
val el_array : char
val el_bindata : char
val el_undefined : char
val el_oid : char
val el_bool : char
val el_date : char
val el_null : char
val el_regex : char
val el_dbref : char
val el_code : char
val el_symbol : char
val el_codewscope : char
val el_int : char
val el_timestamp : char
val el_long : char
val el_minkey : char
val el_maxkey : char
val st_bin_binary : char
val st_bin_func : char
val st_bin_binary_old : char
val st_bin_uuid : char
val st_bin_md5 : char
val st_bin_user : char

type buf = {
  buf : Buf.buf;
  mutable stack : int list;
  mutable finished : bool;
}

module Oid :
  sig
    val from_string : string -> S.t
    val to_string : string -> S.t
    val counter : int ref
    val gen : unit -> S.t
    val generated_time : S.t -> Time.t
  end

module Append :
  sig
    val init : ?hint:int -> unit -> buf
    val empty : buf
    val size : buf -> int
    val estart : buf -> char -> S.t -> unit
    val int : buf -> S.t -> int -> unit
    val long : buf -> S.t -> int -> unit
    val double : buf -> S.t -> float -> unit
    val bool : buf -> S.t -> bool -> unit
    val null : buf -> S.t -> unit
    val undefined : buf -> S.t -> unit
    val string_base : buf -> S.t -> S.t -> int -> char -> unit
    val string : buf -> S.t -> S.t -> unit
    val symbol : buf -> S.t -> S.t -> unit
    val code : buf -> S.t -> S.t -> unit
    val string_n : buf -> S.t -> S.t -> int -> unit
    val symbol_n : buf -> S.t -> S.t -> int -> unit
    val code_n : buf -> S.t -> S.t -> int -> unit
    val code_w_scope_n : buf -> S.t -> S.t -> int -> buf -> unit
    val code_w_scope : buf -> S.t -> S.t -> buf -> unit
    val start_codewscope : buf -> S.t -> S.t -> unit
    val finish_codewscope : buf -> S.t -> unit
    val binary : buf -> S.t -> char -> S.t -> int -> unit
    val oid : buf -> S.t -> S.t -> unit
    val new_oid : buf -> S.t -> unit
    val regex : buf -> S.t -> S.t -> S.t -> unit
    val bson : buf -> S.t -> buf -> unit
    val timestamp : buf -> S.t -> int * int -> unit
    val date : buf -> S.t -> int -> unit
    val time_t : buf -> S.t -> Time.t -> unit
    val start_object : buf -> S.t -> unit
    val start_array : buf -> S.t -> unit
    val finish_object : buf -> unit
    val finish_array : buf -> unit
    val finish : buf -> unit
    val get : buf -> S.t
  end

module type Iterator_sig =
  sig
    module S : S_sig
    type iter = { ibuf : S.t; mutable pos : int; mutable first : bool; }
    val init : buf -> iter
    val from_buffer : S.t -> iter
    val iterator_type : iter -> char
    val key : iter -> S.t
    val value : iter -> int
    val int_raw : iter -> int
    val long_raw : iter -> int
    val double_raw : iter -> float
    val bool_raw : iter -> bool
    val oid : iter -> S.t
    val string : ?offset:int -> iter -> S.t
    val symbol : ?offset:int -> iter -> S.t
    val bslen : S.t -> int -> int
    val cstring : ?offset:int -> iter -> S.t
    val string_len : iter -> int
    val int : iter -> int
    val long : iter -> int
    val double : iter -> float
    val timestamp : iter -> int * int
    val bool : iter -> bool
    val code : iter -> S.t
    val code_scope : iter -> buf
    val date : iter -> int
    val time_t : iter -> Time.t
    val bin_type : iter -> char
    val bin_len : iter -> int
    val bin_data : iter -> S.t
    val regex : iter -> S.t
    val regex_opts : iter -> S.t
    val subobject : iter -> buf
    val subiterator : iter -> iter
    val next : iter -> char
    val find : buf -> S.t -> iter * char
  end

module IteratorF(S : S_sig) : Iterator_sig with module S = S
module Iterator : Iterator_sig with module S = S
module IteratorSS : Iterator_sig with module S = SS

(*module Element :
sig
  val element : buf -> S.t option -> Iterator.iter -> unit
end*)

module Print :
sig
  val print : buf -> unit
  val print_raw : S.t -> int -> int -> unit
end

