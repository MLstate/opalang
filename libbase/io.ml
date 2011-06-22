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


(**)

module String = BaseString

exception Corruption

module Compr =
struct
  let compress_string ?(level = 6) ?(header = true) inbuf =
    let length = String.length inbuf in
    let outbuf = String.create length in
    let zs = Zlib.deflate_init level header in
    let (_, _(*used_in*), used_out) = Zlib.deflate zs inbuf 0 length outbuf 0 length Zlib.Z_NO_FLUSH in
    let rec aux finished used_out =
      Printf.eprintf "aux_compr: used_out = %d\n" used_out;
      if finished then used_out
      else
        let (finished, _, add) = Zlib.deflate zs inbuf 0 0 outbuf used_out (length - used_out) Zlib.Z_FINISH in
        aux finished (used_out + add)
    in
    let used_out = aux false used_out in
    Zlib.deflate_end zs ;
    String.sub outbuf 0 used_out (* FIXME: utiliser StringX *)

  let decompress_string ?(header = true) inbuf =
    let length = String.length inbuf in
    let outbuf = String.create length in
    let zs = Zlib.inflate_init header in
    let (_, _(*used_in*), used_out) = Zlib.inflate zs inbuf 0 length outbuf 0 length Zlib.Z_SYNC_FLUSH in
    let rec aux first finished used_out =
      Printf.eprintf  "aux_uncompr: used_out = %d\n" used_out;
      if finished then used_out
      else
        let dummy_byte = if first && not header then 1 else 0 in
        let (finished, _, add) = Zlib.inflate zs inbuf length dummy_byte outbuf used_out (length - used_out) Zlib.Z_SYNC_FLUSH in
        aux false finished (used_out + add)
    in
    let used_out = aux true false used_out in
    Zlib.inflate_end zs ;
    String.sub outbuf 0 used_out

  let compr source =
    let sourcel = String.length source in
    let refill =
      let pos = ref 0 in
      fun buf ->
        let l = String.length buf in
        let nb = min l (sourcel - !pos) in
        String.blit source !pos buf 0 nb ; (* TODO: unsafe *)
        pos := !pos + nb ;
        nb
    in
    let b = Buffer.create (sourcel / 2) in
    Zlib.compress refill (fun buf len -> Buffer.add_substring b buf 0 len) ;
    b

  let decompr source =
    let sourcel = String.length source in
    let refill =
      let pos = ref 0 in
      fun buf ->
        let l = String.length buf in
        let nb = min l (sourcel - !pos) in
        String.blit source !pos buf 0 nb ; (* TODO: unsafe *)
        pos := !pos + nb ;
        nb
    in
    let b = Buffer.create (sourcel / 2) in
    Zlib.uncompress refill (fun buf len -> Buffer.add_substring b buf 0 len) ;
    b
end

type crypto_key

module Write : sig
  type t
  type mode = [`append | `create | `normal]
(*   type stream_mode = { compressed : bool ; encrypted : bool } *)
(*   val set_compressed : t -> unit *)
(*   val set_uncompressed : t -> unit *)
(*   val set_encrypted : t -> crypto_key -> unit *)
(*   val set_unencrypted : t -> unit *)
  val make : ?mode:mode -> ?rights:int -> ?size:int -> string -> t

  (** Write operations: {b these are asynchronous} and only write in a buffer.
      Use [output] to flush ([close] {b doesn't} do it automatically) *)
  (** FIXME: int is cast to int32 !! OUCH ! But we need to discuss breaking
      binary compatibility before changing the format :/ *)
  val add_bool : t -> bool -> unit
  val add_int_32 : t -> int32 -> unit
  val add_int : t -> int -> unit
  val add_int_64 : t -> int64 -> unit
  val add_string : t -> string -> unit
  val add_char : t -> char -> unit
  val add_float : t -> float -> unit
  val add_something : t -> 'a -> unit
  val add_list : t -> (t -> 'a -> unit) -> 'a list -> unit
  val position : t -> int
  val output : t -> unit
  val close : t -> unit
  val seek : t -> int -> unit
end =
struct
  type t =
      { oc : out_channel
      ; b : Buffer.t }
  type mode = [`append | `create | `normal]
  type stream_command = [`compressed | `encrypted | `uncompressed | `clear]

  let make ?(mode=`normal) ?(rights=File.default_rights) ?(size=256) file =
    let open_f = match mode with
    | `normal -> open_out_bin
    | `append -> open_out_gen [Open_wronly; Open_binary; Open_append] rights
    | `create -> open_out_gen [Open_wronly; Open_binary; Open_creat; Open_trunc] rights
    in
    { oc = open_f file
    ; b = Buffer.create size }

  let add_int_32 w i =
    let i = ref i in
    for c = 0 to 3 do
      Buffer.add_char w.b (char_of_int (Int32.to_int (Int32.shift_right_logical !i 24))) ;
      i := Int32.shift_left !i 8
    done
  let add_int w i = add_int_32 w (Int32.of_int i)
  let add_int_64 w i =
    let i = ref i in
    for c = 0 to 7 do
      Buffer.add_char w.b (char_of_int (Int64.to_int (Int64.shift_right_logical !i (64-8)))) ;
      i := Int64.shift_left !i 8
    done
  let add_string w s =
    add_int w (String.length s) ;
    Buffer.add_string w.b s

  let seek w p = seek_out w.oc p

  (* written in reverse *)
  let add_list w f l =
    add_int w (List.length l) ;
    List.iter (f w) (List.rev l)
  let add_char w c =
    Buffer.add_char w.b c
  let add_bool w b =
    add_char w (if b then '\001' else '\000')
  let add_string_z w s =
    let r = Compr.compr s in
    add_string w (Buffer.contents r)
  let add_float w f =
    let n = ref (Int64.bits_of_float f) in
    for i = 0 to 7 do
      Buffer.add_char w.b (char_of_int (Int64.to_int (Int64.shift_right_logical !n 56))) ;
      n := Int64.shift_left !n 8
    done
  let add_something_z w (v:'a) =
    add_string_z w (Marshal.to_string v [])
  let add_something w (v:'a) =
    add_string w (Marshal.to_string v [])
  let contents_z w = Buffer.contents (Compr.compr (Buffer.contents w.b))
  let output w =
    (*     let r = Compr.compr (contents w.b) in *)
    output w.oc (Buffer.contents w.b) 0 (Buffer.length w.b) ;
    flush w.oc ;
    Buffer.clear w.b

  let position w = out_channel_length w.oc + Buffer.length w.b
  let close w = close_out w.oc
end


(* let output_value_z oc (data:'a) = *)
(*   let r = Compr.compr (Marshal.to_string data []) in *)
(*   output_value oc r *)

(* let input_value_z ic = *)
(*   let r = input_value ic in *)
(*   Marshal.from_string (Compr.decompr r) 0 *)

(* FIXME: ajouter compression, ou checksum, ou cryptage...
   d'une façon globale / Faire en Monadique intégral


FIXME: pb si le fichier est supprimé ou raccourci en cours de lecture...
*)
module Read : sig
  type t
(*   val set_crypto_key : t -> crypto_key -> t *)
  val length : t -> int
  val pos : t -> int
  val eof : t -> bool
  val make : ?buffer_size:int -> ?rights:int -> (* ~filename: *)string -> t
  val close : t -> unit
  val seek : t -> int -> unit
  val read_char : t -> char
  val read_bool : t -> bool
  val read_int : t -> int
  val read_int_32 : t -> Int32.t
  val read_int_64 : t -> Int64.t
  val read_float : t -> float
  val read_string : t -> string
(*   val read_string_z : t -> string *)
(*   val read_something_z : t -> 'a *)
  val read_something : t -> 'a
  val read_list : t -> (t -> 'a) -> 'a list
  val read_bytes : t -> int -> string
end =
struct
  type t =
      { ff : in_channel
      ; mutable pos : int (* position du début du tableau (inclus) *)
      ; mutable offset : int  (* position dans le tableau *)
      ; mutable endpos : int (* position de la fin du tableau (inclus) *)
      ; buffer_size : int
(*       ; maxpos : int *)
      ; b : string }
  (** lecture de l OCTETS *)
  let read t =
    let p = pos_in t.ff in
(*     Printf.eprintf "read PRE current=%d t.pos=%d t.endpos=%d t.offset=%d\n" p t.pos t.endpos t.offset ; *)
    let n = input t.ff t.b 0 t.buffer_size in
    (* if n = 0 then Base.warning "Read.read: unexpected end of file. Perhaps a wrong path to a file?" ; *)
    t.pos <- p ;
    t.offset <- 0 ;
    t.endpos <- p + n - 1
(*     Printf.eprintf "read POST current=%d t.pos=%d t.endpos=%d t.offset=%d\n" p t.pos t.endpos t.offset *)
    (* Printf.eprintf "read done" *)
  let seek t p =
(*     Printf.eprintf  "seek pos=%d t.pos=%d t.endpos=%d t.offset=%d\n" p t.pos t.endpos t.offset ;      *)
    seek_in t.ff p ;
    if p < t.pos or p > t.endpos then read t
    else t.offset <- p - t.pos
  let make ?(buffer_size=4096) ?(rights=File.default_rights) f =
    let ff = open_in_gen [Open_rdonly; Open_binary] rights f in
(*     let l = in_channel_length ff in *)
    { ff = ff
    ; pos = 0
    ; offset = 0
    ; endpos = -1
(*     ; maxpos = l *)
    ; buffer_size = buffer_size
    ; b = String.create buffer_size }
  let length t = (* t.maxpos *) in_channel_length t.ff (* FIXME: temporel *)
  let pos t = t.pos + t.offset
  let eof t = pos t = length t
  let close t = close_in t.ff
  let read_char t =
    let p = pos t in
    if p >= length t then raise End_of_file ;
    if p > t.endpos then read t ;
    let r = t.b.[t.offset] in (* FIXME: unsafe ? / prouver la correction d'abord *)
    t.offset <- succ t.offset ;
    r
  let read_bool t =
    match read_char t with
    | '\000' -> false
    | '\001' -> true
    | _ -> raise Corruption
  let read_int_32 t =
    let i = ref Int32.zero in
    for c = 0 to 3 do
      let b = int_of_char (read_char t) in
      i := Int32.logor (Int32.shift_left !i 8) (Int32.of_int b)
    done ;
    !i
  let read_int_64 t =
    let i = ref Int64.zero in
    for c = 0 to 7 do
      let b = int_of_char (read_char t) in
      i := Int64.logor (Int64.shift_left !i 8) (Int64.of_int b)
    done ;
    !i
  let read_int t =
    Int32.to_int (read_int_32 t)
  let read_bytes t l =
    if l >= Sys.max_string_length then
      failwith (Printf.sprintf "read_bytes: limit (%d > %d) exceeded" l Sys.max_string_length)
    else
      String.init l (fun _ -> read_char t)
  let read_string t =
    read_bytes t (read_int t)
  let read_string_z t =
    Buffer.contents (Compr.decompr (read_string t))
  let read_something_z t =
    Marshal.from_string (read_string_z t) 0
  let read_something t =
    Marshal.from_string (read_string t) 0
  let read_float t =
    Int64.float_of_bits (read_int_64 t)
  let read_list t f =
    let l = read_int t in
    let rec aux acc n =
      if n = 0 then acc
      else
        aux ((f t) :: acc) (pred n)
    in aux [] l
end
