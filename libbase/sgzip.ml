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
    @author Frederic Ye
**)

(* Same as Gzip (Libzip) but uses a string instead of a stream *)


exception Error of string

let buffer_size = 1024

let char_buffer = String.create 1

type out_channel =
  { out_buffer: string;
    mutable out_pos: int;
    mutable out_avail: int;
    out_stream: Zlib.stream;
    mutable out_string: string;
    mutable out_size: int32;
    mutable out_crc: int32;
    only_deflate : bool }

let output_byte oz b =
  oz.out_string <- Printf.sprintf "%s%c" oz.out_string (Char.unsafe_chr b)

let open_out ?(level = 6) ?(only_deflate=false) () =
  if level < 1 || level > 9 then invalid_arg "Gzip.open_out: bad level";
  let oz = {
    out_buffer = String.create buffer_size;
    out_pos = 0;
    out_avail = buffer_size;
    out_stream = Zlib.deflate_init level false;
    out_string = "";
    out_size = Int32.zero;
    out_crc = Int32.zero;
    only_deflate = only_deflate } in
  (* Write minimal header *)
  if not only_deflate then (
    output_byte oz 0x1F;                  (* ID1 *)
    output_byte oz 0x8B;                  (* ID2 *)
    output_byte oz 8;                     (* compression method *)
    output_byte oz 0;                     (* flags *)
    for i = 1 to 4 do output_byte oz 0 done; (* mtime *)
    output_byte oz 0;                     (* xflags *)
    output_byte oz 0xFF;                  (* OS (unknown) *)
  );
  oz

let rec output oz buf pos len =
  if pos < 0 || len < 0 || pos + len > String.length buf then
    invalid_arg "Gzip.output";
  (* If output buffer is full, flush it *)
  if oz.out_avail = 0 then begin
    oz.out_string <- Printf.sprintf "%s%s" oz.out_string (String.sub oz.out_buffer 0 oz.out_pos);
    oz.out_pos <- 0;
    oz.out_avail <- String.length oz.out_buffer
  end;
  let (_, used_in, used_out) =
    try
      Zlib.deflate oz.out_stream buf pos len
                                 oz.out_buffer oz.out_pos oz.out_avail
                                 Zlib.Z_NO_FLUSH
    with Zlib.Error(_, _) ->
      raise (Error("error during compression")) in
  oz.out_pos <- oz.out_pos + used_out;
  oz.out_avail <- oz.out_avail - used_out;
  if not oz.only_deflate then (
    oz.out_size <- Int32.add oz.out_size (Int32.of_int used_in);
    oz.out_crc <- Zlib.update_crc oz.out_crc buf pos used_in;
  );
  if used_in < len then output oz buf (pos + used_in) (len - used_in)

let output_char oz c =
  char_buffer.[0] <- c;
  output oz char_buffer 0 1

let output_byte oz b =
  output_char oz (Char.unsafe_chr b)

let write_int32 oz n =
  let r = ref n in
  for i = 1 to 4 do
    oz.out_string <- Printf.sprintf "%s%c" oz.out_string (Char.unsafe_chr (Int32.to_int !r));
    r := Int32.shift_right_logical !r 8
  done

let flush oz =
  let rec do_flush () =
    (* If output buffer is full, flush it *)
    if oz.out_avail = 0 then begin
      oz.out_string <- Printf.sprintf "%s%s" oz.out_string (String.sub oz.out_buffer 0 oz.out_pos);
      oz.out_pos <- 0;
      oz.out_avail <- String.length oz.out_buffer
    end;
    let (finished, _, used_out) =
      Zlib.deflate oz.out_stream oz.out_buffer 0 0
                                 oz.out_buffer oz.out_pos oz.out_avail
                                 Zlib.Z_FINISH in
    oz.out_pos <- oz.out_pos + used_out;
    oz.out_avail <- oz.out_avail - used_out;
    if not finished then do_flush() in
  do_flush();
  (* Final data flush *)
  if oz.out_pos > 0 then
    oz.out_string <- Printf.sprintf "%s%s" oz.out_string (String.sub oz.out_buffer 0 oz.out_pos);
  (* Write CRC and size *)
  if not oz.only_deflate then (
    write_int32 oz oz.out_crc;
    write_int32 oz oz.out_size;
  );
  (* Dispose of stream *)
  Zlib.deflate_end oz.out_stream

let close_out oz =
  flush oz
