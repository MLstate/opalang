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
(** original signature (minus a few things) *)
val open_box : int -> unit
val close_box : unit -> unit
val print_string : string -> unit
val print_as : int -> string -> unit
val print_int : int -> unit
val print_float : float -> unit
val print_char : char -> unit
val print_bool : bool -> unit
val print_space : unit -> unit
val print_cut : unit -> unit
val print_break : int -> int -> unit
val print_flush : unit -> unit
val print_newline : unit -> unit
val force_newline : unit -> unit
val print_if_newline : unit -> unit
val set_margin : int -> unit
val get_margin : unit -> int
val set_max_indent : int -> unit
val get_max_indent : unit -> int
val set_max_boxes : int -> unit
val get_max_boxes : unit -> int
val over_max_boxes : unit -> bool
val open_hbox : unit -> unit
val open_vbox : int -> unit
val open_hvbox : int -> unit
val open_hovbox : int -> unit
val open_tbox : unit -> unit
val close_tbox : unit -> unit
val print_tbreak : int -> int -> unit
val set_tab : unit -> unit
val print_tab : unit -> unit
val set_ellipsis_text : string -> unit
val get_ellipsis_text : unit -> string
type tag = string
val open_tag : tag -> unit
val close_tag : unit -> unit
val set_tags : bool -> unit
val set_print_tags : bool -> unit
val set_mark_tags : bool -> unit
val get_print_tags : unit -> bool
val get_mark_tags : unit -> bool
val set_formatter_out_channel : out_channel -> unit
val set_formatter_output_functions :
  (string -> int -> int -> unit) -> (unit -> unit) -> unit
val get_formatter_output_functions :
  unit -> (string -> int -> int -> unit) * (unit -> unit)
type formatter_tag_functions = Format.formatter_tag_functions = {
  mark_open_tag : tag -> string;
  mark_close_tag : tag -> string;
  print_open_tag : tag -> unit;
  print_close_tag : tag -> unit;
}
val set_formatter_tag_functions :
  formatter_tag_functions -> unit
val get_formatter_tag_functions :
  unit -> formatter_tag_functions
val set_all_formatter_output_functions :
  out:(string -> int -> int -> unit) ->
  flush:(unit -> unit) ->
  newline:(unit -> unit) ->
  spaces:(int -> unit) ->
  unit
val get_all_formatter_output_functions :
  unit ->
  (string -> int -> int -> unit) *
    (unit -> unit) *
    (unit -> unit) *
    (int -> unit)
type formatter = Format.formatter
val formatter_of_out_channel : out_channel -> formatter
val std_formatter : formatter
val err_formatter : formatter
val formatter_of_buffer : Buffer.t -> formatter
val stdbuf : Buffer.t
val str_formatter : formatter
val flush_str_formatter : unit -> string
val make_formatter :
  (string -> int -> int -> unit) -> (unit -> unit) -> formatter
val pp_open_hbox : formatter -> unit -> unit
val pp_open_vbox : formatter -> int -> unit
val pp_open_hvbox : formatter -> int -> unit
val pp_open_hovbox : formatter -> int -> unit
val pp_open_box : formatter -> int -> unit
val pp_close_box : formatter -> unit -> unit
val pp_open_tag : formatter -> string -> unit
val pp_close_tag : formatter -> unit -> unit
val pp_print_string : formatter -> string -> unit
val pp_print_as : formatter -> int -> string -> unit
val pp_print_int : formatter -> int -> unit
val pp_print_float : formatter -> float -> unit
val pp_print_char : formatter -> char -> unit
val pp_print_bool : formatter -> bool -> unit
val pp_print_break : formatter -> int -> int -> unit
val pp_print_cut : formatter -> unit -> unit
val pp_print_space : formatter -> unit -> unit
val pp_force_newline : formatter -> unit -> unit
val pp_print_flush : formatter -> unit -> unit
val pp_print_newline : formatter -> unit -> unit
val pp_print_if_newline : formatter -> unit -> unit
val pp_open_tbox : formatter -> unit -> unit
val pp_close_tbox : formatter -> unit -> unit
val pp_print_tbreak : formatter -> int -> int -> unit
val pp_set_tab : formatter -> unit -> unit
val pp_print_tab : formatter -> unit -> unit
val pp_set_tags : formatter -> bool -> unit
val pp_set_print_tags : formatter -> bool -> unit
val pp_set_mark_tags : formatter -> bool -> unit
val pp_get_print_tags : formatter -> unit -> bool
val pp_get_mark_tags : formatter -> unit -> bool
val pp_set_margin : formatter -> int -> unit
val pp_get_margin : formatter -> unit -> int
val pp_set_max_indent : formatter -> int -> unit
val pp_get_max_indent : formatter -> unit -> int
val pp_set_max_boxes : formatter -> int -> unit
val pp_get_max_boxes : formatter -> unit -> int
val pp_over_max_boxes : formatter -> unit -> bool
val pp_set_ellipsis_text : formatter -> string -> unit
val pp_get_ellipsis_text : formatter -> unit -> string
val pp_set_formatter_out_channel : formatter -> out_channel -> unit
val pp_set_formatter_output_functions :
  formatter -> (string -> int -> int -> unit) -> (unit -> unit) -> unit
val pp_get_formatter_output_functions :
  formatter -> unit -> (string -> int -> int -> unit) * (unit -> unit)
val pp_set_all_formatter_output_functions :
  formatter -> out:(string -> int -> int -> unit) -> flush:(unit -> unit) ->
  newline:(unit -> unit) -> spaces:(int -> unit) -> unit
val pp_get_all_formatter_output_functions :
  formatter -> unit ->
  (string -> int -> int -> unit) * (unit -> unit) * (unit -> unit) *
    (int -> unit)
val pp_set_formatter_tag_functions :
  formatter -> formatter_tag_functions -> unit
val pp_get_formatter_tag_functions :
  formatter -> unit -> formatter_tag_functions
val fprintf : formatter -> ('a, formatter, unit) format -> 'a
val printf : ('a, formatter, unit) format -> 'a
val eprintf : ('a, formatter, unit) format -> 'a
val bprintf : Buffer.t -> ('a, formatter, unit) format -> 'a
val kfprintf : (formatter -> 'a) -> formatter ->
  ('b, formatter, unit, 'a) format4 -> 'b
val ifprintf : formatter -> ('a, formatter, unit) format -> 'a

(** new things *)
type 'a pprinter = Format.formatter -> 'a -> unit

val pp_fmt : ('a, Format.formatter, unit) format -> Format.formatter -> 'a

val pp_fst : 'a pprinter -> ('a * 'b) pprinter
val pp_snd : 'b pprinter -> ('a * 'b) pprinter

(**
   [pp_list sep pp_a a_list]
   A exemple of use, via Format
   {[
   Format.fprintf fmt "[ %a ]" (LangPrint.pp_list ";@ " pp_a) a_list
   ]}
*)
val pp_list :
  (unit, formatter, unit) format ->
  ?singleton:('b pprinter) ->
  'b pprinter ->
  'b list pprinter

(**
   Because [Format.sprintf] is no good.

   This function build a string from pp_printer.

   <!> use for small string.
   There is also a implementation using [FBfufer] for large strings.

   <!> beware, in the majority of cases, you are not supposed to produce
   strings during the compilation (rather generate the string directly in
   the correct channel)
*)
val sprintf :
  ('a, Format.formatter, unit, string) format4 -> 'a

(**
   Because [Format.ksprintf] is no good at least as much as [Format.sprintf]
*)
val ksprintf :
  (string -> 'result) ->
  ('a, Format.formatter, unit, 'result) format4 -> 'a

(**
   For backward compatibility.
   Should be removed one day. :)
*)
val to_string : 'a pprinter -> 'a -> string
