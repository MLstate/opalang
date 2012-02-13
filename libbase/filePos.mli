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
   Getting line number and pos in line from a global char offset in a file.

   @author Rudy Sicard
   @author David Rajchenbach-Teller
   @author Mathieu Barbin
*)

(**
   This implementation works with a cache mechanism.
   You should first store files, and then you can perform some call to the API.

   Originally, these functionnalities was dispesed and diverging in

   + Trx_runtime
   + QmlLoc
   + OpaParser

   This module is there to group once for all, all functionnality related to
   errors reporting from a source file, with line location, and nice colored citations.

   TODO(proposition):
   add a support in the citation part to cache what citation have already be done.
   and a function to reset this cache, to correct the probleme of multiple citation.
*)

(** {6 Alias for common types} *)

(** Just for the lisibility of the interface *)
(** *)
type filename = string
type content = string
type absolute_char_offset = int
type line_number = int
type column_number = int

(** {6 Caching mechanisme} *)

(** [add_file filename content] Add a file to parsed files *)
val add_file : filename -> content -> unit

(** [get_file_content filename] Returns content of the file if is present.
    @raise Not_found if the file in not in the cache table *)
val get_file_content : filename -> content

(** Get (line number, pos in line) from a filename and an absolute position.
    @raise Not_found if the file is not in the cache table *)
val get_pos : filename -> absolute_char_offset -> line_number * column_number

(** Given line and column number produces a string of the shape "line XX, column XX" *)
val get_pos_string : line_number * column_number -> string

(** Get (line number, pos in line) for a given file content.
    WARNING  The content will not be cached, so use only for single requests *)
val get_pos_no_cache : content -> absolute_char_offset -> line_number * column_number

(** Get the offset of the current line. This is the identity if no newline are detected after *)
val get_line : filename -> absolute_char_offset -> absolute_char_offset * line_number

(** Get the offset of the next newline. This is the identity if no newline are detected after *)
val get_next_line : filename -> absolute_char_offset -> absolute_char_offset * line_number

(**
   Get the global begin and end offsets from the line number
   @raise Not_found if no such line, or if the file is not cached
*)
val line_position : filename -> line_number -> absolute_char_offset * absolute_char_offset

(** remove a file from the cached files *)
val uncache : filename -> unit

(** remova all files from the cached files *)
val clear : unit -> unit

(** {6 Position tracking} *)

(**
   A segment in a file
*)
type range = {
  start : absolute_char_offset ;
  stop : absolute_char_offset ;
}

(**
   Several segments in a file.
   Assert : if you use only the API for building pos (private),
   you can be sure that there is no overlap between ranges
*)
type filerange = {
  filename : filename ;
  ranges : range HdList.t
}

(**
   The type of a position in source code.

   + [Builtin] : No position, inserted by the compiler (precise the pass)
   Any error at [Builtin] is the result of an internal error and should be treated as a failure.
   + [Files] : This come for at least one file, with at least one range.

   The private cache is used for speeding up some redondant request
*)
type private_cache

type pos = private
  | Builtin of string
  | Files of filerange HdList.t * private_cache

(**
   Build a new pos from nothing. Use to replace the Builtin value,
   you should at least say what pass insert this position.
*)
val nopos : string -> pos

(**
   Get at least one of filenames present in a pos.
   returns ["builtin_pass"] if there is no files
*)
val get_file : pos -> filename

(**
   Get one location of the pos.
   returns ["builtin_pass", 0] if there is no files
*)
val get_one_loc : pos -> filename * line_number

(**
   Get the start of the first filename found in a pos.
   returns [0] if there is no file in the pos
*)
val get_first_char : pos -> absolute_char_offset

(**
   Build a new pos from a filename and a range.
   @raise Invalid argument if stop < start
*)
val make_pos : filename -> absolute_char_offset -> absolute_char_offset -> pos

(**
   In some cases, you do not have the global offset,
   but just the line number. Build a position corresponding
   to the full line.
*)
val make_pos_from_line : filename -> line_number -> pos

(**
   Merge ranges from filenames.
   The positions should contain only exactly one range and merging
   just gives you the position containing the smallest range covering
   them
*)
val merge_pos_for_parser : pos -> pos -> pos

(**
   Merge ranges from filenames.
   If there is at least one filename, there no more Builtin are preserved.
*)
val merge_pos : pos -> pos -> pos
val merge_pos_list : pos list -> pos

(**
   Return [true] only if there is no filename in the pos. (Builtin)
*)
val is_empty : pos -> bool

(**
   Build a string regrouping the location present in the pos.
   The syntax is emacs tuareg-mode frienly.
   There is just one difference, [FilePos] count char index from [0], where
   emacs count from [1] for global chars. For line number, and column, it
   is the same convention.
*)
val to_string : pos -> string

(**
   The same function, with a pp interface.
   @deprecated, use [pp] instead
*)
val pp_pos : Format.formatter -> pos -> unit

(**
   Printer for pos, with a format interface.
*)
val pp : Format.formatter -> pos -> unit

(**
   Print the list of files in a position.
*)
val pp_files : Format.formatter -> pos -> unit


(** {6 Deprecated API} *)

(**
   Used some how by the old qml, will be delated as soon as we can.
   DO NOT USE IT IN NEW CODE
*)
val to_old_pos_many : pos -> (int*int) StringMap.t
val to_old_pos :  (unit -> string * int * int) -> pos ->  string *int * int


(** {6 Citations} *)

(**
   This is used for printing citation of the source code from a position.
   It is possible to parametrize the citation with some parameters.

   {[
   Citation:

   x =
     y = 5 + "toto"
             ^^^^^^
     z = y * 5
     z + y

   This expression has type string
   but the context expected an expression of type int
   ]}

*)

(**
   Parameters are :
   + [truncate_lines] : by printing the citation, truncate the lines with a length greater than the parameter.
   by default, it is [Some 80]. Principally, it avoid that the message is not visible. The lines containing
   the range to highlight are not truncated.
   + [length_before] : the maximal number of lines to print before the first line contained in the range
   + [length_after] : the maximal number of lines to print after the first line contained in the range
   default is [5]
   + [length_between] : the maximal number of lines to print between to range of the file.
   default is [5]
   + [color] choose the color for highlighting the range. Default is [Some `red].
   + [max_length_citation] eventually truncate the highlighted range if too long. Default is [Some 200].
   It replace the midle part of the range by ["[...]"].
*)
type options = {
  truncate_lines : int option ;
  lines_before : int ;
  lines_after : int ;
  lines_between : int ;
  color : Ansi.color option ;
  max_length_citation : int option ;
}

val default_options : options

(** The function to print a citation *)
val citation : ?options:options -> Format.formatter -> pos -> unit

(** The default citation for using it directly in Format.fprintf *)
val pp_citation : Format.formatter -> pos -> unit

(** print the contents of the private table *)
val debug : unit -> unit
