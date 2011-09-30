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
(** {4 From OCaml Standard Library} *)
(**
   This module includes the standard one
   @see <http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html> Ocaml Manual
*)
(** *)
external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : string -> int -> char -> unit = "%string_safe_set"
val make : int -> char -> string
val copy : string -> string
val sub : string -> int -> int -> string
val fill : string -> int -> int -> char -> unit
val blit : string -> int -> string -> int -> int -> unit
val concat : string -> string list -> string
val iter : (char -> unit) -> string -> unit
val escaped : string -> string
val index : string -> char -> int
val rindex : string -> char -> int
val index_from : string -> int -> char -> int
val rindex_from : string -> int -> char -> int
val contains : string -> char -> bool
val contains_from : string -> int -> char -> bool
val rcontains_from : string -> int -> char -> bool
val uppercase : string -> string
val lowercase : string -> string
val capitalize : string -> string
val uncapitalize : string -> string
type t = string
val compare : t -> t -> int
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit
  = "%string_unsafe_set"
external unsafe_blit : string -> int -> string -> int -> int -> unit
  = "caml_blit_string" "noalloc"
external unsafe_fill : string -> int -> int -> char -> unit
  = "caml_fill_string" "noalloc"
val create : int -> string

(** {4 MLstate String} *)

(**
   Classification (depending on the kind of returned type) :

   + Allocation (returns a string)
   + Iterators (e.g. fold)
   + Condition (returns a bool)
   + Search
   + Decons
   + IO
   + Conversion and escaping (string -> string)
*)

(** {6 Allocation (returns a string)} *)

(**
   [insert "insert in a string" 6 " something"]
   returns ["insert something in a string"]
   @raise Invalid_argument if [ofs > len]
*)
val insert : string -> int -> string -> string
val unsafe_sub : string -> int -> int -> string

(** Build a string of size [n] with a function. If [n<0], fill it in reverse order *)
val init : int -> (int -> char) -> string

(** Like init but with an [acc]. rev_fold_init if [n<0]
    TODO: reverse ordre [acc * 'a] *)
val fold_init : int -> ('a -> char * 'a) -> 'a -> string
  (* val rev_fold_init *)
val of_chars : char list -> string
val map : (char -> char) -> string -> string
  (* val rev_map *)

(**
   [multi_replace s (pat * repl) list] replace in [s] all occurrences of
   each [pat] by its corresponding [repl]. Returns an error if a [pat]
   is exactly the beginning of another one (e.g: 'to' and 'toto'). If a
   pattern is included elseway in another way, the longer will be applyed
   in case of collision.
   E.g; multi_replace "todo domino" ["todo","bibi"; "do","trio"]
     >> "bibi triomino"
   Implemented using [Buffer]. Returns a fresh string.
*)
val multi_replace : string -> (string * string) list -> string

(**
   [replace s pat repl] replace in [s] all occurrence of [pat] by [repl].
   Implemented using multi_replace. Returns a fresh string.
*)
val replace : string -> string -> string -> string

(**
   [citation sep length s] will return a list of maxlength [max length sep].
   If the string is shorter than len, return the string [s], if not, return
   a fresh string beginning and ending like [s] but with in the middle the
   [sep] string.
   {[
   String.citation "[...]" 10 "this is a much too long string"
   ]}
   returns something like
   {[
   "this [...] string"
   ]}
*)
val citation : string -> int -> string -> string

(** retuns a string of [len] containing random char from ['a'-'z'] *)
val random : int -> string

(**
   [ltrim is_space "   this has been ltrimed  "]
   returns
   ["this string has been ltrimed  "]
*)
val ltrim : ?is_space:(char -> bool) -> string -> string

(**
   [rtrim is_space "   this has been rtrimed  "]
   returns
   ["   this has been rtrimed"]
*)
val rtrim : ?is_space:(char -> bool) -> string -> string

(**
   [trim is_space "   this has been trimed  "]
   returns
   ["this has been trimed"].
   More effecient than (ltrim (rtrim s))
   The default value for [is_space] is [Char.is_space]
*)
val trim : ?is_space:(char -> bool) -> string -> string

(**
   Remove the quote of a string if it does have some.
   This is done just once, this is not a [trim is_quote].
   + [strip_quotes "\"toto\""] returns ["toto"]
   + [strip_quotes "\"\"toto\"\""] returns ["\"toto\""]
*)
val strip_quotes : string -> string

(** [repeat "bala" 2] returns ["balabala"] *)
val repeat : string -> int -> string

(**
   Returns the left part of a string.
   Count from end if [i < 0], like in many languages.
   + [left "hello" 2] returns ["he"]
   + [left "hello" (-2)] returns ["hel"]

   @raise Invalid_argument if [i] is not between [-len] and [len] included *)
val left : string -> int -> string

(**
   Returns the right part of a string.
   Count from begin if [i < 0], like in many languages.
   + [left "hello" 2] returns ["lo"]
   + [left "hello" (-2)] returns ["llo"]

   @raise Invalid_argument if [i] is not between [-len] and [len] included *)
val right : string -> int -> string

(**
   Returns the left part of a string until a char is found.
   [left_at 'l' "hello"] returns ["he"].

   This is identity if the char is not found.
*)
val left_at : string -> char -> string

(**
   Returns the right part of a string until a char is found.
   [right_at 'e' "hello"] returns ["llo"].

   This is identity if the char is not found.
*)
val right_at : string -> char -> string

(** Removes trailing \n and \r at the end starting from the end of the string until an other char is detected *)
val remove_trail : string -> string

(**
   [remove_prefix "foo" "foobar"] returns ["bar"]
   @raise Not_found if the string does not have such prefix
*)
val remove_prefix : string -> string -> string

(** just like [remove_prefix], but returns the original string when
    instead of raising not found
*)
val remove_prefix_if_possible : string -> string -> string

(**
   [remove_suffix suffix string]
   Same behaviour as [remove_prefix]
*)
val remove_suffix : string -> string -> string

(**
   Same behaviour as [remove_prefix_if_possible]
*)
val remove_suffix_if_possible : string -> string -> string

(**
   Completes a string to [n] chars using [c], or strip if [length s > n].
   + [complete_left 8 'A' "babo"] returns ["AAAAbabo"]
   + [complete_left 2 'A' "babo"] returns ["bo"]

   <!> if [length s = i], this is identity (no copy)

   @raise Invalid_argument if [n < 0]
*)
val complete_left : int -> char -> string -> string

(**
   Completes a string to n chars using c, or strip if length s > n.
   + [complete_right 8 'A' "babo"] returns ["baboAAAA"]
   + [complete_right 2 'A' "babo"] returns ["ba"]

   <!> if [length s = i], [s] is returned (no copy)

   @raise Invalid_argument if [n < 0]
*)
val complete_right : int -> char -> string -> string

(** [complete] is an alias for [complete_right] *)
val complete : int -> char -> string -> string

(**
   Like [String.concat] but with more flexibility
   [sconcat ~left ~right ?nil separator elts].

   1 allocation, 2 iterations on the list.
*)
val sconcat :
  ?left:string ->
  ?right:string -> ?nil:string -> string -> string list -> string

(**
   Like [sconcat] combined with [List.rev] but more optimized.

   1 allocation, 2 iterations on the list.
*)
val rev_sconcat :
  ?left:string ->
  ?right:string -> ?nil:string -> string -> string list -> string

(**
   Like [sconcat] but with an extra [map] function.

   1 allocation, 1 revmap, 2 iterations on the list (can be optimized to 1-1, next commit)
*)
val concat_map :
  ?left:string ->
  ?right:string ->
  ?nil:string -> string -> ('a -> string) -> 'a list -> string

(**
   Like [concat_map] combined with [List.rev] but more optimized.

   1 allocation, 1 revmap, 2 iterations on the list (can be reduced to 1-1) *)
val rev_concat_map :
  ?left:string ->
  ?right:string ->
  ?nil:string -> string -> ('a -> string) -> 'a list -> string

(** {6 Iterators} *)
(** *)
val fold : ('a -> char -> 'a) -> 'a -> string -> 'a
val rev_fold : ('a -> char -> 'a) -> 'a -> string -> 'a
val exists : (char -> bool) -> string -> bool
val for_all : (char -> bool) -> string -> bool

(** {6 Condition (returns a bool) } *)
(** *)
val equal_insensitive : string -> string -> bool
val compare_insensitive : string -> string -> int

(**
   Check that each char is in ['a'-'z'|'A'-'Z'|'0'-'9'|'_']
   @see "Char.is_alpha" (same test but ['_'])
*)
val is_word : string -> bool

(**
   Check if each char is in ['0'-'9'].
   The implementation returns [false] on negative int.
*)
val is_int : string -> bool

(**
   All char are digits, with one optional dot.
   + implies than [Pervasives.float_of_string] does not fail.
   + is_int implies is_float
   + including e.g. [".1"]

   The implementation returns [false] on negative float.
*)
val is_float : string -> bool

(**
   Tell if a string is a valid universal identifier, working on most of languages.
   This means that it uses only alphanumeric char, and [_] but does not start with
   a numeric char
*)
val is_universal_ident : string -> bool

(**
   [is_substring "lo" "hello" 3] returns [true]

   Does not fail if the prefix or the offset is longuer than the string, simply returns [false].

   @raise Invalid_argument if [ofs < 0]
*)
val is_substring : string -> string -> int -> bool

(** Like [is_substring] but with a custom comparaison between char *)
val is_substring_compare : (char -> char -> int) -> string -> string -> int -> bool

(** Like [is_substring] but not case sensitive *)
val is_substring_insensitive : string -> string -> int -> bool

(** [is_contained "lo" "hello"] returns [true].

    Implemented from right to left. Use [is_contained_from] or
    [is_contained_until] if you need to be more specific.
*)
val is_contained : string -> string -> bool

(**
   [is_contained_from pat s start]
   right to left implemented.
   The [start] ofset is the min index where the patern can start in s.
   The function return the index where the [pat] starts in [s].

   If [start < 0], the function ignore [start] and take [0] as [start]
*)
val is_contained_from : string -> string -> int -> int option

(**
   [is_contained_from_until pat s start stop]
   right to left implemented.
   The [start] ofset is the min index where the patern can start, the
   [stop] the index where [pat] should end before.
   The function return the index where the [pat] starts in [s].

   If [start < 0], the function ignore [start] and take [0] as [start].
   If [stop < 0] returns [None]
*)
val is_contained_from_until : string -> string -> int -> int -> int option

(** [is_prefix "youpla" "youplaboum"] returns [true]

    Does not fail if the prefix is longuer than the string, simply returns [false] *)
val is_prefix : string -> string -> bool

(** [is_suffix "boum" "youplaboum"] returns [true]

    Does not fail if the suffix is longuer than the string, simply returns [false] *)
val is_suffix : string -> string -> bool

(** {6 Search} *)

(** Find the first char [c] satisfying a predicate and returns this char *)
val find_opt : (char -> bool) -> string -> char option

(**
   Find the first char [c] satisfying a predicate and
   returns the index in the string of [c]
*)
val findif : (char -> bool) -> string -> int option
val findi : char -> string -> int option

val rev_findif : (char -> bool) -> string -> int option
val rev_findi : char -> string -> int option

(** {6 Decons} *)

(**
   Like the unix command line.
   [tail ?(lines=10) text] returns the [lines] last lines of [text]
*)
val tail : ?lines:int -> string -> string

(** @raise Invalid_argument [last_char ""] *)
val last_char : string -> char

(**
   [len_from p s ofs]
   returns the length of the bigger prefix [p] of [s] starting at [ofs]
   such that all char of [p] satisfy a predicate [f].
*)
val len_from : (char -> bool) -> string -> int -> int

(**
   Same than [Hashtbl.hash]
*)
val hash : string -> int

val char_list_of_string : string -> char list
val rev_char_list_of_string : string -> char list


(** {6 Splitting} *)

(**
   cuts a string at the occurrences of the given character. The separation
   character is removed. Empty strings are'nt returned (that is,
   [slice '/' "/path//x/"] returns [["path";"x"]]). Non tail-recursive.
*)
val slice : char -> string -> string list

(**
   same as slice, but cuts at every character present in the string given as
   first parameter.
*)
val slice_chars : string -> string -> string list

(**
   split a string in two substrings, at the first occurrence of [char], which is removed.
   If [char] doesn't appear in [string], returns [(string,"")]
*)
val split_char : char -> string -> string * string

(**
   split a string in two substrings, at the last occurrence of [char], which is removed.
   If [char] doesn't appear in [string], returns [(string,"")]
*)
val split_char_last : char -> string -> string * string


(** {6 UNDOCUMENTED : use it at your own risk} *)
(** TODO: remove from String *)
(** *)
val width : string -> int
val limit : int -> string -> string
val limit_width : int -> string -> string
val name_of_int : int -> string
val name_of_int_upper : int -> string
val remove_first_char : string -> string
val remove_accents : string -> string

val delete_trailing_whitespace : string -> string
val to_hex : string -> string
val from_hex : string -> string

(** {6 Conversion and escaping} *)

val base64encode : string -> string

val base64decode : string -> string
  (** @raise Invalid_argument if the argument is not a valid base64 encoded string *)

(**
   [escape ~valid_chars ~escape_chars str] returns a string
   containing only chars that are valid ([String.for_all valid_chars]
   holds on the output).
   The output string may be empty.
   [escape] is injective, ie, it never creates collision and it does not
   depend on any global state (ie calling it with the same input always
   gives the same output).

   @precond not (valid_chars escape_char)
   @precond 0-9 and a-f should be valid_chars
*)
val escape : valid_chars:(char -> bool) -> escape_char:char -> string -> string

val equal : string -> string -> bool
