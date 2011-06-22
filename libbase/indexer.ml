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
(*
    @author Louis Gesbert
**)

(**
   This module extracts search-words from a text: it's used, for instance, by
   the database full-search function.

   Filtering is quite naive for now, but can be improved.

   This module uses the Ulex syntax extension
*)

exception Result of int StringMap.t (* internal use, to return result at eof *)

(* Based on 10002fr.equ file (most common English words on Usenet list), taken
   from Moby Word Lists by Grady Ward, a public domain package:
   http://www.gutenberg.org/ebooks/3201.  The regular expression contains all
   the words up to line 70 of the file, excluding "article", "writes", "UUCP",
   single letter words and capitalized words (all of which are variants of lower
   case words already on the list).

   Unoptimized regexp:
   "\\(the\\|to\\|of\\|and\\|is\\|in\\|that\\|it\\|for\\|you\\|on\\|be\\|have\\|are\\|with\\|not\\|this\\|or\\|as\\|was\\|but\\|at\\|from\\|by\\|an\\|if\\|they\\|about\\|would\\|can\\|one\\|my\\|will\\|all\\|do\\|edu\\|has\\|like\\|there\\|me\\|out\\|your\\|what\\|which\\|some\\|so\\|we\\|more\\|who\\|any\\|don't\\|up\\|get\\|am\\|just\\|he\\|no\\|other\\)$"

   Optimized regexp: *)
let common_words = Str.regexp "\\(a\\(bout\\|ll\\|n[dy]\\|re\\|[mnst]\\)\\|b\\(ut\\|[ey]\\)\\|can\\|do\\(n't\\)?\\|edu\\|f\\(or\\|rom\\)\\|get\\|h\\(a\\(s\\|ve\\)\\|e\\)\\|i[fnst]\\|just\\|like\\|m\\(ore\\|[ey]\\)\\|not?\\|o\\(ne\\|ther\\|ut\\|[fnr]\\)\\|so\\(me\\)?\\|t\\(h\\(at\\|e\\(re\\|y\\)?\\|is\\)\\|o\\)\\|up\\|w\\(as\\|e\\|h\\(at\\|ich\\|o\\)\\|i\\(ll\\|th\\)\\|ould\\)\\|your?\\)$"

(** @param map is a map from word to number of its occurences; this function
    returns the updated map with occurences from s *)
let utf8_string map s =
  let bad_utf8 () =
    (* Base.jlog ~level:3 "Invalid utf-8 in string, not indexing";*) map in
  let add_word map w =
    match StringMap.find_opt w map with
      | Some num ->  StringMap.add w (num + 1) map
      | None -> StringMap.add w 1 map
  in
  let extract_word acc = lexer
    | xml_ideographic ->
        (* Fixme: splitting words in ideographic languages is non-trivial. See
           for example http://www.mnogosearch.org/doc33/msearch-cjk.html *)
        (* add_word acc (Ulexing.utf8_lexeme lexbuf) -- index single ideograms ? *)
        acc (* we don't index at all, for now *)
    | xml_letter+ ->
        if Ulexing.lexeme_length lexbuf <= 1 then
          acc
        else
          let word = Cactutf.lowercase (Ulexing.utf8_lexeme lexbuf) in
          if String.length word <= 1 || Str.string_match common_words word 0 then
            acc
          else
            add_word acc word
    | eof -> raise (Result acc)
    | _ -> acc
  in
  let rec lex acc lexbuf = lex (extract_word acc lexbuf) lexbuf
  in
  try
    lex map (Ulexing.from_utf8_string s)
  with
    | Result map -> map
    | Ulexing.Error | Ulexing.InvalidCodepoint _ -> bad_utf8()
    | Utf8.MalFormed -> bad_utf8() (* that one is undocumented in ulex (?) *)
