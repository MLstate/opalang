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
(* The lexer definition *)
open RequestParse
(*open Printf*)

type lexical_error = Illegal_character

exception Lexical_error of lexical_error * int * int

let parse = lexer
  | [' ' '\t']+ -> SPACE (Ulexing.utf8_lexeme lexbuf)
  | '\r' '\n' -> EOL
  | '\r' -> EOL
  | '\n' -> EOL
  | ([^':' ' ' '\t' '\n' '\r'])+ -> WORD (Ulexing.utf8_lexeme lexbuf)
  | ":" -> COLON
  | eof -> EOF

let parse_request str =
  (* The following line cost 1/4 of the time: *)
  let buffer = Ulexing.from_utf8_string str in
  let res() =
      (* Trick from Alain Frisch to use Ulex with OCamlyacc *)
      (* http://caml.inria.fr/pub/ml-archives/caml-list/2005/01/52cbc2cd2be4fc7ea0f00c39a760bf59.en.html *)
      request (fun _ -> parse buffer) (Lexing.from_string "dummy")
  in
  try ((Ulexing.lexeme_end buffer, res()):int * RequestType.parse_request)
  with _ -> Ulexing.lexeme_end buffer, RequestType.Incomplete
