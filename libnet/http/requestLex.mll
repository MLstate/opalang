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
(* The lexer definition *)
{
open Base
open RequestParse
open Printf

type lexical_error = Illegal_character

exception Lexical_error of lexical_error * int * int
}

rule token = parse
    ([^':' ' ' '\t' '\n' '\r'])+ { WORD (Lexing.lexeme lexbuf) }
  | [' ' '\t']+ { SPACE (Lexing.lexeme lexbuf) }
  | '\r' '\n' { EOL }
  | '\r' { EOL }
  | '\n' { EOL }
  | ":" { COLON }
  | eof { EOF }
  | _ { raise (Lexical_error(Illegal_character, Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

{
let parse_request str =
  let buffer = Lexing.from_string str in
  let res() = request (fun _ -> token buffer) (Lexing.from_string "dummy") in
  try ((Lexing.lexeme_end buffer, res()):int * RequestType.parse_request)
  with Lexical_error _ | Parsing.Parse_error -> Lexing.lexeme_end buffer, RequestType.Incomplete
}
