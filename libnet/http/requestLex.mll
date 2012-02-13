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
(* The lexer definition *)
{
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
