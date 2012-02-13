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
open RequestParse
open Printf

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
