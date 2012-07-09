(*
    Copyright © 2011, 2012 MLstate

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
module JP = JsonParse
module JT = JsonTypes

(** JSON parser based on syntax described on http://www.json.org/

    This lexer uses Ulex, although there is no need for utf-8 support here,
    because ocamllex created problems with OPA. *)

(* Types *)
(* TODOk1 - Review this... *)
let regexp t_blank = [' ' '\t' '\n' '\r']

let regexp t_digit = ['0'-'9']
let regexp t_digits = t_digit+
let regexp t_int = '0'| '-''0' | '-'? ['1'-'9'] t_digit*
let regexp t_frac = "." t_digits
let regexp t_e = ("e"|"E") ("+"|"-")?
let regexp t_exp = t_e t_digits
let regexp t_number = (t_int|'0') t_frac? t_exp?

let regexp t_hexa_digit = ['0'-'9''A'-'F''a'-'f']
let regexp t_hexa = t_hexa_digit t_hexa_digit t_hexa_digit t_hexa_digit

let regexp t_ident = ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*

let b = Buffer.create 100000

(** Lexing rules *)
let rec get_token = lexer

    (* End of stream / file *)
    | eof       ->  JP.EOF

    | '{'	->  JP.LCURLY
    | '}'	->  JP.RCURLY
    | '['	->  JP.LBRACKET
    | ']'	->  JP.RBRACKET
    | ':'       ->  JP.COLON
    | ','       ->  JP.COMMA
    | t_int     ->  JP.INT (int_of_string (Ulexing.utf8_lexeme lexbuf))

    | "NaN"
    | "Infinity"
    | "-Infinity"
    | t_number  ->  JP.FLOAT (float_of_string (Ulexing.utf8_lexeme lexbuf))

    | "true"    ->  JP.TRUE
    | "false"   ->  JP.FALSE
    | "null"    ->  JP.NIL
    | "undefined" ->  JP.NIL
    | "u"       ->  JP.NIL

    (* An ident without quotes *)
    (* This is not from JSON spec, it's added only for compatibility in OPA *)
    | t_ident   ->  JP.IDENT (Ulexing.utf8_lexeme lexbuf)

    (* Spaces *)
    | t_blank   ->  get_token lexbuf

    (* Strings *)
    | '\"'      ->  Buffer.reset b;
                    get_string b lexbuf
    | _		-> failwith ("unknown token: " ^ (Ulexing.utf8_lexeme lexbuf))

(** Parse a string, handle escaping *)
 and get_string s = lexer
    | eof           ->  assert false
    | '\"'          ->  JP.STRING (Buffer.contents s)
    | "\\\""        ->  Buffer.add_char s '"'; get_string s lexbuf
    | "\\\\"        ->  Buffer.add_char s '\\'; get_string s lexbuf
    | "\\/"         ->  Buffer.add_char s '/'; get_string s lexbuf
    | "\\b"         ->  Buffer.add_char s '\b'; get_string s lexbuf
    | "\\f"         ->  Buffer.add_char s '\012'; get_string s lexbuf
    | "\\n"         ->  Buffer.add_char s '\n'; get_string s lexbuf
    | "\\r"         ->  Buffer.add_char s '\r'; get_string s lexbuf
    | "\\t"         ->  Buffer.add_char s '\t'; get_string s lexbuf
    | "\\u" t_hexa  ->
        let lx = Ulexing.utf8_lexeme lexbuf in
        let i = int_of_string ("0x"^(String.sub lx 2 4)) in
        let res = Cactutf.cons i in
        Buffer.add_string s res;
        get_string s lexbuf
    | [^'\\''\"']+  ->  Buffer.add_string s (Ulexing.utf8_lexeme lexbuf); get_string s lexbuf
    | _		    ->  failwith "unterminated string"

(** Print token contained on given string. Used for debug. *)
let print_tokens str =
  let pr = function
    | JP.EOF -> "eof"
    | JP.LCURLY -> "{"
    | JP.RCURLY -> "}"
    | JP.COLON -> ":"
    | JP.LBRACKET -> "["
    | JP.RBRACKET -> "]"
    | JP.COMMA -> ","
    | JP.TRUE -> "true"
    | JP.FALSE -> "false"
    | JP.NIL -> "null"
    | JP.STRING s -> "\"" ^ s ^ "\""
    | JP.IDENT s -> "$" ^ s ^ "$"
    | JP.INT i -> string_of_int i
    | JP.FLOAT f -> string_of_float f
  in

  let buffer = Ulexing.from_utf8_string str in
  Printf.printf "ml json: %S\n\n%!" str;
  let tok = ref JP.NIL in
  while !tok <> JP.EOF do
    tok := get_token buffer;
    Printf.printf "token = %s\n" (pr !tok)
  done

(** Transform a string to type that you want with given constructor.
    [transform emptyM addM emptyL addL cint cfloat cstring cbool cvoid str]

    @param emptyM Constructor for an empty record
    @param addM Constructor for add a field to a record
    @param emptyL Constructor for an empty list
    @param emptyL Constructor for add an element to a list
    @param cint Constructor for an int
    @param cfloat Constructor for a float
    @param cstring Constructor for a string
    @param cbool Constructor for a bool
    @param cvoid Constructor for a void
    @return Constructed value
*)
let transform utf8 (*emptyM addM emptyL addL cons_int cons_float cons_string cons_bool cons_void*) str =
(*
  (* convert the JSON AST to an OPA type *)
  let rec conv = function
    | Int i    -> cons_int i
    | Float f  -> cons_float f
    | String s -> cons_string s
    | Bool b   -> cons_bool b
    | Void     -> cons_void ()
    | Array l  -> List.fold_right (fun e acc -> addL (conv e) acc) l (emptyL())
    | Record l -> List.fold_right (fun (s,v) acc -> addM s (conv v) acc) l (emptyM())
  in
*)
  (*DEBUG - print_tokens str;*)
  let buffer =
    if utf8 then Ulexing.from_utf8_string str
    else Ulexing.from_latin1_string str
  in
  let res() =
      (* Trick from Alain Frisch to use Ulex with OCamlyacc *)
      (* http://caml.inria.fr/pub/ml-archives/caml-list/2005/01/52cbc2cd2be4fc7ea0f00c39a760bf59.en.html *)
      JP.json (fun _ -> get_token buffer) (Lexing.from_string "dummy") in
  try Some((*conv*) (res())) with _ -> None
