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

{
  (* to know what these tokens correspond to, simply
   * look at association list below *)
  type token =
  | Yield
  | With
  | While
  | Void
  | Var
  | Typeof
  | Try
  | True
  | TimesEqual
  | Times
  | Tilda
  | Throw
  | This
  | Switch
  | Super
  | String of (string)
  | Static
  | Semic
  | Rparen
  | Return
  | Regexp of (string * string)
  | Rcurly
  | Rbracket
  | Question
  | Public
  | Protected
  | Private
  | PlusPlus
  | PlusEqual
  | Plus
  | PercentEqual
  | Percent
  | Package
  | Null
  | New
  | MinusMinus
  | MinusEqual
  | Minus
  | LtLtEqual
  | LtLt
  | Lt
  | Lparen
  | Let
  | Le
  | Lcurly
  | Lbracket
  | LT
  | Interface
  | Integer of (string)
  | Instanceof
  | In
  | Import
  | Implements
  | If
  | Ident of (string)
  | GtGtGtEqual
  | GtGtGt
  | GtGtEqual
  | GtGt
  | Gt
  | Ge
  | Function
  | For
  | Finally
  | False
  | Extends
  | Export
  | EqualEqualEqual
  | EqualEqual
  | Equal
  | Enum
  | Else
  | EOF
  | Dot
  | Do
  | DivEqual
  | Div
  | Delete
  | Default
  | Debugger
  | Continue
  | Const
  | Comma
  | Colon
  | Class
  | ChapeauEqual
  | Chapeau
  | Catch
  | Case
  | Break
  | BarEqual
  | BarBar
  | Bar
  | BangEqualEqual
  | BangEqual
  | Bang
  | AmperEqual
  | AmperAmper
  | Amper

  (* the ecmascript defines two kinds of lexing: for the places in the ast
   * where a token starting with / is a regular expression, and the places where
   * it is the division or the division-assignment /=
   * to avoid having to have some information flow from the parser to the lexer,
   * this is done by looking at the previous token (see function lex) *)
  let can_have_a_division = ref false

  (* mapping keywords to tokens *)
  let keywords_list = [
    "break", Break;
    "case", Case;
    "catch", Catch;
    "continue", Continue;
    "debugger", Debugger;
    "default", Default;
    "delete", Delete;
    "do", Do;
    "else", Else;
    "finally", Finally;
    "for", For;
    "function", Function;
    "if", If;
    "in", In;
    "instanceof", Instanceof;
    "new", New;
    "return", Return;
    "switch", Switch;
    "this", This;
    "throw", Throw;
    "try", Try;
    "typeof", Typeof;
    "var", Var;
    "void", Void;
    "while", While;
    "with", With;

    "class", Class;
    "const", Const;
    "enum", Enum;
    "export", Export;
    "extends", Extends;
    "import", Import;
    "super", Super;

    "implements", Implements;
    "interface", Interface;
    "let", Let;
    "package", Package;
    "private", Private;
    "protected", Protected;
    "public", Public;
    "static", Static;
    "yield", Yield;

    "null", Null;
    "true", True;
    "false", False;
  ]
  let keywords = Hashtbl.create 100
  let () = List.iter (fun (a,b) -> Hashtbl.add keywords a b) keywords_list

  (* using a single buffer to store the result of parsing string literals, regexp literals, etc. *)
  let b = Buffer.create 1000
}

let identifier_part = ['a'-'z''A'-'Z''_''$''0'-'9']
let identifier = ['a'-'z''A'-'Z''_''$'] identifier_part*
let hexa = ['0'-'9''a'-'f''A'-'F']

rule main = parse
| ['\t''\012''\013'' ']+ { main lexbuf }

(* beware that we must not throw newlines to be able to implement semicolon
 * insertions *)
| ['\n' '\r']+ { LT }

| "//" [^'\n''\r']* { main lexbuf }

(* beware that if a newline appears in a multi line comment
 * then we _must_ generate a newline token *)
| "/*" { multiline_comment false lexbuf }

| '/' { if !can_have_a_division then Div else (Buffer.clear b; regexp_body lexbuf) }
| "/=" { if !can_have_a_division then DivEqual else (Buffer.clear b; Buffer.add_char b '='; regexp_body lexbuf) }

| '{' { Lcurly }
| '}' { Rcurly }
| '[' { Lbracket }
| ']' { Rbracket }
| '(' { Lparen }
| ')' { Rparen }
| '.' { Dot }
| ';' { Semic }
| ',' { Comma }
| '<' { Lt }
| '>' { Gt }
| "<=" { Le }
| ">=" { Ge }
| "==" { EqualEqual }
| "!="  { BangEqual }
| "===" { EqualEqualEqual }
| "!==" { BangEqualEqual }
| "+" { Plus }
| "-" { Minus }
| "*" { Times }
| "%" { Percent }
| "++" { PlusPlus }
| "--" { MinusMinus }
| "<<" { LtLt }
| ">>" { GtGt }
| ">>>" { GtGtGt }
| "&" { Amper }
| "|" { Bar }
| "^" { Chapeau }
| "!" { Bang }
| "~" { Tilda }
| "&&" { AmperAmper }
| "||" { BarBar }
| "?" { Question }
| ":" { Colon }
| "=" { Equal }
| "+=" { PlusEqual }
| "-=" { MinusEqual }
| "*=" { TimesEqual }
| "%=" { PercentEqual }
| "<<=" { LtLtEqual }
| ">>=" { GtGtEqual }
| ">>>=" { GtGtGtEqual }
| "&=" { AmperEqual }
| "|=" { BarEqual }
| "^=" { ChapeauEqual }

| identifier as s { try Hashtbl.find keywords s with Not_found -> Ident s }
| ('0' | ['1'-'9'] ['0'-'9']*) '.' ['0'-'9']* (['e''E'] ['-''+']? ['0'-'9']+)?
| '.' ['0'-'9']+ (['e''E'] ['-''+']? ['0'-'9']+)?
| ('0' | ['1'-'9'] ['0'-'9']*) (['e''E'] ['-''+']? ['0'-'9']+)?
| '0' ['x''X'] hexa*
 as s { Integer s }
| "'" { Buffer.clear b; string false lexbuf }
| '"' { Buffer.clear b; string true lexbuf }
| eof { EOF }
| _ as c { raise (Stream.Error (Printf.sprintf "unexpected character %C in main lexing" c)) }

(* regular expression are not really parsed, you simply interpret them enough
 * so that you can find the end of the regexp
 * in particular, escapes are *not* interpreted, and so the string in the regexp
 * node and token should not be escaped when printed *)
and regexp_body = parse
| ['\r''\n'] { raise (Stream.Error "Line terminator inside a regexp literal") }
| '\\' _ as s { Buffer.add_string b s; regexp_body lexbuf }
| '[' as c { Buffer.add_char b c; character_class lexbuf; regexp_body lexbuf }
| [^'\\''\r''\n''[' '/']+ as s { Buffer.add_string b s; regexp_body lexbuf }
| '/' { let s = Buffer.contents b in
        Buffer.clear b;
        regexp_flags s lexbuf }
| _ as c { raise (Stream.Error (Printf.sprintf "unexpected character %C in regexp body" c)) }
| eof { raise (Stream.Error "unterminated regexp body ") }
and character_class = parse
| ']' as c { Buffer.add_char b c }
| '\\' _ as s { Buffer.add_string b s; character_class lexbuf }
| [^'\\' ']']+ as s { Buffer.add_string b s; character_class lexbuf }
| _ as c { raise (Stream.Error (Printf.sprintf "unexpected character %C in character class" c)) }
| eof { raise (Stream.Error "unterminated character class ") }
and regexp_flags s1 = parse
| identifier_part* as s2 { Regexp (s1,s2) }

(* [double] is true when the string is enclosed in double quotes
 * and false when it is enclosed in single quotes *)
and string double = parse
| "'" { if double then (Buffer.add_char b '\''; string double lexbuf)
        else String (Buffer.contents b) }
| '"' { if double then String (Buffer.contents b)
        else (Buffer.add_char b '"'; string double lexbuf) }
| [^'\'' '"' '\\''\n''\r']+ as s { Buffer.add_string b s; string double lexbuf }
| ['\n' '\r'] { raise (Stream.Error "Line terminator inside a single string literal") }
| "\\" (['0'-'7'] ['0'-'7']? ['0'-'7']? as s) { Buffer.add_char b (Char.chr (int_of_string s)); string double lexbuf }
| "\\b" { Buffer.add_char b '\008'; string double lexbuf }
| "\\t" { Buffer.add_char b '\t'; string double lexbuf }
| "\\n" { Buffer.add_char b '\n'; string double lexbuf }
| "\\v" { Buffer.add_char b '\011'; string double lexbuf }
| "\\f" { Buffer.add_char b '\012'; string double lexbuf }
| "\\r" { Buffer.add_char b '\r'; string double lexbuf }
| "\\\\" { Buffer.add_char b '\\'; string double lexbuf }
| "\\" (['"''\''] as c) { Buffer.add_char b c; string double lexbuf }
| "\\u" (hexa hexa hexa hexa as s) { Buffer.add_string b (Scanf.sscanf s "%x" (fun d -> Cactutf.cons d)); string double lexbuf }
| "\\x" (hexa hexa as s) { Buffer.add_string b (Scanf.sscanf s "%x" (fun d -> Cactutf.cons d)); string double lexbuf }
| eof { raise (Stream.Error "unterminated string literal comment") }
| "\\" { Buffer.add_char b '\\'; string double lexbuf }
| _ as c { raise (Stream.Error (Printf.sprintf "unexpected character %C in a string literal" c)) }

(* [newline] is true when a newline has been parsed in the comment *)
and multiline_comment newline = parse
| [^'*''\n''\r']* { multiline_comment newline lexbuf }
| ['\r''\n'] { multiline_comment true lexbuf }
| "*/" { if newline then LT else main lexbuf }
| '*' { multiline_comment newline lexbuf }
| eof { raise (Stream.Error "unterminated multiline comment") }

{
(* this global variable is used to ensure that the lexer never returns
 * two consecutive new lines, which is useful in the parser, because
 * if you want to look at the first non newline token, you need a lookahead
 * of 2 with this (otherwise the lookahead would be unbounded) *)
let just_parsed_a_line_terminator = ref true

(* the main lexing function: called the actual lexer, and updates the global
 * state *)
let rec lex lexbuf =
  match main lexbuf with
  | LT when !just_parsed_a_line_terminator ->
    (* INVARIANT: there is never two consecutive LT in the token stream *)
    (* can have a division doesn't change *)
    (* just_parsed_a_line_terminator is still true *)
    lex lexbuf
  | LT ->
    (* can have a division doesn't change *)
    just_parsed_a_line_terminator := true;
    LT

  (* these symbols cannot be followed by a division *)
  | Lbracket
  | Lcurly
  | Rcurly
  | Lparen
  | Dot
  | Semic
  | Comma
  | Lt
  | Gt
  | Le
  | Ge
  | EqualEqual
  | BangEqual
  | EqualEqualEqual
  | BangEqualEqual
  | Plus
  | Minus
  | Times
  | Percent
  | LtLt
  | GtGt
  | GtGtGt
  | Amper
  | Bar
  | Chapeau
  | Bang
  | Tilda
  | AmperAmper
  | BarBar
  | Question
  | Colon
  | Equal
  | PlusEqual
  | MinusEqual
  | TimesEqual
  | PercentEqual
  | LtLtEqual
  | GtGtEqual
  | GtGtGtEqual
  | AmperEqual
  | BarEqual
  | ChapeauEqual
  | Div
  | DivEqual
  | Break
  | Case
  | Catch
  | Continue
  | Debugger
  | Default
  | Delete
  | Do
  | Else
  | Finally
  | For
  | Function
  | If
  | In
  | Instanceof
  | New
  | Return
  | Switch
  | This
  | Throw
  | Typeof
  | Try
  | Var
  | Void
  | While
  | With
  | Class
  | Const
  | Enum
  | Export
  | Extends
  | Import
  | Super
  | Implements
  | Interface
  | Let
  | Package
  | Private
  | Protected
  | Public
  | Static
  | Yield
      as r ->
    just_parsed_a_line_terminator := false;
    can_have_a_division := false;
    r

  (* these symbols can be followed by a division *)
  | EOF (* don't care *)
  | Rbracket
  | PlusPlus
  | MinusMinus
  | Rparen
  | Ident _
  | False
  | True
  | Null
  | Regexp _
  | String _
  | Integer _
      as r ->
    just_parsed_a_line_terminator := false;
    can_have_a_division := true;
    r
let init_lexer () =
  can_have_a_division := false;
  just_parsed_a_line_terminator := true

let stream lexbuf =
  Stream.from (
    fun _ ->
      match lex lexbuf with
      | EOF -> None
      | t -> Some t
  )

let stream_of_file file =
  init_lexer ();
  try
    let ic_ = open_in file in
    Gc.finalise close_in ic_; (* garbage collecting the input channel *)
    let lexbuf = Lexing.from_channel ic_ in
    stream lexbuf, lexbuf
  with Sys_error diagnostic ->
    Printf.printf "Couldn't open file %s: %s\n%!" file diagnostic;
    exit 1

let stream_of_string string =
  init_lexer ();
  let lexbuf = Lexing.from_string string in
  stream lexbuf, lexbuf
}
