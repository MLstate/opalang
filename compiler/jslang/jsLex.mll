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

{

type pos = FilePos.pos
let dummy_pos = FilePos.nopos "JsLex"

type doc_comment_elt =
| CommentLine of pos * string
| CommentTag of pos * string * string

(* to know what these tokens correspond to, simply look at association
 * list below *)
type token =
| Yield of pos
| With of pos
| While of pos
| Void of pos
| Var of pos
| Typeof of pos
| Try of pos
| True of pos
| TimesEqual of pos
| Times of pos
| Tilda of pos
| Throw of pos
| This of pos
| Switch of pos
| Super of pos
| String of pos * string
| Static of pos
| Semic of pos
| Rparen of pos
| Return of pos
| Regexp of pos * string * string
| Rcurly of pos
| Rbracket of pos
| Question of pos
| Public of pos
| Protected of pos
| Private of pos
| PlusPlus of pos
| PlusEqual of pos
| Plus of pos
| PercentEqual of pos
| Percent of pos
| Package of pos
| Null of pos
| New of pos
| MinusMinus of pos
| MinusEqual of pos
| Minus of pos
| LtLtEqual of pos
| LtLt of pos
| Lt of pos
| Lparen of pos
| Let of pos
| Le of pos
| Lcurly of pos
| Lbracket of pos
| LT of pos
| Interface of pos
| Integer of pos * string
| Instanceof of pos
| In of pos
| Import of pos
| Implements of pos
| If of pos
| Ident of pos * string
| GtGtGtEqual of pos
| GtGtGt of pos
| GtGtEqual of pos
| GtGt of pos
| Gt of pos
| Ge of pos
| Function of pos
| For of pos
| Finally of pos
| False of pos
| Extends of pos
| Export of pos
| EqualEqualEqual of pos
| EqualEqual of pos
| Equal of pos
| Enum of pos
| Else of pos
| EOF of pos
| Dot of pos
| Do of pos
| DivEqual of pos
| Div of pos
| Delete of pos
| Default of pos
| Debugger of pos
| Continue of pos
| Const of pos
| Comma of pos
| Colon of pos
| Class of pos
| ChapeauEqual of pos
| Chapeau of pos
| Catch of pos
| Case of pos
| Break of pos
| BarEqual of pos
| BarBar of pos
| Bar of pos
| BangEqualEqual of pos
| BangEqual of pos
| Bang of pos
| AmperEqual of pos
| AmperAmper of pos
| Amper of pos

(* These tokens are used only when parsing comments for bsl files
   and are not produced in normal lexing*)
| DocComment of pos * doc_comment_elt list

(* used for debug only, not error messages *)
let string_of_token = function
  | Break _ -> "break"
  | Case _ -> "case"
  | Catch _ -> "catch"
  | Continue _ -> "continue"
  | Debugger _ -> "debugger"
  | Default _ -> "default"
  | Delete _ -> "delete"
  | Do _ -> "do"
  | Else _ -> "else"
  | Finally _ -> "finally"
  | For _ -> "for"
  | Function _ -> "function"
  | If _ -> "if"
  | In _ -> "in"
  | Instanceof _ -> "instanceof"
  | New _ -> "new"
  | Return _ -> "return"
  | Switch _ -> "switch"
  | This _ -> "this"
  | Throw _ -> "throw"
  | Try _ -> "try"
  | Typeof _ -> "typeof"
  | Var _ -> "var"
  | Void _ -> "void"
  | While _ -> "while"
  | With _ -> "with"
  | Class _ -> "class"
  | Const _ -> "const"
  | Enum _ -> "enum"
  | Export _ -> "export"
  | Extends _ -> "extends"
  | Import _ -> "import"
  | Super _ -> "super"
  | Implements _ -> "implements"
  | Interface _ -> "interface"
  | Let _ -> "let"
  | Package _ -> "package"
  | Private _ -> "private"
  | Protected _ -> "protected"
  | Public _ -> "public"
  | Static _ -> "static"
  | Yield _ -> "yield"
  | True _ -> "true"
  | False _ -> "false"
  | Null _ -> "null"
  | Regexp (_,s1,s2) -> Printf.sprintf "Regexp /%s/%s" s1 s2
  | String (_,s) -> Printf.sprintf "%S" s
  | Ident (_,s) -> "Ident " ^ s
  | Integer (_,s) -> s
  | LT _ -> "LT"
  | EOF _ -> "EOF"
  | Lbracket _ -> "["
  | Rbracket _ -> "]"
  | Lcurly _ -> "{"
  | Rcurly _ -> "}"
  | Lparen _ -> "("
  | Rparen _ -> ")"
  | Dot _ -> "."
  | Semic _ -> ";"
  | Comma _ -> ","
  | Lt _ -> "<"
  | Gt _ -> ">"
  | Le _ -> "<="
  | Ge _ -> ">="
  | EqualEqual _ -> "=="
  | BangEqual _ -> "!="
  | EqualEqualEqual _ -> "==="
  | BangEqualEqual _ -> "!=="
  | Plus _ -> "+"
  | Minus _ -> "-"
  | Times _ -> "*"
  | Percent _ -> "%"
  | PlusPlus _ -> "++"
  | MinusMinus _ -> "--"
  | LtLt _ -> "<<"
  | GtGt _ -> ">>"
  | GtGtGt _ -> ">>>"
  | Amper _ -> "&"
  | Bar _ -> "|"
  | Chapeau _ -> "^"
  | Bang _ -> "!"
  | Tilda _ -> "~"
  | AmperAmper _ -> "&&"
  | BarBar _ -> "||"
  | Question _ -> "?"
  | Colon _ -> ":"
  | Equal _ -> "="
  | PlusEqual _ -> "+="
  | MinusEqual _ -> "-="
  | TimesEqual _ -> "*="
  | PercentEqual _ -> "%="
  | LtLtEqual _ -> "<<="
  | GtGtEqual _ -> ">>="
  | GtGtGtEqual _ -> ">>>="
  | AmperEqual _ -> "&="
  | BarEqual _ -> "|="
  | ChapeauEqual _ -> "^="
  | Div _ -> "/"
  | DivEqual _ -> "/="
  | DocComment _ -> "/**"

  (* the ecmascript defines two kinds of lexing: for the places in the ast
   * where a token starting with / is a regular expression, and the places where
   * it is the division or the division-assignment /=
   * to avoid having to have some information flow from the parser to the lexer,
   * this is done by looking at the previous token (see function lex) *)
  let can_have_a_division = ref false

  (* mapping keywords to tokens *)
  let keywords_list = [
    "break",      (fun pos -> Break pos);
    "case",       (fun pos -> Case pos);
    "catch",      (fun pos -> Catch pos);
    "continue",   (fun pos -> Continue pos);
    "debugger",   (fun pos -> Debugger pos);
    "default",    (fun pos -> Default pos);
    "delete",     (fun pos -> Delete pos);
    "do",         (fun pos -> Do pos);
    "else",       (fun pos -> Else pos);
    "finally",    (fun pos -> Finally pos);
    "for",        (fun pos -> For pos);
    "function",   (fun pos -> Function pos);
    "if",         (fun pos -> If pos);
    "in",         (fun pos -> In pos);
    "instanceof", (fun pos -> Instanceof pos);
    "new",        (fun pos -> New pos);
    "return",     (fun pos -> Return pos);
    "switch",     (fun pos -> Switch pos);
    "this",       (fun pos -> This pos);
    "throw",      (fun pos -> Throw pos);
    "try",        (fun pos -> Try pos);
    "typeof",     (fun pos -> Typeof pos);
    "var",        (fun pos -> Var pos);
    "void",       (fun pos -> Void pos);
    "while",      (fun pos -> While pos);
    "with",       (fun pos -> With pos);

    "class",      (fun pos -> Class pos);
    "const",      (fun pos -> Const pos);
    "enum",       (fun pos -> Enum pos);
    "export",     (fun pos -> Export pos);
    "extends",    (fun pos -> Extends pos);
    "import",     (fun pos -> Import pos);
    "super",      (fun pos -> Super pos);

    "implements", (fun pos -> Implements pos);
    "interface",  (fun pos -> Interface pos);
    "let",        (fun pos -> Let pos);
    "package",    (fun pos -> Package pos);
    "private",    (fun pos -> Private pos);
    "protected",  (fun pos -> Protected pos);
    "public",     (fun pos -> Public pos);
    "static",     (fun pos -> Static pos);
    "yield",      (fun pos -> Yield pos);

    "null",       (fun pos -> Null pos);
    "true",       (fun pos -> True pos);
    "false",      (fun pos -> False pos);
  ]
  let keywords = Hashtbl.create 100
  let () = List.iter (fun (a,b) -> Hashtbl.add keywords a b) keywords_list

  (* using a single buffer to store the result of parsing string literals, regexp literals, etc. *)
  let b = Buffer.create 1000

  let get_doc_comment_elt () =
    let re = Str.regexp "[ \t\\*]*@\\([a-zA-Z0-9]*\\)[ \t]*\\(.*\\)" in
    let line = Buffer.contents b in
    Buffer.clear b;
    if Str.string_match re line 0 then
      let tag = Str.matched_group 1 line in
      let args = Str.matched_group 2 line in
      CommentTag (dummy_pos, tag, args)
    else
      CommentLine (dummy_pos, line)
}

let identifier_part = ['a'-'z''A'-'Z''_''$''0'-'9']
let identifier = ['a'-'z''A'-'Z''_''$'] identifier_part*
let hexa = ['0'-'9''a'-'f''A'-'F']

rule main lex_comments = parse
| ['\t''\012''\013'' ']+ { main lex_comments lexbuf }

(* beware that we must not throw newlines to be able to implement semicolon
 * insertions *)
| ['\n' '\r']+ { LT dummy_pos }

| "//" [^'\n''\r']* { main lex_comments lexbuf }

| "/**/" { main lex_comments lexbuf }

| "/**" {
  if lex_comments then doc_comment [] lexbuf
  else multiline_comment false lexbuf
}

(* beware that if a newline appears in a multi line comment
 * then we _must_ generate a newline token *)
| "/*" { multiline_comment false lexbuf }

| '/' {
  if !can_have_a_division then
    Div dummy_pos
  else (
    Buffer.clear b;
    regexp_body lexbuf
  )
}
| "/=" {
  if !can_have_a_division then
    DivEqual dummy_pos
  else (
    Buffer.clear b;
    Buffer.add_char b '=';
    regexp_body lexbuf
  )
}
| '{' { Lcurly dummy_pos }
| '}' { Rcurly dummy_pos }
| '[' { Lbracket dummy_pos }
| ']' { Rbracket dummy_pos }
| '(' { Lparen dummy_pos }
| ')' { Rparen dummy_pos }
| '.' { Dot dummy_pos }
| ';' { Semic dummy_pos }
| ',' { Comma dummy_pos }
| '<' { Lt dummy_pos }
| '>' { Gt dummy_pos }
| "<=" { Le dummy_pos }
| ">=" { Ge dummy_pos }
| "==" { EqualEqual dummy_pos }
| "!="  { BangEqual dummy_pos }
| "===" { EqualEqualEqual dummy_pos }
| "!==" { BangEqualEqual dummy_pos }
| "+" { Plus dummy_pos }
| "-" { Minus dummy_pos }
| "*" { Times dummy_pos }
| "%" { Percent dummy_pos }
| "++" { PlusPlus dummy_pos }
| "--" { MinusMinus dummy_pos }
| "<<" { LtLt dummy_pos }
| ">>" { GtGt dummy_pos }
| ">>>" { GtGtGt dummy_pos }
| "&" { Amper dummy_pos }
| "|" { Bar dummy_pos }
| "^" { Chapeau dummy_pos }
| "!" { Bang dummy_pos }
| "~" { Tilda dummy_pos }
| "&&" { AmperAmper dummy_pos }
| "||" { BarBar dummy_pos }
| "?" { Question dummy_pos }
| ":" { Colon dummy_pos }
| "=" { Equal dummy_pos }
| "+=" { PlusEqual dummy_pos }
| "-=" { MinusEqual dummy_pos }
| "*=" { TimesEqual dummy_pos }
| "%=" { PercentEqual dummy_pos }
| "<<=" { LtLtEqual dummy_pos }
| ">>=" { GtGtEqual dummy_pos }
| ">>>=" { GtGtGtEqual dummy_pos }
| "&=" { AmperEqual dummy_pos }
| "|=" { BarEqual dummy_pos }
| "^=" { ChapeauEqual dummy_pos }

| identifier as s {
  try
    Hashtbl.find keywords s dummy_pos
  with Not_found -> Ident (dummy_pos, s)
}
| ('0' | ['1'-'9'] ['0'-'9']*) '.' ['0'-'9']* (['e''E'] ['-''+']? ['0'-'9']+)?
| '.' ['0'-'9']+ (['e''E'] ['-''+']? ['0'-'9']+)?
| ('0' | ['1'-'9'] ['0'-'9']*) (['e''E'] ['-''+']? ['0'-'9']+)?
| '0' ['x''X'] hexa*
 as s { Integer (dummy_pos, s) }
| "'" { Buffer.clear b; string false lexbuf }
| '"' { Buffer.clear b; string true lexbuf }
| eof { EOF dummy_pos }
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
| identifier_part* as s2 { Regexp (dummy_pos,s1,s2) }

(* [double] is true when the string is enclosed in double quotes
 * and false when it is enclosed in single quotes *)
and string double = parse
| "'" {
  if double then (
    Buffer.add_char b '\'';
    string double lexbuf
  ) else (
    let s = Buffer.contents b in
    Buffer.clear b;
    String (dummy_pos, s)
  )
}
| '"' {
  if double then (
    let s = Buffer.contents b in
    Buffer.clear b;
    String (dummy_pos, s)
  ) else (
    Buffer.add_char b '"';
    string double lexbuf
  )
 }
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
| "*/" { if newline then LT dummy_pos else main false lexbuf }
| '*' { multiline_comment newline lexbuf }
| eof { raise (Stream.Error "unterminated multiline comment") }

and doc_comment elts = parse
| "*/" {
  let elt = get_doc_comment_elt () in
  DocComment (dummy_pos, List.rev (elt :: elts))
}
| "\\\n" { Buffer.add_char b ' '; doc_comment elts lexbuf }
| ['\r''\n'] {
  let elt = get_doc_comment_elt () in
  doc_comment (elt :: elts) lexbuf
}
| _ as c { Buffer.add_char b c; doc_comment elts lexbuf }
| eof { raise (Stream.Error "unterminated multiline comment") }

{
(* this global variable is used to ensure that the lexer never returns
 * two consecutive new lines, which is useful in the parser, because
 * if you want to look at the first non newline token, you need a lookahead
 * of 2 with this (otherwise the lookahead would be unbounded) *)
let just_parsed_a_line_terminator = ref true

(* the main lexing function: called the actual lexer, and updates the global
 * state *)
let rec lex lex_comments lexbuf =
  match main lex_comments lexbuf with
  | LT _ when !just_parsed_a_line_terminator ->
    (* INVARIANT: there is never two consecutive LT in the token stream *)
    (* can have a division doesn't change *)
    (* just_parsed_a_line_terminator is still true *)
    lex lex_comments lexbuf
  | LT _ as r ->
    (* can have a division doesn't change *)
    just_parsed_a_line_terminator := true;
    r

  (* these symbols cannot be followed by a division *)
  | Lbracket _
  | Lcurly _
  | Rcurly _
  | Lparen _
  | Dot _
  | Semic _
  | Comma _
  | Lt _
  | Gt _
  | Le _
  | Ge _
  | EqualEqual _
  | BangEqual _
  | EqualEqualEqual _
  | BangEqualEqual _
  | Plus _
  | Minus _
  | Times _
  | Percent _
  | LtLt _
  | GtGt _
  | GtGtGt _
  | Amper _
  | Bar _
  | Chapeau _
  | Bang _
  | Tilda _
  | AmperAmper _
  | BarBar _
  | Question _
  | Colon _
  | Equal _
  | PlusEqual _
  | MinusEqual _
  | TimesEqual _
  | PercentEqual _
  | LtLtEqual _
  | GtGtEqual _
  | GtGtGtEqual _
  | AmperEqual _
  | BarEqual _
  | ChapeauEqual _
  | Div _
  | DivEqual _
  | Break _
  | Case _
  | Catch _
  | Continue _
  | Debugger _
  | Default _
  | Delete _
  | Do _
  | Else _
  | Finally _
  | For _
  | Function _
  | If _
  | In _
  | Instanceof _
  | New _
  | Return _
  | Switch _
  | This _
  | Throw _
  | Typeof _
  | Try _
  | Var _
  | Void _
  | While _
  | With _
  | Class _
  | Const _
  | Enum _
  | Export _
  | Extends _
  | Import _
  | Super _
  | Implements _
  | Interface _
  | Let _
  | Package _
  | Private _
  | Protected _
  | Public _
  | Static _
  | Yield _
      as r ->
    just_parsed_a_line_terminator := false;
    can_have_a_division := false;
    r

  (* these symbols can be followed by a division *)
  | EOF _ (* don't care *)
  | Rbracket _
  | PlusPlus _
  | MinusMinus _
  | Rparen _
  | Ident _
  | False _
  | True _
  | Null _
  | Regexp _
  | String _
  | Integer _
      as r ->
    just_parsed_a_line_terminator := false;
    can_have_a_division := true;
    r

  (* When lexing comments, we don't have to produce a 100% correct
     token sequence, since the corresponding parser will mostly ignore
     these tokens. Therefore, we ignore the rest of the state in those
     cases *)
  | DocComment _ as s -> s

let init_lexer () =
  can_have_a_division := false;
  just_parsed_a_line_terminator := true

let stream lex_comments lexbuf =
  Stream.from (
    fun _ ->
      match lex lex_comments lexbuf with
      | EOF _ -> None
      | t -> Some t
  )

let stream_of_file ?(lex_comments=false) file =
  init_lexer ();
  try
    let ic_ = open_in file in
    Gc.finalise close_in ic_; (* garbage collecting the input channel *)
    let lexbuf = Lexing.from_channel ic_ in
    stream lex_comments lexbuf, lexbuf
  with Sys_error diagnostic ->
    Printf.printf "Couldn't open file %s: %s\n%!" file diagnostic;
    exit 1

let stream_of_string ?(lex_comments=false) string =
  init_lexer ();
  let lexbuf = Lexing.from_string string in
  stream lex_comments lexbuf, lexbuf
}
