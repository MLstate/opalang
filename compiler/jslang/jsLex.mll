(*
    copyright © 2011 MLstate

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

(* These tokens are used only when parsing comments for bsl files and
   are not produced in normal lexing. The boolean flag indicates
   whether the comment contains a line terminator or not. *)
| DocComment of pos * doc_comment_elt list * bool

(* The lexer keeps the following invariant: any consecutive sequence
   of new lines and comments will have at most one new line at the
   beginning followed by a sequence of comments. This makes the
   parsing a little bit easier. *)
type state = {
  filename: string;

  lex_comments: bool;

  (* the ecmascript standard defines two kinds of lexing: for the
   * places in the ast where a token starting with / is a regular
   * expression, and the places where it is the division or the
   * division-assignment /= to avoid having to have some information
   * flow from the parser to the lexer, this is done by looking at the
   * previous token (see function lex) *)
  mutable can_have_a_division: bool;

  (* true whenever the lexer has produced an LT token and read only
     LTs and comments afterwards *)
  mutable just_parsed_a_line_terminator: bool;

  (* This queue holds back comments with no newlines, and possibly in
     the end a comment with a newline or a non-comment token. It is
     used to satisfy the lexer invariant mentioned above *)
  mutable waiting_tokens: token Queue.t;

  mutable flush_tokens: bool;
}

let mp s lexbuf =
  FilePos.make_pos s.filename
    (Lexing.lexeme_start lexbuf)
    (Lexing.lexeme_end lexbuf)

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

  let get_doc_comment_elt s start =
    let re = Str.regexp "[ \t\\*]*@\\([a-zA-Z0-9]*\\)[ \t]*\\(.*\\)" in
    let line = Buffer.contents b in
    Buffer.clear b;
    let pos = FilePos.make_pos s.filename start (start + String.length line) in
    if Str.string_match re line 0 then
      let tag = Str.matched_group 1 line in
      let args = Str.matched_group 2 line in
      CommentTag (pos, tag, args)
    else
      CommentLine (pos, line)
}

let identifier_part = ['a'-'z''A'-'Z''_''$''0'-'9']
let identifier = ['a'-'z''A'-'Z''_''$'] identifier_part*
let hexa = ['0'-'9''a'-'f''A'-'F']

rule main s = parse
| ['\t''\012''\013'' ']+ { main s lexbuf }

(* beware that we must not throw newlines to be able to implement semicolon
 * insertions *)
| ['\n' '\r']+ { LT (mp s lexbuf) }

| "//" [^'\n''\r']* { main s lexbuf }

| "/**/" { main s lexbuf }

| "/**" {
  let start = Lexing.lexeme_start lexbuf in
  if s.lex_comments then
    doc_comment s start false [] lexbuf
  else multiline_comment s start false lexbuf
}

(* beware that if a newline appears in a multi line comment
 * then we _must_ generate a newline token *)
| "/*" {
  let start = Lexing.lexeme_start lexbuf in
  multiline_comment s start false lexbuf
}

| '/' {
  if s.can_have_a_division then
    Div (mp s lexbuf)
  else (
    Buffer.clear b;
    let start = Lexing.lexeme_start lexbuf in
    regexp_body s start lexbuf
  )
}
| "/=" {
  if s.can_have_a_division then
    DivEqual (mp s lexbuf)
  else (
    Buffer.clear b;
    Buffer.add_char b '=';
    let start = Lexing.lexeme_start lexbuf in
    regexp_body s start lexbuf
  )
}
| '{' { Lcurly (mp s lexbuf) }
| '}' { Rcurly (mp s lexbuf) }
| '[' { Lbracket (mp s lexbuf) }
| ']' { Rbracket (mp s lexbuf) }
| '(' { Lparen (mp s lexbuf) }
| ')' { Rparen (mp s lexbuf) }
| '.' { Dot (mp s lexbuf) }
| ';' { Semic (mp s lexbuf) }
| ',' { Comma (mp s lexbuf) }
| '<' { Lt (mp s lexbuf) }
| '>' { Gt (mp s lexbuf) }
| "<=" { Le (mp s lexbuf) }
| ">=" { Ge (mp s lexbuf) }
| "==" { EqualEqual (mp s lexbuf) }
| "!="  { BangEqual (mp s lexbuf) }
| "===" { EqualEqualEqual (mp s lexbuf) }
| "!==" { BangEqualEqual (mp s lexbuf) }
| "+" { Plus (mp s lexbuf) }
| "-" { Minus (mp s lexbuf) }
| "*" { Times (mp s lexbuf) }
| "%" { Percent (mp s lexbuf) }
| "++" { PlusPlus (mp s lexbuf) }
| "--" { MinusMinus (mp s lexbuf) }
| "<<" { LtLt (mp s lexbuf) }
| ">>" { GtGt (mp s lexbuf) }
| ">>>" { GtGtGt (mp s lexbuf) }
| "&" { Amper (mp s lexbuf) }
| "|" { Bar (mp s lexbuf) }
| "^" { Chapeau (mp s lexbuf) }
| "!" { Bang (mp s lexbuf) }
| "~" { Tilda (mp s lexbuf) }
| "&&" { AmperAmper (mp s lexbuf) }
| "||" { BarBar (mp s lexbuf) }
| "?" { Question (mp s lexbuf) }
| ":" { Colon (mp s lexbuf) }
| "=" { Equal (mp s lexbuf) }
| "+=" { PlusEqual (mp s lexbuf) }
| "-=" { MinusEqual (mp s lexbuf) }
| "*=" { TimesEqual (mp s lexbuf) }
| "%=" { PercentEqual (mp s lexbuf) }
| "<<=" { LtLtEqual (mp s lexbuf) }
| ">>=" { GtGtEqual (mp s lexbuf) }
| ">>>=" { GtGtGtEqual (mp s lexbuf) }
| "&=" { AmperEqual (mp s lexbuf) }
| "|=" { BarEqual (mp s lexbuf) }
| "^=" { ChapeauEqual (mp s lexbuf) }

| identifier as i {
  let pos = mp s lexbuf in
  try
    Hashtbl.find keywords i pos
  with Not_found -> Ident (pos, i)
}
| ('0' | ['1'-'9'] ['0'-'9']*) '.' ['0'-'9']* (['e''E'] ['-''+']? ['0'-'9']+)?
| '.' ['0'-'9']+ (['e''E'] ['-''+']? ['0'-'9']+)?
| ('0' | ['1'-'9'] ['0'-'9']*) (['e''E'] ['-''+']? ['0'-'9']+)?
| '0' ['x''X'] hexa*
 as i { Integer ((mp s lexbuf), i) }
| "'" {
  let start = Lexing.lexeme_start lexbuf in
  Buffer.clear b;
  string s start false lexbuf
}
| '"' {
  let start = Lexing.lexeme_start lexbuf in
  Buffer.clear b;
  string s start true lexbuf
}
| eof { EOF (mp s lexbuf) }
| _ as c { raise (Stream.Error (Printf.sprintf "unexpected character %C in main lexing" c)) }

(* regular expression are not really parsed, you simply interpret them enough
 * so that you can find the end of the regexp
 * in particular, escapes are *not* interpreted, and so the string in the regexp
 * node and token should not be escaped when printed *)
and regexp_body s start = parse
| ['\r''\n'] { raise (Stream.Error "Line terminator inside a regexp literal") }
| '\\' _ as str { Buffer.add_string b str; regexp_body s start lexbuf }
| '[' as c {
  Buffer.add_char b c;
  character_class lexbuf;
  regexp_body s start lexbuf
}
| [^'\\''\r''\n''[' '/']+ as str {
  Buffer.add_string b str;
  regexp_body s start lexbuf
}
| '/' {
  let body = Buffer.contents b in
  Buffer.clear b;
  regexp_flags s start body lexbuf
}
| _ as c { raise (Stream.Error (Printf.sprintf "unexpected character %C in regexp body" c)) }
| eof { raise (Stream.Error "unterminated regexp body ") }
and character_class = parse
| ']' as c { Buffer.add_char b c }
| '\\' _ as s { Buffer.add_string b s; character_class lexbuf }
| [^'\\' ']']+ as s { Buffer.add_string b s; character_class lexbuf }
| _ as c { raise (Stream.Error (Printf.sprintf "unexpected character %C in character class" c)) }
| eof { raise (Stream.Error "unterminated character class ") }
and regexp_flags s start body = parse
| identifier_part* as flags {
  let pos = FilePos.make_pos s.filename start (Lexing.lexeme_end lexbuf) in
  Regexp (pos,body,flags)
}

(* [double] is true when the string is enclosed in double quotes
 * and false when it is enclosed in single quotes *)
and string s start double = parse
| "'" {
  if double then (
    Buffer.add_char b '\'';
    string s start double lexbuf
  ) else (
    let str = Buffer.contents b in
    let pos = FilePos.make_pos s.filename start (Lexing.lexeme_end lexbuf) in
    Buffer.clear b;
    String (pos, str)
  )
}
| '"' {
  if double then (
    let str = Buffer.contents b in
    let pos = FilePos.make_pos str start (Lexing.lexeme_end lexbuf) in
    Buffer.clear b;
    String (pos, str)
  ) else (
    Buffer.add_char b '"';
    string s start double lexbuf
  )
 }
| [^'\'' '"' '\\''\n''\r']+ as str {
  Buffer.add_string b str;
  string s start double lexbuf
}
| ['\n' '\r'] { raise (Stream.Error "Line terminator inside a single string literal") }
| "\\" (['0'-'7'] ['0'-'7']? ['0'-'7']? as str) {
  Buffer.add_char b (Char.chr (int_of_string str));
  string s start double lexbuf
}
| "\\b" { Buffer.add_char b '\008'; string s start double lexbuf }
| "\\t" { Buffer.add_char b '\t'; string s start double lexbuf }
| "\\n" { Buffer.add_char b '\n'; string s start double lexbuf }
| "\\v" { Buffer.add_char b '\011'; string s start double lexbuf }
| "\\f" { Buffer.add_char b '\012'; string s start double lexbuf }
| "\\r" { Buffer.add_char b '\r'; string s start double lexbuf }
| "\\\\" { Buffer.add_char b '\\'; string s start double lexbuf }
| "\\" (['"''\''] as c) { Buffer.add_char b c; string s start double lexbuf }
| "\\u" (hexa hexa hexa hexa as str) {
  Buffer.add_string b (Scanf.sscanf str "%x" (fun d -> Cactutf.cons d));
  string s start double lexbuf
}
| "\\x" (hexa hexa as str) {
  Buffer.add_string b (Scanf.sscanf str "%x" (fun d -> Cactutf.cons d));
  string s start double lexbuf
}
| eof { raise (Stream.Error "unterminated string literal comment") }
| "\\" { Buffer.add_char b '\\'; string s start double lexbuf }
| _ as c { raise (Stream.Error (Printf.sprintf "unexpected character %C in a string literal" c)) }

(* [newline] is true when a newline has been parsed in the comment *)
and multiline_comment s start newline = parse
| [^'*''\n''\r']* { multiline_comment s start newline lexbuf }
| ['\r''\n'] { multiline_comment s start true lexbuf }
| "*/" {
  if newline then
    let pos = FilePos.make_pos s.filename start (Lexing.lexeme_end lexbuf) in
    LT pos
  else
    main s lexbuf
}
| '*' { multiline_comment s start newline lexbuf }
| eof { raise (Stream.Error "unterminated multiline comment") }

and doc_comment s start newline elts = parse
| "*/" {
  let elt = get_doc_comment_elt s start in
  let pos = FilePos.make_pos s.filename start (Lexing.lexeme_end lexbuf) in
  DocComment (pos, List.rev (elt :: elts), newline)
}
| "\\\n" {
  Buffer.add_char b ' ';
  doc_comment s start true elts lexbuf
}
| ['\r''\n'] {
  let elt = get_doc_comment_elt s start in
  doc_comment s start true (elt :: elts) lexbuf
}
| _ as c {
  Buffer.add_char b c;
  doc_comment s start newline elts lexbuf
}
| eof { raise (Stream.Error "unterminated multiline comment") }

{

let rec consume_token s lexbuf =
  let send_token token =
    if Queue.is_empty s.waiting_tokens then
      token
    else (
      s.flush_tokens <- true;
      flush s lexbuf
    )
  in

  match main s lexbuf with
  | LT _ when s.just_parsed_a_line_terminator ->
    (* can_have_a_division doesn't change *)
    (* just_parsed_a_line_terminator and flush_tokens is still true *)
    consume_token s lexbuf
  | LT _ as r ->
    (* can_have_a_division doesn't change *)
    s.just_parsed_a_line_terminator <- true;
    r
  | DocComment (pos, _, contains_newline) as r ->
    if s.just_parsed_a_line_terminator then (
      r
    ) else if contains_newline then (
      (* We must produce a newline in this case *)
      s.just_parsed_a_line_terminator <- true;
      s.flush_tokens <- true;
      Queue.add r s.waiting_tokens;
      LT pos
    ) else (
      Queue.add r s.waiting_tokens;
      consume_token s lexbuf
    )

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
    s.just_parsed_a_line_terminator <- false;
    s.can_have_a_division <- false;
    send_token r

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
    s.just_parsed_a_line_terminator <- false;
    s.can_have_a_division <- true;
    send_token r

(* the main lexing function: called the actual lexer, and updates the global
 * state *)
and flush s lexbuf =
  try
    Queue.take s.waiting_tokens
  with Queue.Empty ->
    s.flush_tokens <- false;
    consume_token s lexbuf

and lex s lexbuf =
  if s.flush_tokens then
    flush s lexbuf
  else
    consume_token s lexbuf

let initial_state filename lex_comments = {
  filename;
  lex_comments;
  can_have_a_division = false;
  just_parsed_a_line_terminator = true;
  waiting_tokens = Queue.create ();
  flush_tokens = false;
}

let stream filename lex_comments lexbuf =
  let initial_state = initial_state filename lex_comments in
  Stream.from (
    fun _ ->
      match lex initial_state lexbuf with
      | EOF _ -> None
      | t -> Some t
  )

let stream_of_file ?(lex_comments=false) filename =
  try
    let content = File.content filename in
    FilePos.add_file filename content;
    let lexbuf = Lexing.from_string content in
    stream filename lex_comments lexbuf, lexbuf
  with Sys_error diagnostic ->
    Printf.printf "Couldn't open file %s: %s\n%!" filename diagnostic;
    exit 1

let stream_of_string ?(filename="nofile") ?(lex_comments=false) string =
  let lexbuf = Lexing.from_string string in
  stream filename lex_comments lexbuf, lexbuf
}
