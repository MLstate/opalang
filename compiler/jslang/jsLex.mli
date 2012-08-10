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

(**
   The javascript lexer

   The lexer is stateful. Thus, after creating a token stream, only
   one thread can access it.

   This module is not meant to be called directly, use instead the high level
   functions provided in jsParse if you want to parse some javascript.
*)

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

(* These tokens are used only when parsing comments for bsl files
   and are not produced in normal lexing*)
| DocComment of pos * doc_comment_elt list * bool

val string_of_token : token -> string

type state

val initial_state : (* filename *) string -> (* lex_comments *) bool -> state
val lex : state -> Lexing.lexbuf -> token
val stream : string -> bool -> Lexing.lexbuf -> token Stream.t

(**
   These high level functions automatically init the lexer
   when called
*)
val stream_of_file : ?lex_comments:bool -> string -> token Stream.t * Lexing.lexbuf
val stream_of_string : ?filename:string -> ?lex_comments:bool -> string -> token Stream.t * Lexing.lexbuf
