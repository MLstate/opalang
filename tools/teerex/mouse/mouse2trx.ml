(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(*
    @author Adam Koprowski
**)

(**
 * A tool for converting the Mouse format to TRX format.
**)

(* TODO Maybe instead of syntax conversion, add .mouse as a supported format to TRX
        and do AST conversion? (problem: what if we want to add productions?) *)

open MouseAst

let header () = Printf.sprintf "
## Grammar generated automatically by %s from %s
## DO NOT EDIT!!!!!!!\n\n

%%%%memoization=none
" Sys.argv.(0) Sys.argv.(1)

let mouse2trx g =
  let char_in_range = function
    | '-' -> "\\-"
    | c -> Char.escaped c
  in
  let rec item2str = function
    | Call c -> c
    | Parens e -> Printf.sprintf "( %s )" (exp2str e)
    | Literal l -> Printf.sprintf "\"%s\"" l
    | Any -> "."
    | Range (c1, c2) -> Printf.sprintf "[%c-%c]" c1 c2
    | CharClass c -> Printf.sprintf "[%s]" (Base.List.to_string char_in_range c)
  and prefix2str = function
    | `AND -> "&"
    | `NOT -> "!"
    | `NORMAL -> ""
  and suffix2str = function
    | `OPTION -> "?"
    | `STAR -> "*"
    | `PLUS -> "+"
    | `NORMAL -> ""
  and primary2str (prefix, item, suffix) =
    Printf.sprintf "%s%s%s" (prefix2str prefix) (item2str item) (suffix2str suffix)
  and seq2str e = BaseString.concat_map "  " primary2str e ^ " $"
  and exp2str e = BaseString.concat_map " / " seq2str e in
  let print_rule (Rule (name, e, desc)) =
    Printf.sprintf "%s%s : {unit} <- %s;\n\n" 
      (match desc with None -> "" | Some n -> Printf.sprintf "/* %s */\n" n)
      name
      (exp2str e)
  in
  Base.List.to_string print_rule g

let _ =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s [grammar].mouse\n" Sys.argv.(0)
  else
    let s = File.content Sys.argv.(1) in
    let _, g = Mouse.parse_mouse_grammar s in
    Printf.printf "%s+%s" (header ()) (mouse2trx g)

