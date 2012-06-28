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

