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

open Base

let string_of_production = function
  | `string s -> `prod (sprintf "(text %s)" s)
  | `production (hd, tl) -> `prod (sprintf "(print_%s (%s))" hd tl)
  | `ocaml o -> `prod (sprintf "(%s)" o) (* FIXME: text ? *)
  | `var v -> `prod v
  | `operator o -> `change o
  | `space -> `change "<+>"

let rec output_productions ?(first=false) ?(operator="<<>>") = function
  | hd :: tl ->
      begin
	match string_of_production hd with
	| `change operator -> output_productions ~operator tl
	| `prod p -> 
	    if first then p ^ (output_productions tl)
	    else sprintf " %s %s%s" operator p (output_productions tl)
      end
  | [] -> ""

let to_ocaml s =
  try
    let _, map = Prxparse.parse_prxparse_file s in
    let _, code = StringMap.fold (
      fun name rules (first, acc) ->
	let init = sprintf "%s %s = function\n" (if first then "let rec" else "and") (sprintf "print_%s" name) in
	let code = List.fold_left (
	  fun acc (pattern, productions) ->
	    acc ^
	      sprintf "| %s -> %s\n" pattern (if productions=[] then "text \"\"" else output_productions ~first:true productions)
	) init rules in	  
	false, acc ^ code
    ) map (true, "") in
    code
  with
  | _ -> Printf.eprintf "Syntax error"; assert false
