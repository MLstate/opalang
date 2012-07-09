(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)


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
