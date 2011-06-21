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
module B = Base
let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))
module O = Ocaml
module Cons = O.Cons
module G = Grammar
module T = Tools
module L = B.List
module A = B.Array
module S = B.String
module C = B.Char

let rec pr = function
    G.Prevent s -> "(Prevent "^s^")"
  | G.Literal (s,mod_opt) -> "(Literal ("^s^(match mod_opt with Some _mod -> ","^_mod | None -> "")^"))"
  | G.SubPattern (name,c,sub) -> "(SubPattern ("^name^","^c^","^(S.concat " " (L.map pr sub))^"))"
  | G.Ident name -> "(Ident "^name^")"
  | _ -> "blurb"

let rewrite_ident prevents = function
  | G.Literal (s,mod_opt) :: _ -> B.sprintf " ((%s !%s%s .)+ $_)" prevents s (Option.default "" mod_opt)
  | l when prevents <> "" or l = [] -> B.sprintf " ((%s .)+ $_) " prevents
  | _ -> raise T.LiteralRequired

let rewrite_pattern var_lst lst =
  let rec aux lst = function
    | [] -> ""
    | G.Prevent s :: tail -> aux (s :: lst) tail
    | G.Literal (s,mod_opt) :: tail when lst = [] -> s ^ (Option.default "" mod_opt) ^ " " ^ (aux [] tail)
    | G.SubPattern (name, c, sub) :: tail when lst = [] ->
        let lst = (L.rev_map snd @* snd) <| L.fold_left T.bruijnise (0, []) sub in
        let tuplized = S.concat ", " <| L.map (T.prefix "__") lst in
          B.sprintf " (%s {{ %s }})%s %s" (aux [] sub) tuplized c (aux [] tail)
    | G.Ident name :: tail ->
        let t = try T.type_to_str <| L.assoc name var_lst with Not_found -> name in
        let acc = L.fold_left (fun acc s -> acc ^ " !" ^ s) "" lst in
        let str =
          if t = "string" then rewrite_ident acc tail
          else acc ^ " " ^ t
        in B.sprintf " %s %s" str <| aux [] tail
    | _ -> assert false
  in aux [] lst

let rewrite_defines = function
  | G.Define (G.Constr (name, lst), pat) -> (
      try
        let var_lst = L.map T.tuple_of_var lst in
        let rule = rewrite_pattern var_lst pat in
        if L.is_empty var_lst then B.sprintf "%s {{ %s }}" rule name
        else
          let tuplized =
            let lst = snd <| L.fold_left T.bruijnise (0, []) pat in
            let lst = L.map (fun x ->
	      try L.assoc (fst x) lst with
		| Not_found ->
		  failwith (B.sprintf
	      "Fatal Error : %s is not bound when defining %s"
	      (fst x) name))  var_lst
            in S.concat ", " <| L.map (T.prefix "__") lst
          in B.sprintf "%s {{ %s (%s) }}" rule name tuplized
      with T.LiteralRequired -> failwith <|
        B.sprintf "Error when trying to generate the teerex rule for the message: %s" name
    )
  | _ -> assert false

let do_it types lst =
  let rules = S.concat "\n /" <| L.map rewrite_defines lst in
    O.Verbatim "%%memoization=none\n\ninclude default.trx\ntypes:{{\n"
     :: types
     @ [ O.Verbatim "\n}}\nmsg <- "
       ; O.Verbatim rules
       ; O.Verbatim "\n+msgs : {msg list} <- msg*"
       ; O.Verbatim "\n+msg1 : {msg} <- msg"
       ]
