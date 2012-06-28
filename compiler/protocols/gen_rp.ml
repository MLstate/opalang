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
let eprintf fmt = Format.kfprintf (fun _ -> Format.pp_print_flush Format.err_formatter ()) Format.err_formatter fmt
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

let us = S.unsafe_sub

(* Unescape string: "\\n" --> "\n" etc., fairly complete wrt. ocaml string escapes. *)

let is_lu ch = C.is_lower ch || C.is_upper ch
let is_hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let c3i c1 c2 c3 = ((C.code c1)-48)*100+((C.code c2)-48)*10+((C.code c3)-48)
let c2h c1 c2 = (C.hexa_value (c1))*16+(C.hexa_value c2)

let unescape s =
  let l = S.length s in
  let r = S.copy s in
  let rec aux i j =
    if i < l
    then
      (match s.[i] with
       | '\\' ->
           let k = i + 1 in
           if k < l
           then
             (match s.[k] with
              | '\\' -> (r.[j] <- '\\'; aux (i+2) (j+1))
              | '"' -> (r.[j] <- '"'; aux (i+2) (j+1))
              | '\'' -> (r.[j] <- '\''; aux (i+2) (j+1))
              | 'n' -> (r.[j] <- '\n'; aux (i+2) (j+1))
              | 'r' -> (r.[j] <- '\r'; aux (i+2) (j+1))
              | 't' -> (r.[j] <- '\t'; aux (i+2) (j+1))
              | 'b' -> (r.[j] <- '\b'; aux (i+2) (j+1))
              | ' ' -> (r.[j] <- ' '; aux (i+2) (j+1))
              | '0'..'9' ->
                  (if k + 3 < l
                   then
                     (match s.[k+1],s.[k+2] with
                      | ('0'..'9','0'..'9') -> (r.[j] <- C.chr (c3i s.[k] s.[k+1] s.[k+2]); aux (i+4) (j+1))
                      | _ -> (r.[j] <- '\\'; r.[j+1] <- s.[k]; aux (i+2) (j+2)))
                   else (r.[j] <- '\\'; r.[j+1] <- s.[k]; aux (i+2) (j+2)))
              | 'x' ->
                  (if k + 3 < l
                   then
                     (match s.[k+1],s.[k+2] with
                      | (ch1,ch2) when (is_hex ch1 && is_hex ch2) ->
                          (r.[j] <- C.chr (c2h s.[k+1] s.[k+2]); aux (i+4) (j+1))
                      | _ -> (r.[j] <- '\\'; r.[j+1] <- s.[k]; aux (i+2) (j+2)))
                   else (r.[j] <- '\\'; r.[j+1] <- s.[k]; aux (i+2) (j+2)))
              | ch -> (r.[j] <- '\\'; r.[j+1] <- ch; aux (i+2) (j+2)))
           else (r.[j] <- '\\'; aux (i+1) (j+1))
       | ch -> (r.[j] <- ch; aux (i+1) (j+1)))
    else j
  in
  us r 0 (aux 0 0)

(*unescape "\\r\\n\\t\\b\\\\\\\"\\'\048\x61\n" = "\r\n\t\b\\\"'0a\n";;*)

let geteqstr str = unescape (S.strip_quotes str)

(*geteqstr "\"abc\\r\\n\"" = "abc\r\n";;*)

let pre_map lst =
  L.map (function
            | (G.Raw (name,(pre,opts),sects)) ->
                let pre = geteqstr pre in
                (pre,G.Raw (name,(pre,opts),sects))
            | _ -> raise (Failure "pre_map")) lst

let parse_str v = function
  | (_v,t_opt,Some (l,_opts),fs_opt) ->
      (match fs_opt, t_opt with
       | (Some (fs,_),_) -> "("^fs^" ("^v^"))"
       | (_,None) -> v
       | (_,Some (O.TypeConst O.TypeUnit)) -> "((function \"()\" -> () | _ -> raise (Failure \"unit_of_string\")) ("^v^"))"
       | (_,Some (O.TypeConst O.TypeBool)) -> "(bool_of_string ("^v^"))"
       | (_,Some (O.TypeConst O.TypeInt)) -> "(int_of_string ("^v^"))"
       | (_,Some (O.TypeConst O.TypeInt64)) -> "(Int64.of_string ("^v^"))"
       | (_,Some (O.TypeName (_,["int64"]))) -> "(Int64.of_string ("^v^"))"
       | (_,Some (O.TypeConst O.TypeFloat)) -> "(float_of_string ("^v^"))"
       | (_,Some (O.TypeConst O.TypeString)) -> v
       | (_,Some (O.TypeName ([O.TypeConst O.TypeString], ["list"]))) -> "(Str.split (Str.regexp_string \";\") ("^v^"))"
       | (_,Some t) ->
           OcamlPrint.Output.type_expr stderr t; prerr_newline (); flush stderr;
           failwith "Can't convert such a type from string.")
  | _ -> v

let parse_null = function
  | (_v,t_opt,Some (l,_opts),fs_opt) ->
      (match fs_opt, t_opt with
       | (Some (fs,_),_) -> false
       | (_,None) -> true
       | (_,Some (O.TypeConst O.TypeUnit)) -> false
       | (_,Some (O.TypeConst O.TypeBool)) -> false
       | (_,Some (O.TypeConst O.TypeInt)) -> false
       | (_,Some (O.TypeConst O.TypeInt64)) -> false
       | (_,Some (O.TypeName (_,["int64"]))) -> false
       | (_,Some (O.TypeConst O.TypeFloat)) -> false
       | (_,Some (O.TypeConst O.TypeString)) -> true
       | (_,Some (O.TypeName ([O.TypeConst O.TypeString], ["list"]))) -> false
       | (_,Some t) ->
           OcamlPrint.Output.type_expr stderr t; prerr_newline (); flush stderr;
           failwith "Can't convert such a type from string.")
  | _ -> true

let has_lu str =
  let len = S.length str in
  let rec aux n = if n >= len then false else if is_lu str.[n] then true else aux (n+1) in
  aux 0

let get_sect name i = function
  | (_v,t_opt,Some (l,opts),fs_opt) as sect ->
      (*eprintf "opts: %s\n" (S.concat "," opts); flush stderr;*)
      let idx = string_of_int (i+1) in
      let ls = geteqstr l in
      let lslen = S.length ls in
      let _lws = if L.mem "m" opts then "_lws" else "" in
      let _ci = if L.mem "i" opts && has_lu ls then "_ci" else "" in
      let skip1 = if L.mem "s" opts then "let p = skip_sptab str strlen p in\n    " else "" in
      let skip2 = if L.mem "l" opts then "let p = skip_lws str strlen p in\n    " else "" in
      let pn = parse_null sect in
      let s = if pn then "v" else "s" in
      let getstr = "let (p,_l,"^s^idx^") = (upto_mark"^_lws^_ci^" "^l^" "^(string_of_int lslen)^" str strlen p) in\n    " in
      let trim = if L.mem "t" opts then "let "^s^idx^" = rmldtrsp0 "^s^idx^" _l in\n    " else "" in
      let getv = if pn then "" else "let v"^idx^" = "^(parse_str ("s"^idx) sect)^" in\n    " in
      skip1^skip2^getstr^trim^getv
  | _ -> name

let get_sects name opts = function
  | [] -> ("%n", "", name)
  | sects ->
      (*eprintf "name(2): %s opts=%s\n" name (S.concat "," opts); flush stderr;*)
      let lets = L.mapi (fun i sect -> (get_sect name i sect)) sects in
      let skip = if L.mem "s" opts then "let p = skip_sptab str strlen p in\n    " else "" in
      let vs = L.mapi (fun i _ -> "v"^(string_of_int (i+1))) sects in
      ("p",
       "\n    let p = %n in\n    "^skip^(S.concat "" lets),
       "("^name^" ("^(S.concat ", " vs)^"))")

let rec map_pre_to_cons lst pre =
  match L.fold_left (fun l (p,n) -> if p = pre then n::l else l) [] lst with
  | [] -> eprintf "map_pre_to_cons: no matches\n"; flush stderr; ("%n","","<no matches>")
  | [(G.Raw (name,(p,opts),sects))] -> get_sects name opts sects
  | raws -> eprintf "map_pre_to_cons: multiple matches\n"; flush stderr; ("%n","","<multiple matches>")

let do_it types name lst file =
  let hdrs = L.fold_left (fun l r -> match r with G.Raw (_,(p,_),_) ->
                               let p = unescape (S.strip_quotes p) in
                               if S.length p >= 1 then p::l else l | _ -> l) [] lst in
  let premap = pre_map lst in
  eprintf "mkrp: Generating %s/%s\n" (Unix.getcwd()) file;
  Mkrp.mktab1
    ~ci:true
    ~from_n:true
    ~str_to_arg:(map_pre_to_cons premap)
    ~prefix:name
    ~header:"open HttpTools\nopen HttpServerCore\n"
    ~argtys:[("str","string");("strlen","int")]
    ~intype:"rawmsg"
    ~restype:"int * rawmsg"
    ~errordef:[]
    file (Array.of_list hdrs) 2 0
