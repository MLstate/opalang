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
(* Accept: see the interface file for docs. *)
open Printf
open Base
open HttpTools
open HttpServerTypes

type el = string list * string list list * float option

type t = string option * char option * el list

let flre = Str.regexp "[ \t]*\\([0-9]+\\.[0-9]\\)+[ \t]*"
let get_fl str = if Str.string_match flre str 0 then float_of_string (Str.matched_group 1 str) else 0.0

let is_q = function ("q"::_::_) -> true | _ -> false
let get_q lst =
  match List.find_opt (function ("q"::_::_) -> true | _ -> false) lst with
  | Some (_::v::_) -> Some (get_fl v)
  | _ -> None
let mv_q (l1,l2) = let (l2q,l2a) = List.partition is_q l2 in (l1,l2a,get_q l2q)

let split_opt regexp_opt str = match regexp_opt with | Some regexp -> Str.split regexp str | None -> [str]
let pair_opt = function | Some (a,b) -> Some a, Some b | None -> None, None

let regexp1 = Str.regexp "[ \t]*,[ \t]*"
let regexp2 = Str.regexp "[ \t]*;[ \t]*"
let regexp3 ch_opt = Option.map (fun ch -> (ch, Str.regexp (Printf.sprintf "[ \t]*%c[ \t]*" ch))) ch_opt
let regexp3sl = regexp3 (Some '/')
let regexp3mi = regexp3 (Some '-')
let regexp4 = Str.regexp "[ \t]*=[ \t]*"

let make_el (regexp3_opt:(char * Str.regexp) option) (str:string) : el =
  let _, regexp_opt = pair_opt regexp3_opt in
  (function | s1::sl -> mv_q (split_opt regexp_opt s1, List.map (Str.split regexp4) sl)
            | [] -> ([],[],None))
    (Str.split regexp2 str)

let make_el_ch (ch_opt:char option) (str:string) : el =
  make_el (match ch_opt with Some ch -> Some (ch,Str.regexp (sprintf "[ \t]*%c[ \t]*" ch)) | None -> None) str

let make (regexp3_opt:(char * Str.regexp) option)  (str:string) : t =
  let str, comment_opt = strcom str (String.length str) in
  let ch_opt, _ = pair_opt regexp3_opt in
  (comment_opt, ch_opt, List.map (make_el regexp3_opt) (Str.split regexp1 str))

let size ((_,_,phdr):t) = List.length phdr

let q_value ((_,_,q_opt):el) = Option.default 1.0 q_opt

let q_rank ((comment_opt,ch_opt,phdr):t) : t =
  let cmp (q1,_) (q2,_) = Pervasives.compare q2 q1 in
  (comment_opt,ch_opt,List.map snd (List.sort cmp (List.map (fun ph -> (q_value ph,ph)) phdr)))

let q_nth ((_,_,phdr):t) (n:int) : el option =
  try Some (List.nth phdr n)
  with Invalid_argument "List.nth" | Failure "nth" -> None

(* TODO: do we want "text" to match "text/*" ??? *)
(* or "text/html;level=1" to match "text/html" *)
let hmtch sl pl =
  try List.for_all2 (fun s p -> p = "*" || s = "*" || s = p) sl pl
  with Invalid_argument "List.for_all2" -> false

let q_max ((_,_,phdr):t) ((sl1,sl2,_):el) : float * el option =
  List.fold_left
    (fun (a,plmax) ((pl1,pl2,_) as pl) ->
       if hmtch sl1 pl1 
          && try List.for_all2 hmtch sl2 pl2 with Invalid_argument "List.for_all2" -> false
       then let q = q_value pl in if q > a then (q,Some pl) else (a,plmax) 
       else (a,plmax)) (0.0,None) phdr

let q_preferred (cphdr:t) (sls:el list) : el option =
  let maxs = List.map (q_max cphdr) sls in
  match List.fold_left (fun (q1,sl) (q2,mx) -> if q1 > q2 then (q1,sl) else (q2,mx)) (0.0,None) maxs with
  | (0.0,_) -> None (* <-- RFC says we return 406 Unacceptable for this. *)
  | (_,Some (sl1,sl2,q_opt)) -> Some (sl1,sl2,q_opt)
  | _ -> None (* We'll treat malformed q's as 0 and missing q's as 1, see RFC *)

let string_of_el (chstr:string) ((sl1,sl2,q_opt):el) : string =
  String.concat "; " ((String.concat chstr sl1)
                     ::(List.map (String.concat "=") sl2)
                     @(match q_opt with Some q -> ["q="^(sprintf "%2.1f" q)] | None -> []))

let to_string ((comment_opt,ch_opt,phdr):t) : string =
  let chstr = match ch_opt with Some ch -> String.make 1 ch | None -> "" in
  (String.concat ", " (List.map (string_of_el chstr) phdr))
  ^(match comment_opt with Some comment -> " "^comment | None -> "")

let to_string_opt (phdr_opt:t option) : string = Option.default "" (Option.map to_string phdr_opt)

let get_accept (req:HttpServerTypes.request) : t option =
  try
    match List.find_opt (function HttpServerCore_parse.Accept _ -> true | _ -> false)
                        req.HttpServerTypes.request_header with
    | Some (HttpServerCore_parse.Accept str) -> Some (make regexp3sl str)
    | _ -> None
  with _ -> None

let set_accept (req:HttpServerTypes.request) (phdr_opt:t option) : HttpServerTypes.request =
  HttpServer.replace_request_header (HttpServerCore_parse.Accept (to_string_opt phdr_opt)) req

let get_accept_charset (req:HttpServerTypes.request) : t option =
  try
    match List.find_opt (function HttpServerCore_parse.Accept_Charset _ -> true | _ -> false)
                        req.HttpServerTypes.request_header with
    | Some (HttpServerCore_parse.Accept_Charset str) -> Some (make None str)
    | _ -> None
  with _ -> None

let set_accept_charset (req:HttpServerTypes.request) (phdr_opt:t option) : HttpServerTypes.request =
  HttpServer.replace_request_header (HttpServerCore_parse.Accept_Charset (to_string_opt phdr_opt)) req

let get_accept_encoding (req:HttpServerTypes.request) : t option =
  try
    match List.find_opt (function HttpServerCore_parse.Accept_Encoding _ -> true | _ -> false)
                        req.HttpServerTypes.request_header with
    | Some (HttpServerCore_parse.Accept_Encoding str) -> Some (make None str)
    | _ -> None
  with _ -> None

let set_accept_encoding (req:HttpServerTypes.request) (phdr_opt:t option) : HttpServerTypes.request =
  HttpServer.replace_request_header (HttpServerCore_parse.Accept_Encoding (to_string_opt phdr_opt)) req

let get_accept_language (req:HttpServerTypes.request) : t option =
  try
    match List.find_opt (function HttpServerCore_parse.Accept_Language _ -> true | _ -> false)
                        req.HttpServerTypes.request_header with
    | Some (HttpServerCore_parse.Accept_Language str) -> Some (make regexp3mi str)
    | _ -> None
  with _ -> None

let set_accept_language (req:HttpServerTypes.request) (phdr_opt:t option) : HttpServerTypes.request =
  HttpServer.replace_request_header (HttpServerCore_parse.Accept_Language (to_string_opt phdr_opt)) req

