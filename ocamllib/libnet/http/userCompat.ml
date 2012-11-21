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

(* depends *)
module String = BaseString
module List = BaseList

(*open Printf*)
open UserCompatType
(*open Rp_brow*)

module UCT = UserCompatType
module HSCp = HttpServerCore_parse
module HST = HttpServerTypes
module HSC = HttpServerCore

let ug = String.unsafe_get

(* Optimised version of String.is_contained_from *)
let icf p lp s ls n =
  let fin = ls - lp in
  let rec aux i =
    if i > fin
    then None
    else
      let rec aux0 p0 = p0 = lp or (let pp1 = i + p0 in pp1 < ls && ug p p0 = ug s pp1 && aux0 (succ p0)) in
      if aux0 0 then Some i else aux (succ i)
  in
  aux n

let icfs plplst s ls n =
  let rec aux = function
    | (p,lp)::t ->
        (match icf p lp s ls n with
         | Some i -> Some i
         | None -> aux t)
    | [] -> None
  in
  aux plplst

let verlstch ch str = List.map int_of_string (String.slice ch str)

let verlst = verlstch '.'
let verlstm = verlstch '-'

let regexp1 = Str.regexp "[ ]*\\([0-9\\.]+\\)[ ]*"
let browver str n =
  if Str.string_match regexp1 str n
  then verlst (Str.matched_group 1 str)
  else []

let wkvar str strlen n =
  match icf "Chrome/" 7 str strlen n with
  | Some m -> UCT.Chrome (browver str (m+7))
  | None ->
      (match icf "Safari/" 7 str strlen n with
       | Some m -> UCT.Safari (browver str (m+7))
       | None -> UCT.UnidentifiedWV)

let regexp2 = Str.regexp " \\([0-9\\-]+\\);"
let wii str strlen n =
  match icf "Nintendo Wii" 12 str strlen n with
  | Some _ ->
      (try
         match Str.search_forward regexp2 str n with
         | _ -> UCT.Wii (verlstm (Str.matched_group 1 str))
       with Not_found -> UCT.Wii [])
  | None -> UCT.UnidentifiedRE

let googlebot str strlen n =
  match icf "Googlebot/" 10 str strlen n with
  | Some m -> UCT.Bot (UCT.Googlebot (browver str (m+10)))
  | None -> UCT.UnidentifiedRE

let regexp3 = Str.regexp " \\([0-9\\.]+\\))"
let psp str strlen n =
  match icfs [("PSP",3);("PLAYSTATION 3",13)] str strlen n with
  | Some _ ->
      (try
         match Str.search_forward regexp3 str n with
         | _ -> PS (verlst (Str.matched_group 1 str))
       with Not_found -> PS [])
  | None -> UCT.UnidentifiedRE

let mspie str strlen n =
  match icf "MSPIE" 5 str strlen n with
  | Some m -> UCT.Trident (browver str (m+5))
  | None -> UCT.UnidentifiedRE

let msie str strlen n =
  match icf "MSIE" 4 str strlen n with
  | Some m -> UCT.Trident (browver str (m+4))
  | None -> UCT.UnidentifiedRE

let chk_specials slst str strlen n =
  let rec aux = function
    | (sp::rest) ->
        let res = sp str strlen n in
        if res = UCT.UnidentifiedRE
        then aux rest
        else res
    | [] -> UCT.UnidentifiedRE
  in
  aux slst

let fn_Googlebot str strlen n _str =
  match icf "/" 1 str strlen n with
  | Some m -> UCT.Bot (UCT.Googlebot (browver str (m+1)))
  | None ->
  (match icf "-Image/" 7 str strlen n with
   | Some m -> UCT.Bot (UCT.Googlebot (browver str (m+7)))
   | None -> UCT.Bot (UCT.Googlebot []))

let fn_MSNBOT str strlen n _str =
  match icf "/" 1 str strlen n with
  | Some m -> UCT.Bot (UCT.Msnbot (browver str (m+1)))
  | None -> UCT.Bot (UCT.Msnbot [])

let fn_msnbot str strlen n _str =
  match icf "/" 1 str strlen n with
  | Some m -> UCT.Bot (UCT.Msnbot (browver str (m+1)))
  | None ->
  (match icf "-media/" 7 str strlen n with
  | Some m -> UCT.Bot (UCT.Msnbot (browver str (m+7)))
  | None ->
  (match icf "-webmaster/" 11 str strlen n with
  | Some m -> UCT.Bot (UCT.Msnbot (browver str (m+11)))
  | None -> UCT.Bot (UCT.Msnbot [])))

let fn_Yahoobot _str _strlen _n _str = UCT.Bot UCT.Yahoobot

let fn_YahooSeeker str strlen n _str = msie str strlen n

let fn_Mozilla str strlen n _str =
  match icf "AppleWebKit/" 12 str strlen n with
  | Some m -> UCT.Webkit (browver str (m+12),wkvar str strlen (m+12))
  | None ->
  (match icf "Gecko" 5 str strlen n with
   | Some _ ->
       (match icf "rv:" 3 str strlen n with
        | Some l -> UCT.Gecko (browver str (l+3))
        | None ->
            (match icf "KHTML/" 6 str strlen n with
             | Some l -> UCT.KHTML (browver str (l+6))
             | None -> UCT.UnidentifiedRE))
   | None -> chk_specials [msie; mspie; psp; googlebot] str strlen n)

let regexp4 = Str.regexp "(\\([0-9\\.]+\\))"
let fn_Nokia str _strlen n _str =
  try
    UCT.Nokia (match Str.search_forward regexp4 str n with
           | _ -> verlst (Str.matched_group 1 str))
  with Not_found -> UCT.Nokia []

let fn_Opera str strlen n _str =
  match icf "Presto/" 7 str strlen n with
  | Some m -> UCT.Presto (browver str (m+7))
  | None ->
      (match wii str strlen n with
       | UCT.UnidentifiedRE -> UCT.Presto []
       | re -> re)

let fn_Microsoft str strlen n _str =
  match icf "Pocket Internet Explorer/" 25 str strlen n with
  | Some m -> UCT.Trident (browver str (m+25))
  | None -> UCT.UnidentifiedRE

let fn_MOT str strlen n _str = msie str strlen n

let fn_HTC str strlen n _str = msie str strlen n

let fn_Text name str strlen n _str =
  match icf "/" 1 str strlen n with
  | Some m -> UCT.Text (name,browver str (m+1))
  | None -> UCT.UnidentifiedRE

let fn_Seamonkey str strlen n _str =
  match icf "Gecko" 5 str strlen n with
   | Some _ ->
       (match icf "rv:" 3 str strlen n with (* <-- it doesn't actually have this *)
        | Some l -> UCT.Gecko (browver str (l+3))
        | None -> UCT.Gecko [])
   | None -> UCT.Gecko []

let fn_Dillo str strlen n _str =
  match icf "/" 1 str strlen n with
  | Some m -> UCT.Dillo (browver str (m+1))
  | None -> UCT.UnidentifiedRE

let fn_PSP str _strlen n _str =
  try UCT.PS (match Str.search_forward regexp1 str n with
              | _ -> verlst (Str.matched_group 1 str))
  with Not_found -> UCT.PS []

let fn_Links str _strlen n _str =
  try UCT.Text ("links",
                match Str.search_forward regexp1 str n with
                | _ -> verlst (Str.matched_group 1 str))
  with Not_found -> UCT.Text ("links",[])

(* MUST be in same order as string list in mkrp.ml *)
let browrpfn = [| fn_Mozilla; fn_Nokia; fn_Opera; fn_Microsoft; fn_MOT; fn_HTC; (fn_Text "w3m"); fn_Seamonkey; fn_Dillo; fn_PSP;
                  (fn_Text "wget"); (fn_Text "lwp"); (fn_Text "lynx"); fn_Links; (fn_Text "amaya");
                  fn_Googlebot; fn_msnbot; fn_MSNBOT; fn_Yahoobot; fn_YahooSeeker |]

let search_forward_opt regexp str n =
  try Some (Str.search_forward regexp str n)
  with Not_found -> None

let regexp6 = Str.regexp "\\(X11\\|Windows\\|MSIE\\|Microsoft\\|Macintosh\\|iPhone\\|iPod\\|Symbian\\|J2ME\\|Wii\\|PSP\\|PLAYSTATION\\|BeOS\\|Konqueror\\)"
let get_environment str =
  match search_forward_opt regexp6 str 0 with
  | Some _pos ->
      (match Str.matched_group 1 str with
       | "X11" | "Konqueror" -> UCT.X11
       | "Windows" | "MSIE" | "Microsoft" -> UCT.Windows
       | "Macintosh" -> UCT.Macintosh
       | "iPhone" | "iPod" -> UCT.IPhone
       | "Symbian" -> UCT.Symbian
       | "J2ME" -> UCT.J2ME
       | "Wii" -> UCT.WII
       | "PSP" | "PLAYSTATION" -> UCT.PLAYSTATION
       | "BeOS" -> UCT.BeOS
       | _ -> UCT.UnidentifiedEIE)
  | None -> UCT.UnidentifiedEIE

let get_renderer str =
  try Rp_brow.brow_call browrpfn str (String.length str)
  with Rp_brow.ParseFail_brow -> UCT.UnidentifiedRE

let regexp7 = Str.regexp "\\([0-9]+\\)x\\([0-9]+\\)"
let get_resolution str n =
  match search_forward_opt regexp7 str n with
  | Some _pos ->
      {width=int_of_string (Str.matched_group 1 str);
       height=int_of_string (Str.matched_group 2 str)}
  | None -> {width=(-1); height=(-1)}

let get_user_compat req =
  match List.find_opt (function HSCp.User_Agent _ -> true | _ -> false) req.HST.request_header with
  | Some (HSCp.User_Agent str) ->
      let renderer = get_renderer str in
      let environment = get_environment str in
      Some { environment = environment; renderer = renderer }
  | _ -> None

