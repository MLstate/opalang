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
(* FIXME: remove open *)
module B = Base
let (<|) f a = f a
let (|>) a f = f a
let ( @* ) g f x = g(f(x))
module O = Ocaml
module Cons = O.Cons
module G = Grammar
module T = Tools
module L = B.List

let get_define = function | G.Define (G.Constr (name, lst), pattern) -> name, lst | _ -> assert false
let get_raw = function | G.Raw (name, (pre1,opts), lst) -> name, lst | _ -> assert false

let compare_pattern_of_ getdr dr =
  let name, lst = getdr dr in
  let pt = if L.length lst > 0 then [O.PatAny] else [] in
  (O.PatTuple [O.PatConstructor ([Ident.source name],pt);
            O.PatConstructor ([Ident.source name],pt)],
   None,
   Cons.bool true)

let gen_get_compare_ name getdr lst =
  O.Let [O.Pat (O.PatVar (Ident.source ("compare_"^name))),
       O.Function ((L.map (compare_pattern_of_ getdr) lst)@[(O.PatAny,None,(Cons.bool false))])]

let gen_get_compare_msg = gen_get_compare_ "msg" get_define
let gen_get_compare_rawmsg = gen_get_compare_ "rawmsg" get_raw

let name_pattern_of_ getdr dr =
  let name, lst = getdr dr in
  let pt = if L.length lst > 0 then [O.PatAny] else [] in
  (O.PatConstructor ([Ident.source name],pt), None, Cons.string name)

let gen_get_name_ name getdr lst =
  O.Let [O.Pat (O.PatVar (Ident.source (B.sprintf "get_%s_name" name))), O.Function ((L.map (name_pattern_of_ getdr) lst))]

let gen_get_msg_name = gen_get_name_ "msg" get_define
let gen_get_rawmsg_name = gen_get_name_ "rawmsg" get_raw

let get_pattern_of_ getdr dr =
  let name, lst = getdr dr in
  (B.sprintf "\nlet get_%s rh =\n   " name)^
    if L.length lst <> 0
    then
      let pat = B.sprintf "%s _v" name in
      (B.sprintf "match List.find_opt (function %s -> true | _ -> false) rh with\n  | Some (%s) -> Some _v\n  | _ -> None"
               pat pat)
    else
      (B.sprintf "match List.find_opt (function %s -> true | _ -> false) rh with\n  | Some %s -> Some %s\n  | _ -> None"
               name name name)

let gen_get_value_ getdr lst = O.Verbatim (String.concat "\n" (L.map (get_pattern_of_ getdr) lst))

let gen_get_msg_value = gen_get_value_ get_define
let gen_get_rawmsg_value = gen_get_value_ get_raw
