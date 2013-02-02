(*
    Copyright © 2011, 2012 MLstate

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
(* CF mli *)

module String = Base.String
let (@*) = InfixOperator.(@*)

type ext = string
type lang = ML | JS | C | NODEJS
type format = [ `compiled | `interpreted ]
type t = Language of lang * format

let mli = Language (ML,    `interpreted)
let ml  = Language (ML,    `compiled)
let js  = Language (JS,    `compiled)
let c   = Language (C,     `compiled)
let nodejs  = Language (NODEJS,    `compiled)

external fieldlang : t -> lang = "%field0"

let is_ml ml = (fieldlang ml) = ML
let is_js js = (fieldlang js) = JS
let is_c c = (fieldlang c) = C
let is_nodejs c = (fieldlang c) = NODEJS

let compare = Pervasives.compare

let of_string ?format s =
  let dformat = Option.default `compiled format in
  match String.lowercase s with
  | "ml" | "ocaml" -> Some (Language (ML, dformat))
  | "ml:top" when format <> Some `compiled -> Some (Language (ML, `interpreted))
  | "js" | "javascript" -> Some (Language (JS, dformat))
  | "nodejs" -> Some (Language (NODEJS, dformat))
  | "c" -> Some (Language (C, dformat))
  | _ -> None

let to_string (Language (l, c)) =
  match l, c with
  | ML, `compiled -> "ml"
  | ML, `interpreted -> "ml:top"
  | JS, _ -> "js"
  | C, _ -> "c"
  | NODEJS, _ -> "nodejs"

let pp fmt = Format.pp_print_string fmt @* to_string
(* one failure -> None *)
let of_list =
  let rec stop_fold accu = function
    | [] -> Some accu
    | t::q ->
        begin
          match of_string t with
          | None -> None
          | Some lang ->
              if List.exists (fun t -> t = lang) accu
              then stop_fold accu q
              else stop_fold (lang::accu) q
        end
  in stop_fold []

let parse s =
  let li = String.slice_chars "/ ,{}" s in
  of_list li

let gen_print f list = String.concat_map ~left:"{" ~right:"}" ", " f list

let print = gen_print to_string
let pp_list fmt t = Format.pp_print_string fmt (print t)

let formate f (Language (lg, _)) = Language (lg, f)

(* private *)
let lang (Language (lg, _)) = lg

let pp_meta fmt t =
  let s =
    match fieldlang t with
    | ML -> "ml"
    | JS -> "js"
    | NODEJS -> "nodejs"
    | C -> "c"
  in
  Format.fprintf fmt "L.%s" s
