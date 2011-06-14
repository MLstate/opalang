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

type t = {
  mutable content : Datas.t ;
}

(*******************)
(* Screen printing *)
(*******************)

let print_full n = Printf.sprintf "content=%s" (Datas.to_string n.content)

let to_string node = Printf.sprintf "{%s}" (print_full node)

(************************)
(* Access to the fields *)
(************************)

let get_content node = node.content

let is_occupied node =
  match node.content with
  | Datas.UnsetData -> false
  | _ -> true

(************************)
(* Creation and updates *)
(************************)

let create ?content () =
  let content =
    match content with
    | Some d -> d
    | _ -> Datas.empty
  in
  { content = content }



