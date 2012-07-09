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
module String = BaseString
module Format = BaseFormat

(*
  type t = string
  <!> how to share abstract value between modules in ocaml ?
  beware : keep sync with implementation in bslKeyMap
*)
type t = string
let compare = String.compare
external to_string : string -> t = "%identity"
let pp = Format.pp_print_string
external of_string : t -> string = "%identity"
let normalize name =
  let name = String.lowercase (String.trim name) in
  String.replace name "." "_"
let normalize_string = normalize

let equal = String.equal
let hash = String.hash
