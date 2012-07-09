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
type expr =
  | SetDistant of JsIdent.t list
  | TaggedString of string * QmlAst.tagged_string_kind


module List = BaseList
let compare_tagged_string_kind = Pervasives.compare

let compare e1 e2 =
  match e1, e2 with
  | SetDistant l1, SetDistant l2 ->
      List.make_compare JsIdent.compare l1 l2
  | SetDistant _, _ -> -1
  | _, SetDistant _ -> 1
  | TaggedString (s1, kind1), TaggedString (s2, kind2) ->
      (match String.compare s1 s2 with
       | 0 -> compare_tagged_string_kind kind1 kind2
       | c -> c)
