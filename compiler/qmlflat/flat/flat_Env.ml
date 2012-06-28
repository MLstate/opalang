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
(* CF mli *)

type env = {
  options: Qml2ocamlOptions.argv_options ;
  bymap: Flat_Bsl.FlatBSL.ByPassMap.t ;
  typing: QmlTyper.env ;
}

let initial options bymap typing = {
  options = options ;
  bymap = bymap ;
  typing = typing ;
}

let posindex = ref 0
let string_of_pos pos =
  #<If:TESTING>
    FilePos.to_string pos
  #<Else>
    if FilePos.is_empty pos
    then (
      let p = !posindex in
      incr(posindex);
      Printf.sprintf "<no-position:%d>" p
    )
    else
      FilePos.to_string pos
  #<End>

let env_context env context =
  QmlError.Context.merge2 (QmlError.Context.annotmap env.typing.QmlTypes.annotmap) context

let context_error context fmt = QmlError.i_error None context fmt
let internal_error env expr fmt =
  let context = QmlError.Context.annoted_expr env.typing.QmlTypes.annotmap expr in
  QmlError.i_error None context fmt
