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
module Context =
struct
  (* context *)
  type context = FilePos.pos
  let merge2 = FilePos.merge_pos
  let merge = List.fold_left merge2

  (* constructors *)
  let pos p = p
  let type_ = function
    | BslTypes.Const    (pos, _)
    | BslTypes.TypeVar  (pos, _)
    | BslTypes.Void     (pos)
    | BslTypes.Bool     (pos)
    | BslTypes.Option   (pos, _)
    | BslTypes.OpaValue (pos, _)
    | BslTypes.Fun      (pos, _, _)
    | BslTypes.Callback      (pos, _, _)
    | BslTypes.External (pos, _, _) -> pos

  (* output *)
  let full = FilePos.pp_citation
  let console fmt ctx = Format.fprintf fmt "%a@\n" FilePos.pp_pos ctx
end
type context = Context.context
module E = PassError.LangError(Context)
include E
