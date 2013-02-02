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
module type E =
sig
  type effect
  type effects
  type typ
  val string_of_typ : typ -> string
  val flatten_effect : effects -> effect

  type env = (effects IdentMap.t * typ IdentMap.t)
  val infer_code : ?initial_env:env -> (BslKey.t -> QmlAst.ty * effect) -> QmlAst.code -> env
end

module SideEffect : E with type effect = bool
module SlicerEffect : E with type effect = bool
