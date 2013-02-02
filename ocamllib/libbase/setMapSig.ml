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
module type S =
sig
  type key
  type elt
  module M : (BaseMapSig.S with type key = key)
  module S : (BaseSetSig.S with type elt = elt)
  type t = S.t M.t
  val empty : 'a M.t
  val find_opt : M.key -> 'a M.t -> 'a option
  val find : M.key -> S.t M.t -> S.t
  val add : M.key -> S.elt -> S.t M.t -> S.t M.t
  val remove : M.key -> S.elt -> S.t M.t -> S.t M.t
end
