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
module type WeakedType = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
end

module Make :
  functor (HK : Hashtbl.HashedType) ->
    functor (HD:WeakedType) -> sig

      type key = HK.t

      type t

      val create : int -> t

      val add : t -> key -> HD.t -> unit

      val strong : t -> key -> HD.t -> unit

      val relax : t -> key -> unit

      val remove : t -> key -> unit

      val mem : t -> key -> bool

      val find : t -> key -> HD.t

      val size : t -> int

end
