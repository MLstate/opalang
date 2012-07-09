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
  include BaseMapSig.S
  val append : key -> 'a -> 'a list t -> 'a list t
  val fold_elt : ('a -> key -> 'b -> 'a) -> 'a -> 'b list t -> 'a
  val append_left : 'a list t -> 'a list t -> 'a list t
end
