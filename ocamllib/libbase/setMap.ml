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
module Make (Keys: OrderedTypeSig.S)(Values: OrderedTypeSig.S) :
  (SetMapSig.S with type key = Keys.t and type elt = Values.t) =
struct
  type key = Keys.t
  type elt = Values.t

  module M = BaseMap.Make(Keys)
  module S = BaseSet.Make(Values)

  type t = S.t M.t

  let empty = M.empty
  let find_opt = M.find_opt

  let find k m =
    match find_opt k m with
    | Some s -> s
    | None   -> S.empty

  let add (k:M.key) (v:S.elt) (m:S.t M.t) : S.t M.t =
    let (set:S.t) = find k m in
    M.add k (S.add v set) m

  let remove (k:M.key) (v:S.elt) (m:S.t M.t) : S.t M.t =
    let (set:S.t) = find k m in
    M.add k (S.remove v set) m

end
