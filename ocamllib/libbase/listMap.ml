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
module Make (Ord: OrderedTypeSig.S) =
struct
  module M = BaseMap.Make (Ord)
  include M
  let append k v m =
    add k (match find_opt k m with
           | Some l -> v :: l
           | _ -> [v]
          ) m

  let fold_elt f acc m =
    fold (fun k l acc -> List.fold_left (fun acc v -> f acc k v) acc l) m acc

  let append_left m1 m2 =
    fold_elt (fun m k v -> append k v m) m1 m2
end
