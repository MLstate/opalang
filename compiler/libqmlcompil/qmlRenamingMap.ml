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

type id_to_id = Ident.t IdentMap.t

type t = {
  map : id_to_id;
  rev : id_to_id;
}

let empty = {
  map = IdentMap.empty;
  rev = IdentMap.empty;
}

let add t original_id new_id = {
  map = IdentMap.add original_id new_id t.map;
  rev = IdentMap.add new_id original_id t.rev;
}

let from_map map = {
  map = map;
  rev = IdentMap.fold (fun k v acc -> IdentMap.safe_add v k acc) map IdentMap.empty;
}

let original_from_new t new_id =
  IdentMap.find new_id t.rev

let original_from_new_opt t new_id =
  IdentMap.find_opt new_id t.rev

let new_from_original t original_id =
  IdentMap.find original_id t.map

let new_from_original_opt t original_id =
  IdentMap.find_opt original_id t.map

let remove_from_original t original_id =
  let old_id = new_from_original t original_id in
  {
    map = IdentMap.remove original_id t.map;
    rev = IdentMap.remove old_id t.rev;
  }

let filter t filter =
  IdentMap.fold
    (fun original_id new_id filtered ->
       match filter original_id new_id with
       | true -> add filtered original_id new_id
       | false -> filtered)
    t.map empty

let pp f m =
  IdentMap.iter (fun k v -> Format.fprintf f "%s <-> %s@ " (Ident.to_string k) (Ident.to_string v)) m.map
