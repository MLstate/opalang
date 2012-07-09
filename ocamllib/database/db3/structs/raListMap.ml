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


module RA = RAList
module RAL = RAList.AsList
module RAA = RAList.AsArray
exception Found

type key = Revision.t
type 'a t = (Revision.t * 'a) RA.ra_list

let empty = RAL.empty

let is_empty = RAL.is_empty

let add key value lst =
  try
    let kk,_ = RAL.head lst in
    (match Revision.compare key kk with
     | 0 -> RAA.update lst 0 (key,value)
     | 1 -> RAL.cons (key,value) lst
     | -1 -> (
         (* TODO imlement *)
         assert false)
     | _ -> assert false)
  with RA.Empty ->
    RAL.cons (key,value) lst
      (*

        if not (RAL.is_empty lst) then
        assert (Revision.compare (fst (RAL.head lst)) key = -1
        && (error "Try to insert an older revision : %s, head is at %s"
        (Revision.to_string key) (Revision.to_string (fst (RAL.head lst))); false));
        RAL.cons (key,value) lst
      *)
let fold f lst acc =
  let f = fun (k,v) a -> f k v a in
  RAL.fold f lst acc

let iter f lst =
  let f = fun k v () -> f k v in
  fold f lst ()

let rev_iter f lst =
  let f = fun (k,v) () -> f k v in
  RAL.rev_fold f lst ()

let find key lst =
  let res = ref None in
  let f = fun k v -> if Revision.equal key k then (res := Some v; raise Found) in
  try iter f lst; raise Not_found
  with Found -> Option.get !res

let find_inf key lst =
  let res = ref None in
  let f = fun k v -> if Revision.compare key k =1 then (res := Some (k,v); raise Found) in
  try iter f lst; raise Not_found
  with Found -> Option.get !res

let size = RAA.size

let max = RAL.head

let keys lst =
  let f = fun k _ a -> k :: a in
  fold f lst []

let remove_last = RAL.tail
