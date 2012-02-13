(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
