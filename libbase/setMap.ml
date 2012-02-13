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
