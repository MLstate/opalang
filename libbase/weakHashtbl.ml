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
(* http://remi.vanicat.free.fr/ocaml/hweak/ *)
(* Perhaps we can replace this module by (modified?) above hweak
   implementation... ? *)

module type WeakedType = sig
  type t
  val compare : t -> t -> int
  val hash : t -> int
end

module Make (HK : Hashtbl.HashedType)(HD : WeakedType) = struct
  module WS = Weak.Make
    (struct
       include HD
       let equal a b = HD.compare a b = 0
     end)

  module HT = Hashtbl.Make(HK)

  type key = HK.t

  module S = Set.Make(HD)

  type t = {
    htbl : HD.t HT.t;
    wset : WS.t;
    mutable strong : S.t;
  }

  let create i = {
    htbl = HT.create i;
    wset = WS.create i;
    strong = S.empty;
  }

  let remove t k =
    try
      let v = HT.find t.htbl k in
      WS.remove t.wset v;
      HT.remove t.htbl k;
      t.strong <- S.remove v t.strong
    with Not_found -> ()

  let add t k v =
    let vcopy = Obj.obj (Obj.dup (Obj.repr v)) in
    HT.add t.htbl k vcopy;
    WS.add t.wset v;
    Gc.finalise (fun _ -> HT.remove t.htbl (k:HK.t)) v

  let mem t k = (HT.mem t.htbl k)

  let find t k = WS.find t.wset (HT.find t.htbl k)

  let strong t k v =
    if not (mem t k) then add t k v;
    t.strong <- S.add v t.strong

  let relax t k =
    t.strong <- S.remove (find t k) t.strong

  let size t =
    HT.length t.htbl

end
