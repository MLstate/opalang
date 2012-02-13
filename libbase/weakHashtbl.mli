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
