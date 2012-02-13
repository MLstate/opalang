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
module type S =
sig
  type key
  type elt
  module M : (BaseMapSig.S with type key = key)
  module S : (BaseSetSig.S with type elt = elt)
  type t = S.t M.t
  val empty : 'a M.t
  val find_opt : M.key -> 'a M.t -> 'a option
  val find : M.key -> S.t M.t -> S.t
  val add : M.key -> S.elt -> S.t M.t -> S.t M.t
  val remove : M.key -> S.elt -> S.t M.t -> S.t M.t
end
