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
  type black_ref (*internal type*)
  external black : 'a ref -> black_ref = "%identity"
  external unblack : black_ref -> 'a ref = "%identity"

  ##extern-type reference('a) = black_ref

  ##register [opacapi] create : 'a -> reference('a)
  let create x = black (ref x)

  ##register set : reference('a), 'a -> void
  let set r x = (unblack r) := x

  ##register get : reference('a) -> 'a
  let get r = !(unblack r)

(**
   Atomic compare-and-swap, based on physical equality

   @param latest The value expected to be held by [r]
   @param replacement The value to put in the content of [r], iff it still contains [latest].
   @return [true] If the value of [r] was physically equal to [latest], in which case the new value of [r] is now set to [replacement] / [false] otherwise
*)
##register compare_and_swap: reference('a),'a,'a -> bool
let compare_and_swap r latest replacement =
  if !(unblack r) == latest then
    begin
      (unblack r) := replacement;
      true
    end
  else
    false
