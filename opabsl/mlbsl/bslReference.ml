(*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
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
