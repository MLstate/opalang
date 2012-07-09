(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
