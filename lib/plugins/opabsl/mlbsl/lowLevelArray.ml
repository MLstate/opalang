(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(**
   Native arrays support.
*)

(**
   This module is the server side implementation of low level array (imperative).
   Do not use for anything. You should probably thing before using this structure
   if it is well-adapted to your need. Maybe a functionnal structure supporting
   parallel computing would be appropriate.

   <!> The directive @@llarray inserted by the compiler is resolved by the Ocaml
   backends to a value of type llarray. Any changes here should be reported as well
   in the backend implementation.
*)

##extern-type llarray('a) = Obj.t array
(* the inner type is masked to avoid generalisation problem wit ocaml *)
(* only works if llarray is exported as a concrete type *)

##register set : llarray('a), int, 'a -> void
(*let set (a:'a llarray) i (v:'a) = Array.unsafe_set a i (Obj.repr v)*)
let set (a:'a llarray) i (v:'a) = a.(i) <- (Obj.repr v)

##register get : llarray('a), int -> 'a
let get (a:'a llarray)  i : 'a = Obj.magic (a.(i))

##register size : llarray('a) -> int
let size a= Array.length a

##register create : int, 'a -> llarray('a)
let create n (v:'a) : 'a llarray =
  let a = Array.make n (Obj.magic 0) in
  for i = 0 to pred n do
    a.(i) <- (Obj.repr v)
  done;
  a

##register concat : llarray('a), llarray('a) -> llarray('a)
(* here, we cannot use normal Array.append because with explicit
   closures, we may have an etherogeneous array, and this would segfault
   if the first item is a float. *)
let concat a1 a2 = Base.Array.append_memory a1 a2
