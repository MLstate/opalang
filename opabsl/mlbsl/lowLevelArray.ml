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
