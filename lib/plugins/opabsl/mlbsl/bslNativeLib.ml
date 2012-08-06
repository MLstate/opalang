(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(*
   @author Mathieu Barbin
   @author Louis Gesbert
   @author David Rajchenbach-Teller, 2010
**)


(**
   Conversion between usual OPA data structures and their OCaml equivalent.

   @author Mathieu Barbin
   @author Louis Gesbert
   @author David Rajchenbach-Teller, 2010
*)

(**
   {1 Option}
*)

include BslUtils

(**
   {1 Lists}
*)

(**
   Type [list('a)], as known by OPA
*)


(**
   Type ['a list], as known by OCaml
*)
##property[mli]
##extern-type caml_list('a) = 'a list
##property[endmli]


##register [opacapi] cons : 'a, caml_list('a) -> caml_list('a)
  let cons x l = x :: l

##register [opacapi] empty_list : caml_list('a)
  let empty_list : 'a caml_list = []

##register hd : caml_list('a) -> option('a)
 let hd = function |x::_ -> Some x | _ -> None

##register tl : caml_list('a) -> option(caml_list('a))
 let tl = function |_::y -> Some y | _ -> None

##register caml_list_to_opa_list : ('a -> 'b), caml_list('a) -> opa[list('b)]

##register opa_list_to_ocaml_list : ('a -> 'b), opa[list('a)] -> caml_list('b)

(**
   {1 Tuples}
*)

##register ocaml_tuple_2 : opa[tuple_2('a,'b)] -> caml_tuple_2('a,'b)

##register ocaml_tuple_4 : opa[tuple_4('a,'b,'c,'d)] -> caml_tuple_4('a,'b,'c,'d)

##register ocaml_tuple_5 : opa[tuple_5('a,'b,'c,'d,'e)] -> caml_tuple_5('a,'b,'c,'d,'e)
(**
   {1 Continuations}
*)

##property[mli]
##extern-type continuation('a) = 'a QmlCpsServerLib.continuation
##property[end_mli]

(**
   {1 Standard Exceptions}
*)
