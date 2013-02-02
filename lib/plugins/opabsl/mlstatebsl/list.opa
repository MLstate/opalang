/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
Nil = {nil} : list

list =
  cons(e, l) = { hd = e; tl = l } : list
  empty = {nil} : list
  fold_left(f,accu,l) =
    rec aux(accu, l) =
          match l with
          | [] -> accu
          | [hd | tl] -> aux((f(accu, hd)), tl)
          end
    aux(accu,l)
  length(l) =
    rec aux(len, l) =
          match l with
          | [] -> len
          | [hd | tl] -> aux((len + 1), tl)
          end
    aux(0, l)
  map(f,l) =
    rec aux(l) =
          match l with
          | [] -> Nil
          | [hd | tl] -> cons((f(hd)), (aux(tl)))
          end
    aux(l)
  rec rev_append(l1, l2) =
        match l1 with
        | [] -> l2
        | [hd | tl] -> rev_append(tl, (cons(hd, l2)))
        end
  rec iter(f, l) =
        match l with
        | [] -> {} : {}
        | [hd | tl] ->
            match { _f0 = f(hd) } with
            | {_f0 = {}} -> iter(f, tl)
            end
        end
  sc = %%BslString.concat%% : string, string -> string
  rec ponctuate(s, f, l) =
        match l with
        | [] -> ""
        | [hd | tl] ->
            match tl with
            | [] -> f(hd)
            | [_ | _] -> sc((sc((f(hd)), s)), (ponctuate(s, f, tl)))
            end
        end
  formodule_cons = cons
  formodule_empty = empty
  formodule_fold_left = fold_left
  formodule_length = length
  formodule_map = map
  formodule_rev = l -> rev_append(l, Nil)
  formodule_ponctuate = ponctuate
  formodule_iter = iter
  {{
     cons = formodule_cons //used by test
  ; empty = formodule_empty //used by test
  ; fold_left = formodule_fold_left // used by magic
  ; length = formodule_length // used by test
  ; map = formodule_map //used by magic
  ; rev = formodule_rev // used by test
  ; ponctuate = formodule_ponctuate // used by test
  ; iter = formodule_iter //used by test
   }}
