/*
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
