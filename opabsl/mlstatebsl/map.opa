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
type map('v0, 'v1) =
  {empty : {}} /
  {left : map('v0, 'v1);
    key : 'v0;
    value : 'v1;
    right : map('v0, 'v1);
    height : int}

make_map(compare) =
  empty_map = {empty} : map
  singleton(key, value) = //used by add
    { left = empty_map
    ; key = key
    ; value = value
    ; right = empty_map
    ; height = 1 } : map
  height(m) = //used by create, ball
    match m : map with
    | {empty = empty} -> 0
    | {height = height...} -> height
    end
  create(l, x, d, r) = //used by bal
    hl = height(l)
    hr = height(r)
    { left = l
    ; key = x
    ; value = d
    ; right = r
    ; height = if (hl >= hr)
               then hl + 1
               else hr + 1 } : map
  bal(l, x, d, r) = //used by add
    hl = height(l)
    hr = height(r)
    if (hl > (hr + 2))
    then match l : map with
         | {empty = empty} -> @fail("degenerated map")
         | {right = right; value = value; key = key; left = left...} ->
             if (height(left) >= height(right))
             then create(left, key, value, (create(right, x, d, r)))
             else match right : map with
                  | {empty = empty} -> @fail("degenerated map")
                  | {right = rright;
                      value = rvalue;
                      key = rkey;
                      left = rleft...} ->
                      create((create(left, key, value, rleft)), rkey, rvalue,
                        (create(rright, x, d, r)))
                  end
         end
    else if (hr > (hl + 2))
         then match r : map with
              | {empty = empty} -> @fail("degenerated map")
              | {right = right; value = value; key = key; left = left...} ->
                  if (height(right) >= height(left))
                  then create((create(l, x, d, left)), key, value, right)
                  else match left : map with
                       | {empty = empty} -> @fail("degenerated map")
                       | {right = rright;
                           value = rvalue;
                           key = rkey;
                           left = rleft...} ->
                           create((create(l, x, d, rleft)), rkey, rvalue,
                             (create(rright, key, value, right)))
                       end
              end
         else { left = l
              ; key = x
              ; value = d
              ; right = r
              ; height = if (hl >= hr)
                         then hl + 1
                         else hr + 1 } : map
  rec add(x, data, m) =
        match m : map with
        | {empty = empty} -> singleton(x, data)
        | {height = height;
            right = right;
            value = value;
            key = key;
            left = left} ->
            c = compare(x, key)
            if (c == 0)
            then { left = left
                 ; key = x
                 ; value = data
                 ; right = right
                 ; height = height } : map
            else if (c < 0)
                 then bal((add(x, data, left)), key, value, right)
                 else bal(left, key, value, (add(x, data, right)))
        end
  rec fold(f, m, acc) =
        match m : map with
        | {empty = empty} -> acc
        | {right = right; value = value; key = key; left = left...} ->
            fold(f, right, (f(key, value, (fold(f, left, acc)))))
        end
  formodule_empty = empty_map
  formodule_fold = fold
  formodule_add = add
  {{ empty = formodule_empty //used by test
          ; fold = formodule_fold //used by test
          ; add = formodule_add //use by test
   }}

map = @nonexpansive(make_map(compare_raw))//This is generally wrong!

type stringmap('v2) = map(string, 'v2)

type intmap('v3) = map(int, 'v3)

intmap_empty = map.empty

intmap_add = map.add

intmap_fold = map.fold

stringmap_empty = map.empty

stringmap_add = map.add

stringmap_fold = map.fold
