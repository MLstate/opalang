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
include Lazy


let deep_force obj =
  let stack = Stack.create () in
  let pux obj = Stack.push obj stack in
  let aux obj =
    let obj = Lazy.force (Obj.magic obj) in
    if Obj.is_block obj && Obj.tag obj < Obj.no_scan_tag then
      for i = 0 to Obj.size obj - 1 do
        let sub = Obj.field obj i in
        pux sub
      done
  in
  (Stack.push obj stack;
   try
     while true do
       let first = Stack.pop stack in
       aux first
     done
   with Stack.Empty -> ());
  obj
