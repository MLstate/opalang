(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
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
