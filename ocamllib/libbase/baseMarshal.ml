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
module Obj = BaseObj

include Marshal

let buffer = Buffer.create 1000

let special_string = Obj.repr "i_am_a_trashed_closure"
let rec replacement_fun x =
  Printf.printf "[1;31mBEWARE: unmarshalled fun is being called with %s.\n%!" (Obj.dump ~depth:5 x);
  Printf.printf "I will pretend that nothing happened but the program may segfault at any time.[39;22m\n%!";
  Obj.magic replacement_fun
let replacement_fun = Obj.repr replacement_fun
let trashed_funs = Queue.create ()

let rec trash_obj obj =
  if Obj.is_block obj && Obj.tag obj < Obj.no_scan_tag then (
    for i = 0 to Obj.size obj - 1 do
      let sub = Obj.field obj i in
      if Obj.tag sub = Obj.closure_tag then (
        Queue.add sub trashed_funs;
        Obj.set_field obj i special_string
      ) else
        trash_obj sub
    done
  )

let trash_obj obj =
  let obj = Obj.repr obj in
  if Obj.tag obj = Obj.closure_tag || Obj.tag obj = Obj.infix_tag then
    failwith "trash_obj: cannot trash a closure"
  else (
    assert (Queue.is_empty trashed_funs);
    trash_obj obj
  )

let rec restore_obj obj =
  if Obj.is_block obj && Obj.tag obj < Obj.no_scan_tag then (
    for i = 0 to Obj.size obj - 1 do
      let sub = Obj.field obj i in
      if sub = special_string then
        Obj.set_field obj i (Queue.take trashed_funs)
      else
        restore_obj sub
    done
  )

let restore_obj obj =
  let obj = Obj.repr obj in
  if obj = special_string then
    failwith "cannot restore"
  else
    restore_obj obj

let rec fake_restore_obj obj =
  if Obj.is_block obj && Obj.tag obj < Obj.no_scan_tag then (
    for i = 0 to Obj.size obj - 1 do
      let sub = Obj.field obj i in
      if sub = special_string then
        Obj.set_field obj i replacement_fun
      else
        fake_restore_obj sub
    done
  )

let fake_restore_obj obj =
  let obj = Obj.repr obj in
  if obj = special_string then
    failwith "cannot fake_restore"
  else
    fake_restore_obj obj

let marshal_no_fun oc obj =
  trash_obj obj;
  try
    Marshal.to_channel oc obj [];
    restore_obj obj;
  with
  | e ->
    restore_obj obj;
    raise e

let unmarshal_no_fun ic =
  let obj = Marshal.from_channel ic in
  fake_restore_obj obj;
  obj
