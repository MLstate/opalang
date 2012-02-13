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
include Obj

(* just a hack to know if we are running in bytecode or in native *)
let native_runtime =
  match Obj.size (Obj.repr (fun x -> x)) with
  | 1 -> false
  | 2 -> true
  | _ -> assert false
let bytecode_runtime = not native_runtime

let buffer = Buffer.create 1000

let rec stringify ?(depth=max_int) t =
  if depth < 0 then
    Buffer.add_char buffer '.'
  else
    let depth = depth - 1 in
    let tag = Obj.tag t in
    if tag = Obj.int_tag then
      Buffer.add_string buffer (string_of_int (Obj.obj t : int))
    else if tag = 0 then (
      Buffer.add_char buffer '(';
      let size = Obj.size t in
      if size <> 0 then (
        stringify ~depth (Obj.field t 0);
        for i = 1 to size - 1 do
          Buffer.add_char buffer ',';
          stringify ~depth (Obj.field t i);
        done
      );
      Buffer.add_char buffer ')';
    )
    else if tag = Obj.lazy_tag then
      Buffer.add_string buffer "<lazy>"
    else if tag = Obj.closure_tag then
      Buffer.add_string buffer "<closure>"
    else if tag = Obj.object_tag then
      Buffer.add_string buffer ("<object " ^ string_of_int (Oo.id (Obj.obj t)) ^ ">" )
    else if tag = Obj.infix_tag then
      Buffer.add_string buffer "<infix>"
    else if tag = Obj.forward_tag then
      Buffer.add_string buffer "<forward>"
    else if tag < Obj.no_scan_tag then (
      Buffer.add_string buffer "Tag";
      Buffer.add_string buffer (string_of_int tag);
      Buffer.add_char buffer '(';
      let size = Obj.size t in
      if size <> 0 then (
        stringify ~depth (Obj.field t 0);
        for i = 1 to size - 1 do
          Buffer.add_char buffer ',';
          stringify ~depth (Obj.field t i);
        done
      );
      Buffer.add_char buffer ')';
    ) else if tag = Obj.no_scan_tag then
      Buffer.add_string buffer "<no_scan_tag>"
    else if tag = Obj.abstract_tag then
      Buffer.add_string buffer "<abstract>"
    else if tag = Obj.string_tag then (
      Buffer.add_char buffer '"';
      let s = Obj.obj t in
      if String.length s <= 200 then
        Buffer.add_string buffer (String.escaped s)
      else (
        Buffer.add_string buffer (String.escaped (String.sub s 0 100));
        Buffer.add_string buffer "...";
        Buffer.add_string buffer (String.escaped (String.sub s (String.length s - 100 - 1) 100));
      );
      Buffer.add_char buffer '"'
    ) else if tag = Obj.double_tag then (
      Buffer.add_string buffer (string_of_float (Obj.obj t))
    ) else if tag = Obj.double_array_tag then (
      Buffer.add_string buffer "[|";
      let t : float array = Obj.obj t in
      let size = Array.length t in
      if size <> 0 then (
        Buffer.add_string buffer (string_of_float t.(0));
        for i = 1 to size - 1 do
          Buffer.add_char buffer ';';
          Buffer.add_string buffer (string_of_float t.(i));
        done;
      );
      Buffer.add_string buffer "|]";
    ) else if tag = Obj.custom_tag then
      Buffer.add_string buffer "<custom>"
    else if tag = Obj.final_tag then
      Buffer.add_string buffer "<final>"
    else if tag = Obj.out_of_heap_tag then
      Buffer.add_string buffer "<out_of_heap>"
    else if tag = Obj.unaligned_tag then
      Buffer.add_string buffer "<unaligned>"
    else
      Buffer.add_string buffer "<UNKNOWN>"

let dump ?depth x =
  stringify ?depth (Obj.repr x);
  let s = Buffer.contents buffer in
  Buffer.reset buffer;
  s

let print ?prefix x =
  match prefix with
  | None -> print_endline (dump x)
  | Some s -> Printf.printf "%s: %s\n%!" s (dump x)
