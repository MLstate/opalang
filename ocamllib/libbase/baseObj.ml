(*
    Copyright © 2011, 2012 MLstate

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
include Obj

(* just a hack to know if we are running in bytecode or in native *)
let native_runtime =
  match Obj.size (Obj.repr (fun x -> x)) with
  | 1 -> false
  | 2 -> true
  | _ -> assert false
let bytecode_runtime = not native_runtime

let buffer = Buffer.create 1000

let rec stringify ?(custom=fun _ -> None) ?(depth=max_int) t =
  match custom t with
  | Some stringify -> stringify buffer t
  | None ->
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
        stringify ~custom ~depth (Obj.field t 0);
        for i = 1 to size - 1 do
          Buffer.add_char buffer ',';
          stringify ~custom ~depth (Obj.field t i);
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
        stringify ~custom ~depth (Obj.field t 0);
        for i = 1 to size - 1 do
          Buffer.add_char buffer ',';
          stringify ~custom ~depth (Obj.field t i);
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
    (* else if tag = Obj.final_tag then
      Buffer.add_s tring buffer "<final>" *)
    else if tag = Obj.out_of_heap_tag then
      Buffer.add_string buffer "<out_of_heap>"
    else if tag = Obj.unaligned_tag then
      Buffer.add_string buffer "<unaligned>"
    else
      Buffer.add_string buffer "<UNKNOWN>"

let dump ?custom ?depth x =
  stringify ?custom ?depth (Obj.repr x);
  let s = Buffer.contents buffer in
  Buffer.reset buffer;
  s

let print ?prefix x =
  match prefix with
  | None -> print_endline (dump x)
  | Some s -> Printf.printf "%s: %s\n%!" s (dump x)

(* To detect cycles in full_size *)
module Set = struct
  exception ERROR
  let create() = Hashtbl.create 101
  let getadd a = ((Obj.magic a) lxor 0)
  let add set a =
    let b = getadd a in
    if Hashtbl.mem set b then raise ERROR
    else Hashtbl.add set b ()
  let rm set a = Hashtbl.remove set (getadd a)
end



let size a =
  let word_size = 8 in
  let header_size = 1 in
  let int_size = 1 in
  let field_size = 1 in
  let unknown_size = 1 in
  let double_size = if word_size = 8 then 1 else 2 in
  let set = Set.create() in
  let rec aux depth t =
    try
      Set.add set t;
      if depth < 10000 then failwith "Obj.full_size: Too deep";
      let r = aux_ (depth+1) t in
      Set.rm set t;
      r
    with Set.ERROR -> 0 (* Cycle detected *)
  and aux_fields depth t =
    let nb_field = Obj.size t in
    let r = ref 0 in
    for i = 0 to nb_field - 1 do
      r:= !r + aux depth (Obj.field t i)
    done;
    !r + header_size + field_size * nb_field
  and aux_ depth t =
    let v = Obj.obj t in
    let tag = Obj.tag t in
    if tag = Obj.int_tag then int_size
    else if tag = 0 then
      aux_fields depth t
    else if tag = Obj.string_tag then
      let len = String.length v in
      let pad = if len mod word_size = 0 then 0 else 1 in
      header_size + len + pad
    else if tag = Obj.double_tag then
      double_size
    else if tag = Obj.double_array_tag then
      header_size + double_size * (Array.length v)
    else if tag = Obj.out_of_heap_tag then
      0
    else if tag = Obj.lazy_tag ||
           tag = Obj.closure_tag ||
           tag = Obj.object_tag ||
           tag = Obj.infix_tag ||
           tag = Obj.forward_tag ||
           tag = Obj.no_scan_tag ||
           tag = Obj.abstract_tag ||
           tag = Obj.custom_tag ||
           (* tag = Obj.final_tag || *)
           tag = Obj.unaligned_tag ||
           tag >= Obj.no_scan_tag
    then
      unknown_size
    else (* => tag < Obj.no_scan_tag *)
      aux_fields depth t
  in
  (aux 0 (Obj.repr a)) * word_size
