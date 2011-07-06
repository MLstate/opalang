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

let sprintf fmt = Printf.sprintf fmt
let eprintf fmt = Printf.eprintf fmt

type t = {
  mutable content : Datas.t;
  mutable disk_file : string option;
  mutable on_disk : bool;
}

(*******************)
(* Screen printing *)
(*******************)

let print_full n =
    match n.on_disk, n.disk_file with
    | true, Some file -> sprintf "content(%s)=%s" file (File.content file)
    | _, _ -> sprintf "content=%s" (Datas.to_string n.content)

let to_string node = sprintf "{%s}" (print_full node)

(************************)
(* Access to the fields *)
(************************)

let get_content node =
  match node.on_disk, node.disk_file with
  | true, Some file -> Datas.Data (DataImpl.Binary (File.content file))
  | _, _ -> node.content

let set_content ?(max_size=1048576) node datas =
  match node.disk_file, datas with
  | Some file, Datas.Data (DataImpl.Binary str) when String.length str > max_size ->
      (*eprintf "Writing file: %s\n%!" file;*)
      let os = open_out file in
      output_string os str;
      close_out os;
      node.on_disk <- true;
      node.content <- Datas.UnsetData
  | Some file, _ ->
      if node.on_disk && File.exists file then ((*eprintf "Unlinking: %s\n%!" file;*) Unix.unlink file);
      node.on_disk <- false;
      node.content <- datas
  | _, _ ->
      node.content <- datas

let is_occupied node =
  match node.on_disk, node.disk_file, node.content with
  | true, Some _, _ -> true
  | _, _, Datas.UnsetData -> false
  | _, _, _ -> true

(************************)
(* Creation and updates *)
(************************)

let create ?disk_file ?max_size ?content () =
  let node = { content=Datas.UnsetData; disk_file; on_disk=false; } in
  set_content ?max_size node (Option.default Datas.UnsetData content);
  node

let delete node =
  match node.on_disk, node.disk_file with
  | true, Some file -> if File.exists file then ((*eprintf "Unlinking: %s\n%!" file;*) Unix.unlink file)
  | _, _ -> ()

let cmp s1 s2 len str1 str2 =
  try let rec aux pos = pos >= len || (str1.[s1+pos] = str2.[s2+pos] && aux (succ pos)) in aux 0
  with Invalid_argument "index out of bounds" -> false

let verify_file file str =
  (File.exists file) &&
    (try
       (*eprintf "open file %s\n%!" file;*)
       let ic = open_in_bin file in
       let buf = String.create 4096 in
       let rec aux pos =
         match input ic buf 0 4096 with
         | 0 -> pos = String.length str
         | n -> (cmp 0 pos n buf str) && (aux (pos+n))
       in
       aux 0
     with Sys_error _ -> false)

(*verify_file "../output_local.txt" str;;
verify_file "../output_local.txt" (str^"x");;*)

let equals_data node datas =
  match node.on_disk, node.disk_file, datas with
  | true, Some file, Datas.Data (DataImpl.Binary str) -> verify_file file str
  | _, _, _ -> datas = node.content

let equals node1 node2 =
  match node1.on_disk, node1.disk_file, node1.content, node2.on_disk, node2.disk_file, node2.content with
  | true, Some file1, _, true, Some file2, _ -> file1 = file2
  | true, Some file1, _, _, _, Datas.Data (DataImpl.Binary str2) -> verify_file file1 str2
  | _, _, Datas.Data (DataImpl.Binary str1), true, Some file2, _ -> verify_file file2 str1
  | _, _, datas1, _, _, datas2 -> datas1 = datas2

(*let file = "../dog.txt";;
let n = create ~disk_file:file ~max_size:10 ~content:(Datas.Data (DataImpl.Binary "0123456789\n")) ();;
assert(n = {content = Datas.UnsetData; disk_file = Some file; on_disk = true; });;
let d0 = get_content n;;
assert(d0 = Datas.Data (DataImpl.Binary "0123456789\n"));;
let tf0t = equals n (Datas.Data (DataImpl.Binary "0123456789\n"));;
let tf0f = equals n (Datas.Data (DataImpl.Binary "abc\n"));;
assert(tf0t && not tf0f);;
let () = set_content ~max_size:10 n (Datas.Data (DataImpl.Binary "abc\n"));;
assert(n = {content = Datas.Data (DataImpl.Binary "abc\n"); disk_file = Some "../dog.txt"; on_disk = false});;
let d1 = get_content n;;
assert(d1 = Datas.Data (DataImpl.Binary "abc\n"));;
let tf0t = equals n (Datas.Data (DataImpl.Binary "abc\n"));;
let tf0f = equals n (Datas.Data (DataImpl.Binary "0123456789\n"));;
assert(tf0t && not tf0f);;
*)

