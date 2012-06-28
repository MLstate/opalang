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
(* CF mli *)

(* Annotation *)
type t = int
let next =
  let r = ref 0 in
  (fun () ->
     let i = !r in
     incr(r);
     i)
let to_string x = "#" ^ string_of_int x
let hash = Hashtbl.hash
let equal : int -> int -> bool = (=)
let compare : int -> int -> int = compare
external to_int : t -> int = "%identity"

module AnnotMap = IntMap
module AnnotSet = IntSet

(* AST *)
type pos = FilePos.pos

type label = {
  annot : t ;
  pos : pos ;
}

let annot label = label.annot
let pos label = label.pos
let make_label annot pos = {
  annot ;
  pos ;
}

let next_label pos = {
  annot = next () ;
  pos = pos ;
}

let refresh label = next_label label.pos

let nolabel s = next_label (FilePos.nopos s)

module Magic =
struct

  external label : 'a -> label = "%field0"

  external imp_reset_label : Obj.t -> label -> unit = "%setfield0"

  let annot ast =
    let label = label ast in
    label.annot

  (*
    Obj.dup reallocate a fresh block of the same size, and make a shallow copy of fields.
    cf in ocaml/byterun/obj.c, function caml_obj_dup
  *)
  let new_label ast label =
    let ast = Obj.dup (Obj.repr ast) in
    imp_reset_label ast label;
    Obj.obj ast

  let new_annot ast t =
    let label = label ast in
    let label = { label with annot = t } in
    new_label ast label

  let pos ast =
    let label = label ast in
    label.pos

  let new_pos ast pos =
    let label = label ast in
    let label = { label with pos = pos } in
    new_label ast label

  let merge_pos ast pos =
    let label = label ast in
    let pos = FilePos.merge_pos pos label.pos in
    let label = { label with pos = pos } in
    let ast = Obj.dup (Obj.repr ast) in
    imp_reset_label ast label;
    Obj.obj ast
end
