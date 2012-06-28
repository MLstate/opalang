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

(* depends *)
module List = Base.List

(* refactoring in progress *)

(* alias *)
module ServerLib = Flat_Common.ServerLib

(* shorthands *)
module Q = QmlAst

(* -- *)

type label = Flat_Common.label

let cmp (f1, _) (f2, _) = String.compare f1 f2

let may_be_simple ~info label value =
  let vtable = Ocaml.make_repr (Flat_Shared.vtable [ label ]) in
  let info =
    match info with
    | Some info -> Ocaml.make_repr (Ocaml.Cons.some info)
    | None -> Ocaml.make_repr Ocaml.Cons.none
  in
  let value = Ocaml.make_repr value in
  let anarray = Ocaml.AnArray [ vtable ; info ; value ] in
  Ocaml.Cons.app ServerLib.may_be_simple anarray

let static_init ~is_lazy ~info fields =
  match fields with

  (*
    The function ServerLib.unsafe_init_static cannot be called with 0 or 1 argument,
    This would corrupt the sharing.
  *)
  | [] ->
      ServerLib.empty

  | [ label, value ] ->
      may_be_simple ~info:None label value

  (* *)
  | _ ->
      let fields = List.stable_sort cmp fields in
      let fields = List.uniq ~cmp fields in
      let fields, values = List.split fields in
      let vtable = Ocaml.make_repr (Flat_Shared.vtable fields) in
      let info =
        match info with
        | Some info -> Ocaml.make_repr (Ocaml.Cons.some info)
        | None -> Ocaml.make_repr Ocaml.Cons.none
      in
      let values =
        if is_lazy
        then
          List.map (fun value -> Ocaml.make_repr (Ocaml.Lazy value)) values
        else
          List.map Ocaml.make_repr values
      in
      let anarray = Ocaml.AnArray ( vtable :: info :: values ) in
      Ocaml.Cons.app ServerLib.unsafe_init_static anarray

let extend record fields =
  let fields = List.stable_sort cmp fields in
  let fields = List.uniq ~cmp fields in
  let extend = List.map (fun (f, e) -> Ocaml.Cons.tuple [ Flat_Shared.field f ; e ]) fields in
  Ocaml.Cons.app3 ServerLib.extend_with_array record (Ocaml.AnArray extend)
