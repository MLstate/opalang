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
(* CF mli *)

(* depends *)
module String = Base.String

(* refactoring in progress *)

(* alias *)
module FCons = Flat_Common.FCons
module ServerLib = Flat_Common.ServerLib

(* -- *)

type vtable = Flat_Field.label list

let table : (string list, Flat_Common.expr) Hashtbl.t = Hashtbl.create 1024
let reset () = Hashtbl.clear table

let vtable ( vtable : vtable ) =
  try
    Flat_Common.Var (Hashtbl.find table vtable)
  with
  | Not_found ->
      let ident = Ident.next "shared_vtable" in
      let param, var = FCons.param_var ident in
      (* helping for debug: comment out the fields of the var *)
      let var =
        let comments = String.concat_map ~left:"[" ~right:"]" " ; " (Printf.sprintf "%S") vtable in
        Ocaml.Comments (comments, var) in
      let fields_expr = List.map Ocaml.Cons.string vtable in
      let make = Ocaml.Cons.app ServerLib.VTable.register (Ocaml.AnArray fields_expr) in
      let let_definition = Ocaml.make_Let param make in
      Hashtbl.add table vtable var ;
      Flat_Common.NewVar (let_definition, var)
