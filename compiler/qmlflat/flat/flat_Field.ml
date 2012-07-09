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

(* refactoring in progress *)

(* alias *)
module FCons = Flat_Common.FCons

(* -- *)

type label = string

let table : (string, Flat_Common.expr) Hashtbl.t = Hashtbl.create 1024
let reset () = Hashtbl.clear table

let label ( label : label ) =
  try
    Flat_Common.Var (Hashtbl.find table label)
  with
  | Not_found ->
      let ident = Ident.nextf "label_%s" label in
      let param, var = FCons.param_var ident in
      let register = Ocaml.Cons.app Flat_Common.ServerLib.Field.register (Ocaml.Cons.string label) in
      let let_definition = Ocaml.make_Let param register in
      Hashtbl.add table label var ;
      Flat_Common.NewVar (let_definition, var)
