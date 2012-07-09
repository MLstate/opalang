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

(* refactoring in progress *)

(* alias *)
module FCons = Flat_Common.FCons
module ServerLib = Flat_Common.ServerLib

(* -- *)

type label = Flat_Common.label
type ocaml_val = Ocaml.expr

(*
  Compositional state.
  Should be reset before each compilation unit.
*)
module State =
struct
  let toplevel_fields : ocaml_val list ref = ref []
  let toplevel_vtables : ocaml_val list ref = ref []
  let toplevel_simples : ocaml_val list ref = ref []
  let toplevel_caches : ocaml_val list ref = ref []

  let reset () =
    toplevel_fields := [] ;
    toplevel_vtables := [] ;
    toplevel_simples := [] ;
    toplevel_caches := [] ;
    ()
end

let reset = State.reset

let field label =
  match Flat_Field.label label with
  | Flat_Common.NewVar (let_definition, var) ->
      State.toplevel_fields := let_definition :: !State.toplevel_fields ;
      var
  | Flat_Common.Var var ->
      var

let vtable labels =
  match Flat_VTable.vtable labels with
  | Flat_Common.NewVar (let_definition, var) ->
      State.toplevel_vtables := let_definition :: !State.toplevel_vtables ;
      var
  | Flat_Common.Var var ->
      var

let simple label =
  match Flat_Simple.simple label with
  | Flat_Common.NewVar (let_definition, var) ->
      State.toplevel_simples := let_definition :: !State.toplevel_simples ;
      var
  | Flat_Common.Var var ->
      var

let cache () =
  let ident = Ident.next "access_cache" in
  let param, var = FCons.param_var ident in
  let make_cache = Ocaml.Cons.app ServerLib.FieldAccess.make_cache Ocaml.Cons.unit in
  let let_definition = Ocaml.make_Let param make_cache in
  State.toplevel_caches := let_definition :: !State.toplevel_caches ;
  var

module Let =
struct

  let fields () =
    let fields = List.rev !State.toplevel_fields in
    State.toplevel_fields := [] ;
    fields

  let vtables () =
    let vtables = List.rev !State.toplevel_vtables in
    State.toplevel_vtables := [] ;
    vtables

  let simples () =
    let simples = List.rev !State.toplevel_simples in
    State.toplevel_simples := [] ;
    simples

  let caches () =
    let caches = List.rev !State.toplevel_caches in
    State.toplevel_caches := [] ;
    caches

  let insert list =
    let acc = list in
    let acc = List.rev_append !State.toplevel_caches acc in
    let acc = List.rev_append !State.toplevel_simples acc in
    let acc = List.rev_append !State.toplevel_vtables acc in
    let acc = List.rev_append !State.toplevel_fields acc in
    reset () ;
    acc

  let all () = insert []
end
