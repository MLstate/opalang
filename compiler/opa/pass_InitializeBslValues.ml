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
(* depends *)
module String = Base.String
module Random = Base.Random

(* shorthands *)
module Q = QmlAst
module Cons = QmlAstCons.TypedExpr


let () = Random.ensure_init ()

let set_executable_id = Opacapi.ExecInit.set_id

let add_application ~stdlib_gamma gamma annotmap fn arguments code =
  let annotmap, fn = OpaMapToIdent.typed_val fn annotmap stdlib_gamma in
  let annotmap, app = Cons.apply gamma annotmap fn arguments in
  let ident = Ident.next "__dummy" in
  let gamma = QmlTypes.Env.Ident.add ident (QmlTypes.Scheme.id (Q.TypeRecord (Q.TyRow ([],None)))) gamma in
  let label = Annot.nolabel "pass_InitializeBslValues" in
  gamma, annotmap, Q.NewVal (label, [ident, app]) :: code

let process_code ~stdlib_gamma gamma annotmap code =

  (* generating the server id *)
  let annotmap, id =
    Cons.string annotmap (
      #<If:DIFFING>
        "the_executable_id_is_not_supported_in_diffing_mode"
      #<Else>
        String.random 32
      #<End>
    ) in
  let gamma, annotmap, code = add_application ~stdlib_gamma gamma annotmap set_executable_id [id] code in

  gamma, annotmap, code
