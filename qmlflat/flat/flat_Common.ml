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

(* refactoring in progress *)

(* shorthands *)
module Q = QmlAst

(* alias *)

(* -- *)

type primitive = Ocaml.expr

let serverlib = "QmlFlatServerLib"

let make path =
  let path = List.map Ident.source (serverlib :: path) in
  Ocaml.Var (Ocaml.Pated (path, false))

module ServerLib =
struct
  module Field =
  struct
    let register = make [ "Field" ; "register" ]
  end
  module FieldAccess =
  struct
    let make_cache = make [ "FieldAccess" ; "make_cache" ]
  end
  module VTable =
  struct
    let register = make [ "VTable" ; "register" ]
  end
  module Simple =
  struct
    let register = make [ "Simple" ; "register" ]
  end

  let runtime_error = make [ "runtime_error" ]

  let unwrap_record = make [ "unwrap_record" ]

  let get_vtable = make [ "get_vtable" ]

  let empty = make [ "empty" ]
  let true_ = make [ "true_" ]
  let false_ = make [ "false_" ]

  let wrap_bool = make [ "wrap_bool" ]
  let unwrap_bool = make [ "unwrap_bool" ]
  let none = make [ "none" ]
  let some = make [ "some" ]
  let unwrap_option = make [ "unwrap_option" ]

  let simple = make [ "simple" ]

  let dot = make [ "dot" ]
  let dot_opt = make [ "dot_opt" ]
  let unsafe_get = make [ "unsafe_get" ]

  let dot_with_cache = make [ "dot_with_cache" ]

  let extend_with_array = make [ "extend_with_array" ]
  let unsafe_init_static  = make [ "unsafe_init_static" ]
  let may_be_simple = make [ "may_be_simple" ]

  let do_exit = make [ "do_exit" ]
end

type label = string

type let_definition = Ocaml.expr

type expr = Ocaml.expr

type shared_variable =
  | NewVar of let_definition * expr
  | Var of expr

module FCons =
struct
  type ident = Ident.t

  let const = function
    | Q.Int i -> Ocaml.Int i
    | Q.Float f -> Ocaml.Float f
    | Q.String s -> Ocaml.String s

  (*
    Ident are now shared between ocamlAst and QmlAst
  *)
  let ident id = id

  let pat id = Ocaml.PatVar (ident id)
  let param id = Ocaml.Pat (pat id)
  let var id = Ocaml.Cons.var id

  let patas pat id = Ocaml.PatAs (pat, id)

  let param_var id =
    param id, var id

  let pat_var id =
    pat id, var id

  let param_pat_var id =
    let id = ident id in
    let pat = Ocaml.PatVar id in
    let param = Ocaml.Pat pat in
    let var = var id in
    param, pat, var

end
