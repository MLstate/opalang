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

(**
   Flat Compiler : Common utils
   @author Mathieu Barbin
*)

(**
   The type of an expression accessing a function of the serverlib
*)
type primitive = Ocaml.expr

(** {6 ServerLib} *)

(**
   Utils for interfacing with the QmlFlatServerLib.
   Keep synchronous.
*)

(**
   The name of the main module of the flat runtime.
   Use it only for string generation. If you are manipulating
   ocaml ast, use directly the module ServerLib.
*)
val serverlib : string

module ServerLib :
sig
  module Field :
  sig
    val register : primitive
  end
  module FieldAccess :
  sig
    val make_cache : primitive
  end
  module VTable :
  sig
    val register : primitive
  end
  module Simple :
  sig
    val register : primitive
  end

  val runtime_error : primitive

  val unwrap_record : primitive

  val get_vtable : primitive

  val empty : primitive
  val true_ : primitive
  val false_ : primitive

  val wrap_bool : primitive
  val unwrap_bool : primitive
  val none : primitive
  val some : primitive
  val unwrap_option : primitive

  val dot : primitive
  val dot_opt : primitive
  val unsafe_get : primitive

  val dot_with_cache : primitive

  val extend_with_array : primitive
  val unsafe_init_static : primitive
  val may_be_simple : primitive

  val do_exit : primitive
end

(** {6 Shared Stuff} *)

(**
   The type of fields name
*)
type label = string

(**
   The type used in OcamlAst for a new let-binding
*)
type let_definition = Ocaml.expr

(**
   The type used in OcamlAst for an expression composed on an identifier
   refering to one of previously defined field
*)
type expr = Ocaml.expr

(**
   The type you can get when you build ocaml label.
   You should put in the generated code the definition
   before accessing it using the identifier.
*)
type shared_variable =
  | NewVar of let_definition * expr
  | Var of expr

(**
   Generation of ocaml identifiers.
   As we want to patch OcamlAst for beeing able to perform some analysis,
   we may rather pass through there for generating ocaml ident, pat, and params.
*)
module FCons :
sig

  type ident = Ident.t

  (**
     Simple traduction from qml const into ocaml const
  *)
  val const : QmlAst.const_expr -> Ocaml.const_expr

  val pat : ident -> Ocaml.pattern
  val param : ident -> Ocaml.param_formel
  val var : ident -> Ocaml.expr

  val patas : Ocaml.pattern -> ident -> Ocaml.pattern

  (** combinaison *)
  val param_var :
    ident -> Ocaml.param_formel * Ocaml.expr
  val pat_var :
    ident -> Ocaml.pattern * Ocaml.expr
  val param_pat_var :
    ident -> Ocaml.param_formel * Ocaml.pattern * Ocaml.expr
end
