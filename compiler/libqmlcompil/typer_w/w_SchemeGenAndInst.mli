(*
    Copyright Â© 2011 MLstate

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
(*
   @author FranÃ§ois Pessaux
*)

exception Private_type_not_opaque of W_Algebra.simple_type

(* ************************************************************************** *)
(** {b Descr}: Exception raised when during generalization, a non-generalizable
    type variable was encountered although we were requested to forbid such
    variables remaining.
    This is used to prevent toplevel definition from having non-generalizable
    variables (i.e. "weakly polymorphic" type variable, "_'a" variables à la
    OCaml.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Non_generalizable_type_var of
  (W_Algebra.simple_type * (** The global type hosting a non-generalizable type
       variable. *)
   W_Algebra.simple_type)  (** The non-generalizable type variable. *)

(* ************************************************************************** *)
(** {b Descr}: Exception raised when during generalization, a non-generalizable
    row variable was encountered although we were requested to forbid such
    variables remaining.
    This is used to prevent toplevel definition from having non-generalizable
    variables (i.e. "weakly polymorphic" type variable, "_'a" variables à la
    OCaml.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Non_generalizable_row_var of
  (W_Algebra.simple_type * (** The global type hosting a non-generalizable row
       variable. *)
   W_Algebra.simple_type)  (** The non-generalizable row variable embedded
       in a type. *)

(* ************************************************************************** *)
(** {b Descr}: Exception raised when during generalization, a non-generalizable
    column variable was encountered although we were requested to forbid such
    variables remaining.
    This is used to prevent toplevel definition from having non-generalizable
    variables (i.e. "weakly polymorphic" type variable, "_'a" variables à la
    OCaml.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
exception Non_generalizable_column_var of
  (W_Algebra.simple_type * (** The global type hosting a non-generalizable
       column variable. *)
   W_Algebra.simple_type)  (** The non-generalizable column variable embedded
       in a type. *)

val generalize:
  forbid_non_gen_vars: bool ->W_Algebra.simple_type -> W_Algebra.types_scheme
val generalize2:
  extra_variables:
    (W_Algebra.simple_type list *
     W_Algebra.row_type list *
     W_Algebra.column_type list) -> W_Algebra.simple_type ->
  W_Algebra.types_scheme
val trivial_scheme: W_Algebra.simple_type -> W_Algebra.types_scheme
val specialize: W_Algebra.types_scheme -> W_Algebra.simple_type
val specialize2:
  deep: bool ->
  W_Algebra.types_scheme ->
  ((W_Algebra.simple_type list *
    W_Algebra.row_type list *
    W_Algebra.column_type list) *
   W_Algebra.simple_type)
val specialize_with_given_variables_mapping:
  deep: bool ->
  (W_Algebra.simple_type * W_Algebra.simple_type) list ->
  (W_Algebra.row_variable * W_Algebra.row_variable) list ->
  (W_Algebra.column_variable * W_Algebra.column_variable) list ->
  W_Algebra.types_scheme -> W_Algebra.simple_type

val type_forall: W_Algebra.simple_type -> W_Algebra.simple_type
val get_type_forall_scheme:
  W_Algebra.simple_type -> W_Algebra.types_scheme option

val automatically_instantiate_if_forall:
  W_Algebra.simple_type ->
  (W_Algebra.simple_type * (W_Algebra.types_scheme option))
