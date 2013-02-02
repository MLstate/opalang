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
(*
   @author François Pessaux
*)


type ill_formed_column_type_reason =
  | IFCTR_row_and_column_vars  (** The ill-formed sum type contains both row
       and column variables. *)
  | IFCTR_sum_case_with_row_variable  (** The ill-formed sum type doesn't
       contain column variable but some of its cases are rows containing row
       variables. *)


(* ************************************************************************** *)
(** {b Descr}: Exception raised when trying to export to QML a
    [W_Algebra.simple_type] having both row and column variables or a sum
    containing case(s) ended by a row variable. Such types are a problem since
    they give several ways to unify, hence making us losing the property of
    principal type existence. Having a sum with cases doesn't fit eht public
    algebra. If several cases have a row variable then there is no principal
    type. If only one case has a row variable, then principal typing should
    still hold, but anyway, as stated it doesn't fit the public type algebra,
    hence is also rejected.                                                   *)
(* ************************************************************************** *)
exception Ill_formed_column_type of
  (W_Algebra.simple_type * ill_formed_column_type_reason)

exception Cyclic_type of W_Algebra.simple_type

(** Function prepare multiples exporting. *)
val prepare_export : unit -> unit

(** Function finalize a sequence of exporting and returns a gamma
    which contains definition for the cyclic types. *)
val finalize_export : unit -> QmlTypes.gamma

(** val type_scheme_to_qml_type_scheme:
     W_Algebra.types_scheme -> QmlTypes.typescheme
    @raise Ill_formed_column_type
    @raise Cyclic_type *)
val simple_type_to_qml_type: W_Algebra.simple_type -> QmlAst.ty

(** @raise Ill_formed_column_type
    @raise Cyclic_type *)
val type_scheme_to_annotmap_type_scheme:
  W_Algebra.types_scheme -> (W_Algebra.simple_type, unit) QmlGenericScheme.tsc
