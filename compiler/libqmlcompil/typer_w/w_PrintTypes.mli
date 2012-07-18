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
(*
   @author François Pessaux
*)

val print_only_function : unit -> unit
val print_function_with_n_args : unit -> unit
val print_subtype_of_function : unit -> unit
val set_error_type1 : W_Algebra.simple_type -> unit
val set_error_type2 : W_Algebra.simple_type -> unit
val pp_simple_type: Format.formatter -> W_Algebra.simple_type -> unit
val pp_fake_simple_type: Format.formatter -> W_Algebra.simple_type -> unit
val pp_simple_type_prepare_sequence: W_Algebra.simple_type list -> unit
val pp_simple_type_start_sequence:
  Format.formatter -> W_Algebra.simple_type -> unit
val pp_simple_type_continue_sequence:
  Format.formatter -> W_Algebra.simple_type -> unit
val pp_simple_type_end_sequence:
  Format.formatter -> W_Algebra.simple_type -> unit
val pp_nothing_end_sequence: unit -> unit
val pp_scheme: Format.formatter -> W_Algebra.types_scheme -> unit
val print_simple_type: W_Algebra.simple_type -> unit
val print_scheme: W_Algebra.types_scheme -> unit
