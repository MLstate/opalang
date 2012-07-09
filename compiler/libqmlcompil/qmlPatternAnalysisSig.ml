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
(**
   Signatures for [QmlPatternAnalysis]
   @author Rudy Sicard
*)

(**
   This module contains types and signatures used by [QmlPatternAnalysis].
   This module has no mli, to avoid code duplication.
   It does not contain any implementation.
*)

(**
   A type for indicating if a pattern is complete or not.
   {[
   | { a ; b }
   ]}
   would be flaged [`closed]
   {[
   | { a ; b ; ... }
   ]}
   would be flaged [`open_]
*)
type row_flag = [ `open_ | `closed ]

(**
   An alias for manipulating record fields
*)
type field = string

(**
   The interface of the argument module to the Analysis functor
*)
module type ONIONLANG =
sig
  (** {6 Types} *)

  type ident
  type const
  type ty
  type expr

  (** {6 Mandatory Functions} *)

  (**
     Comparison on const type.
     Should return -1, 0, or 1
  *)
  val compare_const : const -> const -> int

  (**
     Generate a new unique ident given a field
     and the identifier of the top-level declaration
     containing the processed expression

     It is possible to return a fresh ident which does not depend on its arguments,
     but is could also be interressant to name identifier depending on the context,
     for a better debugging.
  *)
  val gen_ident : ?ident:ident -> field -> ident

  (**
     Take a type, the fields appearing in a non strict record pattern, and returns
     all possible set of fields corresponding to possible sum cases,
     with a strictness flag ([`closed] means strict which means without [...])
     precising if the completed field list is still open.

     Some examples:

     Example 1)
     in this context:
     {[
     type toto = { a ; b ; c } / { d }
     f =
       | { a ; b ; ... } : toto ->
       | _ ->
     ]}
     a call to:
     {[
     strictify_record_ty toto [ "a" ; "b" ]
     ]}
     should return:
     {[
     [ [ "a" ; "b" ; "c" ], `closed ]
     ]}

     Example 2)
     in this context:
     {[
     type toto = { a ; b ; c } / { a ; b ; d }
     f =
       | { a ; b ; ... } : toto ->
       | _ ->
     ]}
     a call to:
     {[
     strictify_record_ty toto [ "a" ; "b" ]
     ]}
     should return:
     {[
     [ [ "a" ; "b" ; "c" ], `closed ;  [ "a" ; "b" ; "d" ], `closed ]
     ]}

     Example 3)
     in this context:
     {[
     f =
       | { a ; b ; ... } ->
     ]}
     a call to:
     {[
     strictify_record_ty { a ; b ; ... } [ "a" ; "b" ]
     ]}
     should return:
     {[
     [ [ "a" ; "b" ], `open_ ]
     ]}

     If all flags in the returned list are strict, this will helps pattern to be free
     of unstrict sub-patterns.
  *)
  val strictify_record_ty : ty -> field list -> ((field list) * row_flag) list

  (**
     Indicate if a type is an open disjuntion.
     For Qml, this means that the type is a sum type,
     with an open column variable.
  *)
  val is_open_ty : ty option -> bool

  (**
     Given as context a set of field, and a type, return the type of a given field
     the set of field is used to select cases for sum types.
     This function can assume that the field list used for selecting the case in the sum
     leads to an uniq choice in this sum.
  *)
  val strict_get_field_type : ty -> field list -> field -> ty

  (**
     Get missing constants in a pattern matching, given a list of matched constants.
     Hypothesis: the input list in sorted, according the [compare_const] function.
     The function is authorized to contain some jokes.
     It is used to produce an hint inviting to complete an pattern matching being not exhaustive.
  *)
  val get_missing : const list -> const list


  (** {6 Printing} *)
  (**
     Used for formulating errors/warnings messages
  *)

  val print_id    : Format.formatter -> ident -> unit
  val print_ty    : Format.formatter -> ty    -> unit
  val print_expr  : Format.formatter -> expr  -> unit
  val print_const : Format.formatter -> const -> unit
end
