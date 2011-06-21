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
   Simple pattern analysis for optimizing javascript pattern matching generation.
   @author Mathieu Barbin
   @author Francois Pessaux
*)

(**
   This module defines private AST in order to have a pattern classification for
   computing optimization in the javascript back-ends.

   The module contains the definition of these AST, and the analysis and transformation
   function for projecting QML patterns into the correct kind of patterns, depending on
   the structure of the pattern and the type of the matched expression.
*)

(** {6 Flags for pattern terminaison} *)
(**
   Matching sums and column
   [`closed] means that all cases are present,
   [`open_] means that the pattern has an open variable.
*)
type flag = [`closed | `open_]
type colvar = flag
type rowvar = flag

val pp_flag : Format.formatter -> flag -> unit

(** {6 Dedicated Algebra for Pattern manipulation} *)

type label = Annot.label

(**
   A private AST for trivial patterns.
*)
module T :
sig
  type pat =
    | Const of label * QmlAst.const_expr
    | Var   of label * Ident.t
    | Any   of label
    | As    of label * pat * Ident.t

  val pp : Format.formatter -> pat -> unit
end

(**
   A private AST for representing sum patterns.
   In [qmljsimp] we do not have tags for representing the full structure of the record,
   so we do not to differenciate row variable.

   The only thing we should take care of, is the [colvar] when we match some fields.
   If the [colvar] is open, we should then perform an extra check of size.
   In this case (colvar = `open_), we assert than there were not any open [row_var]
   in the structure of the pattern (typer inconsistency).

   If the [colvar] is closed, we should not complete the 3-dots with S_PatAny for optimization,
   that means that the simple fact that the presence of matched field is suffisant for validating
   the pattern.

   Invariant: in a list of [pat] of the same level, the [col_var] is homogenous.
   For optimization of representation, and size computation, the fields are stored into
   an array rather than a list.
*)
type pat =
  | Fields of label * (string * pat) array * rowvar * colvar
      (**
         Gathering all fields, and a final info for taging strict pattern.
         [`open] pattern are those with a Some rowvar, e.g. {[{ a ; ... }]}
         Invariant: the list is sorted by fields. (lexicographic order)
      *)

  | Const  of label * QmlAst.const_expr
      (**
         For sub-patterns only
      *)

  | Var    of label * Ident.t
  | Any    of label

  | As     of label * pat * Ident.t
      (**
         For extra bindings efficient compilation.
      *)


val pp : Format.formatter -> pat -> unit

(** {6 Projection} *)

type ('pat, 'right_hand) matching = ('pat * 'right_hand) list

(**
   The projection of patterns depends on all patterns present in a given
   pattern matching. For projecting correctly all pattern of a pattern matching,
   we should analysis all of them.
   Invariant: in the [pat] list, the [col_var] is homogenous
*)
type 'right_hand t =
  | Trivial of (T.pat, 'right_hand) matching
  | Pat     of (pat, 'right_hand) matching

(**
   The type is given because sometimes, the result of structural analysis do not
   correspond to the inferred type of the pattern, because of e.g. coercion added
   in the code.
   e.g.:
   {[
   f(e) =
     match e with
     | { foo } ->
     | _ ->
   ]}
   and
   {[
   type foo = { foo } / { bar }
   f(e : foo) =
     match e with
     | { foo } ->
     | _ ->
   ]}
   In the first case the pattern is open, because the function is polymorphic in column.
   In the second case, the pattern is strict.
   If you look just at the structure of patterns, you cannot differenciate the 2 forms.
*)
val analysis :
  gamma:QmlTypes.gamma ->
  annotmap:QmlAst.annotmap ->
  ty:QmlAst.ty ->
  (QmlAst.pat * 'right_hand) list ->
  'right_hand t
