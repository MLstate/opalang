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

type expansions_memory
val empty_memory : expansions_memory
val incrementally_expand_abbrev:
  W_TypingEnv.t -> expansions_memory -> W_Algebra.simple_type ->
  (W_Algebra.simple_type * expansions_memory)

val expand_abbrev_n_times:
  int -> W_TypingEnv.t -> expansions_memory -> W_Algebra.simple_type ->
  (W_Algebra.simple_type * expansions_memory)

(* ************************************************************************** *)
(** {b Descr}: Fully expand a named type untils either it is no more a named
    type or it can't be expanded anymore. In other words, follow the sequence
    of type abbreviations.
    If this function is called on something else than a named type, it simply
    returns the type itself.
    Note that during expansion, the original type is modified in place, setting
    is field [W_Algebra.nst_unwinded] to [Some ...] (and in depth) each time
    an abbrev is expanded.
    The expansion is just "deep in surface": we follow the cascading
    abbreviations but do not expand abbreviations present in the structure of
    the type (anyway, since we stop when we get something else than a named
    type, this remark may seem obvious, but that's better to say this clearly).
    This function is currently used to report errors in order to find fields
    suggestions in case of error on a dot-expression. It allows to find a
    record type hidden under a type abbreviation, hence seing the structure of
    this type, it is possible to see its available fields. Not expanding the
    abbreviations would leave the named type as it is, hence not showing
    fields, hence not allowing to give close available fields as a hint.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
val fully_expand_abbrev:
  W_TypingEnv.t -> expansions_memory -> W_Algebra.simple_type ->
    W_Algebra.simple_type

(* ************************************************************************** *)
(* {b Descr}: Performs a deep occur check, checking if the type variable passed
   as argument ([ty_var]) is in fact the same type than the other one ([ty])
   passed as argument by following type aliases.
   This is very important to prevent unification to create cyclic types in case
   of unifying types like ['a] and ['a t] in case where [t] is defined as
   [type 'a t = 'a u = 'a v = ... 'a].
   In effect, if we don't be careful, seing ['a] and ['a t] unification would
   simply establish a link from ['a] to ['a t] hence creating a cycle. This
   would result into an exportation error, although in fact, ['a] and ['a t]
   are exactly the same type.
   This function doesn't modify the structure of type. It unwinds [ty] without
   modifying its structure and in particular, without setting the field
   [W_Algebra.nst_unwinded] of types.
   {b Visibility}: Exported outside this module.                              *)
(* ************************************************************************** *)
val deep_exact_occur_expand_abbrev:
  W_TypingEnv.t -> expansions_memory -> ty_var: W_Algebra.simple_type ->
  in_ty: W_Algebra.simple_type -> bool
