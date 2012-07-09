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
   @author Fran�ois Pessaux
*)



(* ************************************************************************** *)
(** {b Descr}:  Being exported in [Typer_w.ml], this function is made visible
    for [pass_Typing.ml] and [opaTopEnv.ml] in order to clear the type of
    exceptions by initializing it to a new sum type. This way, there is no
    pollutive reminding of exceptions of the previous compilation unit if the
    present one doesn't depend on it.
    ATTENTION: This function must be called before [enrich_type_exception]
    otherwise assert will fail.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
val reset_type_exception: unit -> unit

(* ************************************************************************** *)
(** {b Descr}: Being indirectly exported in [Typer_w.ml] as
    [init_type_exception], this function allows to accumulate all the type
    constraints related to the type of exceptions, constraints obtained from
    the modules the current compilation unit depends on. In effect, each module
    "saw" some exceptions, so defined them and since there is one unique type
    for exceptions, we must merge all these constraints on the same type.
    This function is hence in charge of unifying a type (assumed to be a sum
    describing a "view" of some exceptions) with the unique type globally
    representing exceptions in the context of the current compilation unit.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
val enrich_type_exception : W_TypingEnv.t -> W_Algebra.simple_type -> unit

(* ************************************************************************** *)
(** {b Descr}:
    This function allows retrieving the type of exceptions as a [simple_type]
    (assumed to be a sum) type
    This function serves 2 purposes:
     1) When we need to make reference during inference to the type of
        "exceptions", we are sure we are always returned the "same" type in
        which all the exceptions cases will be accumulated. so, when one needs
        to "create", "get" the type of an exception, we simply call the present
        function.
     2) Being exported in [Typer_w.ml] it is made visible for [pass_Typing.ml]
        in order to allow this latter so save the  structure of the type of
        exceptions we inferred during the typechecking of the current
        compilation unit. Hence, all modules depending on the present
        compilation unit will be able, to know our exceptions definitions.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
val type_exception : unit -> W_Algebra.simple_type
