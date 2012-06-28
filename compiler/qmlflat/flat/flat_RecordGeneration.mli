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
   Flat Compiler : Static record generation
   @author Mathieu Barbin
*)

(**
   The compilation process perform already some side-effects
   about the sharing variables (fiels, vtable, and simple).

   So, the generated expression may contain unbound variables.
   Those variables are introduced using the module [Flat_Shared].

   Other generated variable are in Flat_Cache.
   This is possible to insert them between toplevel expression.
*)

type label = Flat_Common.label


(**
   The initializer is used only if we know statically that we are dealing with a non-empty
   and a non-simple record. If not, the init is not static.
*)
val static_init :
  is_lazy:bool ->
  info:Ocaml.expr option ->
  (label * Ocaml.expr) list ->
  Ocaml.expr

(**
   Optimized initializer for potential simple record.
   <!> Beware, if the record is simple the info will be discarded.
*)
val may_be_simple :
  info:Ocaml.expr option ->
  label -> Ocaml.expr -> Ocaml.expr

val extend :
  Ocaml.expr ->
  (label * Ocaml.expr) list ->
  Ocaml.expr
