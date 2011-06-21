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
   Flat Compiler : Dot generation
   @author Mathieu Barbin
*)

(**
   Depending on what do we know at compile time (typer), the
   strategy of dot generation is different. In goes from
   a dichotomic search (without any optimization) to a direct access.

   The compilation process perform already some side-effects
   about the sharing variables (fiels, vtable, and simple).

   The dot may introduce extra caches, and we may want to keep them
   not too far from new val.
   It is possible get the caches at any moment by flushing the
   module [Flat_Cache]. In this may, we may choose to put them
   at toplevel in the beginning of the code, or between toplevel values
   if wanted.
*)

val compile :
  env:Flat_Env.env ->
  context:QmlError.context ->
  ty_expr:QmlAst.ty option ->
  Ocaml.expr ->
  Flat_Field.label ->
  Ocaml.expr
