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
   Flat Compiler : Expression Compilation
   @author Mathieu Barbin
*)

(**
   <!> The expression generated depends on other generated values,
   which are :
   -shared value
     +fields
     +vtable
     +simple
   -caches (for record access).

   The insertion of theses values is handled by the compilation of
   top level values.
*)
val expr : Flat_Env.env -> QmlAst.expr -> Ocaml.expr
