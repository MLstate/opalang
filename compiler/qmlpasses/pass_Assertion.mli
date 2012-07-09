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
   Processing assert directives
   @author Mathieu Barbin
*)

(**
   Depending on the option --no-assert, remove assert directives, or
   rewrite them using a cond and the [\@fail] directive.

   Resolution of assert directives takes places after typing and slicing
   to avoid too much differences between the code with or without asserts,
   but is done before explicit instantiation, to avoid the introduction
   of unused type variable (used e.g. just for magic to string in assert
   error messages)
*)

val process_code : no_assert:bool ->
  QmlTypes.gamma -> QmlAst.annotmap -> QmlAst.code -> QmlAst.annotmap * QmlAst.code
