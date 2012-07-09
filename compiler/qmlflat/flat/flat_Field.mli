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
   Flat Compiler : Field generation
   @author Mathieu Barbin
*)

(**
   Compile time management of field generation.
   The type flatruntime.Field.t is a shared string.
   As long as we need to introduce new fields in the code,
   we use this module for getting either a new field, which means
   its definition, and the variable for accessing it.
*)

(**
   The type of label of field, as in QmlAst
*)
type label = string

val label : label -> Flat_Common.shared_variable

(**
   The module should be restarted before each compilation unit.
*)
val reset : unit -> unit
