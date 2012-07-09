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
   Flat Compiler : Simple Record generation
   @author Mathieu Barbin
*)

(**
   Compile time management of simple record generation.
   The type flatruntime.record is shared in case of simple records.
   As long as we need to introduce new simple record in the code,
   we use this module for getting either the shared record,
   which means its definition, and the variable for accessing it.
*)

(**
   The type you should give for building a vtable.
   The expr is on ocaml expression containing an identifier
   pointing on the shared field corresponding to the label.
*)
type label = Flat_Field.label

val simple : label -> Flat_Common.shared_variable

(**
   The module should be restarted before each compilation unit.
*)
val reset : unit -> unit
