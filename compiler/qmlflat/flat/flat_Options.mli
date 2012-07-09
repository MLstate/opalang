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
   Flat Compiler : Command line options
   @author Mathieu Barbin
*)

val options : (Base.Arg.key * Base.Arg.spec * Base.Arg.doc) list

(** {6 Getters} *)

val no_cache : unit -> bool
val no_optim : unit -> bool

(** {6 Setters} *)

(**
   Only if needed.
*)
