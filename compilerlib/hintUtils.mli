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
   Regrouping utils for Hint.
*)

(** {6 Ident} *)

(**
   Given a string list of possible choices of ident,
   and an ident, returns the proposition list
   where the propositions have been sorted so that
   the first elt is the closest to the searched name, etc...

   This code was initially somewhere in opa,
   but is cool, and may be used by more modules.

   If the propositions list is empty, the returned
   list will be empty.
*)
val get_closest_names :
  string list -> string -> string list

(**
   A sugar if you are sure that the list has at least a
   length greater than 2.
   @raise Invalid_argument if not
*)
val get_closest_names_2 :
  string list -> string -> string * string

(**
   [pp_suggestion all fmt typo]
   Print a suggestion with clothest results.
   Will print:
   {[
   Hint:
     Perhaps you meant ... or ... ?
   ]}
*)
val pp_suggestion : string list -> Format.formatter -> string -> unit
