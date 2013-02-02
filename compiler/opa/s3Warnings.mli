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
   Regrouping all warnings for opa S3.
   @author Mathieu Barbin
*)

(**
   <!> Read carrefully the documentation.
   This module is not meant to define any wclass.
   It is simply a regroupement of all warnings we want
   to be available in the binary opa.exe version S3

   We could have define it directly in main.ml,
   but this would have lead to have too much commit on the
   main. (we may want to combine lots of warning_set in this file)
*)

val warning_set : WarningClass.Set.t
