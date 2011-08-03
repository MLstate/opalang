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
   OpaTop dedicated parser.
   @author Mathieu Barbin
*)

(**
   This module is just an higher level manipulation of opa syntax for producing
   code expressed in Qml AST.

   It uses the standard opa parser, and then use the traduction OpaToQml.
   It uses OManager for errors report.
*)

(** {6 Types alias} *)

type filename = string
type contents = string

(**
   The parser uses a cache, and FilePos for building positions in the AST.
   It is important that the filename passed to the function is well specified.
*)
val parse : ?filename:filename -> contents -> QmlAst.code
