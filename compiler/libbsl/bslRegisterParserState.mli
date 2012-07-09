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
   Private imperative state for the BslRegiserParser.

   @author Mathieu Barbin
   @author Mehdi Bouaziz
*)

(**
   This module contains some global variables for being set,
   and interracting with the [BslRegisterParser]
*)

(** {6 Type alias, for lisibility} *)

type filename       = string
type line_number    = int

(** {6 Imperative State} *)
(**
   BslRegisterLib uses the parser in a [File.lines_fold_i],
   and send to trx only a small part of the file (one line,
   or a few, with [##{] and [}##] directives : TODO)
*)

(**
   The function tells the parser what {b file} it is actually beeing parsed.
*)
val init_file : filename:filename -> unit

(**
   The function tells the parser what {b line} of the current file
   it is beeing parsed.
   It also resets the scope of TypeVariables.
*)
val init_line : line_number:line_number -> unit

(**
   For building correctly the contents of the directive [Args],
   the parser should memorize the last parsed directive in a bypass
   file.
*)
(** *)
val set_last_directive : BslDirectives.bypasslang_directive -> unit
val get_last_directive : unit -> BslDirectives.bypasslang_directive option

(** {6 Positions} *)

(**
   Building a position from the current position of the Parser.
   It uses the state of the global private variables for knowing
   the context of the parsing.
*)
val make_pos : unit -> FilePos.pos

(**
   A pprinter for the current position of the Parser.
   It uses the state of the global variables for knowing
   the context of the parsing.
*)
val pp_citation : Format.formatter -> unit -> unit

(** {6 Types Variables} *)

module TypeVar :
sig
  (**
     Build a typevar from the parsed name.
     <!> depends on the scope.
  *)
  val var : string -> BslTypes.typevar

  (**
     Used only in case you do not have the name, for anonymous typevariables only : [_]
  *)
  val fresh : unit -> BslTypes.typevar

  (**
     This reset the scope of typevars.
     This function is called by init_line, but is exported in the API if needed.
  *)
  val reset : unit -> unit
end

(** {6 Errors and Warnings} *)

(**
   Both errors and warnings uses the citation corresponding
   to the current context.
*)

(**
   The error uses OManager.error
*)
val error : ('a, 'b) OManager.oformat -> 'a

(**
   The warning uses the wclass [bsl.register].
*)
val warning : ('a, unit) OManager.oformat -> 'a
