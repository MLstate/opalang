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
(*
    @author Adam Koprowski
**)

type item = Call of string
          | Parens of exp
	  | Literal of string
	  | Any
	  | Range of char * char
	  | CharClass of char list
and primary = [`AND | `NOT | `NORMAL] * item * [`OPTION | `STAR | `PLUS | `NORMAL]
and seq = primary list
and exp = seq list

type grammar_rule = Rule of string * exp * string option (* name, definition, optional descriptive name *)

type mouse_grammar = grammar_rule list

