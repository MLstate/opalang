(*
    Copyright © 2011, 2012 MLstate

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
val except_html_char : string
val allowed_special_char : string
val string_of_int : int -> string
(* escape all chars to html entities using a formater taking starting position and number of char to process *)
val htmlentities_append : Buffer.t -> string -> int -> int -> unit
(* escape all chars to html entities *)
val htmlentities : string -> string
