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
   Animation displayed in the terminal during a compilation.
   @author Louis Gesbert
   @author Mehdi Bouaziz
*)

type anim = string array array

type t

val init : ?och:out_channel -> auto_walking:bool -> anim -> anim -> t
val reset : t -> unit
val update : t -> unit
