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

module type DB =
sig

  type t

  type flags = DB_create | DB_rdwr | DB_rdonly

  exception DB_error of string

  val opendb : string -> flags list -> int -> t
  val find : t -> string -> string
  val replace : t -> string -> string -> unit
  val remove : t -> string -> unit
  val iter : (string -> string -> unit) -> t -> unit
  val close : t -> unit

end

