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
type status = Cached | Updated | Removed
type t = { dbm : Dbm.t; ht : (string, status * KeySet.t) Hashtbl.t; }
val create : ?create:bool -> ?hint:int -> string -> t
val set_path : t -> Path.t -> unit
val replace : t -> Path.t -> KeySet.t -> unit
val find : t -> Path.t -> KeySet.t
val find_all : t -> Path.t -> Path.t list
val remove : ?cb:(Path.t -> string -> unit) -> t -> Path.t -> unit
val write : t -> unit
val close : t -> unit
