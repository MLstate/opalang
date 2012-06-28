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
    Reduced QmlFlat Server-side library.
    @author Mathieu Barbin
*)

(**
   Exporting only what is inserted by the back-end.
   Compatible with ServerLib types.

   This module redefines all needed functions from Flat_Runtime for beeing compatible with the module ServerLib.
*)

type flat_record = Obj.t array

module Field :
sig
  type t = ServerLib.field
  val register : string -> ServerLib.field
end

module FieldAccess :
sig
  type cache
  val make_cache : unit -> cache
end

module VTable :
sig
  type t
  val register : string array -> t
end

type record = ServerLib.ty_record

module Simple :
sig
  val register : string -> record
end

val runtime_error : string -> 'a
external unwrap_record : record -> _ array = "%identity"

external get_vtable : ServerLib.ty_record -> VTable.t = "%field0"

val empty : record
val true_ : record
val false_ : record

val wrap_bool : bool -> record
val unwrap_bool : record -> bool

val none : record
val some : 'a -> record
val unwrap_option : record -> 'a option

val dot : Field.t -> record -> 'a
val dot_opt : Field.t -> record -> 'a option
val unsafe_get :  int -> record -> 'a

val dot_with_cache : FieldAccess.cache -> Field.t -> record -> 'a

val extend_with_array : record -> (Field.t * Obj.t) array -> record
external unsafe_init_static : flat_record -> record = "%identity"
val may_be_simple : flat_record -> record

val do_exit : int -> 'a
