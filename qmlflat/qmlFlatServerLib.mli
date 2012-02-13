(*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
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
