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
    Complete QmlFlat Server-side library.

    Implementation of server-side operations for the QmlFlat compiler.

    @author Mathieu Barbin
    @author David Rajchenbach-Teller
    @author Esther Baruk (modifications)
*)

(**
   Runtime error on server due to a type error.
   Such an exception can possibly escape this module.
   A runtimerror happens never alone, a message has been printed
   on stderr before raising the exception.
*)
exception RuntimeError

(** Print a message and raise exception [RuntimeError]. *)
val runtime_error : string -> 'a


(** {6 Field} *)

(**
   Field are shared strings.
*)

module Field :
sig
  (**
     An opaque type for fields
  *)
  type t

  (**
     Physicall identity.
  *)
  val equal : t -> t -> bool

  (**
     Returns -1, 0 or 1
  *)
  val compare : t -> t -> int

  (**
     Returns true if and only if a < b.
     More effecient than compare.
  *)
  val lt : t -> t -> bool

  (**
     Returns true if and only if a > b
     More effecient than compare.
  *)
  val gt : t -> t -> bool

  (**
     Side effect on a private Hashtbl.
     Beware, we are trusting the input string for efficiency
     purpose. Do not corrupt it !!
  *)
  val register : string -> t

  (**
     Without side effect.
  *)
  val field_of_name : string -> t option
  val name_of_field : t -> string option

  (**
     <!> Beware, we are giving you the shared field.
     Do not perform side effect on the name.
     (if we get scared, we can return a copy)
  *)
  external name : t -> string = "%identity"
end

(*
(**
   Return the name of a field. Used for error reporting.

   If this function returns [None], you are in trouble because
   this means that your field does not exist.
*)
val name_of_field: field -> string option

(**
   Declare a new field, and store it in the global table.
   After a call to :
   {[
   let field = declare_field "field"
   ]}
   any call to [name_of_field field] returns Some "field"
*)
val register_field : string -> field
val register_fields : string list -> unit

*)

(** {6 Field Access} *)

module FieldAccess :
sig
  (**
     An opaque information which may be used
     to accelerate several consecutive searches,
     provided that you have ordered your searches by increasing [field].
  *)
  type t

  (**
     An opaque type for cache field_access
     Value of this type are mutable and modified when searching fields
     This type was also called hint in the code and function names
  *)
  type cache

  (**
     A safe value for [field_access] which you can use to initialize e.g. caches.
  *)
  val default : t
  val start_from : t
  val make_cache : unit -> cache
end

(** {6 VTable} *)

(**
   A virtual table is a sorted array of field.
   So that the stability of the sorted property can be
   ensured by typing, the type is private.
   Check related to this property are limited to a few constructors.
*)

module VTable :
sig
  (**
     An opaque type for each vtable of record.
  *)
  type t

  (**
     Create a vtable. The array should be sorted by [Field.compare].
     Unicity of fields is required in a vtable.

     [vtable] are shared between records, this is an invariant.
     There is a runtime cache of vtables, based on string array hashing.

     In the llvm compiler, the structure is an index in a vtable map.
     In the flat, this is a [vtable].
  *)
  val register : string array -> t

  (**
     Dynamically, you may use to allow the creation of new VTable,
     by using [register], or not, by using [shared], which return
     [None] if no such VTable was already known.
  *)
  val shared : string array -> t option

  (**
     <!> Do not corrupt the vtable.
  *)
  external export : t -> Field.t array = "%identity"

  (**
     Safe function which does not segfault on any object.
     Tell if the object is a vtable
  *)
  val check : Obj.t -> bool
end

(** {6 Records} *)

(**
   1) Complex records:

   The contents of a complex record.
   It contains :
   - as first element : the vtable
   - as second element : a prospective information
   - as remaining elements : the record values.
   You must be absolutely certain that the values are {e not packed}.
   This representation causes no more problems with floats because the first element
   of the array is a field array (the vtable).

   About lazy values :
   We don't consider it because lazy and non-lazy values are the same at runtime, encapsulated
   in a value of type Obj.t.

   2) Empty record

   This is a complex record, with a empty vtable.
   We ensure that the value empty is shared. This is an invariant.

   3) Simple record

   Record with one uniq field of type void are optimized, and represented directly by the field.
*)
type flat_record = Obj.t array

module Complex :
sig
  type t

  (** {6 Getters} *)

  val get_value : int -> t -> 'a
  val get_info : t -> Obj.t
  external get_vtable : t -> VTable.t = "%field0"

  (** {6 Constructors} *)

  (**
     Initialize a complex record with a set of already evaluated field values
  *)
  external init_from_evaluated : flat_record -> t = "%identity"

  (** return a fresh record, sharing data *)
  val update_info : t -> Obj.t -> t

  (** side effect, modification in place *)
  val inject_info : t -> Obj.t -> unit
end

type record = Complex.t (**An opaque type for records. *)

(**
   An unwrap for matching record with arrays.
*)
external unwrap_record : record -> _ array = "%identity"

(**
   Runtime check
*)
val is_record : Obj.t -> bool
val is_simple : record -> bool

(**
   Exported for ServerLib. Probably possible to fix.
*)
val val_shift : int

val init_from_list : (Field.t * 'a) list -> record
(**
   Sharing of simple record

   Construct a trivial record such as [{foo}].

   Performance note:
   Two trivial records are structurally equal
   if and only if they are physically equal.
   This performance note is preserved thanks to the string sharing
   ensured by the systematic use of Field, VTable, and Simple modules.
*)
module Simple :
sig
  val register : string -> record
end

(**
   {7 Operations on records}
*)

(**
   The shared empty record.
*)
val empty : record
val shared_void : Obj.t

(**
   Ignore a value, return [void], i.e. the shared empty record.
*)
val void_of_unit : unit -> record

(**
   Return [true] if the record is empty, [false] otherwise.
   For this purpose, check for physical equality with [empty].
*)
val is_empty : record -> bool

(**
   Bool are shared
*)
val true_ : record
val false_ : record

(**
   returns either the shared [true_] or [false_]
*)
val wrap_bool : bool -> record

(**
   test for physically equality with [true_]
*)
val unwrap_bool : record -> bool

(**
   Helpers for option projection.
*)

(**
   none is shared
*)
val none : record

(**
   <!> the 'a should be an opa value
*)
val some : 'a -> record

(**
   unwrapping option
*)
val unwrap_option : record -> 'a option
val wrap_option : 'a option -> record

(**
   Returns the number of fields in this record.
   Invariant: if the number is 0, the record is the shared empty record.

   If the record is empty, this is 0.
   If the record is simple, this is 1.
   Otherwise, this is the length of the vtable of the Complex record,
*)
val number_of_fields : record -> int

(**
   Info attached to a complex record.
*)
type 'a info constraint 'a = [> ]

(**
   Get the information attached to a record.

   @return [None] if no information has been attached to a record, [Some x] if the record
   has been initialized with an argument [info].
*)
val get_record_info: record -> 'a info option

(**
   Depending on the given flat_record:
   -return the shared empty record
   -build a simple record
   -build a complex record

   The given record may contain lazy values.
   <!> If your record is simple, info is ignored.
   <!> If your record has only 1 field, and contain
   a lazy value, it is evaluated right-away.
*)
val safe_init : flat_record -> record

(**
   Optimized version for potential simple record.
   Called with an array of size 3 (1 data only).
   If the data is empty, this build a shared simple record
   if not, keep the record as is. (the vtable should be the
   correct shared one)
*)
val may_be_simple : flat_record -> record

(**
   Initialize a record with a given array containing a array of fields [vtable], an additional information
   [info] (see below) and a sequence of values.

   The arguments need to respect the following:
   - the order of fields in [vtable] {e must} match the order of values
   - the fields of [vtable] {e must} be sorted by increasing order
   - there {e must} be no duplicate field in [vtable]
   - you {e must} never overwrite [vtable] or the values yourself
   - values {e must} spread from index 2 to the end of the array
   - if you are using [unsafe_init_lazy], the record {e must} be something that does not
   evaluate to the empty record or to a record of size 1 with one field of type [void].
   - if you are using [unsafe_init_lazy], the evaluation function {e must} never raise an
   exception.

   In addition:
   - [vtable] should necessarly be shared between records with the same vtable, so
   as to limit memory usage and improve cache-consistency.

   This initialization is in O(1).

   Initialization can be either eager, with [unsafe_init], or lazy, with [unsafe_init_lazy].

   Additional information [info] for special-purpose records can be provided as option value at
   index 1. This information may be recovered by [get_record_info] but may also lost by record extension.

   This function must stay external and be a call to identity, else it blows up compilation time with
   ocamlopt.opt.
 *)

external unsafe_init_static : flat_record -> record = "%identity"

(**
   Extend a record with some other fields * value.
   [extend a b] adds all the fields of [b] to [a].
   If a field appears both in [a] and [b], the value
   of [b] is kept.
   The array given as extension should be sorted by field, and field
   should be uniq in this array.
*)
val extend_with_array : record -> (Field.t * Obj.t) array -> record

(**
   Access a given field in a record.
   Behavior is unspecified if the record doesn't have this field.
*)
val dot : Field.t -> record -> 'a

(**
   As [dot] but returns [Some x] in case of success,
   [None] in case of failure.
*)
val dot_opt : Field.t -> record -> 'a option

(**
   The following behave as [search_field] et al,
   except they also take as argument a cache, which
   hopefully contains the index of the field in the record.
   If the cache value doesn't point to the index, it is simply discarded,
   and updated for the next search
*)

val dot_with_cache :
  FieldAccess.cache -> Field.t -> record -> 'a

(**
   [unsafe_get n r] returns the value of the [n]th field of record [r].
   The record should be valid.
   The index is the index of the vtable, starting from 0.
*)
val unsafe_get : int -> record -> 'a

(** {6 Compiler Interface} *)

(**
   CF module Flat_Common.FlatServerLib, which reproduces the hieararchy
   of primitives, for code generation.
*)
