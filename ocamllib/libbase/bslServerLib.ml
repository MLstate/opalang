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
   Standard interface for back-end specific low_level operations.
*)

(**

   {6 General documentation}

   This is the standard interface for low-level operations that all backends
   must provide.

   It is intended to be used by the primitives of any BSL implementations.

   This is a server lib, meaning that it is used \@runtime.
   It is not included in libbsl, neither used \@compiletime

   Currently, we have 3 implementations of this interface :
   + qmltop
   + qmlflat
   + qmlfake

   The BSL are compiled as library with just a mli called {b serverLib.mli}
   containing just an [include BslServerLib.S].

   At link time, we provide the corresponding implementation.
   <!> this is a tricky and fragile point of our build-system.


   {6 Explicit Projection}

   For projecting complex data type in an external primitive for buildint a plugin,
   you'll need to manipuate explicitely some projection helpers.
   Cf the functor BslServerLibProjection.
*)

module type S = sig

  (* ------------------------------------------------------------ *)
  (**{6 Basic operations}                                         *)
  (* ------------------------------------------------------------ *)

  (**
     Compare specification.
     For non record, Pervasives.compare
     For records:
       lexicographic on present fields.
       in case of equality of vtables,
       lexicographic on attached values.
     e.g.
     {[
     { z = "yop" ; x = 0 } > { x = 42 ; y }
     ]}

     Simple record are handled correctly, as they were not optimized.
  *)
  val compare : 'a -> 'a -> int

  (* ------------------------------------------------------------ *)
  (**{6 Lazy records handling}                                    *)
  (* ------------------------------------------------------------ *)

  (** The type of records *)
  type ty_record

  (** The type of embedded information *)
  type 'a ty_info = [> ] as 'a

  (** Place-holder value. Must be extensible (eg with lazy info) *)
  val empty_record : ty_record

  (** Returns [true] if the given record has embedded lazy-db-record data *)
  val has_lazy_data : ty_record -> bool

  (** Returns the embedded lazy-db-record value if any *)
  val get_lazy_data_opt : ty_record -> ('a ty_info) option

  (** Embeds the given data in a record if Some. With None, clears that data *)
  val embed_lazy_data : ty_record -> ('a ty_info) option -> ty_record

  (**
     Embeds the given data (or clears if None) in a record as a side-effect
     <!> with the flat compiler, that has no effect on simple record
  *)
  val inject_lazy_data : ty_record -> ('a ty_info) option -> unit

  (* ------------------------------------------------------------ *)
  (**{6 Manipulate records}                                       *)
  (* ------------------------------------------------------------ *)
  (** The type of fields of records*)

  type field

  (** comparison between fields *)
  val compare_field : field -> field -> int

  (** Type of a value suitable to find the field_index, see val fields_indexes *)
  type fields_indexes

  (** Type of a field index, that can provide fast field access content, see vals dot_with_field_index and field_index  *)
  type field_index

  (** Indexed field record, see  val patterns_indexes *)
  type patterns_indexes

  (** Get a fields_indexes *)
  val fields_indexes : field array -> fields_indexes

  (** Get a field_index, can raise an exception if not existing in fields_indexes
      e.g. can be used to describe any set of fields (record fields in a value or a pattern) *)
  val field_index : fields_indexes -> field -> field_index

  (** Access the field corresponding to a field index,
      raise a runtime exception if the field_index is invalid
      does not need to work with empty record *)
  val dot_with_field_index : ty_record -> field_index -> 'a

  (** index a sequence of record field set (like a pattern) to retrieve fastly the corresponding index given a record
      e.g. can be used to implements construct fast pattern matching at runtime *)
  val patterns_indexes : fields_indexes array -> patterns_indexes

  (** comparison between record structure,
      !!! has no obligation to work for empty record on purpose (speed) !!!
      if the returned integer is -2 or -1 it is a comparison otherwise it is the index
      generate a runtime error both record form are not in pattern_index
      if one record only is in the pattern_index it is considered bigger than the other *)
  val compare_structure : patterns_indexes -> ty_record -> ty_record -> int

  (**
     Fold a record. [fold_record folder record acc] parameter
     [record] must be a record else behavior is not know (at best
     you'll get a segfault). [folder] is a function like this :
     [folder field value_of_field acc].
     On lazy records, this leads to a force eval of each value.
  *)
  val fold_record : (field -> 'b -> 'acc -> 'acc ) -> 'a -> 'acc -> 'acc

  (**
     Fold two record. [fold_record folder record1 record2 acc]
     parameter [record1] and [record2] must be a record and they must
     be identical (same structure). Else behavior is not know (at
     best you'll get a segfault). [folder] is a function like this :
     [folder field value_of_field1 value_of_field2 acc].
     Like [fold_record], this force eval lazy records.
  *)
  val fold_2_record : (field -> 'b -> 'b -> 'acc -> 'acc ) -> 'a -> 'a -> 'acc -> 'acc

  (** Get the name of field. If field doesn't exists return [None] *)
  val name_of_field : field -> string option

  (** Get the field corresponding to a string. If given parameter
      not corresponding to any record, return [None]. *)
  val field_of_name : string -> field option

  (**
     If you write a plugin, you may want to use fields definition at toplevel.
     This will create the field if it does not yet exists, without errors.
     Do not use on dynamic fields, because doing so, you may potentially
     fill the memory with untrusted input (e.g. fields names coming from a malicious client)

     <!> Do never corrupt the string you use as argument of [static_field_of_name], it would
     affect directly the shared field. (not copy of the input is done in case of a new field allocation)
  *)
  val static_field_of_name : string -> field
  val static_name_of_field : field -> string


  (* ------------------------------------------------------------ *)
  (**{6 Construct records}                                       *)
  (* ------------------------------------------------------------ *)
  (** Type used for construct a record. *)
  type record_constructor

  (** It's empty (initial) record constructor. *)
  val empty_record_constructor : record_constructor

  (** Add field and associated value to a record constructor. *)
  val add_field : record_constructor -> field -> 'a -> record_constructor

  (** Make a record from a record constructor. *)
  val make_record : record_constructor -> ty_record

  (** Make a simple record if the backend supports it, or a normal record. *)
  val make_simple_record : field -> _

  (** Get just one field of a record, if present *)
  val dot : ty_record -> field -> 'a option

  (** Assume dot would return [Some data] but optimized so that the option is never built.
      This cause a runtime error if the field is not present. *)
  val unsafe_dot : ty_record -> field -> 'a

  (** In sum types, sometimes you just want to check the occurrence of a field, without
      accessing its data *)
  val is_present : ty_record -> field -> bool

  (* ------------------------------------------------------------ *)
  (**{6 Exit functions}                                           *)
  (* ------------------------------------------------------------ *)

  (** Specify the only function that should be run whenever exiting with do_exit
      WARNING : it erased all previously specified function by at_exit
      use get_exit, if you want to preserve previously specified function *)
  val at_exit : (unit -> unit) -> unit

  (** Return the last function registered by at_exit, to do composition in whatever order you want *)
  val get_exit : unit -> (unit -> unit)

  (** To exit the backend (may not really exit for testing backends,
      e.g. qmltop) *)
  val do_exit : int -> 'a

  (* ------------------------------------------------------------ *)
  (**{6 API for explicit utilisation of projections}              *)
  (* ------------------------------------------------------------ *)

  (**
     Low level manipulation of values.

     This feature is needed for people which are manipulating radioactive
     types and need to deal with a lot of projection between opa and the
     backend language.

     It is meant to bring more safety regarding to the interfaces of the bsl.

     This is about the constructor opa[] of register directives.

     {[
     ##opa-type toto('a, 'b)
     ##register foo : opa[float], opa[toto('a, 'b)], opa['a] -> opa[unit]
     ]}
     will produce the following interface
     {[
     type ('a, 'b) opa_toto
     external wrap_opa_toto : ServerLib.ty_record -> ('a, 'b) opa_toto
     external unwrap_opa_toto : ('a, 'b) opa_toto -> ServerLib.ty_record

     val foo :
     ServerLib.ty_float ->
     ('a, 'b) opa_toto -> 'a -> ServerLib.ty_void
     ]}

     Alternative approach :

     An automatisation is done by the bsl, using [CTrans].
     The CTrans have choice to generate call to the instance of the ServerLib,
     or generating inline code for optimization. (e.g. if the projection is
     the identity, it does not generate anything.)
  *)

  (** {9 Constants} *)
  (**
     The implementation is free, it can be anything.
  *)

  (** *)
  type ty_float
  type ty_int
  type ty_null
  type ty_string

  (** *)
  val wrap_float : float -> ty_float
  val unwrap_float : ty_float -> float

  val wrap_int : int -> ty_int
  val unwrap_int : ty_int -> int

  val null : ty_null

  val wrap_string : string -> ty_string
  val unwrap_string : ty_string -> string

  (**
     In the implementation in any module implementing this interface:
     {[
     type ty_void = ty_record
     type ty_bool
     type ty_option
     ]}
  *)

  type ty_void = ty_record

  (**
     Same than empty record.
     wraping/unwraping is inlined in any implementation.
  *)
  val void : ty_void

  (**
     Booleans
  *)

  type ty_bool = ty_record

  val wrap_bool : bool -> ty_bool
  val unwrap_bool : ty_bool -> bool

  val true_ : ty_bool
  val false_ : ty_bool

  (**
     Options
  *)

  type 'a ty_option = ty_record
  val wrap_option : 'a option -> 'a ty_option
  val unwrap_option : 'a ty_option -> 'a option

  val none : 'a ty_option
  val some : 'a -> 'a ty_option

  (** {9 Marshal} *)

  (**
     Marshaling lazy value causes runtime errors.
     With this function, all pending lazy values contained
     in the value.
     Would loop on cyclic values.
  *)
  val deep_force_eval : 'a -> 'a

  (**
     Since the flat-v2, vtable and fields structure are shared.
     If we use directly the module Marshal returns corrupted values.
     We should inspect these values, and perform all the needed lookup
     to return the same structural value, but with all shared structures.

     For backend without sharing, this is the identity.
  *)
  val sharing_refresh : 'a -> 'a


end
