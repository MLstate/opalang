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

(** Polymorphic map indexed by annotations.

    Here's the semantics of AnnotMaps:

    Fields that the typer HMX uses / fills (may depend on options given to him):
    + ty: the type ! due to how the typer works, the annotation may
    sometimes be slightly more general than what one would expect (or not)
*)

type trace = Annot.t AnnotMap.t

exception AnnotNotFound of string * Annot.t
exception ConflictingAnnotations of Annot.t

type 'a typed_annot =
 {
      a_ty : 'a option ;
      (* TODO: rename a_tsc to a_tsc_gen and perhaps change it's type
         to [TypeVar.t list] and rename even more *)
      a_tsc : ('a, unit) QmlGenericScheme.tsc option ;
      a_tsc_inst : ('a, unit) QmlGenericScheme.tsc option ;
    }

type 'a gen_annotmap = ('a typed_annot) AnnotMap.t

val map : ('a -> 'b) -> 'a gen_annotmap -> 'b gen_annotmap
val map_ty_tsc : ty:('a -> 'b) -> tsc:(('a,unit) QmlGenericScheme.tsc -> ('b,unit) QmlGenericScheme.tsc) -> 'a gen_annotmap -> 'b gen_annotmap

val empty : 'a gen_annotmap
val is_empty : 'a gen_annotmap -> bool
val size : 'a gen_annotmap -> int

(**
   [no_conflict_if_equal=false] by default.
*)
val merge : ?no_conflict_if_equal:bool -> 'a gen_annotmap -> 'a gen_annotmap -> 'a gen_annotmap
val overwrite : 'a gen_annotmap -> 'a gen_annotmap -> 'a gen_annotmap
val unsafe_overwrite : 'a gen_annotmap -> 'a gen_annotmap -> 'a gen_annotmap

(** {6 Add} *)
(** *)

val add : Annot.t -> 'a typed_annot -> 'a gen_annotmap -> 'a gen_annotmap
val add_label : Annot.label -> 'a typed_annot -> 'a gen_annotmap -> 'a gen_annotmap
val add_ty : Annot.t -> 'a -> 'a gen_annotmap -> 'a gen_annotmap
val add_ty_label : Annot.label -> 'a -> 'a gen_annotmap -> 'a gen_annotmap

(** {b Descr}: Labels an annotation with a type scheme in case this type scheme
    is created at the annotation's potition. This corresponds to a point
    in the source where a type scheme is involved and appears by generalization.
    In other words, this allows to remind the final type scheme obtained after
    having generalized a type at the annotation's point.
    {b Note}: In terms of refactoring, this function should be called
    "add_tsc_gen", by opposition to the function [add_tsc_inst] below. *)
val add_tsc :
  Annot.t -> ('a, unit) QmlGenericScheme.tsc -> 'a gen_annotmap ->
  'a gen_annotmap
val add_tsc_label : Annot.label -> ('a, unit) QmlGenericScheme.tsc -> 'a gen_annotmap ->
  'a gen_annotmap

val add_tsc_opt : Annot.t -> ('a, unit) QmlGenericScheme.tsc option -> 'a gen_annotmap -> 'a gen_annotmap
val add_tsc_opt_label : Annot.label -> ('a, unit) QmlGenericScheme.tsc option -> 'a gen_annotmap -> 'a gen_annotmap

(** {b Descr}: Labels an annotation with a type scheme in case this type scheme
    is instantiated at the annotation's potition. This corresponds to a point
    in the source where a type scheme is involved and used by instantiation.
    In other words, this allows to remind the original type scheme that got
    instantiated at the annotation's point. *)
val add_tsc_inst :
  Annot.t -> ('a, unit) QmlGenericScheme.tsc -> 'a gen_annotmap ->
  'a gen_annotmap
val add_tsc_inst_opt : Annot.t -> ('a, unit) QmlGenericScheme.tsc option -> 'a gen_annotmap -> 'a gen_annotmap

val add_tsc_inst_label :
  Annot.label -> ('a, unit) QmlGenericScheme.tsc -> 'a gen_annotmap ->
  'a gen_annotmap
val add_tsc_inst_opt_label : Annot.label -> ('a, unit) QmlGenericScheme.tsc option -> 'a gen_annotmap -> 'a gen_annotmap

(** {6 Find} *)
(** *)

val find : Annot.t -> 'a gen_annotmap -> 'a typed_annot
val find_ty : Annot.t -> 'a gen_annotmap -> 'a
val find_tsc : Annot.t -> 'a gen_annotmap -> ('a, unit) QmlGenericScheme.tsc
val find_tsc_inst : Annot.t -> 'a gen_annotmap -> ('a, unit) QmlGenericScheme.tsc

val find_label : Annot.label -> 'a gen_annotmap -> 'a typed_annot
val find_ty_label : Annot.label -> 'a gen_annotmap -> 'a
val find_tsc_label : Annot.label -> 'a gen_annotmap -> ('a, unit) QmlGenericScheme.tsc
val find_tsc_inst_label : Annot.label -> 'a gen_annotmap -> ('a, unit) QmlGenericScheme.tsc

val find_opt : Annot.t -> 'a gen_annotmap -> 'a typed_annot option
val find_ty_opt : Annot.t -> 'a gen_annotmap -> 'a option
val find_tsc_opt : Annot.t -> 'a gen_annotmap -> ('a, unit) QmlGenericScheme.tsc option
val find_tsc_inst_opt : Annot.t -> 'a gen_annotmap -> ('a, unit) QmlGenericScheme.tsc option

val find_opt_label : Annot.label -> 'a gen_annotmap -> 'a typed_annot option
val find_ty_opt_label : Annot.label -> 'a gen_annotmap -> 'a option
val find_tsc_opt_label : Annot.label -> 'a gen_annotmap -> ('a, unit) QmlGenericScheme.tsc option
val find_tsc_inst_opt_label : Annot.label -> 'a gen_annotmap -> ('a, unit) QmlGenericScheme.tsc option

(** {6 Remove} *)
(** *)
val remove : Annot.t -> 'a gen_annotmap -> 'a gen_annotmap

val remove_tsc : Annot.t -> 'a gen_annotmap -> 'a gen_annotmap
val remove_tsc_inst : Annot.t -> 'a gen_annotmap -> 'a gen_annotmap

val remove_tsc_label : Annot.label -> 'a gen_annotmap -> 'a gen_annotmap
val remove_tsc_inst_label : Annot.label -> 'a gen_annotmap -> 'a gen_annotmap


(* ************************************************************************** *)
(** {b Descr}: Iterates on the map's value, applying the functions passed in
    arguments on the corresponding fields of each annotation map value.
    Functions are expected to return [unit], their application order is the
    order the present function takes them in arguments. In other words,
    functions are applied in the following order:
    1: [f_for_key], 2: [f_for_ty], 3: [f_for_tsc],
    4: [f_for_tsc_inst].
    REMARK: Because the order in which the functions passed as arguments are
    called is fixed, this is not very flexible. I doubt this iterator can be
    very useful apart to implement a debug-print function over maps (like the
    one available in [QmlPrint.debug_QmlAst_annotmap]).
    {b Args}:
     - [f_for_key] : Function to apply on the key of the map's binding.
     - [f_for_ty] : Function to apply on the optional type of the field [a_ty]
         of the bound ['a typed_annot] value.
     - [f_for_tsc] : Function to apply on the optional type scheme of the
         field [a_tsc] of the bound ['a typed_annot] value.
     - [f_for_tsc_inst] : Function to apply on the optional type scheme of the
         field [a_tsc_inst] of the bound ['a typed_annot] value.
    {b Visibility}: Exported outside this module.                             *)
(* ************************************************************************** *)
val iteri:
  f_for_key: (AnnotMap.key -> unit) ->
  f_for_ty: ('a option -> unit) ->
  f_for_tsc: (('a, unit) QmlGenericScheme.tsc option -> unit) ->
  f_for_tsc_inst: (('a, unit) QmlGenericScheme.tsc option -> unit) ->
  'a gen_annotmap -> unit



(** {6 Imperative maps} *)
(** *)
module Ref :
sig
(** Functional annotmap are nice, but many want global refs for
    convenience. But rather than having a global reference for every
    one, we can have "local" global references for everyone. But then,
    we want to share some implementation, this is what this functor does.

    USAGE: each time you want to have your own global reference to an annotmap, do:

    module MyRef : QmlAnnotMap.Ref.REF =
    struct
      type ty = QmlAst.ty
      let _global = ref QmlAnnotMap.empty
    end

    module MyAnnotRef = QmlAnnotMap.Ref.Make (MyRef)

    and then use import/export/etc.
**)

module type REF = sig type ty val _global : (ty gen_annotmap) ref end

module type ANNOTMAPREF =
sig
  type ty

(* general functions *)
  val clear : unit -> unit
  val import : ty gen_annotmap -> unit
  val merge : ty gen_annotmap -> unit
  val overwrite : ty gen_annotmap -> unit
  val export : unit -> ty gen_annotmap
  val get_opt : Annot.t -> (ty typed_annot) option
  val get : Annot.t -> ty typed_annot
  val set : Annot.t -> ty typed_annot -> unit

(* specific functions: please add more if useful *)
  val set_ty : Annot.t -> ty -> unit
  val get_ty : Annot.t -> ty
  val get_ty_opt : Annot.t -> ty option
  val get_tsc_opt : Annot.t -> (ty, unit) QmlGenericScheme.tsc option
end

module Make (Ref: REF) : (ANNOTMAPREF with type ty = Ref.ty)

end (* module Ref *)
