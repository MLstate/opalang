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

(**
   Extra functions on types
*)


(**)

module Basic :
sig
  val string : QmlAst.ty
end

module Inspect :
sig
  (* Functions to unfold typenames (all subtly different) *)
  val find_and_specialize : QmlTypes.gamma -> QmlAst.typeident -> QmlAst.ty list -> QmlAst.ty
  (** if a typename is an alias give the type in alias definition *)
  val follow_alias : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty option

  (**
     get the implementation of a types, traversing named types.
  *)
  val follow_alias_noopt : ?until:string -> QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty

  (**
     like [follow_alias_noopt] but traverse also private types.
  *)
  val follow_alias_noopt_private : ?until:string -> QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty

  (* ************************************************************************ *)
  (* {b Descr}: Exception raised when a type contains a sub-term being a named
     type defined as @private. The embedded type is the named type expression
     whose named is bound to a @private type definition.
     This is used when a toplevel value definition has a type in which a
     @private type to the package appears and this definition is not marked
     @private, hence is exported outside the package. Since the @private type
     is not visible the toplevel definition must not be visible also.         *)
  (* ************************************************************************ *)
  exception Escaping_private_type of QmlAst.ty

  (* ************************************************************************ *)
  (** {b Descr}: Ensures that no type private to the current package escapes,
      i.e. appears in the type passed as argument. *)
  (* ************************************************************************ *)
  val check_no_private_type_escaping : QmlTypes.gamma -> QmlAst.ty -> unit

  (** if a type is an alias give follow the alias as a type list.
      @raises QmlTyperException.Exception *)
  val get_deeper_type_until :  QmlTypes.gamma -> (QmlAst.ty -> bool) -> QmlAst.ty -> QmlAst.ty

  (** for a typename, follow all alias until it gets the last typename,
      for other types it is identity.
      @raises QmlTyperException.Exception *)
  val get_deeper_typename : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty

  (* Other inspection functions *)
  val is_type_arrow : QmlTypes.gamma -> QmlAst.ty -> bool

  (**
     See if the internal implementation of [ty] is the type [void].
  *)
  val is_type_void : QmlTypes.gamma -> QmlAst.ty -> bool

  (**
     See if the internal implementation of [ty] is the type [bool].
  *)
  val is_type_bool : QmlTypes.gamma -> QmlAst.ty -> bool

  val get_arrow_params : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty list option
  val get_arrow_through_alias_and_private :
    QmlTypes.gamma -> QmlAst.ty -> (QmlAst.ty list * QmlAst.ty) option

  (** Returns the type of data of the [ty] map. If the given [ty] is not a type
      of map (or an alias) throws [Not_found].*)
  val get_data_type_of_map : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty

  (** Returns all types inside the given type that could be in a mutable container *)
  val get_type_potentially_in_non_pure_type : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty list
end

(** Utils for arrow types *)
module TypeArrow : sig

  type 'a type_arrow_utils = QmlAst.ty list -> QmlAst.ty -> 'a

  (**
     Returns the number of arguments of [ty] without distinction between a function
     which returns a function and its curryfied version.
     @see "nary_arity" for an example
     @raise Invalid_argument if the [ty] is not a [TypeArrow]
  *)
  val curryfied_arity : int type_arrow_utils
end
