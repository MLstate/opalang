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
   Extra functions on types
*)


(**)

module TypeOrder : (OrderedTypeSig.S with type t = QmlAst.ty)

module TypeSet : (BaseSetSig.S with type elt = QmlAst.ty)
module TypeMap : (BaseMapSig.S with type key = QmlAst.ty)

type 'a typemap = 'a TypeMap.t
type typeset = TypeSet.t

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
  val follow_alias_noopt : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty

  (**
     like [follow_alias_noopt] but traverse also private types.
  *)
  val follow_alias_noopt_private : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty

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

  (** from gamma a typeident ty and arguments type tys gitve you the type 'tys ty' (qml) or 'ty(tys)' (opa) *)
  val from_typename : QmlTypes.gamma -> QmlAst.typeident -> QmlAst.ty list -> QmlAst.ty

  (** for a name type give the definition (dereferencing all alias if recurse is true (default)) of this type as a list of type
      1 element if definition is not a sum type
      many for a sum type *)
  val expand_type : ?recurse:bool -> QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty

  (** if a type is an alias give follow the alias as a type list.
      @raises QmlTyperException.Exception *)
  val get_deeper_type_until :  QmlTypes.gamma -> (QmlAst.ty -> bool) -> QmlAst.ty -> QmlAst.ty

  (** for a typename, follow all alias until it gets the last typename,
      for other types it is identity.
      @raises QmlTyperException.Exception *)
  val get_deeper_typename : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty

  (** collect all typevars of a type with filtering, used by Specialisation *)
  val get_vars : ?filter:(QmlAst.typevar -> bool) -> QmlAst.ty -> QmlTypeVars.TypeVarSet.t

  (* Other inspection functions *)
  (** deconstruct a type arrow or assert fails *)
  val typeArrow_to_tuple : QmlAst.ty -> QmlAst.ty * QmlAst.ty
  val is_type_arrow : QmlTypes.gamma -> QmlAst.ty -> bool

  (**
     See if the internal implementation of [ty] is the type [void].
  *)
  val is_type_void : QmlTypes.gamma -> QmlAst.ty -> bool

  (**
     See if the internal implementation of [ty] is the type [bool].
  *)
  val is_type_bool : QmlTypes.gamma -> QmlAst.ty -> bool

  val get_arrow_result : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty option
  val get_arrow_param : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty option
  val get_arrow_params : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty list option
  val get_arrow_through_alias_and_private : QmlTypes.gamma -> QmlAst.ty -> (QmlAst.ty list * QmlAst.ty) option

  (** returns a list of all the arrow types that appear in the given type
   *  if the rhs of an arrow is a typename or a private type, this function
   *  expands the type:
   *  with the gamma : [ type ('a,'b) t = ('a -> 'a) -> private('b -> 'b) ]
   *  and the input ['a -> ('a,'b) t]
   *  it returns the two elements:
   *  [ 'a -> ('a -> 'a) -> 'b -> 'b; 'a -> 'a ]
  *)
  val find_arrow_types : QmlTypes.gamma -> QmlAst.ty -> QmlAst.ty list;;
  val expand_arrow_to_rev_list : QmlTypes.gamma -> ?set:TypeSet.t -> QmlAst.ty -> QmlAst.ty list;;


end

(** Utils for arrow types *)
module TypeArrow : sig

  type 'a type_arrow_utils = QmlAst.ty list -> QmlAst.ty -> 'a

  (**
     Returns the number of arguments of [ty] taking in consideration the nary informations.
     examples :
     {[
     let f x = fun y -> x + y
     ]}
     The [nary_arity] of [f] is [1], where the [curryfied_arity] is [2]
  *)
  val nary_arity : int type_arrow_utils

  (**
     Returns the number of arguments of [ty] without distinction between a function
     which returns a function and its curryfied version.
     @see "nary_arity" for an example
     @raise Invalid_argument if the [ty] is not a [TypeArrow]
  *)
  val curryfied_arity : int type_arrow_utils

 (**
     Returns the nary function type transform to unary function type
     e.g.   x,y->z   ==>  x->y->z
  *)
  val nary_to_unary : ?recurse:bool (* true *) -> QmlAst.ty -> QmlAst.ty

end
