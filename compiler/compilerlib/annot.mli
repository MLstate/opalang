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
   Abtract type of annotation, for decorated AST.
*)

(**
   Some recent benches have shown that the indirection introduced by having a record
   {[{ e : expr ; annot : annot }]}
   for manipulating the AST is less efficient that having the annotation as
   field0 of the constructors. (allocation, GC, etc.)
   {[
   K of annot * arg1 * arg2 * etc.
   ]}

   In an other side, AnnotMaps in libqmlcompil are much too complex, and correspond
   to a louable (but partially deprecated) effort to follow the complexe needs of several
   versions of hacky typers.

   Finally, the experience has shown that positions are very important, and can
   have a special traitement to be sure that we do have always a position for warnings,
   error messages, etc.

   This module is a proposition for reacting to these remarks.
   The changes in the framework are considerable in term of portion of code touched,
   but it is just a matter of time (no fondamental difficulty).
*)

(** {6 Annotation} *)

(**
   The abstract type of a annot key.
*)
type t

(**
   The comparaison on [t]
*)
val compare : t -> t -> int

(**
   equality on [t]
*)
val equal : t -> t -> bool

(**
   hash on [t]
*)
val hash : t -> int

(**
   Get a string representation of the annot. Injective.
*)
val to_string : t -> string

(**
   Generate a fresh annot. Global stamps.
*)
val next : unit -> t

(**
   FIXME: document there why we need to transform an annot into a int.
   If there is no good reason, the function should not be exported
*)
val to_int : t -> int

(** {6 Collections} *)
module AnnotMap : BaseMapSig.S with type key = t
module AnnotSet : BaseSetSig.S with type elt = t

(** {6 AST} *)

(**
   Originally defined in QmlLoc, the type [label] regroups in a record a position, and an annot.
   This type is meant to be used as field0 of Konstructors of AST.
   {[
   type ast =
     | K1 of label * arg1 * arg2 * etc.
     | K2 of label * args * etc.
   ]}

   It will lead to have an underscore in pattern matching, most of the time,
   but it is the price to pay for removing the indirection [.expr, .annot] which is
   not better to manipulate for deep patterns.

   Note:
   We could also have had a version :
   {[
   type ast =
     | K1 of pos * annot * arg1 * arg2 * etc.
     | K2 of pos * annot * args * etc.
   ]}
   but the couple (pos * annot) is not often separatly refreshed, so it brings probably nothing
   in term of performances, but make 2 underscore in patterns instead of 1.
*)

(**
   The type used for positions : defined in module [FilePos]
*)
type pos = FilePos.pos

(**
   The type label.
   + annot : A unique annotation.
   + pos : The position in the source, produced by the parser, or empty for generated code.
*)
type label

val pos : label -> pos
val annot : label -> t
val make_label : t -> pos -> label

(**
   Fresh label, with a fresh annot.
*)
val next_label : pos -> label

(**
   Fresh label, with the same pos
*)
val refresh : label -> label

(**
   Frehs annot, with a [no_pos] positions.
   Should be used only for compatibility with old code.
   The string is an indication to the path to the function which
   build the label.
*)
val nolabel : string -> label

(**
   If your AST follows guidelines about label, you can use these functions,
   by reexporting them in a mli for restricting the type ['ast] to your AST.

   The guidelines is the following :
   The type ['a] should have a [label] in its [field0]
*)

module Magic :
sig

  (**
     Get the label of an expression.
  *)
  val label : 'ast -> label

  (**
     Get the annot of an expression.
  *)
  val annot : 'ast -> t

  (**
     Return a new expression by changing the label.
     + new allocation of the block of the expression constructor
     + shallow copy of old children of the constructor
  *)
  val new_label : 'ast -> label -> 'ast

  (**
     Return a new expression by changing the annotation.
     + new allocation of the block of the expression constructor
     + new allocation of a label
     + shallow copy of old children of the constructor
     + shallow copy of the old position
  *)
  val new_annot : 'ast -> t -> 'ast

  (**
     Get the pos of an expression.
  *)
  val pos : 'ast -> pos

  (**
     Return a new expression by changing the position.
     + new allocation of the block of the expression constructor
     + new allocation of a label
     + shallow copy of old children of the constructor
     + shallow copy of the old position
  *)
  val new_pos : 'ast -> pos -> 'ast

  (**
     Same than [new_pos] in term of allocation, but merge the old pos and the new pos.
  *)
  val merge_pos : 'ast -> pos -> 'ast

end
