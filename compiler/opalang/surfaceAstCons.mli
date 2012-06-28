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
open SurfaceAst

module Fresh :
  sig
    val id : unit -> int
    val typevar : unit -> string
    val old_name : ?name:string -> unit -> string
    val name : string -> string
  end

module Label :
  sig
    val label : 'a QmlLoc.label -> QmlLoc.annot
    val undecorate : 'a QmlLoc.label -> 'a
    val copy_label : QmlLoc.annot -> QmlLoc.annot
    val builtin :  unit ->  QmlLoc.annot
  end

module ExprIdent : SurfaceAstConsSig.IDENT with type ident = Ident.t
module StringIdent : SurfaceAstConsSig.IDENT with type ident = string

val encode_tuple : 'a list -> (string * 'a) list

val with_builtin_position :                  (unit -> 'a) -> 'a
val with_position :          FilePos.pos ->   (unit -> 'a) -> 'a
val with_label :             QmlLoc.annot -> (unit -> 'a) -> 'a
val with_same_pos :      _ * QmlLoc.annot -> (unit -> 'a) -> 'a

val with_builtin_position' :                 ('a -> 'b) -> 'a -> 'b
val with_position' :         FilePos.pos ->   ('a -> 'b) -> 'a -> 'b
val with_label' :            QmlLoc.annot -> ('a -> 'b) -> 'a -> 'b
val with_same_pos' :     _ * QmlLoc.annot -> ('a -> 'b) -> 'a -> 'b

module MakeCons : functor (Ident : SurfaceAstConsSig.IDENT) -> SurfaceAstConsSig.CONS with type ident = Ident.ident
(* these two modules are the main ones, see SurfaceAstConsSig for their signature *)
module ExprIdentCons : SurfaceAstConsSig.CONS with type ident = ExprIdent.ident
module StringCons : SurfaceAstConsSig.CONS with type ident = StringIdent.ident

module Fold :
  sig
    val dot : ('a, 'b) expr -> (string * QmlLoc.annot) list -> ('a, 'b) expr
  end

(** Utils to duplicate a tree while regenerating all the annotations *)
module Refresh :
sig
  (** expressions *)
  val const_expr_node : const_expr_node -> const_expr_node
  val const_expr : const_expr -> const_expr
  val record_node : ('a, 'b) record_node -> ('a, 'b) record_node
  val record : ('a, 'b) record -> ('a,'b) record
  val expr : ('a, 'b) expr -> ('a,'b) expr
  val expr_node : ('a, 'b) expr_node -> ('a,'b) expr_node

  (** patterns *)
  val pat : 'a pat -> 'a pat
  val pat_node : 'a pat_node -> 'a pat_node
  val pat_record_node : 'a pat_record_node -> 'a pat_record_node

  (** types *)
  val ty : 'a ty -> 'a ty
  val ty_node : 'a ty_node -> 'a ty_node
  val typeinstance : 'a typeinstance_t -> 'a typeinstance_t
  val typeinstance_node : 'a typeinstance_t_node -> 'a typeinstance_t_node
  val arrow : 'a arrow_t -> 'a arrow_t
  val arrow_node : 'a arrow_t_node -> 'a arrow_t_node
  val sum : 'a sum_t -> 'a sum_t
  val sum_node : 'a sum_t_node -> 'a sum_t_node
  val fields_t_node : 'a fields_t_node -> 'a fields_t_node
  val row : 'a row_t -> 'a row_t
  val row_node : 'a row_t_node -> 'a row_t_node
  val typedef : 'a typedef -> 'a typedef
  val typedef_node : 'a typedef_node -> 'a typedef_node
end
