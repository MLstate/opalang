(*
    Copyright © 2011-2013 MLstate

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


(** This module contains facilities to do some walk on the ast, like maping, folding, testing etc... *)

(** For constuctors and deconstructors, the functions should preferably be put in qmlAstCons.ml *)

(** Some work has been done to factor the code.
    Se also traverse.ml in libqml.
*)

module Row :
sig

  val fold_left : ('a -> string -> QmlAst.ty -> 'a) -> 'a -> QmlAst.ty_row -> 'a
  val fold_left_map : ('a -> QmlAst.ty -> 'a * QmlAst.ty) -> 'a -> QmlAst.ty_row -> 'a * QmlAst.ty_row
  val fold_right : (string -> QmlAst.ty -> 'a -> 'a) -> QmlAst.ty_row -> 'a -> 'a

  val elements : QmlAst.ty_row -> ( string * QmlAst.ty ) list
  val ordered_elements : QmlAst.ty_row -> ( string * QmlAst.ty ) list

  (** like a fold_left, but the fields are sorted by increasing alphabetic order before the fold *)
  val ordered_fold : ('a -> string -> QmlAst.ty -> 'a) -> 'a -> QmlAst.ty_row -> 'a

  (** a more complete pos_of_field, with a double indexation *)
  val pos_of_field : QmlAst.ty_row -> (string * QmlAst.ty) array * (int * QmlAst.ty) StringMap.t

  (** field occur checking *)
  val has_field : string -> QmlAst.ty_row -> bool
  val get_field : string -> QmlAst.ty_row -> QmlAst.ty option

  val length : QmlAst.ty_row -> int

end

module Col :
sig
  (** Folds on the types that are below the edges of ty_col (two levels)
      {e i.e.}, on the column [c] such that [TypeSum c = t],
      if [t = { a: ta; b: tb } / { c: tc }], will fold on [ta, tb, tc] *)
  val fold_left_map : ('a -> QmlAst.ty -> 'a * QmlAst.ty) -> 'a -> QmlAst.ty_col -> 'a * QmlAst.ty_col

  (** Folds on the rows contained in the column, converted to record types
      {e i.e.}, on the column [c] such that [TypeSum c = t],
      if [t = { a: ta; b: tb } / { c: tc }], will fold on [ { a: ta; b: tb }, { c: tc } ]*)
  val fold_records : ('a -> QmlAst.ty -> 'a) -> 'a -> QmlAst.ty_col -> 'a
end

module Ty_sums :
sig
  val elements : QmlAst.ty list -> QmlAst.ty list
  val fold : (QmlAst.ty -> 'a -> 'a) -> QmlAst.ty list -> 'a -> 'a
end

module Type : TraverseInterface.TRAVERSE
  with
    type 'a t = QmlAst.ty constraint 'a = _ * _ * _
  and type 'a container = QmlAst.ty constraint 'a = _ * _ * _

module Top :
sig
  (* maps only at top-level, not recursively *)
  val iter_expr : (QmlAst.expr -> unit) -> QmlAst.code_elt -> unit
  val map_expr : (QmlAst.expr -> QmlAst.expr) -> QmlAst.code_elt -> QmlAst.code_elt
  val fold_expr : ('a -> QmlAst.expr -> 'a) -> 'a -> QmlAst.code_elt -> 'a
  val fold_map_expr : ('a -> QmlAst.expr -> 'a * QmlAst.expr) -> 'a -> QmlAst.code_elt -> 'a * QmlAst.code_elt
  (* same but the ident of expr is also used *)
  val fold_name_expr : ('a -> ( QmlAst.ident * QmlAst.expr) -> 'a ) -> 'a -> QmlAst.code_elt -> 'a
  val fold_names : ('a -> QmlAst.ident -> 'a) -> 'a -> QmlAst.code_elt -> 'a
  val map_name_expr : (QmlAst.ident * QmlAst.expr -> QmlAst.ident * QmlAst.expr) -> QmlAst.code_elt -> QmlAst.code_elt
  val fold_map_name_expr : ('a -> (QmlAst.ident * QmlAst.expr) -> 'a * (QmlAst.ident * QmlAst.expr)) -> 'a -> QmlAst.code_elt -> 'a * QmlAst.code_elt
  val iter_name_expr : (QmlAst.ident * QmlAst.expr -> unit) -> QmlAst.code_elt -> unit
end

module CodeExpr :
sig
  (* maps only at top-level, not recursively *)
  val iter : (QmlAst.expr -> unit) -> QmlAst.code -> unit
  val map : (QmlAst.expr -> QmlAst.expr) -> QmlAst.code -> QmlAst.code
  val fold : ('a -> QmlAst.expr -> 'a) -> 'a -> QmlAst.code -> 'a
  val fold_name_expr : ('a -> (QmlAst.ident *QmlAst.expr) -> 'a) -> 'a -> QmlAst.code -> 'a
  val fold_map_name_expr : ('a -> (QmlAst.ident * QmlAst.expr) -> 'a * (QmlAst.ident * QmlAst.expr)) -> 'a -> QmlAst.code -> 'a * QmlAst.code
  val map_name_expr : (QmlAst.ident * QmlAst.expr -> QmlAst.ident * QmlAst.expr) -> QmlAst.code -> QmlAst.code
  val fold_map : ('a -> QmlAst.expr -> 'a * QmlAst.expr) -> 'a -> QmlAst.code -> 'a * QmlAst.code
  val fold_names : ('a -> QmlAst.ident -> 'a) -> 'a -> QmlAst.code -> 'a

  val iter_with_code_elt : (QmlAst.code_elt -> QmlAst.expr -> unit) -> QmlAst.code -> unit
  val exists : (QmlAst.expr -> bool) -> QmlAst.code -> bool
end

module Code :
sig
  val filter_binding : (QmlAst.ident * QmlAst.expr -> bool) -> QmlAst.code -> QmlAst.code
  val iter_binding : (QmlAst.ident * QmlAst.expr -> unit) -> QmlAst.code -> unit
end

module Pattern :
sig
  include TraverseInterface.TRAVERSE
    with type 'a t = QmlAst.pat constraint 'a = _ * _ * _
    and type 'a container = QmlAst.pat constraint 'a = _ * _ * _

  val get_fields : QmlAst.pat -> ((string * QmlAst.pat) list * bool) option (** extend : true *)
end

module Expr : sig
  include TraverseInterface.TRAVERSE with type 'a t = QmlAst.expr constraint 'a = _ * _ * _
                                      and type 'a container = QmlAst.expr constraint 'a = _ * _ * _

  (**  fold with value environment,
       first arg is used to update the environment:
       -declared values are added with their expression in an option
       -lambda and pattern var are added with None
       -for lambda, the fold is applied in a currified way
  *)
  val fold_with_env : ('env -> QmlAst.ident -> QmlAst.expr option -> 'env) -> 'env -> ('env -> 'a -> QmlAst.expr -> 'a) -> 'a -> QmlAst.expr ->  'a
  (** special case with a default environmemt *)
  val fold_with_exprmap : ?env:(QmlAst.expr option) IdentMap.t -> (QmlAst.expr option IdentMap.t -> 'a -> QmlAst.expr -> 'a) -> 'a -> QmlAst.expr -> 'a
end

module ExprPatt : sig
  val iter_down  : (QmlAst.expr -> unit) -> (QmlAst.pat -> unit) -> QmlAst.expr -> unit
  val foldmap_down : ('a -> QmlAst.expr -> 'a * QmlAst.expr) -> ('a -> QmlAst.pat -> 'a * QmlAst.pat) -> 'a -> QmlAst.expr -> 'a * QmlAst.expr
  val map_down : (QmlAst.expr -> QmlAst.expr) -> (QmlAst.pat -> QmlAst.pat) -> QmlAst.expr -> QmlAst.expr
  val fold_down : ('a -> QmlAst.expr -> 'a) -> ('a -> QmlAst.pat -> 'a) -> 'a -> QmlAst.expr -> 'a
  val iter : (QmlAst.expr -> unit) -> (QmlAst.pat -> unit) -> QmlAst.expr -> unit
  val foldmap : ('a -> QmlAst.expr -> 'a * QmlAst.expr) -> ('a -> QmlAst.pat -> 'a * QmlAst.pat) -> 'a -> QmlAst.expr -> 'a * QmlAst.expr
  val map : (QmlAst.expr -> QmlAst.expr) -> (QmlAst.pat -> QmlAst.pat) -> QmlAst.expr -> QmlAst.expr
  val fold : ('a -> QmlAst.expr -> 'a) -> ('a -> QmlAst.pat -> 'a) -> 'a -> QmlAst.expr -> 'a
end

(** a test of occurrence of anything depending on db in an expr *)
module UseDb :
sig
  val expr : QmlAst.expr -> bool
  val code_elt : ?ignore_declaration:bool -> QmlAst.code_elt -> bool
  val code : ?ignore_declaration:bool -> QmlAst.code -> bool
end

module Misc :
sig
  (** removes all nested Coerce at the top of the expression *)
  val remove_coerce : QmlAst.expr -> QmlAst.expr
  (** shows the number of nodes in the expressions of a code *)
  val code_size : QmlAst.code -> int
end

module DbWalk : sig
  module Query :
    TraverseInterface.TRAVERSE
    with type 'a t = ('b, 'c) QmlAst.Db.query constraint 'a = 'b * 'c * _
    and  type 'a container = ('b, 'c) QmlAst.Db.query constraint 'a = 'b * 'c * _
end
