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
   Alpha Conversion representation and computation on QmlAst.
   @author Mathieu Barbin
*)


(** Uses an env to unify every pass of ast with a possible fold_map.
    The renaming is based on [ExprIdent.next] *)

(** {6 Error reporting} *)

(**
   The module uses QmlError for errors reporting.
   Depending on the function you use, the module use :
   + QmlError.error for public errors
   + QmlError.check_fail for checkers
*)

(** {6 Alpha Conversion representation and options} *)

(** The abstract type for an alpha conv *)
type t

(**
   A standard initial value for an alpha conv
    - refresh only ident name
    - DOES NOT REFRESH ANNOT
*)
val empty : t

(**
   Initializing a value of type [t] for a new alpha conversion.
   The option [weak] means that some ident are allowed not to be bound
   then, they are reproduced as is (no renamed, be carrefull).
   By default, original names are keeped, for a better tracability of the code.
   (only the internal int of the [ExprIdent.Internal] is refreshed)
*)
val next : ?weak:(Ident.t -> bool) -> unit -> t

(**
   Construction from two existing map.
   [map:orig  -> final] and [revmap:final -> orig].
   Not for casual users.
*)
val create_from_maps : map:Ident.t IdentMap.t -> revmap:Ident.t IdentMap.t -> t

(** Changing dynamically some of env properties *)

val env_weak : weak:(Ident.t -> bool) -> t -> t

(** print the association of the original ident and the new corresponding fresh one (for debug only) *)
val to_string : t -> string

(** {6 Computation} *)

(** pat can introduce var *)
val pat : t -> QmlAst.pat -> t * QmlAst.pat

(** an expr does not introduce new bindings : simple map *)
val expr : t -> QmlAst.expr -> QmlAst.expr

(** The primary API on top value : they are all fold_map *)
val code_elt : t -> QmlAst.code_elt -> t * QmlAst.code_elt
val code : t -> QmlAst.code -> t * QmlAst.code

(** Maping an ident with the current bingings *)
val ident : t -> QmlAst.ident -> QmlAst.ident option
(** Finding back the origin name *)
val rev_ident : t -> QmlAst.ident -> QmlAst.ident option

(** Higher level API *)
val next_code : ?weak:(Ident.t -> bool) -> QmlAst.code -> QmlAst.code

(** clean t to keep the minimal information *)
val clean : t -> t
(*  this function update the qmlAlphaConv acc to be able to make external first level renaming without losing the succession org -> alpha -> alpha -> ... -> last alpha
    An alpha convversion must be launched after this
*)
val update : t -> Ident.t IdentMap.t -> t

(**
   Checks related to alpha conversion
*)
module Check :
sig
  type ('env, 'a) checker = ('env -> 'a)  -> 'env PassHandler.cond

  (**
     The id of all cond related to ident
  *)
  val id : PassHandler.cond_id

  (** {6 Alpha-conversion} *)
  (**
     Checks that alpha-conversion preconditions are satisfied.
     The conditions are, e.g., no unbound identifiers and no repeated
     identifiers in letrecs and patterns.
     - Condition name : ["cond.ident.alpha"]
     - Warning class : [cond_ident_alpha]
  *)
  val alpha : ('env, QmlAst.annotmap * QmlAst.code) checker
  val alpha_id : PassHandler.cond_id

  (** {6 Unbound}*)
  (**
      Checks that every identifier is in scope when it use
      - Condition name : ["cond.ident.unbound"]
      - Warning class : [cond_ident_unbound]
     For create the checker you must give a function for retrieve an
     ident from a string.

  *)
  val unbound : ?weak:(Ident.t -> bool) -> (string -> QmlAst.ident) -> ('env, QmlAst.annotmap * QmlAst.code) checker
  val unbound_id : PassHandler.cond_id

  (** {6 Unicity} *)
  (**
     Checks that every identifier is unique
     - Condition name : ["cond.ident.unicity"]
     - Warning class : [cond_ident_unicity]
  *)
  val unicity : ('env,  QmlAst.annotmap * QmlAst.code) checker
  val unicity_id : PassHandler.cond_id

end

(**
   Hackish module, should be removed after the refactoring of positions in the AST.
*)
module HacksForPositions :
sig
  (**
     Special Hackish annotmap, used for finding positions for error messages.
     Used for having an annotmap when the module fails, without changing every
     interfaces by adding an annotmap in argument.
  *)
  val set_annotmap : QmlAst.annotmap -> unit
  val free_annotmap : unit -> unit
end
