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
   Javascript Constructor Helpers
   @author Mathieu Barbin
*)

module Expr :
sig
  val array : ?label:Annot.label -> JsAst.expr list -> JsAst.expr

  val assign : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val assign_ident : ?label:Annot.label -> JsAst.ident -> JsAst.expr -> JsAst.expr
  val binop : ?label:Annot.label -> JsAst.binop -> JsAst.expr -> JsAst.expr -> JsAst.expr

  val true_ : ?label:Annot.label -> unit -> JsAst.expr
  val false_ : ?label:Annot.label -> unit -> JsAst.expr

  val bool : ?label:Annot.label -> bool -> JsAst.expr
  val call : ?label:Annot.label -> ?pure:bool -> JsAst.expr -> JsAst.expr list -> JsAst.expr
  val cond : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr -> JsAst.expr

  (**
     Do not build a comma when the prefix list is empty,
     simply returns the expr.
  *)
  val comma : ?label:Annot.label -> JsAst.expr list -> JsAst.expr -> JsAst.expr

  (**
     [dot expr field] gives the expression [expr.field]. The [own_property]
     flag indicates whether we want to access the object's own property or
     if we accept inherited fields (default: [true]).
  *)
  val dot : ?label:Annot.label -> ?own_property:bool -> JsAst.expr -> string -> JsAst.expr
  val equality : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val exprident : ?label:Annot.label -> Ident.t -> JsAst.expr
  val ident : ?label:Annot.label -> JsIdent.t -> JsAst.expr

  (**
     Build a local native ident.
     Global native ident should rather not be created directly, but produced with an analysis, except for identifer known statically to be global.
  *)
  val native : ?label:Annot.label -> string -> JsAst.expr
  val native_global : ?pure:bool -> ?label:Annot.label -> string -> JsAst.expr



  (**
     @deprecated use [dot] instead
  *)
  val field : ?label:Annot.label -> JsAst.expr -> string -> JsAst.expr
  val float : ?label:Annot.label -> float -> JsAst.expr
  val function_: ?label:Annot.label -> JsAst.ident option -> JsAst.ident list -> JsAst.statement list -> JsAst.expr
  val greater : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val hashref : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val hole : ?label:Annot.label -> QmlAst.expr -> JsAst.expr
  val ident : ?label:Annot.label -> JsAst.ident -> JsAst.expr
  val in_ : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val int : ?label:Annot.label -> int -> JsAst.expr
  val bint : ?label:Annot.label -> Big_int.big_int -> JsAst.expr
  val int_as_string : ?label:Annot.label -> string -> JsAst.expr
  val land_ : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val list : ?label:Annot.label -> JsAst.expr list -> JsAst.expr
  val lor_ : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val neq : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val not_ : ?label:Annot.label -> JsAst.expr -> JsAst.expr
  val null : ?label:Annot.label -> unit -> JsAst.expr
  val obj : ?label:Annot.label -> (string * JsAst.expr) list -> JsAst.expr
  val runtime : ?label:Annot.label -> JsAstRuntime.expr -> JsAst.expr
  val strict_equality : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val strict_neq : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.expr
  val string : ?label:Annot.label -> string -> JsAst.expr
  val this : ?label:Annot.label -> unit -> JsAst.expr
  val unop : ?label:Annot.label -> JsAst.unop -> JsAst.expr -> JsAst.expr
  val undefined : ?label:Annot.label -> unit -> JsAst.expr

  (* builds (function(){ var [idents]; return [expr]})() *)
  val scope : JsIdent.t list -> JsAst.expr -> JsAst.expr
  (* as scope, but just return the expression when the list of local vars is empty *)
  val maybe_scope : JsIdent.t list -> JsAst.expr -> JsAst.expr

  (** {6 Deprecated API} *)

  (**
     Used in the transition between jslang/jslang2.
  *)

  (**
     After the transition, remove this function, and use [comma] combine with [affect] instead.
     It is preferable to lift affectation to the higher level of the current scope, rather than
     having imbrication of affectation. (lisibility of code for debuging)
  *)
  val deprecated_letin : ?label:Annot.label -> (JsAst.ident * JsAst.expr) list -> JsAst.expr -> JsAst.expr

  (**
     After the transition, remove this function, and use [function_] instead
  *)
  val deprecated_lambda :
    ?label:Annot.label ->
    JsAst.ident list ->
    JsAst.ident list ->
    JsAst.expr ->
    JsAst.expr
end

module Statement :
sig
  val def : ?label:Annot.label -> JsAst.ident -> JsAst.statement

  val assign : ?label:Annot.label -> JsAst.expr -> JsAst.expr -> JsAst.statement
  val assign_ident : ?label:Annot.label -> JsAst.ident -> JsAst.expr -> JsAst.statement
  val block : ?label:Annot.label -> JsAst.statement list -> JsAst.statement
  val continue : ?label:Annot.label -> ?label:JsAst.label -> unit -> JsAst.statement
  val comment : ?label:Annot.label -> string -> JsAst.statement
  val expr : ?label:Annot.label -> JsAst.expr -> JsAst.statement
  val function_ : ?label:Annot.label -> JsAst.ident -> JsAst.ident list -> JsAst.statement list -> JsAst.statement
  val if_ : ?label:Annot.label -> JsAst.expr -> JsAst.statement -> JsAst.statement -> JsAst.statement
  val if_no_else : ?label:Annot.label -> JsAst.expr -> JsAst.statement -> JsAst.statement
  val return : ?label:Annot.label -> JsAst.expr -> JsAst.statement
  val switch : ?label:Annot.label -> ?default:JsAst.statement -> JsAst.expr -> (JsAst.expr * JsAst.statement) list -> JsAst.statement
  val var : ?label:Annot.label -> ?expr:JsAst.expr -> JsAst.ident -> JsAst.statement
  val while_ : ?label:Annot.label -> JsAst.expr -> JsAst.statement -> JsAst.statement

  (** {6 Deprecated API} *)

  (**
     Used in the transition between jslang/jslang2.
  *)

  (**
     After the transition, remove this function, and use [function_] instead
  *)
  val deprecated_function :
    ?label:Annot.label ->
    JsAst.ident -> JsAst.ident list ->
    JsAst.ident list ->
    JsAst.expr ->
    JsAst.statement
end

module Ident :
sig
  val ident : Ident.t -> JsAst.ident


  val fresh : JsAst.ident -> JsAst.ident
  val fresh_qml : Ident.t -> JsAst.ident

  (**
     Build a local native ident.
     Global native ident should rather not be created directly, but produced with an analysis.
  *)
  val native : string -> JsAst.ident
  val native_global : ?pure:bool -> string -> JsAst.ident



end
