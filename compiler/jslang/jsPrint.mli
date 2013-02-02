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
   Printer for JsAst, and functor for serialization of js.
   @author Maxime Audoin (previous version)
   @author Mathieu Barbin, adapted for Opa from ocamljs/jslib
   @author Quentin Bourgerie (scoped)
*)

(**
   exported because used by:
   ??
*)
val string_of_ident : JsAst.ident -> string

val escape_string : ?double_quote:bool -> string -> string

(** {6 Pretty printer} *)

(**
   Standard pprinter type
*)
type 'a pprinter = Format.formatter -> 'a -> unit

(**
   For standard js only. Such opa construction, as 'hole' are not supported by this printer ( [assert false] )
*)
class printer :
object
  method pp_f : 'a. Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  method ident : JsAst.ident pprinter
  method unop : JsAst.unop pprinter
  method field : string pprinter
  method objpart : (string * JsAst.expr) pprinter
  method pexpr : leading:bool -> int -> JsAst.expr pprinter
  method block : JsAst.statement list pprinter
  method expr : leading:bool -> JsAst.expr pprinter
  method statements : JsAst.statement list pprinter
  method statement : JsAst.statement pprinter
  method code : JsAst.code pprinter
end

val pp : printer

val pp_keep_comments : printer

val pp_min : printer

(** same as pp, but prints the blocks instead of hiding them *)
val debug_pp : printer

val scoped_pp : printer

val scoped_pp_min : printer

(** {6 Stringfier} *)

(**
   TODO:
   who does need this ?
   maybe do not export this.
*)

(**
   code
*)
val code : JsAst.code -> string

(** {6 Partial Printer} *)

(**
   Js Runtime Serializer.

   This is used for serializing js with some hole.
   Hole correspond to different kind of lexems which evaluates as string representing
   js code as concrete syntax.
*)

module type X =
sig
  (**
     The type of one lexem
  *)
  type lexem
  type t
  val append : t -> lexem -> t
  val empty : t


  (** {6 Nodes} *)
  (**
     We can extend this interface, if we need more precise js ast at runtime.
     Invariant: in the returned t, there are no 2 successives lexem verbatim.
  *)

  val ident : string -> lexem
  val verbatim : string -> lexem
  val qml : QmlAst.expr -> lexem
  val serialized : JsAstRuntime.expr -> lexem list
end

module type S =
sig
  type t
  val code_elt : JsAst.code_elt -> t
end

module Make : functor (X : X) -> S with type t = X.t
