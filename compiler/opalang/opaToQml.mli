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
   Conversion from OpaAst to QmlAst.

   @author Valentin Gatien-Baron
   @author Mathieu Barbin
*)

(** {6 Opa to Qml} *)

(**
   Convesion from SurfaceAst to QmlAst.

   No hack allowed there, this is not a libconvert, but part of the official flow of the compiler.
   Ideally, this traduction should appear once at some point along the passes of opa.
*)

(** {6 Errors} *)

type error
exception Exception of error
val string_of_error : error -> string


(** {6 Options} *)

type options = unit

val options : options

(** {6 Traduction before renaming} *)

module NonuidOpaToQml :
sig
  val code :
    ?options:options ->
    (SurfaceAst.nonuid, SurfaceAst.all_directives) SurfaceAst.code ->
    Ident.t list * QmlAst.code
end

(** {6 Traduction after renaming } *)

module UidsOpaToQml :
sig
  val typeident_aux : ?check:bool -> Ident.t -> QmlAst.TypeIdent.t

  val typedef : Ident.t SurfaceAst.typedef_node -> QmlAst.typedef
  val code :
    ?options:options ->
    (SurfaceAst.uids, SurfaceAst.basic_directive) SurfaceAst.code ->
    Ident.t list * QmlAst.code
end

(** please do not provide QmlAst To OpaAst *)

(** {6 Mixity Parsing } *)
(** Parse a opa syntax file contents and return a qml ast *)
module Parser :
sig
  exception Exception of string
  val of_string : ?filename:string -> string -> QmlAst.code
end

val propagate_slicer_annotation : ('a, [< SurfaceAst.all_directives > `coerce `doctype `opacapi `side_annotation `visibility_annotation ] as 'b) SurfaceAst.expr -> (('a, 'b) SurfaceAst.expr -> ('a, 'b) SurfaceAst.expr)
