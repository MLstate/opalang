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
   Some printers for Ocaml, a Fast and a Pretty.

   @author Adrien Jonquet
   @author Mathieu Barbin
*)

(**
   This module offers a few implementations for printing some ocaml code.

   <!> Important note : please README

   Since the AST of ocaml is a mess, and should be rewrited,
   all cases of the current are not implemented in any printer.
   Only cases used by back-ends are supported. The rest of cases
   should be implemented only once we will have the new AST.
*)

(** {6 Benches} *)

(**
   Valentin did some benches and get to the fact that :

   + if [t] is the time used to print a code without only output_string
   + [2*t] would be the time using [Printf] like functions
   + [4*t] would be the time using [Format] for a pretty printer, using boxes and indentation directives
   + [unknown] with FBuffer
   + [infinite] using string concatenation

   More benches are welcome.
*)

(**
   Fonctorization, for instance using :
   + Buffer
   + FBuffer
   + Formats without formatting
   + out_channel
*)
module type Printer =
sig
  type t
  type 'a printer = t -> 'a -> unit
  (* open OcamlAst *) open Ocaml
  val const_expr : const_expr printer
  val const_type_expr : const_type_expr printer
  val type_name : type_name printer
  val type_expr : type_expr printer
  val pattern : pattern printer
  val param_formel : param_formel printer
  val param_effectif : param_effectif printer
  val mlIdent : mlIdent printer
  val code : code printer
  val expr : expr printer
end

(** {6 Ident printing} *)

val ident : Ident.t -> string

(** {6 Efficient printing} *)

(**
   This printer is used whenever you do not care about the look of the printed code.
   It's main quality reside in the fact that it prints directly in an [output_channel],
   without neither with parsing some format.
*)

module Output : Printer with type t = out_channel

(** {6 Unformated printing, with Buffer interface} *)

module Buf : Printer with type t = Buffer.t

(** {6 Unformated printing, with Format interface} *)

(**
   If you need a format interface for outputting code,
   but you do not care of layout. (no formatting)
*)
module Fmt : Printer with type t = Format.formatter

module FBuf : Printer with type t = FBuffer.t ref

(**
   Extra module, backward compatibility only (netweb)
*)
module Deprecated :
sig
  type 'a printer = FBuffer.t -> 'a -> FBuffer.t
  val const_expr : Ocaml.const_expr printer
  val type_expr : Ocaml.type_expr printer
end

(** {9 Utils} *)

type 'a pprinter = 'a LangPrint.pprinter

(**
   This is just like LangPrint.pp_parameters, but with an Ocaml syntax.
*)
val pp_parameters :
  'a pprinter ->
  string ->
  'a list pprinter

(** {9 Pretty printer} *)

(**
   Only if strictly needed !
   This printer is implemented using Format and object inheritance.
*)
