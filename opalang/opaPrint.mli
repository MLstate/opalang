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
   A family of pretty-printers for the OPA AST.
*)
type 'ident printer =
  <
    code : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.code LangPrint.pprinter;
  code_elt : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.code_elt LangPrint.pprinter;
  code_elt_node : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.code_elt_node LangPrint.pprinter;
  expr : 'dir. ('ident, [< SurfaceAst.all_directives ] as 'dir) SurfaceAst.expr LangPrint.pprinter;
  ty : 'ident SurfaceAst.ty LangPrint.pprinter;
  directive : 'dir. ('ident,[< SurfaceAst.all_directives ] as 'dir) SurfaceAst.directive LangPrint.pprinter;
  variant : 'dir. ([< SurfaceAst.all_directives ] as 'dir) LangPrint.pprinter;
  typevar : 'ident SurfaceAst.typevar LangPrint.pprinter;
  typeident : 'ident SurfaceAst.typeident LangPrint.pprinter;
  ident : 'ident LangPrint.pprinter;
  >

module type Familly = sig
  val string : string printer
  val string_and_pos : string printer
  val ident : Ident.t printer
  val readable_ident : Ident.t printer
  val full_ident : Ident.t printer
end

val makeFamilly : OpaSyntax.t -> (module Familly)

val getDefaultFamilly : unit -> (module Familly)


val string : string printer
val string_and_pos : string printer
val ident : Ident.t printer
val readable_ident : Ident.t printer
val full_ident : Ident.t printer
