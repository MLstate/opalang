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
(* rebel open *)
open SurfaceAst (* importing code, expr, all the directives types *)
(*
open SurfaceAst
open SurfaceAstHelper
open OpaEnv
*)


type ('a,'b) env_both_lcodes = {
  lcodeNotUser : ('a,'b) code ;
  (** Source code added automatically *)

  lcodeUser : ('a,'b) code ;
  (** Source code added at the request of the user *)

  lcodeTypeRenaming : (Ident.t * FilePos.pos) StringMap.t ;
  (** the renaming of types for the current package *)

  exported_values_idents : IdentSet.t ;
  (** The set of values identifiers that are exported outside this package.
      It contains all toplevel values definitions that are not marked by a
      @private directive. *)
  env_bsl : BslLib.env_bsl ;
  (** plugins and bymap *)
}

(**
   The result of parsing a file.
*)
type 'a parsed_file = {
  parsedFile_filename : string ;
  (** The full name of the file*)

  parsedFile_lcode : (string,'a) code ;
  (** The surface Ast *)

  parsedFile_content : string ;
  (** The content of the file *)
}

(* alias *)
type options = OpaEnv.opa_options
