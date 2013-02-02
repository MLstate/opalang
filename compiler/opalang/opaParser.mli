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
   High-level interface to the opa parser.
*)

(**
   This module defines a higher-level interface for calling the trx opa parser.
   It defines as well utils using cache, and reports errors using located messages.
*)

(** {6 Types Alias} *)

(**
   Just for clarty, to avoid confusion between filenames, contents, etc...
*)
type filename = string
type contents = string

type nonuid = SurfaceAst.nonuid

(** {6 Meta-informations} *)

(**
   Hash of all trx, ml and mli use to generate the parser
   Currently : general/surfaceAst.ml and passes/syntax/*.{trx,ml}.
   Used in particular to know if the cache are relevant from a compilation
   of the compiler to an other.
*)
val hash : string

(** {6 Low-Level Parsing} *)

(**
   These function are just a call to the trx parser.
   They may raise [Trx_runtime.SyntaxError].
   [ll_] stands for [low_level].

   The given filename is optional, and given to trx, used for errors messages only.
*)

val ll_expr :  ?filename:filename -> contents -> (nonuid, SurfaceAst.parsing_directive) SurfaceAst.expr
val ll_ty :  ?filename:filename -> contents -> nonuid SurfaceAst.ty
val ll_code : ?filename:filename -> contents -> (nonuid, SurfaceAst.parsing_directive) SurfaceAst.code

(** {6 High-level Parsing} *)

(**
   These function catch the trx runtime error and print a citation of the source,
   before exiting using the function [error] (TODO: use OManager).
*)

val expr : ?filename:filename -> contents -> (nonuid, SurfaceAst.parsing_directive) SurfaceAst.expr
val ty :  ?filename:filename -> contents -> nonuid SurfaceAst.ty

(**
   Used with [cache:true], the function will set/get a cache of the parsed AST from
   [{mlstate_dir}/opa/cache/parser].
   (Default is [cache:false])
*)
val code : ?cache:bool -> ?filename:filename -> ?sugar:bool -> contents -> (nonuid, SurfaceAst.parsing_directive) SurfaceAst.code

(** {6 Deprecated API} *)

(**
   The rest of the API was done before the introduction of guidelines.
   Please do not use in new code.
*)

exception Specific_parse_error of (FilePos.pos * string)

exception No_such_file of string

(**
   [parse_file filename]
   Read the file corresponding to the given filename and parse it
   @param filename The name of the file to be parsed
   @raise Trx_runtime.SyntaxError see above
   @raise Trx_runtime.ExceptionDuringParsing see above
   @raise No_such_file The given filename cannot be read
*)
val parse_file : filename -> (string,SurfaceAst.parsing_directive) SurfaceAst.code
