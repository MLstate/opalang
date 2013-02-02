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

(* @author Rudy Sicard *)

(** warning classes of this module, (to be inserted by users) *)
val warning_set : WarningClass.Set.t

(** list of valid package to import if required *)
val may_import_package : ?package:string -> exists:(string->bool) -> options:OpaEnv.opa_options -> string list

(** eliminates `i18n and `string directive
    while potentially generating translation source and introducing @i18_lang directive *)
val process_directives__i18n__string : options:OpaEnv.opa_options -> ((Ident.t, (SurfaceAst.dependency_directive)) SurfaceAstPassesTypes.env_both_lcodes as 'env)-> 'env
