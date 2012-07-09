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
   Documentation ?
   @author Rudy Sicard
*)

val warning_set : WarningClass.Set.t

(** expand all macro in code *)
val process_code : QmlAst.code -> QmlAst.code

(** the pass version of the previous function *)
val process : options:OpaEnv.opa_options -> 'tmp_env Passes.env_Gen -> 'tmp_env Passes.env_Gen
