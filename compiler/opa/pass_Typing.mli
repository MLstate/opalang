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

module S : ObjectFiles.S with type t = QmlTypes.typescheme IdentMap.t
(**
   Types the whole code, assuming a correct gamma in the environment
*)

val process_code : ?save:bool -> 'tmp_env Passes.env_Gen -> 'tmp_env Passes.env_Gen
