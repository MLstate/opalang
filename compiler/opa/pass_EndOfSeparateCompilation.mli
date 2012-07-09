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
   The last pass of the separate compilation
   In compilation mode, it saves the current environment.
   In linking mode, it loads the previous environments and continues the compilation
   in a not separated way
*)

(**
   The actual pass
*)
val process_code : 'tmp_env Passes.env_Gen -> ('tmp_env Passes.env_Gen -> unit) -> unit
