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
   Sets a reference in the bsl at compile time (at init)
   Its value is a random string used to identify the executable
   of the server
*)

val process_code : stdlib_gamma:QmlTypes.gamma -> QmlTypes.gamma -> QmlAst.annotmap -> QmlAst.code -> QmlTypes.gamma * QmlAst.annotmap * QmlAst.code
