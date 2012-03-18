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
   Removes the type definitions from the code and puts them in the gamma
   after checking their validity.
   The first argument is a function for fields registering.
*)

val process_code : (string -> unit) -> QmlTyper.OfficialTyper.env -> QmlAst.code -> QmlAst.TypeIdentSet.t * QmlTyper.OfficialTyper.env * QmlAst.code * QmlTypes.gamma
