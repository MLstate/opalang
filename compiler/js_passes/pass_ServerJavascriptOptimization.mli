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

val process_code_elt : (JsIdent.t -> bool) -> JsAst.code_elt -> JsAst.code_elt

val process_code :
 client_deps:StringSet.t ->
 Qml2jsOptions.extra_lib list -> BslLib.env_bsl
  -> (JsIdent.t -> bool) -> JsAst.code -> (string list * JsAst.code)
