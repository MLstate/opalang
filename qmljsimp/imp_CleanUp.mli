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
   Performs a few simplifications on the ast
   Right now, only removes dummy bindings [x = x]
   and the part of sequences that don't do side effects
   [(x = 1, y = y, y)] becomes [(x = 1, y)]
*)

val clean_stm : use_shortcut_assignment:bool -> JsAst.statement -> JsAst.code
val clean : use_shortcut_assignment:bool -> JsAst.code -> JsAst.code
