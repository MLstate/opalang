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
   Performs a few simplifications on the ast
   Right now, removes dummy bindings [x = x]
   and the part of sequences that don't do side effects
   [(x = 1, y = y, y)] becomes [(x = 1, y)]
   does some constant folding etc.

   @param use_shortcut_assignment when set, shortens a = a + 1 in a++ for instance
                                  (local inlining does not deal with +=, ++, etc.)
*)

val clean_stm : use_shortcut_assignment:bool -> JsAst.statement -> JsAst.code
val clean : use_shortcut_assignment:bool -> JsAst.code -> JsAst.code
