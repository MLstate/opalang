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
(** Replace all directive [js_ident]. These directive are typically use to
    generates adhoc (i.e. backend specific) JavaScript code but using Opa
    identifier.

    @author Quentin Bourgerie
*)

(**
   Replace all directive like [\@js_ident("val")] by a reference to
   a hoisted identifier which is a valid js identifier ["compiled_ident_of_val_in_js"]

   The pass add also the toplevel declaration calling the [JsIdent_rename] function.

   Example:
   Input code
   {[
   t(x) = "{@js_ident("toto")}(x, y)"
   ]}

   Output code
   {[
   _v0_toto = %%bslJsIdent_rename%%("toto")
   ...
   t(x) = "{_v0_toto}(x, y)"
   ]}
*)
val perform : QmlAst.annotmap -> QmlAst.code -> QmlRenamingMap.t -> QmlAst.annotmap * QmlAst.code
