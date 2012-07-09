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
(** Insert CSS registering *)

(** Insert the registration of a CSS declaration at the begining of
    the code. At runtime for each [css] registered (Support separate
    compilation) [css] is converted on string. And added to the global
    css file. If the [css] is not present on user code, this function
    do nothing.

    Add to code :
    {[
      css_registering = register_css_declaration(css)
    ]}
*)
val perform : QmlAst.code -> QmlAst.code
