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
let process_code code =
  QmlAstWalk.CodeExpr.map
    (QmlAstWalk.Expr.map_up (* map_up because map_down would forget some directives *)
       (function
        | QmlAst.Directive (_, (`lifted_lambda _ | `full_apply _), [e], _) -> e
        | e -> e
       )
    ) code
