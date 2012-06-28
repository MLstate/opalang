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
module Q = QmlAst

let iter_deep f code =
  QmlAstWalk.CodeExpr.iter
    (fun expr ->
       f expr;
       QmlAstWalk.Expr.iter f expr)
    code

let perform register code =
  iter_deep
    (fun expr ->
       match expr with
       | Q.Record (_, lst) -> List.iter (fun (name, _) -> register name) lst

       | Q.Dot (_, _, name)
       | Q.ExtendRecord (_, name, _, _) -> register name

       | Q.Match (_, _, lst) ->
           List.iter
             (fun (pat, _) ->
                QmlAstWalk.Pattern.iter_down
                  (function
                     | Q.PatRecord (_, fields, _) -> List.iter (fun (name, _) -> register name) fields
                     | _ -> ()
                  ) pat
             ) lst
       | _ -> ()
    ) code
