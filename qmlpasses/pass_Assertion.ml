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
module QC = QmlAstCons

let fold_map_expr_nonrec ~no_assert gamma annotmap = function
  | Q.Directive (label, `assert_, [cond], _) ->
      let pos = Annot.pos label in
      let annotmap, void = QC.TypedExpr.cheap_void ~pos annotmap gamma in
      if no_assert
      then
        annotmap, void
      else
        let annotmap, message = QC.TypedExpr.const ~pos annotmap
          (Q.String "assert failure") in
        let annotmap, fail =
          QC.TypedExpr.directive ~pos annotmap `fail [ message ] [ ]
        in
        QC.TypedPat.ifthenelse ~pos annotmap gamma cond void fail

  | e ->
      annotmap, e

let process_code ~no_assert gamma annotmap code =
  let fold_map = QmlAstWalk.Expr.foldmap_down (fold_map_expr_nonrec ~no_assert gamma) in
  QmlAstWalk.CodeExpr.fold_map fold_map annotmap code
