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
(* CF mli *)

(* refactoring in progress *)

module Q = QmlAst

let expr gamma annotmap expr =
  match expr with
  | Q.Directive (_, `warncoerce, [e], [ty]) -> (
      match QmlAnnotMap.find_ty_opt (Q.QAnnot.expr e) annotmap with
      | None ->
          let context = QmlError.Context.annoted_expr annotmap expr in
          QmlError.i_error None context
            "This expression has no annotation of type"
      | Some annotty ->
          let () =
            let (ty, _) = QmlTypes.type_of_type gamma ty in
            if not (QmlMoreTypes.equal_ty ~gamma ty annotty) then
              let context = QmlError.Context.annoted_expr annotmap expr in
              QmlError.warning ~wclass:QmlTyperWarnings.warncoerce context (
                "This expression should have type @{<bright>%a@}@\n"^^
                "but has there type @{<bright>%a@}"
              )
                QmlPrint.pp#ty ty
                QmlPrint.pp#ty annotty
          in
          ()
    )
  | _ -> ()

let process_code gamma annotmap code =
  if WarningClass.is_warn QmlTyperWarnings.warncoerce then
    QmlAstWalk.CodeExpr.iter (QmlAstWalk.Expr.iter (expr gamma annotmap)) code
