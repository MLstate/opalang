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
(* CF mli *)

(* alias *)
module JsIdent = Qmljs_Serializer.JsIdent

(* shorthands *)
module Q = QmlAst

(* TODO - Merge it on JavaScriptCompilation *)
let resolve annotmap code renaming =
  QmlAstWalk.CodeExpr.fold_map
    (QmlAstWalk.Expr.foldmap
       (fun annotmap expr ->
          match expr with
          | Q.Directive (_, `js_ident, [Q.Const (_, Q.String name)], []) ->
              (try
                 let name =
                   try Hashtbl.find Opacapi.table name
                   with Not_found ->
                     QmlError.error (QmlError.Context.annoted_expr annotmap expr)
                       "Not registered in Opacapi"
                 in
                 let jsident =
                   let ident = OpaMapToIdent.val_ ~side:`client name in
                   let ident = QmlRenamingMap.original_from_new renaming ident in
                   JsIdent.resolve ident
                 in
                 QmlAstCons.TypedExpr.ident annotmap jsident QmlAstCons.TypedExpr.ty_string
               with Not_found ->
                 QmlError.error (QmlError.Context.annoted_expr annotmap expr)
                   "Doesn't exists on client side")

          | Q.Directive (_, `js_ident, _, _) ->
              QmlError.i_serror None (QmlError.Context.annoted_expr annotmap expr) "Malformed directive";
              (annotmap, expr)
          | _ -> (annotmap, expr)
       )
    )
    annotmap code

let perform annotmap code renaming =
  let annotmap, code = resolve annotmap code renaming in
  let decls = JsIdent.get_toplevel_declarations () in
  annotmap, (decls @ code)
