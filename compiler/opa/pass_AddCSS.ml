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
module Q = QmlAst

let register_css_ident css_ident =
  try
    let css = QmlAstCons.UntypedExpr.ident css_ident in
    let reg_ident = OpaMapToIdent.val_ Opacapi.Client_code.register_css_declaration in
    let reg = QmlAstCons.UntypedExpr.ident reg_ident in
    let reg_apply = QmlAstCons.UntypedExpr.apply reg [css] in
    let reg_ident = Ident.next "css_registering" in
    let label = Annot.nolabel "Opa_AddCSS.perform" in
    QmlAst.NewVal (label, [(reg_ident, reg_apply)])
  with Not_found ->
    OManager.i_error
      "Value register_css_declaration is not found. Can't generate the css file..."

let perform code =
  let acc =
    List.fold_left (
      fun acc -> function
      | Q.NewVal (_a,iel)
      | Q.NewValRec (_a,iel) ->
          List.fold_left (fun acc (i,_) -> if Ident.original_name i = "css" then i :: acc else acc) acc iel
      | _ -> acc
    ) [] code in
  let decs = List.rev_map register_css_ident acc in
  code @ decs
