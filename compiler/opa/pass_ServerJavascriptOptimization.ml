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

module Format = BaseFormat

module J = JsAst
module C = JsCons

let export_to_global ident e =
  JsCons.Statement.assign
    (JsCons.Expr.dot ~own_property:false
       (JsCons.Expr.native_global "global")
       (Format.to_string JsPrint.pp#ident ident))
    e

let is_exported exported i = JsIdentSet.mem i exported

let process_code_elt exported = function
  | J.Js_var (_, i, Some e) when is_exported exported i -> export_to_global i e
  | J.Js_function (l, i, p, b) when is_exported exported i ->
      export_to_global i (J.Je_function (l, Some i, p, b))
  | x -> x

let process_code exported code =
  (* ignore (PassTracker.print ~passname:"ServerJavascriptOptimization" ~printer_id:"js_exported" (JsIdentSet.pp ", " JsPrint.pp#ident) exported); *)
  List.map (process_code_elt exported) code
