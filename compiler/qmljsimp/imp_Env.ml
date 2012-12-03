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
module J = JsAst

type env = {
  options : Qml2jsOptions.t;
  gamma : QmlTypes.gamma;
  annotmap : QmlAst.annotmap;
  val_ : ?side:[`client|`server] -> string -> Ident.t;
  private_bymap : Imp_Bsl.JsImpBSL.ByPassMap.t;
  bsl_lang : BslLanguage.t;
  srenaming : QmlRenamingMap.t;
  is_distant : Ident.t -> bool;
}

type private_env = {
  local_vars : J.ident list;
  renaming : J.ident IdentMap.t;
  no_warn_x : unit;
}


let next_param name = J.ExprIdent (Ident.next name)

let next_exprident private_env ident =
  let ident = J.ExprIdent ident in
  let private_env = {private_env with local_vars = ident :: private_env.local_vars} in
  private_env, ident

(* Generate a fresh identifier and add it to the list of local variables *)
let next private_env name =
  next_exprident private_env (Ident.next name)

let declare_local_vars private_env =
  let local_vars = private_env.local_vars in
  let private_env = {private_env with local_vars = []} in
  let declarations = List.map (fun v -> JsCons.Statement.var v) local_vars in
  private_env, JsCons.Statement.block declarations

let maybe_declare_local_vars private_env =
  if private_env.local_vars = [] then
    private_env, None
  else
    let private_env, statement = declare_local_vars private_env in
    private_env, Some statement

let reset_renaming private_env = {private_env with renaming = IdentMap.empty}
