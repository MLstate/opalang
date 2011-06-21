/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/
type ServerAst.code = llarray(ServerAst.code_elt)
type ServerAst.code_elt = {
  ident : option(ServerAst.ident);
  client_equivalent : option(JsAst.ident);
  defines : ServerAst.definition;
  ident_deps : llarray(ServerAst.ident)
  rpc_deps : llarray(ServerAst.rpc_key);
  type_deps : llarray(ServerAst.type_key);
  root : Server.reference(bool);
}

type ServerAst.definition =
    {rpc:ServerAst.rpc_key}
  / {`type`:ServerAst.type_key}
  / {nothing}
type ServerAst.rpc_key = string
type ServerAst.type_key = string
type ServerAst.ident = JsAst.ident

ServerAst = {{
  fold_code = LowLevelArray.fold
}}
