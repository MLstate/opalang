/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

/**
 * {1 About this module}
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
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

/**
 * {1 Interface}
 */

ServerAst = {{
  fold_code = LowLevelArray.fold
}}
