/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
  /*ident*/
  i : option(ServerAst.ident);
  /*client_equivalent*/
  c : option(JsAst.ident);
  /*defines*/
  d : ServerAst.definition;
  /*ident_deps*/
  id : llarray(ServerAst.ident)
  /*rpc_deps*/
  rd : llarray(ServerAst.rpc_key);
  /*type_deps*/
  td : llarray(ServerAst.type_key);
  /*root*/
  r : Server.reference(bool);
}

type ServerAst.definition =
    /*rpc*/
    {r:ServerAst.rpc_key}
    /*type*/
  / {t:ServerAst.type_key}
    /*nothing*/
  / {}

type ServerAst.rpc_key = string
type ServerAst.type_key = string
type ServerAst.ident = JsAst.ident

/**
 * {1 Interface}
 */

ServerAst = {{
  fold_code = LowLevelArray.fold
}}
