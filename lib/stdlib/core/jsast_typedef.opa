/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Types definitions for the Javascript runtime ast
 *
 * @category compiler
 * @author Mathieu Barbin
 * @author Valentin Gatien-Baron
 * @stability stabilizing, unfrozen
**/

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * The content of the [JsAst.mini_expr] ident node.
**/
type JsAst.ident = string

/**
 * The run-time representation of the js code,
 * Should support dependency analysis, and alpha renaming
**/
type JsAst.mini_expr =
   /**
    * A string evaluated representing js code.
    * Can be evaluated at compile or run-time.
   **/
   { verbatim : string }

   /**
    * A reference of a toplevel js ident, separated from the code, to be renamed.
   **/
 / { ident : JsAst.ident}

 / { set_distant : llarray(JsAst.ident) }

   /**
    * A reference to a runtime type name
    */
 / { type_use : ServerAst.type_key }
 / { type_def : ServerAst.type_key }

   /**
    * A reference to a server function
    */
 / { rpc_use : ServerAst.rpc_key }
 / { rpc_def : ServerAst.rpc_key }

/**
 * The type for representing the content of a code_elt
**/
type JsAst.content = llarray(JsAst.mini_expr)

/**
 * The type for indexing {!JsAst.code_elt} during the cleaning.
 *
 * The top-level declaration name.
 * References to this ident could be found in nodes {!JsAst.mini_expr} ident cases.
 * The ident is :
 * - only used when doing runtime cleaning.
 * - mandatory for a top-level declaration (otherwise cleaning is broken)
 * - empty in case the content is not a top-level declaration. (e.g. lib.js)
 * The content contains the complete code_element (header and concrete syntax include)
**/
type JsAst.key_ident =
   { key : string }
 / { ident : JsAst.ident }
 / { key : string ; ident : JsAst.ident }

/**
 * The run-time representation of a js top-level declaration.
**/
type JsAst.code_elt = {
  ident : JsAst.key_ident


  definition : ServerAst.definition

  /**
   * Some code_elt are tagged as root by the compiler.
   * They should not be cleaned.
  **/
  root : Server.reference(bool)

  /**
   * The body of the declaration.
  **/
  content : JsAst.content
}

type JsAst.code = llarray(JsAst.code_elt)

/**
 * An intermediate type for folding several JsAst.code
 * @private do not enter the implementation of this type.
**/
type JsAst.lexems = list(string)
