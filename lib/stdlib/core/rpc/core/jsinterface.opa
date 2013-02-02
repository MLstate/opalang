/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Runtime of Js language, for server initialization.
 * @author Rudy Sicard
 * @author Mathieu Barbin
**/

import stdlib.core.{js}

/**
 * {1 About this module}
 *
 * Used by opa2js to generate javascript toplevel identifiers.
 * This is computed on the server, and take care about the renaming
 * of the javascript code.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

@server @publish JsInterface = {{

  /**
   * dynamic xhtml
  **/
  default_opa_event = JsIdent.define_rename("default_opa_event")

  /**
   * opa2js
  **/
  js_void = JsIdent.define_rename("js_void")

  /**
   * opa2js
  **/
  ServerChannel = JsIdent.define_rename("ServerChannel")

}}
