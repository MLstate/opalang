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

  @private define(source) =
    do JsIdent.define(source)
    JsIdent.rename(source)

  /**
   * funaction
  **/
  get_local_unsafe = define("get_local_unsafe")

  /**
   * dynamic xhtml
  **/
  default_opa_event = define("default_opa_event")

  /**
   * opa2js
  **/
  js_void = define("js_void")

  /**
   * opa2js
  **/
  ServerChannel = define("ServerChannel")

}}
