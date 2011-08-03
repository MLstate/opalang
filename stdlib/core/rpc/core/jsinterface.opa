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
