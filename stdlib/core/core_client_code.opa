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
 * @category compiler interface
 * @destination internal use, private
 * @author Valentin Gatien-Baron, 2011 (ad hoc serialization)
 * @author Mathieu Barbin, 2011 (review)
 * @stability stable
 */

/**
 * {1 About this module}
 *
 * {1 Where should I start ?}
 *
 * {1 What if I need more?}
**/

/**
 * {2 Javascript code manipulation}
 *
 * Server side code used to store and retrieve javascript/css files
**/

/**
 * {3 Types}
 */

/**
 * BEWARE: the compiler generate this type in qmljs_Serializer
 * @opacapi
**/
type Client_code.input =
     {adhoc : list(string); package_ : string}
   / {ast : JsAst.code}

/**
 * @opacapi
**/
type Client_code.output =
     {adhoc : string; package_ : string}
   / {ast : JsAst.code}

/**
 * {3 Interface}
 */

@server_private
Core_client_code =
{{
  /*
   * Private registered stack of client code on server (LIFO)
   * Will be served for all client.
   */
  @private
  js_codes = ServerReference.create([]:list(Client_code.output))

  /**
   * Register client code on server to be served for all client
   * @opacapi
  **/
  register_js_code(js_code:Client_code.input) : void =
    code =
      match js_code with
      | ~{adhoc package_} ->
        adhoc = String.concat("",adhoc)
        ~{adhoc package_}
      | {ast=_} as v ->
        v
    ServerReference.update(js_codes,List.cons(code,_))

  /**
   * Retrieve client code on server to be served for all client
   * Used for building the complete javascript code of the application
  **/
  retrieve_js_codes() : list(Client_code.output) =
    l = List.rev(ServerReference.get(js_codes))
    do ServerReference.set(js_codes,[])
    do @assert(l != []) /* making sure this function is called at most once */
    l
}}

/**
 * {2 Server code representation}
 *
 * Server side code used to perform a more clever cleaning of the javascript.
**/

/**
 * {3 Types}
 */

/**
 * BEWARE: the compiler generates this type in pass_GenerateServerAst
 * @opacapi
**/
type ServerCode.t =
     {adhoc : string; package_ : string}

/**
 * {3 Interface}
 */

Core_server_code =
{{
  @private server_codes = ServerReference.create([]:list(ServerCode.t))
  register_server_code(code:ServerCode.t) : void =
    ServerReference.update(server_codes, List.cons(code,_))
  retrieve_server_codes() : list(ServerCode.t) =
    l = List.rev(ServerReference.get(server_codes))
    do ServerReference.set(server_codes, [])
    do @assert(l != []) // same assertion as for the js code
    l
}}

@opacapi Client_code_register_js_code = Core_client_code.register_js_code
@opacapi Core_server_code_register_server_code = Core_server_code.register_server_code
