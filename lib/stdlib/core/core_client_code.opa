/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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

  register_js_code_ast(code) =
    @atomic(ServerReference.set(js_codes, [code | ServerReference.get(js_codes)]))

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
/ {adhoc_e : ServerAst.code; package_ : string}

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
@opacapi Client_code_register_js_code_ast = Core_client_code.register_js_code_ast
@opacapi Core_server_code_register_server_code = Core_server_code.register_server_code

#<Ifstatic:OPA_BACKEND_QMLJS>
#<Else>
@opacapi Client_code_serialize_string_length =
  %%BslPervasives.serialize_string_length%%
#<End>
