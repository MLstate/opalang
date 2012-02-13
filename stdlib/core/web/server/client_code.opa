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
import stdlib.core.{js, rpc.core}

/**
 * {1 About this module}
 *
 * Server side code used to store and retrieve javascript/css files.
 * Used by generated server, not for casual user.
 *
 * {1 Where should I start?}
 *
 * If you simply want to prepare some tea, use function [Tea.prepare].
 *
 *
 * {1 What if I need more?}
**/

/**
 * {1 Interface}
 */

@server Client_code = {{

  /**
   * Register a code_elt.
  **/
  register_js_code_elt(js_elt : JsAst.code_elt) : void =
    Core_client_code.register_js_code({ast=@llarray(js_elt)})

  @private unser_adhoc : string -> JsAst.code =
    `type`(`type`:string) = ~{`type`}
    type_def(type_def:string) = ~{type_def}
    type_use(type_use:string) = ~{type_use}
    rpc(rpc:string) = ~{rpc}
    rpc_def(rpc_def:string) = ~{rpc_def}
    rpc_use(rpc_use:string) = ~{rpc_use}
    set_distant(set_distant:llarray(string)) = ~{set_distant}
    verbatim(verbatim:string) = ~{verbatim}
    ident(ident:string) = ~{ident}
    key(key:string) = ~{key}
    key_ident(key,ident) : JsAst.key_ident = ~{key ident}
    code_elt(content,definition,ident,root) : JsAst.code_elt = ~{content definition ident root}
    %% BslClientCode.unser_adhoc %%(rpc,rpc_def,rpc_use,`type`,type_def,type_use,set_distant,verbatim,ident,key,key_ident,code_elt,_)

  @private unser_server : string -> ServerAst.code =
    rpc(rpc:ServerAst.rpc_key) = ~{rpc}
    `type`(`type`:ServerAst.type_key) = ~{`type`}
    code_elt(client_equivalent,defines,ident,ident_deps,root,rpc_deps,type_deps) : ServerAst.code_elt = ~{client_equivalent defines ident ident_deps root rpc_deps type_deps}
    %% BslClientCode.unser_server %%(code_elt,rpc,`type`,_)

  /**
   * Obtain client processed code as a string (rename and cleaned, but not minified)
  **/
  retrieve_js_file() : string =
    client_codes = Core_client_code.retrieve_js_codes()
    server_codes = Core_server_code.retrieve_server_codes()
    asts = List.map((client_code ->
      match client_code with
      | ~{adhoc=s package_=_} ->
        //do jlog("[init] building js ast from adhoc format for {package_}")
        unser_adhoc(s)
      | ~{ast} -> ast
    ), client_codes)
    server_asts = List.map(({adhoc=s package_=_} ->
      //do jlog("[init] building server ast for {package_}")
      unser_server(s)
    ), server_codes)
    JsAst.js_codes_to_string(server_asts,asts)

  @private
  css_files = Server_reference.create([]:list(string))

  register_css_file(css_file:string) : void =
    Server_reference.update(css_files, List.cons(css_file,_))

  register_css_declaration(css) =
    register_css_file(generate_css_def(css))

  retrieve_css_files() : list(string) = Server_reference.get(css_files)

  retrieve_css_file() : string = String.concat("",retrieve_css_files())
}}

/**
 * {1 Functions exported to the global namespace}
 */

/* client code */
@opacapi Client_code_register_js_code_elt = Client_code.register_js_code_elt
Client_code_register_css_file = Client_code.register_css_file

/**
 * Some export for pass "AddCSS"
 */
@opacapi Client_code_register_css_declaration = Client_code.register_css_declaration
