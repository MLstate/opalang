/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
import-plugin server
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

  // Note: defaults for pack.opa are littleEndian, Signed, Longlong
  @private D = Pack.Decode

  key_ident_code =
    [{Coded=[({Byte=0},[{String=""}]),
             ({Byte=1},[{String=""}]),
             ({Byte=2},[{String=""},{String=""}])]}]

  unser_key_ident(input:Pack.input) : Pack.result(JsAst.key_ident) =
    do D.pinput("unser_key_ident", input)
    match D.unpack(key_ident_code, input.binary, input.pos) with
    | {success=(pos,[{Coded=[({Byte=0},[{String=key}])]}])} -> {success=({input with ~pos},{~key})}
    | {success=(pos,[{Coded=[({Byte=1},[{String=ident}])]}])} -> {success=({input with ~pos},{~ident})}
    | {success=(pos,[{Coded=[({Byte=2},[{String=key},{String=ident}])]}])} -> {success=({input with ~pos},~{key; ident})}
    | {success=(_,[{Coded=[({Byte=n},_)]}])} -> {failure="Client_code.unser_key_ident: bad code {n}"}
    | {success=data} -> {failure="Client_code.unser_key_ident: bad unpack data {data}"}
    | {~failure} -> {~failure}

  mini_expr_code =
    [{Coded=[({Byte=0},[{String=""}]),
             ({Byte=1},[{String=""}]),
             ({Byte=2},[{String=""}]),
             ({Byte=3},[{List=([{String=""}],[])}]),
             ({Byte=4},[{String=""}]),
             ({Byte=5},[{String=""}]),
             ({Byte=6},[{String=""}]),
             ({Byte=7},[{String=""}]),
            ]}]

  // data list -> string low-level array
  dl2slla(dl) = // TODO: Outcome.list
    LowLevelArray.of_list(List.map((data -> match data with | [{~String}] -> String | _ -> ""),dl))

  unser_mini_expr(input:Pack.input) : Pack.result(JsAst.mini_expr) =
    do D.pinput("unser_mini_expr", input)
    match D.unpack(mini_expr_code, input.binary, input.pos) with
    | {success=(pos,[{Coded=[({Byte=0},[{String=verbatim}])]}])} -> {success=({input with ~pos},{~verbatim})}
    | {success=(pos,[{Coded=[({Byte=1},[{String=ident}])]}])} -> {success=({input with ~pos},{~ident})}
    | {success=(pos,[{Coded=[({Byte=2},[{String=verbatim}])]}])} -> {success=({input with ~pos},{~verbatim})}
    | {success=(pos,[{Coded=[({Byte=3},[{List=(_,dl)}])]}])} -> {success=({input with ~pos},~{set_distant=dl2slla(dl)})}
    | {success=(pos,[{Coded=[({Byte=4},[{String=rpc_def}])]}])} -> {success=({input with ~pos},{~rpc_def})}
    | {success=(pos,[{Coded=[({Byte=5},[{String=rpc_use}])]}])} -> {success=({input with ~pos},{~rpc_use})}
    | {success=(pos,[{Coded=[({Byte=6},[{String=type_def}])]}])} -> {success=({input with ~pos},{~type_def})}
    | {success=(pos,[{Coded=[({Byte=7},[{String=type_use}])]}])} -> {success=({input with ~pos},{~type_use})}
    | {success=(_,[{Coded=[({Byte=n},_)]}])} -> {failure="Client_code.mini_expr_ident: bad code {n}"}
    | {success=data} -> {failure="Client_code.mini_expr_ident: bad unpack data {data}"}
    | {~failure} -> {~failure}

  unser_content(input:Pack.input) : Pack.result(JsAst.content) =
    do D.pinput("unser_content", input)
    D.unser_array(unser_mini_expr, Pack.bigEndian, Pack.sizeLong, {verbatim=""}, input)

  definition_code =
    [{Coded=[({Byte=0},[]),
             ({Byte=1},[{String=""}]),
             ({Byte=2},[{String=""}])]}]

  unser_definition(input:Pack.input) : Pack.result(ServerAst.definition) =
    do D.pinput("unser_definition", input)
    match D.unpack(definition_code, input.binary, input.pos) with
    | {success=(pos,[{Coded=[({Byte=0},[])]}])} -> {success=({input with ~pos},{nothing})}
    | {success=(pos,[{Coded=[({Byte=1},[{String=rpc}])]}])} -> {success=({input with ~pos},{~rpc})}
    | {success=(pos,[{Coded=[({Byte=2},[{String=`type`}])]}])} -> {success=({input with ~pos},{~`type`})}
    | {success=(_,[{Coded=[({Byte=n},_)]}])} -> {failure="Client_code.unser_definition: bad code {n}"}
    | {success=data} -> {failure="Client_code.unser_definition: bad unpack data {data}"}
    | {~failure} -> {~failure}

  unser_bool_ref(input:Pack.input) : Pack.result(Server.reference(bool)) = D.unser_ref(D.unser_bool, input)

  unser_code_elt(input:Pack.input): Pack.result(JsAst.code_elt) =
    do D.pinput("unser_code_elt", input)
    match D.unser4(input, unser_content, unser_definition, unser_key_ident, unser_bool_ref) with
    | {success=(input,(content, definition, ident, root))} -> {success=(input,~{content; definition; ident; root})}
    | {~failure} -> {~failure}

  dummy_code_elt : JsAst.code_elt =
    { ident={key=""}; definition={nothing}; root=ServerReference.create(false); content=LowLevelArray.empty }

  unser_code(input:Pack.input): Pack.result(JsAst.code) =
    do D.pinput("unser_code", input)
    D.unser_array(unser_code_elt, Pack.bigEndian, Pack.sizeLong, dummy_code_elt, input)

  unser_adhoc(string:string) : JsAst.code =
    //do ServerReference.set(D.debug,false)
    //do jlog("unser_adhoc")
    match D.unser_from_string(unser_code, string, true) with
    | {success=code} -> /*do jlog("unser_adhoc: code ok")*/ code
    | {~failure} ->
       do error("Client_code.unser_adhoc => {failure}"):void
       LowLevelArray.empty

  unser_string = D.unser_string(Pack.bigEndian, Pack.sizeLong, _)
  unser_string_option = D.unser_option(unser_string,_)

  unser_sarray(input:Pack.input): Pack.result(llarray(string)) =
    D.unser_array(unser_string, Pack.bigEndian, Pack.sizeLong, "", input)

  unser_server_code_elt(input:Pack.input): Pack.result(ServerAst.code_elt) =
    do D.pinput("unser_server_code_elt", input)
    match D.unser7(input,
                   unser_string_option, // client_equivalent
                   unser_definition,    // defines
                   unser_string_option, // ident
                   unser_sarray,        // ident_deps
                   unser_bool_ref,      // root
                   unser_sarray,        // rpc_deps
                   unser_sarray         // type_deps
                  ) with
    | {success=(input,(client_equivalent,defines,ident,ident_deps,root,rpc_deps,type_deps))} ->
       {success=(input,~{client_equivalent;defines;ident;ident_deps;root;rpc_deps;type_deps})}
    | {~failure} -> {~failure}

  dummy_server_code_elt : ServerAst.code_elt =
    { client_equivalent=none;
      defines={nothing};
      ident=none;
      ident_deps=LowLevelArray.empty;
      root=ServerReference.create(false);
      rpc_deps=LowLevelArray.empty;
      type_deps=LowLevelArray.empty
    }

  unser_server_code(input:Pack.input): Pack.result(ServerAst.code) =
    do D.pinput("unser_server_code", input)
    D.unser_array(unser_server_code_elt, Pack.bigEndian, Pack.sizeLong, dummy_server_code_elt, input)

  unser_server(string:string) : ServerAst.code =
    //do ServerReference.set(D.debug,true)
    //do jlog("unser_server")
    match D.unser_from_string(unser_server_code, string, true) with
    | {success=code} -> /*do jlog("unser_server: server code ok")*/ code
    | {~failure} ->
       do Log.error("Client_code.unser_server","{failure}")
       LowLevelArray.empty

  /**
   * Register a code_elt.
  **/
  register_js_code_elt(js_elt : JsAst.code_elt) : void =
    Core_client_code.register_js_code({ast=@llarray(js_elt)})

  /**
   * Obtain client processed code as a string (rename and cleaned, but not minified)
  **/
  retrieve_js_file() : string =
    client_codes = Core_client_code.retrieve_js_codes()
    server_codes = Core_server_code.retrieve_server_codes()
    asts = List.map((
      | ~{adhoc=s package_=_} -> unser_adhoc(s)
      | ~{ast} -> ast
    ), client_codes)
    server_asts = List.map((
      |{adhoc=s package_=_} -> unser_server(s)
      |{adhoc_e=e package_=_} -> e
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
