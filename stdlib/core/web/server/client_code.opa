/*
    Copyright © 2011, 2012 MLstate

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

  // Note: defaults for pack.opa are littleEndian, Unsigned, Longlong

  key_ident_code =
    [{Coded=[({Byte=0},[{String=""}]),
             ({Byte=1},[{String=""}]),
             ({Byte=2},[{String=""},{String=""}])]}]

  unser_key_ident(input:Pack.input) : Pack.result(JsAst.key_ident) =
    match Pack.Decode.unpack(key_ident_code, input.string, input.pos) with
    | {success=(pos,[({Byte=0},[{String=key}])])} -> {success=({input with ~pos},{~key})}
    | {success=(pos,[({Byte=1},[{String=ident}])])} -> {success=({input with ~pos},{~ident})}
    | {success=(pos,[({Byte=2},[{String=key},{String=ident}])])} -> {success=({input with ~pos},~{key, ident})}
    | {success=(_,[({Byte=n},_)])} -> {failure="Client_code.unser_key_ident: bad code {n}"}
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
    match Pack.Decode.unpack(mini_expr_code, input.string, input.pos) with
    | {success=(pos,[({Byte=0},[{String=verbatim}])])} -> {success=({input with ~pos},{~verbatim})}
    | {success=(pos,[({Byte=1},[{String=ident}])])} -> {success=({input with ~pos},{~ident})}
    | {success=(pos,[({Byte=2},[{String=verbatim}])])} -> {success=({input with ~pos},{~verbatim})}
    | {success=(pos,[({Byte=3},[{List=(_,dl)}])])} -> {success=({input with ~pos},~{set_distant=dl2slla(dl)})}
    | {success=(pos,[({Byte=4},[{String=rpcdef}])])} -> {success=({input with ~pos},{~rpcdef})}
    | {success=(pos,[({Byte=5},[{String=rpcuse}])])} -> {success=({input with ~pos},{~rpcuse})}
    | {success=(pos,[({Byte=6},[{String=typedef}])])} -> {success=({input with ~pos},{~typedef})}
    | {success=(pos,[({Byte=7},[{String=typeuse}])])} -> {success=({input with ~pos},{~typeuse})}
    | {success=(_,[({Byte=n},_)])} -> {failure="Client_code.mini_expr_ident: bad code {n}"}
    | {~failure} -> {~failure}

  unser_content(input:Pack.input) : Pack.result(JsAst.content) =
    Pack.Decode.unset_array(unser_mini_expr, {verbatim:""}, input)

  definition_code =
    [{Coded=[({Byte=0},[{String=""}]),
             ({Byte=1},[{String=""}]),
             ({Byte=2},[{String=""},{String=""}])]}]

  unser_definition(input:Pack.input) : Pack.result(ServerAst.definition) =
    match Pack.Decode.unpack(definition_code, input.string, input.pos) with
    | {success=(pos,[({Byte=0},[])])} -> {success=({input with ~pos},{nothing})}
    | {success=(pos,[({Byte=1},[{String=rpc}])])} -> {success=({input with ~pos},{~rpc})}
    | {success=(pos,[({Byte=2},[{String=type_}])])} -> {success=({input with ~pos},{~type_})}
    | {success=(_,[({Byte=n},_)])} -> {failure="Client_code.unser_definition: bad code {n}"}
    | {~failure} -> {~failure}

  unser_bool_ref(input:Pack.input) : Pack.result(Server.reference(bool)) = Pack.Decode.unser_ref(Pack.Decode.unser_bool, _)

  unser_code_elt(input:Pack.input): Pack.result(JsAst.code_elt) =
    match Pack.Decode.unser4(input, unser_content, unser_definition, unser_key_ident, unser_bool_ref) with
    | {success=(input,(content, definition, ident, root))} -> {success=(input,{~content, definition, ident, root})}
    | {~failure} -> {~failure}

  dummy_code_elt : JsAst.code_elt =
    { ident={key=""}; definition={nothing}; root=ServerReference.create(false); content=LowLevelArray.empty }

  unser_code(input:Pack.input): Pack.result(JsAst.code) =
    Pack.Decode.unser_array(unser_code_elt, dummy_code_elt, input)

  unser_adhoc(string:string) : JsAst.code =
    match Pack.Decode.unser(unser_code, string, true) with
    | {success=code} -> code
    | {~failure} -> 
       do Log.error("Client_code.unser_adhoc: {failure}")
       LowLevelArray.empty

  unser_string = Pack.Decode.unser_string(Pack.littleEndian, Pack.sizeLonglong, _)
  unser_string_option = Pack.Decode.unser_option(unser_string,_)

  unser_sarray(input:Pack.input): Pack.result(llarray(string)) = Pack.Decode.unser_array(unser_string, "", input)

  unser_server_code_elt(input:Pack.input): Pack.result(ServerAst.code_elt) =
    match Pack.Decode.unser7(unser_string_option, // client_equivalent
                             unser_definition,    // defines
                             unser_string_option, // ident
                             unser_sarray,        // ident_deps
                             unser_bool_ref,      // root
                             unser_sarray,        // rpc_deps
                             unser_sarray         // type_deps
                            ) with
    | {success=(_,(client_equivalent,defines,ident,ident_deps,root,rpc_deps,type_deps))} ->
       {success=(input,~{client_equivalent,defines,ident,ident_deps,root,rpc_deps,type_deps})}
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
    Pack.Decode.unser_array(unser_server_code_elt, dummy_server_code_elt, input)

  unser_server(string:string) : ServerAst.code =
    match Pack.Decode.unser(unser_server_code, string, true) with
    | {success=code} -> code
    | {~failure} -> 
       do Log.error("Client_code.unser_server: {failure}")
       LowLevelArray.empty

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
    #<Ifstatic:OPA_BACKEND_QMLJS>
    _ -> LowLevelArray.empty
    #<Else>
    %% BslClientCode.unser_adhoc %%(rpc,rpc_def,rpc_use,`type`,type_def,type_use,set_distant,verbatim,ident,key,key_ident,code_elt,_)
    #<End>

  @private unser_server : string -> ServerAst.code =
    rpc(rpc:ServerAst.rpc_key) = ~{rpc}
    `type`(`type`:ServerAst.type_key) = ~{`type`}
    code_elt(client_equivalent,defines,ident,ident_deps,root,rpc_deps,type_deps) : ServerAst.code_elt = ~{client_equivalent defines ident ident_deps root rpc_deps type_deps}
    #<Ifstatic:OPA_BACKEND_QMLJS>
    _ -> LowLevelArray.empty
    #<Else>
    %% BslClientCode.unser_server %%(code_elt,rpc,`type`,_)
    #<End>

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
