/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
import-plugin server
import stdlib.core.{js, rpc.core, pack}

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
  @private U = Pack.Unser

  key_ident_code =
    [{Coded=[({Byte=0},[{String=""}]),
             ({Byte=1},[{String=""}]),
             ({Byte=2},[{String=""},{String=""}])]}]

  unser_key_ident(input:Pack.input) : Pack.result(JsAst.key_ident) =
    do Pack.pinput("unser_key_ident", input)
    match D.unpack(key_ident_code, input.binary, input.pos) with
    | {success=(pos,[{Coded=[({Byte=0},[{String=k}])]}])} -> {success=({input with ~pos},{~k})}
    | {success=(pos,[{Coded=[({Byte=1},[{String=i}])]}])} -> {success=({input with ~pos},{~i})}
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
    do Pack.pinput("unser_mini_expr", input)
    match D.unpack(mini_expr_code, input.binary, input.pos) with
    | {success=(pos,[{Coded=[({Byte=0},[{String=v}])]}])} -> {success=({input with ~pos},{~v})}
    | {success=(pos,[{Coded=[({Byte=1},[{String=i}])]}])} -> {success=({input with ~pos},{~i})}
    | {success=(pos,[{Coded=[({Byte=2},[{String=v}])]}])} -> {success=({input with ~pos},{~v})}
    | {success=(pos,[{Coded=[({Byte=3},[{List=(_,dl)}])]}])} -> {success=({input with ~pos},~{s=dl2slla(dl)})}
    | {success=(pos,[{Coded=[({Byte=4},[{String=rd}])]}])} -> {success=({input with ~pos},{~rd})}
    | {success=(pos,[{Coded=[({Byte=5},[{String=ru}])]}])} -> {success=({input with ~pos},{~ru})}
    | {success=(pos,[{Coded=[({Byte=6},[{String=td}])]}])} -> {success=({input with ~pos},{~td})}
    | {success=(pos,[{Coded=[({Byte=7},[{String=tu}])]}])} -> {success=({input with ~pos},{~tu})}
    | {success=(_,[{Coded=[({Byte=n},_)]}])} -> {failure="Client_code.mini_expr_ident: bad code {n}"}
    | {success=data} -> {failure="Client_code.mini_expr_ident: bad unpack data {data}"}
    | {~failure} -> {~failure}

  unser_content(input:Pack.input) : Pack.result(JsAst.content) =
    do Pack.pinput("unser_content", input)
    U.array(unser_mini_expr, Pack.bigEndian, Pack.longSize, {v=""}, input)

  definition_code =
    [{Coded=[({Byte=0},[]),
             ({Byte=1},[{String=""}]),
             ({Byte=2},[{String=""}])]}]

  unser_definition(input:Pack.input) : Pack.result(ServerAst.definition) =
    do Pack.pinput("unser_definition", input)
    match D.unpack(definition_code, input.binary, input.pos) with
    | {success=(pos,[{Coded=[({Byte=0},[])]}])} -> {success=({input with ~pos},{})}
    | {success=(pos,[{Coded=[({Byte=1},[{String=r}])]}])} -> {success=({input with ~pos},{~r})}
    | {success=(pos,[{Coded=[({Byte=2},[{String=t}])]}])} -> {success=({input with ~pos},{~t})}
    | {success=(_,[{Coded=[({Byte=n},_)]}])} -> {failure="Client_code.unser_definition: bad code {n}"}
    | {success=data} -> {failure="Client_code.unser_definition: bad unpack data {data}"}
    | {~failure} -> {~failure}

  unser_bool_ref(input:Pack.input) : Pack.result(Server.reference(bool)) = U.ref(U.bool, input)

  unser_code_elt(input:Pack.input): Pack.result(JsAst.code_elt) =
    do Pack.pinput("unser_code_elt", input)
    match U.tuple4(input, unser_content, unser_definition, unser_key_ident, unser_bool_ref) with
    | {success=(input,(c, d, i, r))} -> {success=(input,~{c; d; i; r})}
    | {~failure} -> {~failure}

  dummy_code_elt : JsAst.code_elt =
    { i={k=""}; d={}; r=ServerReference.create(false); c=LowLevelArray.empty }

  unser_code(input:Pack.input): Pack.result(JsAst.code) =
    do Pack.pinput("unser_code", input)
    U.array(unser_code_elt, Pack.bigEndian, Pack.longSize, dummy_code_elt, input)

  unser_adhoc(string:string) : JsAst.code =
    //do ServerReference.set(D.debug,false)
    //do jlog("unser_adhoc")
    match U.from_string(unser_code, string, true) with
    | {success=code} -> /*do jlog("unser_adhoc: code ok")*/ code
    | {~failure} ->
       do error("Client_code.unser_adhoc => {failure}"):void
       LowLevelArray.empty

  unser_string = U.string(Pack.bigEndian, Pack.longSize, _)
  unser_string_option = U.option(unser_string,_)

  unser_sarray(input:Pack.input): Pack.result(llarray(string)) =
    U.array(unser_string, Pack.bigEndian, Pack.longSize, "", input)

  unser_server_code_elt(input:Pack.input): Pack.result(ServerAst.code_elt) =
    do Pack.pinput("unser_server_code_elt", input)
    match U.tuple7(input,
                   unser_string_option, // client_equivalent
                   unser_definition,    // defines
                   unser_string_option, // ident
                   unser_sarray,        // ident_deps
                   unser_bool_ref,      // root
                   unser_sarray,        // rpc_deps
                   unser_sarray         // type_deps
           ) with
    | {success=(input,(c,d,i,id,r,rd,td))} ->
       {success=(input,~{c;d;i;id;r;rd;td})}
    | {~failure} -> {~failure}

  dummy_server_code_elt : ServerAst.code_elt =
    { c=none;
      d={};
      i=none;
      id=LowLevelArray.empty;
      r=ServerReference.create(false);
      rd=LowLevelArray.empty;
      td=LowLevelArray.empty
    }

  unser_server_code(input:Pack.input): Pack.result(ServerAst.code) =
    do Pack.pinput("unser_server_code", input)
    U.array(unser_server_code_elt, Pack.bigEndian, Pack.longSize, dummy_server_code_elt, input)

  unser_server(string:string) : ServerAst.code =
    //do ServerReference.set(D.debug,true)
    //do jlog("unser_server")
    match U.from_string(unser_server_code, string, true) with
    | {success=code} -> /*do jlog("unser_server: server code ok")*/ code
    | {~failure} ->
       do Log.error("Client_code.unser_server","{failure}")
       LowLevelArray.empty

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

/**
 * Some export for pass "AddCSS"
 */
@opacapi Client_code_register_css_declaration = Client_code.register_css_declaration
