/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.core.{web.core, js}

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Interface}
 */

@server_private Opa2Js = {{

  /**
   * A common transformation. No back-end dependent, but JavaScript
   * implementation dependent.
   */
  Common = {{

    /**
     * Serialize a server session on [RPC.Json.js_code].
     */
    session(s:channel('a)):option(RPC.Json.js_code) =
      context = match ThreadContext.get({current}).key
                {~client} -> some(client)
                _ -> some(ThreadContext.Client.fake)
                     // NOT none until OpaNetwork corrections
                end
      match ChannelServer.get_id(s,context)
      {some = id} -> some({Record = [("entity", {Record = [("other", {String = id})]})]})
      {none} -> {none}

  }}

  /**
   * Transform a [value] to json value back-end dependent.
   */
  to_json_with_type(serialize_closure_callback, ty:OpaType.ty, value) : RPC.Json.js_code =
    original_ty = ty

    bool_tyrow(b) =
      match b with
      | [ {label="true"; ty= ty_void} ] | [ {label="false"; ty=ty_void}] -> OpaType.is_void( ty_void )
      | _ -> false

    /* For record *******************************/

    aux_rec(value, fields) : RPC.Json.js_code =
        if List.is_empty(fields) then
          /*
            The identifier js_void defined in the client lib
            is the shared version of the following record :
            {Record = [("_size", {Int = 0})]}
          */
          { Direct = JsInterface.js_void }
        else
          (lst, size) =
            OpaValue.Record.fold(
              (field, value, (lst, size) ->
                name = OpaValue.Record.name_of_field_unsafe(field)
                tyfield = OpaValue.Record.type_field_of_field_unsafe(field)
                ty = OpaType.type_of_field_unsafe(fields, tyfield)
                res = to_json_with_type(serialize_closure_callback, ty, value)
              ((name, res)+>lst, size+1)
            ), value, ([], 0))
          _ = size
          {Record = lst}

    /* Main auxiliary function *******************/
    rec aux(value, ty) : RPC.Json.js_code =
      match ty with
      /* Basic case *****************************/
      | {TyConst = {TyInt}} -> {Int = Magic.id(value)}: RPC.Json.js_code
      | {TyConst = {TyFloat}} -> {Float = Magic.id(value)}: RPC.Json.js_code
      | {TyConst = {TyString}} -> {String = Magic.id(value)}: RPC.Json.js_code

      /* Record case ****************************/
      | {TyRecord_row = row}
      | {TyRecord_row = row; TyRecord_rowvar = _} ->
         if bool_tyrow(row) then
         /* Special Record case ********************/
           {Bool=Magic.id(value)}: RPC.Json.js_code
         /* Normal Record case *********************/
         else aux_rec(value, row): RPC.Json.js_code
      | {TySum_col = col}
      | {TySum_col = col; TySum_colvar = _} ->
        row = OpaType.fields_of_fields_list(value, col).f1
        if bool_tyrow(row) then
         /* Special Record case ********************/
           {Bool=Magic.id(value)}: RPC.Json.js_code
         /* Normal Record case *********************/
         else aux_rec(value, row): RPC.Json.js_code


      /* Particular named type ******************/
      /* Session */
      | {TyName_ident = "Session.private.native"; TyName_args = _}
      | {TyName_ident = "channel"; TyName_args = _}
      | {TyName_ident = "Cell.cell"; TyName_args = _} ->
        match Common.session(Magic.id(value)) with
        | {some=js_code} -> js_code
        | {none} -> aux_default(value, ty)
        end

      /* Encapsulated types ***********************/
      | {TyName_args = args; TyName_ident = ident} ->
        if ((%%BslValue.MagicContainer.serializer_get%%(ident)) == none) then
          implem = OpaType.type_of_name(ident,args)
          aux(value, implem): RPC.Json.js_code
        else
          aux_default(value, ty)
      | {TyForall_quant = _; TyForall_body = body} ->
        aux(value, body): RPC.Json.js_code

// TODO - Optimize it if possible...
//       /* Continuation */
//       | {TyName_ident = "continuation"; TyName_args = _} ->
//         Common.session_json(OpaSerialize.partial_serialize(value, ty))
//       /* Closure ********************************/
//       | {TyArrow_params = _; TyArrow_res = _} ->
//         clos = Common.session_json(OpaSerialize.partial_serialize(value, ty))
//         clos = Json.to_string(clos)

      /* Error case *****************************/
      /* We use the full serialization with direct code injection */
      | _ -> aux_default(value, ty)
//        do Log.warning("OPA2JS",
//          "Optimized JavaScript serialization for {OpaType.to_pretty(ty)} is impossible or not yet implemented\nReplaced by the full serialization/unserialization")

    and aux_default(value, ty): RPC.Json.js_code =
        ser_typ = Json.to_text_in_js_ll(aux(Magic.id(original_ty),
                                        @typeof(ty)))
        ser_typ = Text.to_string(ser_typ)
        ser_options = {OpaSerialize.default_options with ~serialize_closure_callback}
        ser_val = OpaSerialize.serialize_with_type_options(original_ty, value, ser_options)
        ser_val = "\"" ^ String.escape_non_utf8_special(ser_val) ^ "\""

        /*
          perform a call to a bslJsIdent function adding a binding
          from: the identifier of the code_elt we are generating via opa2js,
          to: the key of the closure used, as well as any identifiers contained in this code.
          This is needed for the runtime cleaning, because theses dependencies are not seen
          by the cleaning (hidden in a verbatim).
        */

        direct = @js_ident("OpaSerialize_unserialize_unsafe")^"("^ser_val^","^ser_typ^")"
        { Direct = direct }: RPC.Json.js_code

    aux(value, ty): RPC.Json.js_code

    /**
     * Transform a [value] to JavaScript string back-end dependent.
     */
    to_string(toplevel_var, value) =
       to_string_with_type(toplevel_var, @typeof(value), value)

    to_string_with_type(toplevel_var, ty:OpaType.ty, value:'a) =
      refs_opt = JsCleaning.Closure.deps_of_var_for_opa2js(toplevel_var)
      serialize_closure_callback =
        match refs_opt with
        | {none} -> ignore
        | {some=refs} -> ident -> ServerReference.set(refs,[ident|ServerReference.get(refs)])
      txt = Json.to_text_in_js_ll(to_json_with_type(serialize_closure_callback, ty, value))
      // "/*OPA2JS*/" ^ TODO keep in debug mode
      Text.to_string(txt)
}}
