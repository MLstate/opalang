/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/


/************************************************/
/* Some export for pass "ResolveRemoteCalls" ****/
/************************************************/
/* Export module OpaType */
@opacapi @both OpaTsc_implementation = OpaTsc.implementation

/* Export module OpaSerialize */
@opacapi @both OpaSerialize_serialize = OpaSerialize.serialize
@opacapi @both OpaSerialize_serialize_for_js(value) =
  "\"" ^ String.escape_non_utf8_special(OpaSerialize.serialize(value)) ^ "\""

@opacapi @both OpaSerialize_unserialize = OpaSerialize.unserialize
@opacapi @both OpaSerialize_unserialize_unsafe = OpaSerialize.unserialize_unsafe

@opacapi @both OpaSerialize_unserialize_ty = OpaSerialize.unserialize_ty


/* Used for resolve insert server value, with direct js insertion */
@opacapi Opa2Js_to_string = Opa2Js.to_string

@opacapi @both OpaRPC_error_stub(a) =
  error("ERROR STUB : An error occurs when you call this stub => " ^ a)

@opacapi @both OpaRPC_fake_stub(_) =
  error("FAKE STUB : This fake stub should be a compilation error")

/* Export module OpaRPC */
@opacapi @both OpaRPC_unserialize = OpaRPC.unserialize
@opacapi @both OpaRPC_extract_types = OpaRPC.extract_types
@opacapi @both OpaRPC_extract_values = OpaRPC.extract_values

@opacapi @both OpaRPC_serialize = OpaRPC.serialize
@opacapi @both OpaRPC_empty_request = OpaRPC.empty_request
@opacapi @both OpaRPC_add_args_with_type = OpaRPC.add_args_with_type
//: OpaType.ty, void, OpaRPC.request -> OpaRPC.request // Workaround ei bug
@opacapi @both OpaRPC_add_var_types = OpaRPC.add_var_types
@opacapi @both OpaRPC_add_row_types = OpaRPC.add_row_types
@opacapi @both OpaRPC_add_col_types = OpaRPC.add_col_types

/* Export module OpaRPC_Client*/
@opacapi @client OpaRPC_Client_send_to_server              = OpaRPC_Client.send_to_server
@opacapi @client OpaRPC_Client_try_cache                   = OpaRPC_Client.try_cache
@opacapi @client OpaRPC_Client_async_send_to_server        = OpaRPC_Client.async_send_to_server
@opacapi @client OpaRPC_Client_Dispatcher_register         = OpaRPC_Client.Dispatcher.register


/* Export module OpaRPC_Server*/
@opacapi @server OpaRPC_Server_send_to_client              = OpaRPC_Server.send_to_client
@opacapi @server OpaRPC_Server_async_send_to_client        = OpaRPC_Server.async_send_to_client
@opacapi @server OpaRPC_Server_Dispatcher_register         = OpaRPC_Server.Dispatcher.register
@opacapi @server OpaRPC_Server_try_cache                   = OpaRPC_Server.try_cache

@opacapi Scheduler_push = Scheduler.push
