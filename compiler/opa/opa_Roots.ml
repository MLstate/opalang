(*
    Copyright © 2011, 2012 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*)

(*
  Mathieu Sat Oct 30 11:16:54 CEST 2010
  We need simplicity in design, and identified dependencies.

  I considere all "roots" things as a terrible hack, but I agree that we have not yet
  a better mechanism.
  We should e.g. be able to add a toplevel directive in the opa definition of the identifiers
  for highlighting roots. In that way, the information would be closed to the opa-source code,
  not hidden in this file.

  @root serialize = ....
*)

let roots_for_s3
    ~no_server
    =
  (* FIXME: use opacapi *)
  let pass_ei = [
    Opacapi.OpaType.instantiate_col;
    Opacapi.OpaType.instantiate_row;
  ] in

  let pass_resolveRemoteCalls = [
    Opacapi.OpaTsc.implementation;

    Opacapi.OpaSerialize.serialize;
    Opacapi.OpaSerialize.serialize_for_js;
    Opacapi.OpaSerialize.unserialize;
    Opacapi.OpaSerialize.unserialize_unsafe;

    Opacapi.OpaRPC.serialize;
    Opacapi.OpaRPC.empty_request;
    Opacapi.OpaRPC.add_var_types;
    Opacapi.OpaRPC.add_row_types;
    Opacapi.OpaRPC.add_col_types;
    Opacapi.OpaRPC.add_args_with_type;
    Opacapi.OpaRPC.unserialize;
    Opacapi.OpaRPC.extract_types;
    Opacapi.OpaRPC.extract_values;
    Opacapi.OpaRPC.empty_request;

    Opacapi.OpaRPC.client_send_to_server;
    Opacapi.OpaRPC.client_async_send_to_server;

    Opacapi.OpaRPC.server_send_to_client;
    Opacapi.OpaRPC.server_async_send_to_client;

    Opacapi.OpaRPC.fake_stub;
    Opacapi.OpaRPC.error_stub;

    Opacapi.OpaRPC.client_dispatcher_register;
    Opacapi.OpaRPC.server_dispatcher_register;

    Opacapi.OpaRPC.client_try_cache;
    Opacapi.OpaRPC.server_try_cache;

    Opacapi.Opa2Js.to_string
    ] in

  let pass_FunAction = [
    Opacapi.FunActionServer.serialize_call ;
    Opacapi.FunActionServer.serialize_argument ;
    Opacapi.FunActionServer.serialize_ty_argument ;
  ] in

  let pass_JavascriptCompilation = [
    Opacapi.callFA; (* because used in a @js_ident *)
    Opacapi.dom_event_to_opa_event;
    Opacapi.Client_code.register_js_code ;
    Opacapi.Core_server_code.register_server_code;
    Opacapi.Client_code.register_js_code_elt ;
    Opacapi.Client_code.register_css_declaration ;
  ] in

  if no_server then
    pass_ei
  else
    pass_resolveRemoteCalls
    @ pass_ei
    @ pass_FunAction
    @ pass_JavascriptCompilation
