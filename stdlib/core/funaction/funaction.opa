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

import stdlib.core.{web.core, rpc.core, js}

/**
 * Make some magic things with fun action.
 *
 * @category compiler
 * @destination private
 * @author Quentin Bourgerie
 * @author Rudy Sicard
 * @stability almost stable
 */

/**
 * {1 Client-side}
 */

/**
 * {2 Types defined in this module}
 */

/**
 * Type of a fun action
 */
type FunAction.t = Dom.event -> void

/**
 * {2 Interface}
 */

@opacapi @client
callFA(json : string, event : Dom.event): void =
  t = @typeval(FunAction.t)
  (OpaSerialize.unserialize(json, t) ? error("Error on unserialize a fun action"))(event)

/************************************************/
/* Some export for pass "opa_FunAction..."   ****/
/************************************************/
/* Export module FunActionServer */
@opacapi FunActionServer_serialize_call = FunActionServer.serialize_call
@opacapi FunActionServer_serialize_argument = FunActionServer.serialize_argument
@opacapi FunActionServer_serialize_ty_argument = FunActionServer.serialize_ty_argument

@both
FunAction = {{
  /**
   * Serialize a fun action on a javascript code.
   */
  @private
  sendFA(v)=
    sendFA_RTSTRING = @js_ident("callFA")
    v_str = String.escape_non_utf8_special(OpaSerialize.serialize(v))
    "{sendFA_RTSTRING}(\"{v_str}\", event);"

  serialize(f:FunAction.t) =
    json = OpaSerialize.partial_serialize(f, @typeof(f))
    match json with
    | {String = id} -> "{JsInterface.get_local_unsafe}({id})(event)"
    | _ -> sendFA(f)

}}

/**
   This module contains the runtime to serialize js call site in html using the ad hoc S2 mode,
   Entry point used by the compiler are alias defined above:
   FunActionServer_serialize_call and FunActionServer_serialize_argument

   Deserialized : {arg : 'a ; serialized_arg : string} -> 'a

   1)
   < onclick={EXPR = @funaction(e)}>


   2) FunActionLifting
   f(arg1,arg2,...)=e
   < onclick={EXPR = @funaction( f(arg1,arg2,...) ) }>


   3)
   [typage, slicing]
   Slicing in is in client mode in @funaction,
      only the name of f is changed since are argX names are local names


   4) FunActionEnvSerialize
   f(arg1,arg2,...)=e
   < onclick={EXPR = @funaction(
                  arg1_ = FunActionServer_serialize_argument(arg1)
                  arg2_ = FunActionServer_serialize_argument(arg2)
                  ...
                  (@funaction[client_id](f))(
                        @funaction[Deserialize](arg1_),
                        @funaction[Deserialize](arg2_),
                        ...
                  )
                  )
                  } >

   5)
   [explicit instantiation]
   f([[tyarg1]],arg1,arg2,...)=e
   < onclick={EXPR = @funaction(
                  arg1_ = FunActionServer_serialize_argument(arg1)
                  arg2_ = FunActionServer_serialize_argument(arg2)
                  ...
                  (@funaction[client_id](f))(
                        [[tyargs1,]]
                        @funaction[Deserialize](arg1_),
                        @funaction[Deserialize](arg2_),
                        ...
                  )
                  )
                  } >


   6)
   f([tyarg1],arg1,arg2,...)=e[(tyarg1)]
   < onclick={STRING =
                  arg1_ = FunActionServer_serialize_argument(arg1)
                  arg2_ = FunActionServer_serialize_argument(arg2)
                  ...
                  FunActionServer_serialize_call(
                        @jsident(f),
                        [
                          [[FunActionServer_serialize_tyargument(tyargs1)]]
                          arg_1_,
                          arg_2_,
                          ...
                        ]
                  )
                  }
    >


   7) Run time
   < onclick="f_in_js(arg1_value,arg2_value,...)" >

*/

/**
 * {1 Server-side}
 */

/**
 * {2 Types defined in this module}
 */

type FunActionServer.serialized_arg('a) = {arg : 'a ; serialized_arg : string}


/**
 * {2 Interface}
 */

FunActionServer = {{
/** serialize an argument to a FunActionServer.serialized_arg
    the serialized_arg field is suitable as js code source
*/
serialize_argument(__toplevel_var,arg)=
  #<Ifstatic:OPA_OPTIMIZE_RPC_I 1>
    serialized_arg = Opa2Js.to_string(__toplevel_var,arg)
  #<Else>
    unser_RTSTRING   = @js_ident("OpaSerialize_unserialize")
    unserty_RTSTRING = @js_ident("OpaSerialize_unserialize_ty")
    esc(v) = "\"{String.escape_non_utf8_special(v)}\""
    client_unser(tyunser,v)="{unser_RTSTRING}({esc(v)},{tyunser}).some"
    client_unser_ty(v)= "{unserty_RTSTRING}({esc(v)}).some"

    serialized_arg = OpaSerialize.serialize(arg) // std serialisation
    serialized_ty  = OpaSerialize.serialize(@typeof(arg))
    serialized_arg = client_unser(client_unser_ty(serialized_ty),serialized_arg)
  #<End>
  ~{arg serialized_arg}:FunActionServer.serialized_arg


/** same but for type arguments */
serialize_ty_argument(ty:OpaType.ty) = serialize_argument("DUMMY_FA",ty)

// currently implements fun name serialisation assuming the js ident is given
serialize_fun_name(fun_key_name)= fun_key_name
/*
 'fun_action_name' is related to the fun_action name
     (either its js name or a key to retieve the implementation)
 'serialized_args' are arguments alredy serialiased as valid js
  generate 'fun_action_name_in_js(args)' where args are ',' separated
*/
serialize_call(fun_action_name:string, ty_ser_args:list(FunActionServer.serialized_arg(int)), serialized_args:list(FunActionServer.serialized_arg(int)))=
        // dropping the server value for the serialized one
        fun_action_call(fun_action,tyargs,args) =
          ty_call = if tyargs == "" then "" else "({tyargs})"
          ty_call = "({fun_action}){ty_call}"
          arg_call = "{ty_call}({args})"
          event_apply = "{arg_call}(event)"
          event_apply

        get_serialized_args(l)=
           List.map( _.serialized_arg, l)
           |> String.concat(",",_)

        fun_action = serialize_fun_name(fun_action_name)
        args       = get_serialized_args(serialized_args)
        ty_args    = get_serialized_args(ty_ser_args)

        fun_action_call(fun_action, ty_args, args)

}}
