(*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)
(*
    @author Francois Pessaux
*)

module BslUtils = OpabslMLRuntime.BslUtils


##extern-type ClientEvent.t = BslPingRegister.M.event

##extern-type ClientEvent.key = BslPingRegister.M.event_key

##register connect : ClientEvent.t
let connect = BslPingRegister.M.Connect

##register disconnect : ClientEvent.t
let disconnect = BslPingRegister.M.Disconnect

##register inactive : ClientEvent.t
let inactive = BslPingRegister.M.Inactive

(* ************************************************************************** *)
(* Magic conversion of a [BslUtils.opa_threadcontext_client] into a
   [BslPingRegister.Client.key]. Used by [register_event] below.              *)
(* ************************************************************************** *)
external opa_tc_c_2_bsl_pr_c_k :
    BslUtils.opa_threadcontext_client -> BslPingRegister.Client.key =
 "%identity"

(* ************************************************************************** *)
(* Magic conversion of a [BslPingRegister.Client.key] into a
   [BslUtils.opa_threadcontext_client]. Used by [register_event] below.       *)
(* ************************************************************************** *)
external bsl_pr_c_k_2_opa_tc_c :
    BslPingRegister.Client.key -> BslUtils.opa_threadcontext_client =
 "%identity"

module Ping = BslPingRegister.M
module Client = BslPingRegister.Client

##register remove_event : ClientEvent.key -> void
let remove_event = Ping.remove_event

##register set_inactive_delay : option(opa[ThreadContext.client]),     \
  option(int) -> void
let set_inactive_delay opt_tcc opt_time =
  let t = Option.map Time.milliseconds opt_time in
  let opt_tcc' =
    match opt_tcc with
    | None -> None
    | Some tcc -> Some (opa_tc_c_2_bsl_pr_c_k tcc) in
  Ping.set_inactive_delay opt_tcc' t



##register [cps-bypass] register_event : option(opa[ThreadContext.client])\
  ,ClientEvent.t\
  ,(opa[ThreadContext.client], continuation(opa[void]) -> void)\
  ,continuation(ClientEvent.key) -> void
let register_event opt_tcc evt f k =
  let opt_tcc' =
    match opt_tcc with
    | None -> None
    | Some tcc -> Some (opa_tc_c_2_bsl_pr_c_k tcc) in
  let f x = f (bsl_pr_c_k_2_opa_tc_c x) (QmlCpsServerLib.ccont_ml k (fun _ -> ())) in
  QmlCpsServerLib.return k (Ping.register_event opt_tcc' evt f)
