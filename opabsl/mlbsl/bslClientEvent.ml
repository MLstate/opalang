(*
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
*)
(*
    @author Francois Pessaux
*)



##extern-type ClientEvent.t = BslPingRegister.M.event

##extern-type ClientEventKey.t = BslPingRegister.M.event_key

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

##register remove_event : ClientEventKey.t -> void
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
  ,continuation(ClientEventKey.t) -> void
let register_event opt_tcc evt f k =
  let opt_tcc' =
    match opt_tcc with
    | None -> None
    | Some tcc -> Some (opa_tc_c_2_bsl_pr_c_k tcc) in
  let f x = f (bsl_pr_c_k_2_opa_tc_c x) (QmlCpsServerLib.ccont_ml k (fun _ -> ())) in
  QmlCpsServerLib.return k (Ping.register_event opt_tcc' evt f)
