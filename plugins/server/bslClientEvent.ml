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
    @author Francois Pessaux
*)

module BslUtils = OpabslgenMLRuntime.BslUtils


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
    BslNet.opa_threadcontext_client -> BslPingRegister.Client.key =
 "%identity"

(* ************************************************************************** *)
(* Magic conversion of a [BslPingRegister.Client.key] into a
   [BslUtils.opa_threadcontext_client]. Used by [register_event] below.       *)
(* ************************************************************************** *)
external bsl_pr_c_k_2_opa_tc_c :
    BslPingRegister.Client.key -> BslNet.opa_threadcontext_client =
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
