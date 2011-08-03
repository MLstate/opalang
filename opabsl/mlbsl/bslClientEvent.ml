(*
    Copyright © 2011 MLstate

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



##extern-type ClientEvent.t = BslPingRegister.M.event

##extern-type ClientEventKey.t = BslPingRegister.M.event_key

##register connect : ClientEvent.t
let connect = BslPingRegister.M.Connect

##register disconnect : ClientEvent.t
let disconnect = BslPingRegister.M.Disconnect

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

##register register_event : option(opa[ThreadContext.client]),     \
                            ClientEvent.t,                         \
                            (opa[ThreadContext.client] -> void) -> \
                                ClientEventKey.t
let register_event opt_tcc ce cb =
  let opt_tcc' =
    match opt_tcc with
    | None -> None
    | Some tcc -> Some (opa_tc_c_2_bsl_pr_c_k tcc) in
  let cb' x = cb (bsl_pr_c_k_2_opa_tc_c x) in
  BslPingRegister.M.register_event opt_tcc' ce cb'

##register remove_event : ClientEventKey.t -> void
let remove_event = BslPingRegister.M.remove_event

