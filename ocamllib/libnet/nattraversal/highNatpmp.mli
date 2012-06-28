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
val string_of_in_addr_t : Stubsnatpmp.in_addr_t -> string ;;
val string_of_in_addr : Stubsnatpmp.in_addr -> string ;;

val get_gateway_of_natpmp_t : Stubsnatpmp.natpmp_t -> Stubsnatpmp.in_addr_t ;;
val generate_in_addr : unit -> Stubsnatpmp.in_addr ;;

val in_addr_of_string : string -> Stubsnatpmp.in_addr ;;
val pretty_string_of_in_addr : Stubsnatpmp.in_addr -> string ;;

val generate_natpmp_t : unit -> Stubsnatpmp.natpmp_t ;;

type natpmp_err = 
  |  NATPMP_ERR_INVALIDARGS
  |  NATPMP_ERR_SOCKETERROR
  |  NATPMP_ERR_CANNOTGETGATEWAY
  |  NATPMP_ERR_CLOSEERR
  |  NATPMP_ERR_RECVFROM
  |  NATPMP_ERR_NOPENDINGREQ
  |  NATPMP_ERR_NOGATEWAYSUPPORT
  |  NATPMP_ERR_CONNECTERR
  |  NATPMP_ERR_WRONGPACKETSOURCE
  |  NATPMP_ERR_SENDERR
  |  NATPMP_ERR_FCNTLERROR
  |  NATPMP_ERR_GETTIMEOFDAYERR
  |  NATPMP_ERR_UNSUPPORTEDVERSION
  |  NATPMP_ERR_UNSUPPORTEDOPCODE
  |  NATPMP_ERR_UNDEFINEDERROR
  |  NATPMP_ERR_NOTAUTHORIZED
  |  NATPMP_ERR_NETWORKFAILURE
  |  NATPMP_ERR_OUTOFRESOURCES
  |  NATPMP_TRYAGAIN ;;

val string_of_natpmp_err : natpmp_err -> string ;;

exception NATPMP_ERR of (int * natpmp_err) ;;

val string_of_exn_natpmp_err : exn -> string ;;

(*
 * NATPMP_ERR_INVALIDARGS
 * NATPMP_ERR_SOCKETERROR
 * NATPMP_ERR_FCNTLERROR
 * NATPMP_ERR_CANNOTGETGATEWAY
 * NATPMP_ERR_CONNECTERR *)
val initnatpmp : Stubsnatpmp.in_addr_t option -> Stubsnatpmp.natpmp_t ;;

(*
 * 0 = OK
 * NATPMP_ERR_INVALIDARGS
 * NATPMP_ERR_CLOSEERR *)
val closenatpmp : Stubsnatpmp.natpmp_t -> unit ;;

(*
 * 2 = OK (size of the request)
 * NATPMP_ERR_INVALIDARGS
 * NATPMP_ERR_SENDERR *)
val sendpublicaddressrequest : Stubsnatpmp.natpmp_t -> Stubsnatpmp.natpmp_t ;;

(* 0 = OK
 * NATPMP_ERR_INVALIDARGS
 * NATPMP_ERR_GETTIMEOFDAYERR
 * NATPMP_ERR_NOPENDINGREQ *)
val getnatpmprequesttimeout : Stubsnatpmp.natpmp_t -> Stubsnatpmp.natpmp_t * Stubsnatpmp.timeval ;;


(* 
 * 12 = OK (size of the request)
 * NATPMP_ERR_INVALIDARGS
 * NATPMP_ERR_SENDERR *)
val sendnewportmappingrequest : Stubsnatpmp.natpmp_t -> Stubsnatpmp.switch_protocol -> Stubsnatpmp.uint16_t -> Stubsnatpmp.uint16_t -> Stubsnatpmp.uint32_t -> Stubsnatpmp.natpmp_t ;;

(* 0 = OK
 * NATPMP_TRYAGAIN
 * NATPMP_ERR_INVALIDARGS
 * NATPMP_ERR_NOPENDINGREQ
 * NATPMP_ERR_NOGATEWAYSUPPORT
 * NATPMP_ERR_RECVFROM
 * NATPMP_ERR_WRONGPACKETSOURCE
 * NATPMP_ERR_UNSUPPORTEDVERSION
 * NATPMP_ERR_UNSUPPORTEDOPCODE
 * NATPMP_ERR_NOTAUTHORIZED
 * NATPMP_ERR_NETWORKFAILURE
 * NATPMP_ERR_OUTOFRESOURCES
 * NATPMP_ERR_UNSUPPORTEDOPCODE
 * NATPMP_ERR_UNDEFINEDERROR *)
val readnatpmpresponseorretry : Stubsnatpmp.natpmp_t -> Stubsnatpmp.natpmp_t * Stubsnatpmp.natpmpresp_t ;;

module Helpers : sig

  type result = Correct of int (* public port *) | Wrong of (int * natpmp_err) ;;

  val open_a_port : ?forced_gateway:Stubsnatpmp.in_addr_t -> protocol:Stubsnatpmp.switch_protocol
    -> private_port:int -> public_port:int -> lifetime:int32 -> result ;;

  val close_a_port : protocol:Stubsnatpmp.switch_protocol -> private_port:int -> public_port:int -> bool ;;

end ;;
