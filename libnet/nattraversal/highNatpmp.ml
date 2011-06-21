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

let string_of_in_addr_t x = Printf.sprintf "%lu" x;;
let string_of_in_addr addr = string_of_in_addr_t addr ;;

let get_gateway_of_natpmp_t x = Stubsnatpmp.get_gateway_of_natpmp_t x ;;

let generate_in_addr () = 
  match Stubsnatpmp.generate_in_addr () with
  | None -> assert false
  | Some res -> res ;;

let in_addr_of_string s =
  match Stubsnatpmp.inet_aton s (generate_in_addr ()) with
  | (0, _) -> failwith "in_addr_of_string" 
  | (_, res) -> res ;;

let pretty_string_of_in_addr addr = Stubsnatpmp.inet_ntoa addr;;

let generate_natpmp_t () = 
  match Stubsnatpmp.generate_natpmp_t () with
  | None -> failwith "generate_natpmp_t"
  | Some res -> res ;;

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

let string_of_natpmp_err = function
  |  NATPMP_ERR_INVALIDARGS -> "NATPMP_ERR_INVALIDARGS"
  |  NATPMP_ERR_SOCKETERROR -> "NATPMP_ERR_SOCKETERROR"
  |  NATPMP_ERR_CANNOTGETGATEWAY -> "NATPMP_ERR_CANNOTGETGATEWAY"
  |  NATPMP_ERR_CLOSEERR -> "NATPMP_ERR_CLOSEERR"
  |  NATPMP_ERR_RECVFROM -> "NATPMP_ERR_RECVFROM"
  |  NATPMP_ERR_NOPENDINGREQ -> "NATPMP_ERR_NOPENDINGREQ"
  |  NATPMP_ERR_NOGATEWAYSUPPORT -> "NATPMP_ERR_NOGATEWAYSUPPORT"
  |  NATPMP_ERR_CONNECTERR -> "NATPMP_ERR_CONNECTERR"
  |  NATPMP_ERR_WRONGPACKETSOURCE -> "NATPMP_ERR_WRONGPACKETSOURCE"
  |  NATPMP_ERR_SENDERR -> "NATPMP_ERR_SENDERR"
  |  NATPMP_ERR_FCNTLERROR -> "NATPMP_ERR_FCNTLERROR"
  |  NATPMP_ERR_GETTIMEOFDAYERR -> "NATPMP_ERR_GETTIMEOFDAYERR"
  |  NATPMP_ERR_UNSUPPORTEDVERSION -> "NATPMP_ERR_UNSUPPORTEDVERSION"
  |  NATPMP_ERR_UNSUPPORTEDOPCODE -> "NATPMP_ERR_UNSUPPORTEDOPCODE"
  |  NATPMP_ERR_UNDEFINEDERROR -> "NATPMP_ERR_UNDEFINEDERROR"
  |  NATPMP_ERR_NOTAUTHORIZED -> "NATPMP_ERR_NOTAUTHORIZED"
  |  NATPMP_ERR_NETWORKFAILURE -> "NATPMP_ERR_NETWORKFAILURE"
  |  NATPMP_ERR_OUTOFRESOURCES -> "NATPMP_ERR_OUTOFRESOURCES"
  |  NATPMP_TRYAGAIN -> "NATPMP_TRYAGAIN" ;;

exception NATPMP_ERR of (int * natpmp_err) ;;

let string_of_exn_natpmp_err exn =
  match exn with
  | NATPMP_ERR (low_err_num, err) ->
      Printf.sprintf "NATPMP_ERR (low:'%d') : %s" low_err_num (string_of_natpmp_err err)
  | e -> Printexc.to_string e ;;

let polyv_of_int i = 
  let c = 
    match i with 
    | -1 -> NATPMP_ERR_INVALIDARGS
    | -2 -> NATPMP_ERR_SOCKETERROR
    | -3 -> NATPMP_ERR_CANNOTGETGATEWAY
    | -4 -> NATPMP_ERR_CLOSEERR
    | -5 -> NATPMP_ERR_RECVFROM
    | -6 -> NATPMP_ERR_NOPENDINGREQ
    | -7 -> NATPMP_ERR_NOGATEWAYSUPPORT
    | -8 -> NATPMP_ERR_CONNECTERR
    | -9 -> NATPMP_ERR_WRONGPACKETSOURCE
    | -10 -> NATPMP_ERR_SENDERR
    | -11 -> NATPMP_ERR_FCNTLERROR
    | -12 -> NATPMP_ERR_GETTIMEOFDAYERR
    | -14 -> NATPMP_ERR_UNSUPPORTEDVERSION
    | -15 -> NATPMP_ERR_UNSUPPORTEDOPCODE
    | -49 -> NATPMP_ERR_UNDEFINEDERROR
    | -51 -> NATPMP_ERR_NOTAUTHORIZED
    | -52 -> NATPMP_ERR_NETWORKFAILURE
    | -53 -> NATPMP_ERR_OUTOFRESOURCES
    | -100 -> NATPMP_TRYAGAIN
    | _ -> 
	failwith (Printf.sprintf "exception_of_int, unregistered c err return value '%d'" i) in
  (i, c) ;;

let initnatpmp forcegateway = 
  let (force, gateway) = 
    match forcegateway with
    | None -> (0, Int32.of_int 0)
    | Some g -> (1, g) in
  let tmp = Stubsnatpmp.initnatpmp (Some (generate_natpmp_t ())) force gateway in
  match tmp with
  | (0, Some res) -> res 
  | (0, None) -> assert false
  | (i, _) -> 
      let p = polyv_of_int i in
      match snd p with
      | NATPMP_ERR_INVALIDARGS | NATPMP_ERR_SOCKETERROR | NATPMP_ERR_FCNTLERROR 
      | NATPMP_ERR_CANNOTGETGATEWAY | NATPMP_ERR_CONNECTERR -> raise (NATPMP_ERR p)
      | _ -> assert false ;;

let closenatpmp r = 
  match Stubsnatpmp.closenatpmp (Some r) with
  | (0, Some _) -> ()
  | (0, _) -> assert false
  | (i, _) -> 
      let p = polyv_of_int i in
      match snd p with
      | NATPMP_ERR_INVALIDARGS | NATPMP_ERR_CLOSEERR -> raise (NATPMP_ERR p)
      | _ -> assert false ;;


let sendpublicaddressrequest r = 
  match Stubsnatpmp.sendpublicaddressrequest (Some r) with
  | (2, Some res) -> res
  | (2, None) -> assert false
  | (i, _) -> 
      let p = polyv_of_int i in
      match snd p with
      | NATPMP_ERR_INVALIDARGS | NATPMP_ERR_SENDERR -> raise (NATPMP_ERR p)
      | _ -> assert false ;;

let sendnewportmappingrequest r protocol privateport publicport lifetime = 
  match Stubsnatpmp.sendnewportmappingrequest (Some r) protocol privateport publicport lifetime with
  | (12, Some res) -> res
  | (12, None) -> assert false
  | (i, _) -> 
      let p = polyv_of_int i in
      match snd p with
      | NATPMP_ERR_INVALIDARGS | NATPMP_ERR_SENDERR -> raise (NATPMP_ERR p)
      | _ -> assert false ;;

let getnatpmprequesttimeout r = 
  match Stubsnatpmp.getnatpmprequesttimeout (Some r) with
  | (0, Some r, Some res) -> (r, res)
  | (0, _, _) -> assert false
  | (i, _, _) -> 
      let p = polyv_of_int i in
      match snd p with
      | NATPMP_ERR_INVALIDARGS | NATPMP_ERR_GETTIMEOFDAYERR 
      | NATPMP_ERR_NOPENDINGREQ -> raise (NATPMP_ERR p)
      | _ -> assert false ;;

let readnatpmpresponseorretry r =
  match Stubsnatpmp.readnatpmpresponseorretry (Some r) with
  | (0, Some r, Some res) -> (r, res)
  | (0, _, _) -> assert false
  | (i, _, _) -> 
      let p = polyv_of_int i in
      match snd p with
      | NATPMP_TRYAGAIN
      | NATPMP_ERR_INVALIDARGS
      | NATPMP_ERR_NOPENDINGREQ
      | NATPMP_ERR_NOGATEWAYSUPPORT
      | NATPMP_ERR_RECVFROM
      | NATPMP_ERR_WRONGPACKETSOURCE
      | NATPMP_ERR_UNSUPPORTEDVERSION
      | NATPMP_ERR_UNSUPPORTEDOPCODE
      | NATPMP_ERR_NOTAUTHORIZED
      | NATPMP_ERR_NETWORKFAILURE
      | NATPMP_ERR_OUTOFRESOURCES
      | NATPMP_ERR_UNDEFINEDERROR -> raise (NATPMP_ERR p)
      | _ -> assert false ;;
      

module Helpers = struct

  type result = Correct of int | Wrong of (int * natpmp_err);;

  let open_a_port ?forced_gateway ~protocol ~private_port ~public_port ~lifetime =
    try
      let r = initnatpmp forced_gateway in
      let r = sendnewportmappingrequest r protocol private_port public_port lifetime in
      let (r, _) = getnatpmprequesttimeout r in
      let (r,response) = readnatpmpresponseorretry r in (* to trigger exception in case of failure *)
      let result =
	match response.Stubsnatpmp.pnu with
	| Stubsnatpmp.NATPMP_RESPTYPE_UDPPORTMAPPING z
	| Stubsnatpmp.NATPMP_RESPTYPE_TCPPORTMAPPING z ->
	    z.Stubsnatpmp.mappedpublicport
	| _ -> assert false
      in
      let () = closenatpmp r in
      Correct result
    with
    | NATPMP_ERR z -> Wrong z ;;

  let close_a_port ~protocol ~private_port ~public_port =
    try
      let result = open_a_port ?forced_gateway:None ~protocol ~private_port
	~public_port ~lifetime:(Int32.of_int 0) in
      match result with
      | Correct _ -> true
      | Wrong _ -> false
    with
    | NATPMP_ERR _ -> false ;;

end
