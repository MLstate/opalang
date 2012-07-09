(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
*)

type miniupnp_err =
  | MINIUPNP_GENERIC_ERR of int ;;

let string_of_miniupnp_err = function
  | MINIUPNP_GENERIC_ERR i -> Printf.sprintf "MINIUPNP_GENERIC_ERR : '%d'" i ;;

module Helpers =
struct

  type result = Correct | Wrong of miniupnp_err ;;

  let string_of_protocol protocol =
    match protocol with
    | `Udp -> "UDP"
    | `Tcp -> "TCP" ;;

  let open_a_port ~ip ~internalPort ~externalPort ~protocol =
    let protocol = string_of_protocol protocol in
    let ires = Stubsminiupnpc.open_a_port ip externalPort internalPort protocol in
    match ires with
    | 0 -> Correct
    | _ -> Wrong (MINIUPNP_GENERIC_ERR ires) ;;

  let close_a_port ~ip ~externalPort ~protocol =
    let protocol = string_of_protocol protocol in
    let ires = Stubsminiupnpc.close_a_port ip (string_of_int externalPort) protocol in
    match ires with
    | 0 -> Correct
    | _ -> Wrong (MINIUPNP_GENERIC_ERR ires) ;;

end
