/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA.  If not, see <http://www.gnu.org/licenses/>.
*/

/**
 *
 * @author Hugo Venturini, Hugo Heuzard
 * @creation 12/2010
 *
 * @category internet, network
 * @destination public
 *
 */

/**
 * {1 About this module}
 *
 * This module is an IPv4 tool-box.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * The type of an IP address or a Netmask.
 */
@opacapi
type ip = IPv4.ip

type IPv4.ip = { a : int ; b : int ; c : int ; d : int }

/**
 * {1 Interface}
 */

IPv4 =
{{

  /**
   * {2 Parser }
   */

  ip_parser : Parser.general_parser(IPv4.ip) =
    parser s1=Rule.byte "." s2=Rule.byte "." s3=Rule.byte "." s4=Rule.byte -> {a=s1 b=s2 c=s3 d=s4}

  /**
   * {2 Conversion functions }
   */

  @stringifier(IPv4.ip)
  string_of_ip(ip : IPv4.ip) : string = "{ip.a}.{ip.b}.{ip.c}.{ip.d}"

  ip_of_string(ip : string) : IPv4.ip =
      Parser.parse(ip_parser, ip)

  ip_of_string_opt(ip : string) : option(IPv4.ip) =
      Parser.try_parse(ip_parser, ip)

}}
