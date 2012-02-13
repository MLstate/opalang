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
