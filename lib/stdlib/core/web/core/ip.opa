/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
    parser
    | "::1" -> {a=127 b=0 c=0 d=1} // Loopback address (in hybrid IP notation).
    | "::ffff:" ? s1=Rule.byte "." s2=Rule.byte "." s3=Rule.byte "." s4=Rule.byte -> {a=s1 b=s2 c=s3 d=s4}

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
