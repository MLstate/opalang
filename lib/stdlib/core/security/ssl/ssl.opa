/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
    @author David Rajchenbach-Teller
**/

import-plugin server

/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * The information required to prove identity to an interlocutor
 */
type SSL.private_key = external
type SSL.policy      = external
type SSL.secure_type = external
/**
 * A SSL certificate received from an interlocutor
 *
 * The only way to receive such a certificate is through the use of a [SSL.policy]
 */
type SSL.certificate = external

//To create a certificate or a policy, use directives @ssl_static_certificate and @ssl_static_policy

/**
 * {1 Interface}
 */

SSL =
{{
  make_private_key: option(string), option(string), string, string, string -> SSL.private_key =
    %% BslNet.SSL.make_key %%
  make_policy: option(string), option(SSL.certificate -> bool), bool, string, string, string -> SSL.policy =
    %% BslNet.SSL.make_policy %%
  make_secure_type: option(SSL.private_key), option(SSL.policy) -> SSL.secure_type =
    %% BslNet.SSL.make_secure_type %%

  // get_issuer: SSL.certificate -> string = %% BslNet.Ssl.get_issuer %%
  // get_subject:SSL.certificate -> string = %% BslNet.Ssl.get_subject %%
}}
