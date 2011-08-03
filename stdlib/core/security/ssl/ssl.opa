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
    @author David Rajchenbach-Teller
**/

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

  get_issuer: SSL.certificate -> string = %% BslNet.Ssl.get_issuer %%
  get_subject:SSL.certificate -> string = %% BslNet.Ssl.get_subject %%
}}
