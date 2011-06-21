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
 * {1 Actual utilities}
 */

type web_side = {client} / {server}

@both WebUtils = {{
    /**
     * Determine if the code is executed on the client or the server.
     *
     * @return [{client}] if the code is executed on the client, [{server}] if it is executed on the server.
     */
    get_side(): web_side  = @sliced_expr({server = {server} client = {client}})
    is_server() = @sliced_expr({server = true client = false})
    is_client() = @sliced_expr({server = false client = true})

    /**
     * Build a web_response from its status code
     * Supported codes are :
     * 100 101
     * 200 to 206
     * 300 to 305 and 307
     * 400 to 417
     * 500 to 505
     *
     * e.g.: web_response_of_code(404) = {some={wrong_address}}
     */
    web_response_of_code(code) : option(web_response) =
      match code with
      | 100 -> {some={continue}}
      | 101 -> {some={switching_protocols}}

      | 200 -> {some={success}}
      | 201 -> {some={created}}
      | 202 -> {some={accepted}}
      | 203 -> {some={non_authoritative_information}}
      | 204 -> {some={no_content}}
      | 205 -> {some={reset_content}}
      | 206 -> {some={partial_content}}

      | 300 -> {some={multiple_choices}}
      | 301 -> {some={address_moved}}
      | 302 -> {some={found}}
      | 303 -> {some={see_other}}
      | 304 -> {some={not_modified}}
      | 305 -> {some={use_proxy}}
      | 307 -> {some={address_redirected}}

      | 400 -> {some={bad_request}}
      | 401 -> {some={unauthorized}}
      | 402 -> {some={payment_required}}
      | 403 -> {some={forbidden}}
      | 404 -> {some={wrong_address}}
      | 405 -> {some={method_not_allowed}}
      | 406 -> {some={not_acceptable}}
      | 407 -> {some={proxy_authentication_required}}
      | 408 -> {some={request_timeout}}
      | 409 -> {some={conflict}}
      | 410 -> {some={gone}}
      | 411 -> {some={length_required}}
      | 412 -> {some={precondition_failed}}
      | 413 -> {some={request_entity_too_large}}
      | 414 -> {some={request_uri_too_large}}
      | 415 -> {some={unsupported_media_type}}
      | 416 -> {some={requested_range_not_satisfiable}}
      | 417 -> {some={expectation_failed}}

      | 500 -> {some={internal_server_error}}
      | 501 -> {some={not_implemented}}
      | 502 -> {some={bad_gateway}}
      | 503 -> {some={service_unavailable}}
      | 504 -> {some={gateway_timeout}}
      | 505 -> {some={http_version_not_supported}}

      | _ -> {none}


}}
