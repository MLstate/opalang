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
/*
 * Author    : Nicolas Glondu <nicolas.glondu@mlstate.com>
 **/

/**
 * Facebook library modules
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

import stdlib.apis.common
import stdlib.apis.facebook
import stdlib.apis.facebook.lib
import stdlib.apis.facebook.dialog

/**
 * {1 About this module}
 *
 * This module provides simple functions to authenticate users and applications.
 * Important :
 *   Any application using this must respect
 *   {{:http://developers.facebook.com/policy}Facebook policy}
 *
 * {1 Where should I start?}
 *
 * You should first {{:https://www.facebook.com/developers/}register an app}
 * to get a application id, an api key and an application secret. You will
 * also have to configure various parameters on your application.
 *
 * Here is the user authentication process :
 * 1) A user visits your website peacefully
 * 2) He clicks on a link that looks like "Connect with Facebook"
 * 3a) If he has not accepted your application before, he is prompted to accept it
 * or not.
 * 4aa) If he accepts your application, he is redirected to your website on an address
 * you can configure with a "code" in GET parameters
 * 5aa) You submit this code with your application secret to Facebook servers and earn
 * an access token with an expiration date.
 * 4ab) If he rejects your application, he is redirected to the same address with an
 * "error" as a GET paramter. Too bad, you should try asking less permissions.
 * 3b) If he has already accepted your application before, the user is directly
 * redirected to your website with a "code" tou can transform in an application token.
 *
 * To build the url of state 2, you should use [FbAuth.user_login_url]. Once the
 * user is redirected to your webste, you can pass all GET data to [FbAuth.get_token_raw]
 * to obtain an access token or an error if he refused.
 */

/**
 * Parsed result of a token request or an app login in case of success.
 * - [token] An access for current user/application
 * - [expires_in] Duration (in seconds) of token validity (0 means that the token
 *   will not expire)
 */
type FbAuth.token = {
  token      : string
  expires_in : int
}

/**
 * Parsed result of a token request or an app login
 */
type FbAuth.token_res =
  { token : FbAuth.token } / { error : Facebook.error }


FbAuth(conf:Facebook.config) = {{

  /* Static */

  @private graph_url  = "https://graph.facebook.com"
  @private Dialog = FbDialog(conf)

  /**
   * Construction of the url of a login box or a permission request box
   *
   * @param permissions A list of [FbGraph.permission] requested to the user.
   * @param redirect_uri Address where the user will be redirected after
   * accepting (or not). Domain must match the domain configured for
   * your application on Facebook.
   *
   * @return An url where the user should be be directed to login or to
   * request more permissions.
   */
  user_login_url(permissions, redirect_uri) =
    options = {Dialog.default_oauth with scope=permissions}
    Dialog.full_oauth(options, redirect_uri, {page})

  @private access_token_internal(data) =
    match FbLib.fb_get(graph_url, "/oauth/access_token", data) with
    | {none} -> { error = Facebook.network_error }
    | {some=r} ->
      data = API_libs.get_data(r)
      token = API_libs.get_field(data, "access_token")
      if token == "" then
        { error = Facebook.make_error("invalid_code", "Invalid verification code") }
      else
        { token = { token = API_libs.get_field(data, "access_token")
                    expires_in = Int.of_string(API_libs.get_field_def(data, "expires", "0")) } }

  /**
   * Gets an access token from an access code. Requesting several times an
   * access token from one code will always return the same token with
   * the same expiration date.
   *
   * @param code Access code to transform
   * @param redirect_uri Address where the user will be redirected after
   * accepting (or not). Domain must match the domain configured for
   * your application on Facebook.
   *
   * @return A [FbAuth.token_res] with either a token or
   * an error in case of failure.
   */
  get_token_from_code(code, redirect_uri) : FbAuth.token_res =
    if code == "" then { error = Facebook.data_error }
    else
      data = [ ("client_id",conf.app_id), ("client_secret",conf.app_secret)
             , ("redirect_uri",redirect_uri), ("code", code) ]
      access_token_internal(data)

  /**
   * Gets an access token directly from the data provided whitn the user
   * redirection after a [user_login_url]. Requesting several times an
   * access token from the same data will always return the same token with
   * the same expiration date. If the permission [offline_access] is granted,
   * the token will not expire.
   *
   * @param rawdata Data provided with user redirection.
   * @param redirect_uri Address where the user will be redirected after
   * accepting (or not). Domain must match the domain configured for
   * your application on Facebook.
   *
   * @return A [FbAuth.token_res] with either a token or
   * an error in case of failure
   */
  get_token_raw(rawdata, redirect_uri) : FbAuth.token_res =
    match Dialog.oauth_res(rawdata) with
    | { error=e } -> { error=e }
    | { code=c } -> get_token_from_code(c.code, redirect_uri)

  /**
   * Gets an application access token. Such token do not expires.
   *
   * @return A [FbAuth.token_res] with either a token or
   * an error in case of failure
   */
  app_login() =
    data = [ ("client_id",conf.app_id), ("client_secret",conf.app_secret)
           , ("grant_type","client_credentials") ]
    access_token_internal(data)

}}
