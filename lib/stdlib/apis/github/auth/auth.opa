/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/*
 * Author    : Nicolas Glondu <nicolas.glondu@mlstate.com>
 **/

/**
 * GitHub authentication API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

//package stdlib.apis.github.auth
import stdlib.apis.common
import stdlib.apis.github.lib

type GHAuth.conf = {
  client_id : string
  secret    : string
}

type GHAuth.scope =
    {user}        /** DB read/write access to profile info only. */
  / {public_repo} /** DB read/write access, and Git read access to public repos. */
  / {repo}        /** DB read/write access, and Git read access to public and private repos */
  / {gist}        /** Write access to gists. */

@private GHAp = {{

  scope_to_string(s) =
    match s:GHAuth.scope with
    | {user}        -> "user"
    | {public_repo} -> "public_repo"
    | {repo}        -> "repo"
    | {gist}        -> "gist"

}}

GHAuth(conf:GHAuth.conf) = {{

    @private AL = API_libs
    @private c = conf

    user_login_url(scope, redirect_uri) =
      scope_text =
        List.map(GHAp.scope_to_string, scope)
        |> String.implode(identity,",", _)
      args = [("client_id", c.client_id),
              ("redirect_uri", redirect_uri),
              ("scope", scope_text)]
      base = "https://github.com/login/oauth/authorize"
      "{base}?{AL.form_urlencode(args)}"

    get_token(rawdata, redirect_uri) =
      base = "https://github.com"
      path = "/login/oauth/access_token"
      code = AL.get_field(AL.get_data(rawdata), "code")
      data = [("client_id", c.client_id),
              ("redirect_uri", redirect_uri),
              ("client_secret", c.secret),
              ("code", code)]
      match GHLib.full_post(base, path, data, some) with
      | {some=c} ->
        token = AL.get_field(AL.get_data(c), "access_token")
        if token == "" then {none}
        else {some=token}
      | _ -> {none}

}}
