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
