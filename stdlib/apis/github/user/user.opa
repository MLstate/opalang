/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

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

//package stdlib.apis.github.user
import stdlib.apis.github
import stdlib.apis.github.lib

/**
 * GitHub user API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

/**
 * Type of a GitHub user provided to [GHUser.show]
 */
type GHUser.t =
    { self  : string } /** Token of user - provides more information */
  / { login : string } /** Login of user */

/* Types returned by API */

type GitHub.plan = {
  name          : string
  collaborators : int
  space         : int
  private_repos : int
}

type GitHub.user_more = {
  total_private_repo_count : int
  collaborators            : int
  disk_usage               : int
  owned_private_repo_count : int
  private_gist_count       : int
  plan                     : GitHub.plan
}

type GitHub.user = {
  id                : int
  login             : string
  name              : string
  company           : string
  gravatar_id       : string
  created_at        : gdate
  location          : string
  blog              : string
  public_repo_count : int
  public_gist_count : int
  followers_count   : int
  following_count   : int
  user_type         : string
  more              : option(GitHub.user_more)
}

@private GHUp = {{

  @private GP = GHParse

  get_plan(srcmap) =
    m = GP.map_funs(srcmap)
    { name          = m.str("name")
      collaborators = m.int("collaborators")
      space         = m.int("space")
      private_repos = m.int("private_repos")
    } : GitHub.plan

  get_more(m) =
    if m.exists("total_private_repo_count") then
      user_more = {
        total_private_repo_count = m.int("total_private_repo_count")
        collaborators            = m.int("collaborators")
        disk_usage               = m.int("disk_usage")
        owned_private_repo_count = m.int("owned_private_repo_count")
        private_gist_count       = m.int("private_gist_count")
        plan                     = get_plan(m.record("plan"))
      } : GitHub.user_more
      {some=user_more}
    else {none}

  get_user(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id") then
      id = match m.record("id") with
        | {Int=i} -> i
        | {String=s} ->
          String.explode("-",s) |> List.rev
          |> List.head |> Int.of_string
        | _ -> 0
      res = {
        id                = id
        login             = m.str("login")
        name              = m.str("name")
        company           = m.str("company")
        gravatar_id       = m.str("gravatar_id")
        created_at        = m.date("created_at")
        location          = m.str("location")
        blog              = m.str("blog")
        public_repo_count = m.int("public_repo_count")
        public_gist_count = m.int("public_gist_count")
        followers_count   = m.int("followers_count")
        following_count   = m.int("following_count")
        user_type         = m.str("user_type")
        more              = get_more(m)
      } : GitHub.user
      {some=res}
    else {none}

  one_full_user(res) =
    GP.dewrap_obj(res, "user", get_user)

  multiple_full_users(res) =
    GP.dewrap_list(res, "users", get_user)

  multiple_users_ids(res) =
    GP.multiple_strings(res, "users")

  multiple_emails(res) =
    GP.multiple_strings(res, "emails")

  multiple_public_keys(res) =
    GP.dewrap_list(res, "public_keys", GP.get_public_key)

}}

GHUser = {{

  /* Search and display */

  search(req) =
    path = "/user/search/{req}"
    GHLib.api_get(path, [], GHUp.multiple_full_users)

  show(user:GHUser.t) =
    (path, data) = match user with
      | ~{self} ->
        ("/user/show", [("access_token", self)])
      | ~{login} ->
        ("/user/show/{login}", [])
    GHLib.api_get(path, data, GHUp.one_full_user)

  /* Follow(/ing/ers) */

  get_following(user) =
    path = "/user/show/{user}/following"
    GHLib.api_get(path, [], GHUp.multiple_users_ids)

  get_followers(user) =
    path = "/user/show/{user}/followers"
    GHLib.api_get(path, [], GHUp.multiple_users_ids)

  follow(user, token) =
    path = "/user/follow/{user}"
    GHLib.api_post_logged(path, token, GHUp.multiple_users_ids)

  unfollow(user, token) =
    path = "/user/unfollow/{user}"
    GHLib.api_post_logged(path, token, GHUp.multiple_users_ids)

  /* Public keys */

  get_keys(token) =
    path = "/user/keys"
    GHLib.api_get_logged(path, token, GHUp.multiple_public_keys)

  add_key(title, key, token) =
    path = "/user/key/add"
    data = [("title", title), ("key", key),
            ("access_token", token)]
    GHLib.api_post(path, data, GHUp.multiple_public_keys)

  remove_key(id, token) =
    path = "/user/key/remove"
    data = [("id", id), ("access_token", token)]
    GHLib.api_post(path, data, GHUp.multiple_public_keys)

  /* Emails */

  get_emails(token) =
    path = "/user/emails"
    GHLib.api_get_logged(path, token, GHUp.multiple_emails)

  add_email(email, token) =
    path = "/user/email/add"
    data = [("email", email), ("access_token", token)]
    GHLib.api_post(path, data, GHUp.multiple_emails)

  remove_email(email, token) =
    path = "/user/email/remove"
    data = [("email", email), ("access_token", token)]
    GHLib.api_post(path, data, GHUp.multiple_emails)

  /* Not working API call
   update(updates, token) =
   */

}}
