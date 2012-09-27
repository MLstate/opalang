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

package stdlib.apis.github.search
import stdlib.apis.github
import stdlib.apis.github.lib

/**
 * GitHub user API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

type GitHub.search_issue = {
  html_url    : string
  state       : GitHub.state
  number      : int
  gravatar_id : string
  position    : float
  updated_at  : Date.date
  comments    : int
  labels      : list(string)
  title       : string
  user        : string
  created_at  : Date.date
  body        : string
  votes       : int
}

type GitHub.search_repo = {
  `type`      : string
  created     : Date.date
  owner       : string
  watchers    : int
  followers   : int
  username    : string
  pushed_at   : Date.date
  description : string
  created_at  : Date.date
  fork        : bool
  forks       : int
  size        : int
  name        : string
  `private`   : bool
  pushed      : Date.date
  language    : string
}

@private GHSp = {{

  @private GP = GHParse

  get_search_issue(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("title")
    then
      res = {
        html_url    = m.str("html_url")
        state       = GP.get_state(m)
        number      = m.int("number")
        gravatar_id = m.str("gravatar_id")
        position    = m.float("position")
        updated_at  = m.date("updated_at")
        comments    = m.int("comments")
        labels      = GP.get_list(m, "labels", GP.get_str_opt)
        title       = m.str("title")
        user        = m.str("user")
        created_at  = m.date("created_at")
        body        = m.str("body")
        votes       = m.int("votes")
      } : GitHub.search_issue
      {some=res}
    else {none}

  get_search_repo(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("name")
    then
      res = {
        `type`      = m.str("type")
        created     = m.date("created")
        owner       = m.str("owner")
        watchers    = m.int("watchers")
        followers   = m.int("followers")
        username    = m.str("username")
        pushed_at   = m.date("pushed_at")
        description = m.str("description")
        created_at  = m.date("created_at")
        fork        = m.bool("fork")
        forks       = m.int("forks")
        size        = m.int("size")
        name        = m.str("name")
        `private`   = m.bool("private")
        pushed      = m.date("pushed")
        language    = m.str("language")
      } : GitHub.search_repo
      {some=res}
    else {none}

  // Legacies from v2
  issues(res) = GP.dewrap_list(res, "issues", get_search_issue)
  repositories(res) = GP.dewrap_list(res, "repositories", get_search_repo)
  users(res) = GP.dewrap_list(res, "users", GP.get_user)
  user(res) = GP.dewrap_obj(res, "user", GP.get_user)

}}

GHSearch = {{

  @private GP = GHParse

  search_issues(token:string, owner:string, repository:string, state:GitHub.state, keyword:string) =
    path = "/legacy/issues/search/{owner}/{repository}/{GHLib.string_of_state(state)}/{keyword}"
    GHLib.api_get_full(path, token, [], GHSp.issues)

  search_repositories(token:string, keyword:string, language:option(string), start_page:option(int)) =
    options = List.flatten([GHLib.opt_string("language",language),GHLib.opt_int("start_page",start_page)])
    GHLib.api_get_full("/legacy/repos/search/{keyword}", token, options, GHSp.repositories)

  search_users(token:string, keyword:string) =
    GHLib.api_get_full("/legacy/user/search/{keyword}", token, [], GHSp.users)

  search_email(token:string, email:string) =
    GHLib.api_get_full("/legacy/user/email/{email}", token, [], GHSp.user)

}}
