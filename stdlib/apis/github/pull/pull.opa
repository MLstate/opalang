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

//package stdlib.apis.github.pull
import stdlib.apis.github
import stdlib.apis.github.lib

/**
 * GitHub pull request API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */


type GitHub.Pull.create_params = {
  base : string
  head : string
  description : {title:string body:string}/{issue:string}
}

/* Types returned by API */

type GitHub.user_simple = {
  login       : string
  name        : string
  gravatar_id : string
  user_type   : string
}

type GitHub.commit = {
  label      : string
  ref        : string
  sha        : string
  repository : GitHub.repository
  user       : GitHub.user_simple
}

type GitHub.pull_req = {
  title            : string
  body             : string
  user             : GitHub.user_simple
  comments         : int
  votes            : int
  state            : {open}/{closed:Date.date}/{other:string}
  position         : float
  number           : int
  head             : GitHub.commit
  base             : GitHub.commit
  gravatar_id      : string
  diff_url         : string
  html_url         : string
  patch_url        : string
  issue_created_at : Date.date
  issue_updated_at : Date.date
  created_at       : Date.date
  updated_at       : Date.date
  merged_at        : Date.date
//  labels           : list(string) ???
//  mergeable        : string ???
}

@private GHPp = {{

  @private GP = GHParse

  get_user_simple(srcmap) =
    m = GP.map_funs(srcmap)
    { login       = m.str("login")
      name        = m.str("name")
      gravatar_id = m.str("gravatar_id")
      user_type   = m.str("user_type")
    } : GitHub.user_simple

  get_commit(srcmap) =
    m = GP.map_funs(srcmap)
    { label      = m.str("label")
      ref        = m.str("ref")
      sha        = m.str("sha")
      repository = m.record("repository")
                   |> GP.get_repo(true) |> Option.get
      user       = get_user_simple(m.record("user"))
    } : GitHub.commit

  get_pull_req(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("state") then
      state = match m.str("state") with
        | "open"   -> {open}
        | "closed" -> {closed=m.date("closed_at")}
        | x        -> {other=x}
      res = {
        title            = m.str("title")
        body             = m.str("body")
        user             = get_user_simple(m.record("user"))
        comments         = m.int("comments")
        votes            = m.int("votes")
        state            = state
        position         = m.float("position")
        number           = m.int("number")
        head             = get_commit(m.record("head"))
        base             = get_commit(m.record("base"))
        gravatar_id      = m.str("gravatar_id")
        diff_url         = m.str("diff_url")
        html_url         = m.str("html_url")
        patch_url        = m.str("patch_url")
        issue_created_at = m.date("issue_created_at")
        issue_updated_at = m.date("issue_updated_at")
        created_at       = m.date("created_at")
        updated_at       = m.date("updated_at")
        merged_at        = m.date("merged_at")
      } : GitHub.pull_req
      {some=res}
    else none

  pull_requests(res) =
    GP.dewrap_list(res, "pulls", get_pull_req)

}}

GHPull = {{

  @private p(o:string,r:string) = "{o}/{r}"

  create(owner, repo, params:GitHub.Pull.create_params, token) =
    path = "/pulls/{p(owner,repo)}"
    d = match params.description with
      | ~{title body} ->
        [("pull[title]", title), ("pull[body]", body)]
      | ~{issue} -> [("pull[issue]", issue)]
    data =
      [("access_token", token), ("pull[base]", params.base),
       ("pull[head]", params.head)]
      |> List.append(d,_)
    GHLib.api_post(path, data, some)

  @private generic_pull(owner, repo, more:string, token:option(string)) =
    path = "/pulls/{p(owner,repo)}/{more}"
    match token with
    | {none} ->
      GHLib.api_get(path, [], GHPp.pull_requests)
    | {some=t} ->
      GHLib.api_get_logged(path, t, GHPp.pull_requests)

  list(owner, repo, state:{open}/{closed}, token:option(string)) =
    st = match state with
      | {open}  -> "open"
      | {closed} -> "closed"
    generic_pull(owner, repo, st, token)

  number(owner, repo, number:int, token:option(string)) =
    generic_pull(owner, repo, Int.to_string(number), token)

}}
