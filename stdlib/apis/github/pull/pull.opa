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
  company     : string
  user_type   : string
}

type GitHub.ref = {
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
  head             : GitHub.ref
  base             : GitHub.ref
  gravatar_id      : string
  diff_url         : string
  html_url         : string
  patch_url        : string
  issue_created_at : Date.date
  issue_updated_at : Date.date
  created_at       : Date.date
  updated_at       : Date.date
  merged_at        : Date.date
  labels           : list(string)
  mergeable        : bool
}

type GitHub.discussion_elt =
  { commit_id      : string
    committed_date : Date.date
    authored_date  : Date.date
    user           : GitHub.user_simple
    author         : GitHub.user_simple
    parents        : list(list((string,string)))
    message        : string
    committer      : GitHub.commit_user
    tree           : string }
/ { comment_id     : int
    gravatar_id    : string
    user           : GitHub.user_simple
    created_at     : Date.date
    updated_at     : Date.date
    body           : string }
/ { review_pos     : int
    diff_hunk      : string
    user           : GitHub.user_simple
    commit_id      : string
    created_at     : Date.date
    updated_at     : Date.date
    path           : string
    body           : string }
    

type GitHub.full_pull_req = {
  infos      : GitHub.pull_req
  discussion : list(GitHub.discussion_elt)
  issue_user : GitHub.user_simple
}

@private GHPp = {{

  @private GP = GHParse

  get_user_simple(srcmap) =
    m = GP.map_funs(srcmap)
    { login       = m.str("login")
      name        = m.str("name")
      gravatar_id = m.str("gravatar_id")
      company     = m.str("company")
      user_type   = m.str("type")
    } : GitHub.user_simple

  get_ref(srcmap) =
    m = GP.map_funs(srcmap)
    { label      = m.str("label")
      ref        = m.str("ref")
      sha        = m.str("sha")
      repository = m.record("repository")
                   |> GP.get_repo(true) |> Option.get
      user       = get_user_simple(m.record("user"))
    } : GitHub.ref

  get_pull_req_int(m) =
    state = match m.str("state") with
      | "open"   -> {open}
      | "closed" -> {closed=m.date("closed_at")}
      | x        -> {other=x}
    labels = List.filter_map(
      v -> match v:RPC.Json.json with
        | {String=s} -> some(s)
        | _ -> none,
      m.list("labels"))
    { title            = m.str("title")
      body             = m.str("body")
      user             = get_user_simple(m.record("user"))
      comments         = m.int("comments")
      votes            = m.int("votes")
      state            = state
      position         = m.float("position")
      number           = m.int("number")
      head             = get_ref(m.record("head"))
      base             = get_ref(m.record("base"))
      gravatar_id      = m.str("gravatar_id")
      diff_url         = m.str("diff_url")
      html_url         = m.str("html_url")
      patch_url        = m.str("patch_url")
      issue_created_at = m.date("issue_created_at")
      issue_updated_at = m.date("issue_updated_at")
      created_at       = m.date("created_at")
      updated_at       = m.date("updated_at")
      merged_at        = m.date("merged_at")
      labels           = labels
      mergeable        = m.bool("mergeable")
    } : GitHub.pull_req    

  get_pull_req(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("state") then
      some(get_pull_req_int(m))
    else none

  get_discussion_elt(srcmap) : option(GitHub.discussion_elt) =
    m = GP.map_funs(srcmap)
    match m.str("type") with
    | "Commit" ->
      parents = List.filter_map(
        v -> match v:RPC.Json.json with
          | {Record=r} ->
            List.map(
              (k,v1) -> match v1:RPC.Json.json with
                | {String=s} -> (k,s)
                | _ -> (k,Json.serialize(v1)),
              r) |> some
          | _ -> none,
        m.list("parents"))
      { commit_id      = m.str("id")
        committed_date = m.date("committed_date")
        authored_date  = m.date("authored_date")
        user           = get_user_simple(m.record("user"))
        author         = get_user_simple(m.record("author"))
        parents        = parents
        message        = m.str("message")
        committer      = GP.get_commit_user(m.record("committer"))
        tree           = m.str("tree")
      } |> some
    | "IssueComment" ->
      { comment_id     = m.int("comment_id")
        gravatar_id    = m.str("gravatar_id")
        user           = get_user_simple(m.record("user"))
        created_at     = m.date("created_at")
        updated_at     = m.date("updated_at")
        body           = m.str("body")
      } |> some
    | "PullRequestReviewComment" ->
      { review_pos     = m.int("review_pos")
        diff_hunk      = m.str("diff_hunk")
        user           = get_user_simple(m.record("user"))
        commit_id      = m.str("commit_id")
        created_at     = m.date("created_at")
        updated_at     = m.date("updated_at")
        path           = m.str("path")
        body           = m.str("body")
      } |> some
    | _ -> none

  get_full_pull_req(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("state") then
      infos = get_pull_req_int(m)
      discussion = List.filter_map(
        v -> match v : RPC.Json.json with
          | {Record=_} -> get_discussion_elt(v)
          | _ -> none,
        m.list("discussion"))
      res = {
        infos      = infos
        discussion = discussion
        issue_user = get_user_simple(m.record("issue_user"))
      } : GitHub.full_pull_req
      some(res)
    else none

  pull_requests(res) =
    GP.dewrap_list(res, "pulls", get_pull_req)

  one_full_pull_request(res) =
    GP.dewrap_obj(res, "pull", get_full_pull_req)

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
    GHLib.api_post(path, data, GHPp.one_full_pull_request)

  @private generic_pull(owner, repo, more:string, token:option(string), f) =
    path = "/pulls/{p(owner,repo)}/{more}"
    match token with
    | {none} -> GHLib.api_get(path, [], f)
    | {some=t} -> GHLib.api_get_logged(path, t, f)

  list(owner, repo, state:{open}/{closed}, token:option(string)) =
    st = match state with
      | {open}  -> "open"
      | {closed} -> "closed"
    generic_pull(owner, repo, st, token, GHPp.pull_requests)

  number(owner, repo, number:int, token:option(string)) =
    generic_pull(owner, repo, Int.to_string(number),
                 token, GHPp.one_full_pull_request)

  merge(owner, repo, number:int, token) =
    path = "/repos/{p(owner,repo)}/pulls/{number}/merge"
    data = []
    GHLib.v3_put(path, data, some(token), some)

}}
