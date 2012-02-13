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
 * GitHub issue API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

//package stdlib.apis.github.issue
import stdlib.apis.github
import stdlib.apis.github.lib

/* Types returned by API */

type GitHub.issue = {
  user        : string
  gravatar_id : string
  created_at  : Date.date
  updated_at  : Date.date
  state       : {open}/{closed}
  number      : int
  votes       : int
  position    : float
  title       : string
  body        : string
  labels      : list(string)
}

type GitHub.issue_comment = {
  id          : int
  user        : string
  gravatar_id : string
  created_at  : Date.date
  updated_at  : Date.date
  body        : string
}

@private GHIp = {{

  state_to_string(s) =
    match s : {open}/{closed} with
    | {open} -> "open" | {closed} -> "closed"

  @private GP = GHParse

  get_issue(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("title") then
      state = match m.str("state") with
        | "closed" -> {closed}
        | "open" | _ -> {open}
      labels = List.filter_map(
        v -> match v:RPC.Json.json with
          | {String=s} -> some(s) | _ -> none,
        m.list("labels"))
      res = {
        user        = m.str("user")
        gravatar_id = m.str("gravatar_id")
        created_at  = m.date("created_at")
        updated_at  = m.date("updated_at")
        state       = state
        number      = m.int("number")
        votes       = m.int("votes")
        position    = m.float("position")
        title       = m.str("title")
        body        = m.str("body")
        labels      = labels
      } : GitHub.issue
      some(res)
    else none

  get_comment(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id") then
      res = {
        id          = m.int("id")
        user        = m.str("user")
        gravatar_id = m.str("gravatar_id")
        created_at  = m.date("created_at")
        updated_at  = m.date("updated_at")
        body        = m.str("body")
      } : GitHub.issue_comment
      some(res)
    else none

  one_issue(res) =
    GP.dewrap_obj(res, "issue", get_issue)

  multiple_issues(res) =
    GP.dewrap_list(res, "issues", get_issue)

  one_comment(res) =
    GP.dewrap_obj(res, "comment", get_comment)

  comments(res) =
    GP.dewrap_list(res, "comments", get_comment)

  labels(res) = GP.multiple_strings(res, "labels")

}}

GHIssue = {{

  @private p(o:string,r:string) = "{o}/{r}"

  search(owner, repo, state, query:string) =
    st = GHIp.state_to_string(state)
    path = "/issues/search/{p(owner,repo)}/{st}/{query}"
    GHLib.api_get(path, [], GHIp.multiple_issues)

  list_by_state(owner, repo, state) =
    st = GHIp.state_to_string(state)
    path = "/issues/list/{p(owner,repo)}/{st}"
    GHLib.api_get(path, [], GHIp.multiple_issues)

  list_by_label(owner, repo, label:string) =
    path = "/issues/list/{p(owner,repo)}/label/{label}"
    GHLib.api_get(path, [], GHIp.multiple_issues)

  get_issue(owner, repo, number:int) =
    path = "/issues/show/{p(owner,repo)}/{number}"
    GHLib.api_get(path, [], GHIp.one_issue)

 /**
  * Opens a new issue
  *
  * @param owner Owner of the targeted repo
  * @param repo Targeted repo
  * @param title Title of the new issue
  * @param body Body of the new issue
  * @param token Access token of user
  */
  open_issue(owner, repo, title, body, token) =
    path = "/issues/open/{p(owner,repo)}"
    data = [("access_token",token),
            ("title",title), ("body",body)]
    GHLib.api_post(path, data, GHIp.one_issue)

 /**
  * Edits an existing issue
  *
  * @param owner Owner of the targeted repo
  * @param repo Targeted repo
  * @param number ID number of the issue
  * @param title New title of the issue (leave blanck to keep old title)
  * @param body New body of the issue (leave blanck to keep old body)
  * @param token Access token of user
  */
  edit_issue(owner, repo, number:int, title, body, token) =
    path = "/issues/edit/{p(owner,repo)}/{number}"
    data = [("access_token",token),
            ("title",title), ("body",body)]
    GHLib.api_post(path, data, GHIp.one_issue)

  close_issue(owner, repo, number:int, token) =
    path = "/issues/close/{p(owner,repo)}/{number}"
    data = [("access_token",token)]
    GHLib.api_post(path, data, GHIp.one_issue)

  reopen_issue(owner, repo, number:int, token) =
    path = "/issues/reopen/{p(owner,repo)}/{number}"
    data = [("access_token",token)]
    GHLib.api_post(path, data, GHIp.one_issue)

  list_project_labels(owner, repo) =
    path = "/issues/labels/{p(owner,repo)}"
    GHLib.api_get(path, [], GHIp.labels)

  add_label(owner, repo, label:string, number:int, token) =
    path = "/issues/label/add/{p(owner,repo)}/{label}/{number}"
    data = [("access_token",token)]
    GHLib.api_post(path, data, GHIp.labels)

  remove_label(owner, repo, label:string, number:int, token) =
    path = "/issues/label/remove/{p(owner,repo)}/{label}/{number}"
    data = [("access_token",token)]
    GHLib.api_post(path, data, GHIp.labels)

  get_issue_comments(owner, repo, number:int) =
    path = "/issues/comments/{p(owner,repo)}/{number}"
    GHLib.api_get(path, [], GHIp.comments)

  comment(owner, repo, number:int, comment, token) =
    path = "/issues/comment/{p(owner,repo)}/{number}"
    data = [("access_token",token), ("comment",comment)]
    GHLib.api_post(path, data, GHIp.one_comment)

}}
