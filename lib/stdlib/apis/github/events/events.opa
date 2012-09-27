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

package stdlib.apis.github.events
import stdlib.apis.github
import stdlib.apis.github.lib

/**
 * GitHub user API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

type GitHub.page = {
  page_name : string
  title : string
  action : string
  sha : string
  html_url : string
}

type GitHub.push_event = {
  head    : string
  ref     : string
  size    : int
  commits : list({sha      : string
                  message  : string
                  author   : { name  : string
                               email : string
                             }
                  url      : string
                  distinct : bool
                 })
}

type GitHub.payload =
   { repo_comment:GitHub.repo_comment } // CommitCommentEvent
 / { ref_type:string ref:string master_branch:string description:string } // CreateEvent
 / { ref_type:string ref:string } // DeleteEvent
 / { download:GitHub.download } // DownloadEvent
 / { target:GitHub.user } // FollowEvent
 / { forkee:GitHub.repository } // ForkEvent
 / { head:string before:string after:string } // ForkApplyEvent
 / { action:string gist:GitHub.gist } // GistEvent
 / { pages:list(GitHub.page) } // GollumEvent
 / { action:string issue:GitHub.issue comment:GitHub.issue_comment } // IssueCommentEvent
 / { action:string issue:GitHub.issue } // IssuesEvent
 / { member:GitHub.user action:string } // MemberEvent
 / { empty } // PublicEvent - empty payload
 / { action:string number:int pull_request:GitHub.pull_req } // PullRequestEvent
 / { prr_comment:GitHub.repo_comment } // PullRequestReviewCommentEvent // ??? check this, is this repo_comment?
 / { push_event : GitHub.push_event } // PushEvent
 / { team : GitHub.id_name_url user:GitHub.user repo:GitHub.repository } // TeamAddEvent
 / { action:string } // WatchEvent
 / { raw : list((string,RPC.Json.json)) }

type GitHub.event = {
  `type`     : string
  `public`   : bool
  payload    : GitHub.payload
  repo       : GitHub.id_name_url
  actor      : GitHub.short_user
  org        : GitHub.short_user
  created_at : Date.date
  id         : int
}

@private GHEp = {{

  @private GP = GHParse

//   { comment:GitHub.repo_comment } // CommitCommentEvent

  get_commitcommentevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "comment", GP.get_repo_comment)) with
    | {some=repo_comment} ->
      res = {
        repo_comment = repo_comment
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { ref_type:string ref:string master_branch:string description:string } // CreateEvent

  get_createevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    if m.exists("ref")
    then
      res = {
        ref           = m.str("ref")
        master_branch = m.str("master_branch")
        ref_type      = m.str("ref_type")
        description   = m.str("description")
      } : GitHub.payload
      {some=res}
    else none

// / { ref_type:string ref:string } // DeleteEvent

  get_deleteevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    if m.exists("ref")
    then
      res = {
        ref           = m.str("ref")
        ref_type      = m.str("ref_type")
      } : GitHub.payload
      {some=res}
    else none

// / { download:GitHub.download } // DownloadEvent

  get_downloadevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "download", GP.get_download)) with
    | {some=download} ->
      res = {
        download = download
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { target:GitHub.user } // FollowEvent

  get_followevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "target", GP.get_user)) with
    | {some=target} ->
      res = {
        target = target
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { forkee:GitHub.repository } // ForkEvent

  get_forkevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "forkee", GP.get_repo(false))) with
    | {some=forkee} ->
      res = {
        forkee = forkee
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { head:string before:string after:string } // ForkApplyEvent

  get_forkapplyevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    if m.exists("head")
    then
      res = {
        head   = m.str("head")
        before = m.str("before")
        after  = m.str("after")
      } : GitHub.payload
      {some=res}
    else none

// / { action:string gist:GitHub.gist } // GistEvent

  get_gistevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "gist", GP.get_gist)) with
    | {some=gist} ->
      res = {
        action  = m.str("action")
        gist = gist
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { pages:list(GitHub.page) } // GollumEvent

  get_page(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("page_name")
    then
     page = {
       page_name = m.str("page_name")
       title     = m.str("title")
       action    = m.str("action")
       sha       = m.str("sha")
       html_url  = m.str("html_url")
     } : GitHub.page
     {some=page}
   else none

  get_gollumevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_list(m, "pages", get_page)) with
    | [] -> none
    | pages ->
      res = {
        pages = pages
      } : GitHub.payload
      {some=res}

// / { action:string issue:GitHub.issue comment:GitHub.issue_comment } // IssueCommentEvent

  get_issuecommentevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "issue", GP.get_issue),
           GP.get_rec(m, "comment", GP.get_issue_comment)) with
    | ({some=issue},{some=comment}) ->
      res = {
        action  = m.str("action")
        issue   = issue
        comment = comment
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { action:string issue:GitHub.issue } // IssuesEvent

  get_issuesevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "issue", GP.get_issue)) with
    | {some=issue} ->
      res = {
        action  = m.str("action")
        issue = issue
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { member:GitHub.user action:string } // MemberEvent

  get_memberevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "member", GP.get_user)) with
    | {some=member} ->
      res = {
        action  = m.str("action")
        member = member
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { empty } // PublicEvent - empty payload

  get_publicevent(_) : option(GitHub.payload) = {some={empty}}

// / { action:string number:int pull_request:GitHub.pull_req } // PullRequestEvent

  get_pull_requestevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "pull_request", GP.get_pull_req)) with
    | {some=pull_request} ->
      res = {
        action  = m.str("action")
        number  = m.int("number")
        pull_request = pull_request
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { prr_comment:GitHub.repo_comment } // PullRequestReviewCommentEvent // ??? check this, is this repo_comment?

  get_pullrequestreviewcommentevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "comment", GP.get_repo_comment)) with
    | {some=prr_comment} ->
      res = {
        prr_comment = prr_comment
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { push_event : GitHub.push_event } // PushEvent

  get_name_email(srcmap) =
    m = GP.map_funs(srcmap)
    { name  = m.str("name")
      email = m.str("email")
    }

  get_event_commit(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("message")
    then
      res = {
        sha      = m.str("sha")
        message  = m.str("message")
        author   = GP.get_rec(m, "author", get_name_email)
        url      = m.str("url")
        distinct = m.bool("distinct")
      }
      {some=res}
    else none

  get_pushevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    if m.exists("commits")
    then
      res = {
        head    = m.str("head")
        ref     = m.str("ref")
        size    = m.int("size")
        commits = GP.get_list(m, "commits", get_event_commit)
      } : GitHub.push_event
      {some={push_event=res}}
    else none

// / { team : GitHub.id_name_url user:GitHub.user repo:GitHub.repository } // TeamAddEvent

  get_teamaddevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    match (GP.get_rec(m, "team", GP.get_id_name_url),
           GP.get_rec(m, "user", GP.get_user),
           GP.get_rec(m, "repo", GP.get_repo(true))) with
    | (team,{some=user},{some=repo}) ->
      res = {
        team = team
        user = user
        repo = repo
      } : GitHub.payload
      {some=res}
    | _ -> none

// / { action:string } // WatchEvent

  get_watchevent(srcmap) : option(GitHub.payload) =
    m = GP.map_funs(srcmap)
    if m.exists("action")
    then
      res = {
        action = m.str("action")
      } : GitHub.payload
      {some=res}
    else none

  get_payload(m, typ) : GitHub.payload =
    match m.record("payload") with
    | {Record=[]} -> {raw=[]}
    | {Record=r} ->
       match typ with
       | "CommitCommentEvent" -> Option.default({raw=r},get_commitcommentevent({Record=r}))
       | "CreateEvent" -> Option.default({raw=r},get_createevent({Record=r}))
       | "DeleteEvent" -> Option.default({raw=r},get_deleteevent({Record=r}))
       | "DownloadEvent" -> Option.default({raw=r},get_downloadevent({Record=r}))
       | "FollowEvent" -> Option.default({raw=r},get_followevent({Record=r}))
       | "ForkEvent" -> Option.default({raw=r},get_forkevent({Record=r}))
       | "ForkApplyEvent" -> Option.default({raw=r},get_forkapplyevent({Record=r}))
       | "GistEvent" -> Option.default({raw=r},get_gistevent({Record=r}))
       | "GollumEvent" -> Option.default({raw=r},get_gollumevent({Record=r}))
       | "IssueCommentEvent" -> Option.default({raw=r},get_issuecommentevent({Record=r}))
       | "IssuesEvent" -> Option.default({raw=r},get_issuesevent({Record=r}))
       | "MemberEvent" -> Option.default({raw=r},get_memberevent({Record=r}))
       | "PublicEvent" -> Option.default({raw=r},get_publicevent({Record=r}))
       | "PullRequestEvent" -> Option.default({raw=r},get_pull_requestevent({Record=r}))
       | "PullRequestReviewCommentEvent" -> Option.default({raw=r},get_pullrequestreviewcommentevent({Record=r}))
       | "PushEvent" -> Option.default({raw=r},get_pushevent({Record=r}))
       | "TeamAddEvent" -> Option.default({raw=r},get_teamaddevent({Record=r}))
       | "WatchEvent" -> Option.default({raw=r},get_watchevent({Record=r}))
       | _ -> {raw=r}
       end
    | _ -> {raw=[]}

  // NOTE: the spec says "All Events have the same response format" which is
  // complete nonsense.  See the output from list_repo_issue_events.
  get_event(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id")
    then
      typ = m.str("type")
      res = {
        `type`     = typ
        `public`   = m.bool("public")
        payload    = get_payload(m, typ)
        repo       = GP.get_rec(m, "repo", GP.get_id_name_url)
        actor      = GP.get_rec(m, "actor", GP.get_short_user)
        org        = GP.get_rec(m, "org", GP.get_short_user)
        created_at = m.date("created_at")
        id         = GP.get_id(m)
      } : GitHub.event
      {some=res}
    else {none}

  multiple_events(res:WebClient.success(string)) = GP.dewrap_whole_list(res, get_event)

}}

GHEvents = {{

  @private list_events_generic(token, path) =
    GHLib.api_get_full(path, token, [], GHEp.multiple_events)

  list_public_events(token) = list_events_generic(token, "/events")
  list_repo_events(token, user:string, repo:string) = list_events_generic(token, "/repos/{user}/{repo}/events")
  list_repo_issue_events(token, user:string, repo:string) = list_events_generic(token, "/repos/{user}/{repo}/issues/events")
  list_network_public_events(token, user:string, repo:string) = list_events_generic(token, "/networks/{user}/{repo}/events")
  list_org_public_events(token, org:string) = list_events_generic(token, "/orgs/{org}/events")
  list_user_received_events(token, user:string) = list_events_generic(token, "/users/{user}/received_events")
  list_user_received_public_events(token, user:string) = list_events_generic(token, "/users/{user}/received_events/public")
  list_user_performed_events(token, user:string) = list_events_generic(token, "/users/{user}/events")
  list_user_performed_public_events(token, user:string) = list_events_generic(token, "/users/{user}/events/public")
  list_organization_events(token, user:string, org:string) = list_events_generic(token, "/users/{user}/events/orgs/{org}")

}}
