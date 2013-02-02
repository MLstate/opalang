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
 * GitHub issue API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

package stdlib.apis.github.issue
import stdlib.apis.github
import stdlib.apis.github.lib

/* Types returned by API */

type GitHub.issue_event = {
  url        : string
  actor      : GitHub.short_user
  event      : string
  commit_id  : string
  created_at : Date.date
  issue      : option(GitHub.issue)
}

type GitHub.issue_params = {
  filter    : {assigned} / {created} / {mentioned} / {subscribed}
  state     : {open} / {closed}
  labels    : list(string)
  sort      : {created} / {updated} / {comments}
  direction : {asc} / {desc}
  since     : option(Date.date)
}

type GitHub.valnonestar('a) = {val:'a} / {none} / {star}

@private GHIp = {{

  @private GP = GHParse

  get_event(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("event") then
      res = {
        url        =  m.str("url")
        actor      = GP.get_rec(m, "actor", GP.get_short_user)
        event      = m.str("event")
        commit_id  = m.str("commit_id")
        created_at = m.date("created_at")
        issue      = GP.get_rec(m, "issue", GP.get_issue)
      } : GitHub.issue_event
      some(res)
    else none

  one_issue(res) = GP.dewrap_whole_obj(res, GP.get_issue)
  multiple_issues(res) = GP.dewrap_whole_list(res, GP.get_issue)

  one_issue_comment(res) = GP.dewrap_whole_obj(res, GP.get_issue_comment)
  multiple_comments(res) = GP.dewrap_whole_list(res, GP.get_issue_comment)

  one_event(res) = GP.dewrap_whole_obj(res, get_event)
  multiple_events(res) = GP.dewrap_whole_list(res, get_event)

  one_label(res) = GP.dewrap_whole_obj(res, GP.get_label)
  multiple_labels(res) = GP.dewrap_whole_list(res, GP.get_label)

  one_milestone(res) = GP.dewrap_whole_obj(res, GP.get_milestone)
  multiple_milestones(res) = GP.dewrap_whole_list(res, GP.get_milestone)

  labels(res) = GP.multiple_strings(res, "labels")

}}

type GitHub.issue_filter = {assigned} / {created} / {mentioned} / {subscribed}
type GitHub.issue_sort = {created} / {updated} / {comments}
type GitHub.issue_sort2 = {due_date} / {completeness}

GHIssue = {{

  @private GP = GHParse

  @private opt_valnonestar(name:string, vns:option(GitHub.valnonestar('a))) =
    match vns with
    | {some={~val}} -> [(name,"{val}")]
    | {some={none}} -> [(name,"none")]
    | {some={star}} -> [(name,"*")]
    | {none} -> []

  @private opt_filter(filter) =
    match filter with
    | {some=filter} ->
       filter =
         match filter with
         | {assigned} -> "assigned"
         | {created} -> "created"
         | {mentioned} -> "mentioned"
         | {subscribed} -> "subscribed"
         end
      [("filter",filter)]
    | _ -> []

  @private opt_labels(labels) =
    match labels with
    | [_|_] -> [("labels",String.concat(",",labels))]
    | _ -> []

  @private opt_sort(sort) =
    match sort with
    | {some=sort} ->
       sort =
         match sort with
         | {created} -> "created"
         | {updated} -> "updated"
         | {comments} -> "comments"
         end
      [("sort",sort)]
    | _ -> []

  @private opt_sort2(sort) =
    match sort with
    | {some=sort} ->
       sort =
         match sort with
         | {due_date} -> "due_date"
         | {completeness} -> "completeness"
         end
      [("sort",sort)]
    | _ -> []

  @private opt_since(since) =
    match since with
    | {some=since} -> [("since",GHParse.date_to_string(since))]
    | _ -> []

  @private opt_str(name:string, str:option(string)) =
    match str with
    | {some=str} -> [(name,str)]
    | {none} -> []

  list_issues(token:string,
              filter:option(GitHub.issue_filter), state:option(GitHub.state), labels:list(string),
              sort:option(GitHub.issue_sort), direction:option(GitHub.direction), since:option(Date.date)) =
    options =
      List.flatten([opt_filter(filter),GHLib.opt_state(state),opt_labels(labels),
                    opt_sort(sort),GHLib.opt_direction(direction),opt_since(since)])
    GHLib.api_get_full("/issues", token, options, GHIp.multiple_issues)

  list_issues_repo(token:string, user:string, repo:string,
                   milestone:option(GitHub.valnonestar(int)), state:option(GitHub.state),
                   assignee:option(GitHub.valnonestar(string)), mentioned:option(string), labels:list(string),
                   sort:option(GitHub.issue_sort), direction:option(GitHub.direction), since:option(Date.date)) =
    options =
      List.flatten([opt_valnonestar("milestone",milestone),GHLib.opt_state(state),opt_valnonestar("assignee",assignee),
                    opt_str("mentioned",mentioned),opt_labels(labels),opt_sort(sort),GHLib.opt_direction(direction),
                    opt_since(since)])
    GHLib.api_get_full("/repos/{user}/{repo}/issues", token, options, GHIp.multiple_issues)

  get_issue(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/issues/{number}", token, [], GHIp.one_issue)

  create_issue(token:string, user:string, repo:string,
               title:string, body:option(string), assignee:option(string), milestone:option(int), labels:list(string)) =
    json = GHLib.mkopts([{sreq=("title",title)},{sopt=("body",body)},{sopt=("assignee",assignee)},
                         {iopt=("milestone",milestone)},{lst=("labels",labels)}])
    GHLib.api_post_string("/repos/{user}/{repo}/issues", token, json, GHIp.one_issue)

  edit_issue(token:string, user:string, repo:string,
             title:option(string), body:option(string), assignee:option(string),
             state:option(GitHub.state), milestone:option(int), labels:list(string)) =
    json = GHLib.mkopts([{sopt=("title",title)},{sopt=("body",body)},{sopt=("assignee",assignee)},
                         {ocst=("state",GHLib.string_of_state,state)},{iopt=("milestone",milestone)},{lst=("labels",labels)}])
    GHLib.api_patch_string("/repos/{user}/{repo}/issues", token, json, GHIp.one_issue)

  list_assignees(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/assignees", token, [], GP.multiple_short_users)

  check_assignee(token:string, user:string, repo:string, assignee:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/assignees/{assignee}", token, [], GP.expect_204_404)

  list_comments(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/issues/{number}/comments", token, [], GHIp.multiple_comments)

  get_comment(token:string, user:string, repo:string, id:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/issues/comments/{id}", token, [], GHIp.one_issue_comment)

  create_comment(token:string, user:string, repo:string, number:int, body:string) =
    json = GHLib.mkopts([{sreq=("body",body)}])
    GHLib.api_post_string("/repos/{user}/{repo}/issues/{number}/comments", token, json, GHIp.one_issue_comment)

  edit_comment(token:string, user:string, repo:string, body:string, id:int) =
    json = GHLib.mkopts([{sreq=("body",body)}])
    GHLib.api_post_string("/repos/{user}/{repo}/issues/comments/{id}", token, json, GHIp.one_issue_comment)

  delete_comment(token:string, user:string, repo:string, id:int) =
    GHLib.api_delete_string("/repos/{user}/{repo}/issues/comments/{id}", token, "", GP.expect_204)

  list_issue_events(token:string, user:string, repo:string, issue_number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/issues/{issue_number}/events", token, [], GHIp.multiple_events)

  list_repo_events(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/issues/events", token, [], GHIp.multiple_events)

  get_event(token:string, user:string, repo:string, id:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/issues/events/{id}", token, [], GHIp.one_event)

  list_labels(token:string, user:string, repo:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/labels", token, [], GHIp.multiple_labels)

  get_label(token:string, user:string, repo:string, name:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/labels/{name}", token, [], GHIp.one_label)

  create_label(token:string, user:string, repo:string, name:string, color:string) =
    json = GHLib.mkopts([{sreq=("name",name)},{sreq=("color",color)}])
    GHLib.api_post_string("/repos/{user}/{repo}/labels", token, json, GHIp.one_label)

  update_label(token:string, user:string, repo:string, name:string, color:string) =
    json = GHLib.mkopts([{sreq=("name",name)},{sreq=("color",color)}])
    GHLib.api_patch_string("/repos/{user}/{repo}/labels/{name}", token, json, GHIp.one_label)

  delete_label(token:string, user:string, repo:string, name:string) =
    GHLib.api_delete_string("/repos/{user}/{repo}/labels/{name}", token, "", GP.expect_204)

  list_issue_labels(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/issues/{number}/labels", token, [], GHIp.multiple_labels)

  add_issue_labels(token:string, user:string, repo:string, number:int, labels:list(string)) =
    json = GHLib.mkslst(labels)
    GHLib.api_post_string("/repos/{user}/{repo}/issues/{number}/labels", token, json, GHIp.multiple_labels)

  remove_issue_label(token:string, user:string, repo:string, number:int, name:string) =
    GHLib.api_delete_string("/repos/{user}/{repo}/issues/{number}/labels/{name}", token, "", GHIp.multiple_labels)

  replace_issue_labels(token:string, user:string, repo:string, number:int, labels:list(string)) =
    json = GHLib.mkslst(labels)
    GHLib.api_put_string("/repos/{user}/{repo}/issues/{number}/labels", token, json, GHIp.multiple_labels)

  remove_all_issue_labels(token:string, user:string, repo:string, number:int) =
    GHLib.api_delete_string("/repos/{user}/{repo}/issues/{number}/labels", token, "", GP.expect_204)

  get_milestone_issue_labels(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/milestones/{number}/labels", token, [], GHIp.multiple_labels)

  list_milestones(token:string, user:string, repo:string,
                  state:option(GitHub.state), sort:option(GitHub.issue_sort2), direction:option(GitHub.direction)) =
    options = List.flatten([GHLib.opt_state(state),opt_sort2(sort),GHLib.opt_direction(direction)])
    GHLib.api_get_full("/repos/{user}/{repo}/milestones", token, options, GHIp.multiple_issues)

  get_milestone(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/milestones/{number}", token, [], GHIp.one_milestone)

  create_milestone(token:string, user:string, repo:string,
                   title:string, state:option(GitHub.state), description:option(string), due_on:option(Date.date)) =
    json = GHLib.mkopts([{sreq=("title",title)},{ocst=("state",GHLib.string_of_state,state)},
                         {sopt=("description",description)},{sopt=("due_on",Option.map(GHParse.date_to_string,due_on))}])
    GHLib.api_post_string("/repos/{user}/{repo}/milestones", token, json, GHIp.one_milestone)

  update_milestone(token:string, user:string, repo:string, number:int,
                   title:option(string), state:option(GitHub.state), description:option(string), due_on:option(Date.date)) =
    json = GHLib.mkopts([{sopt=("title",title)},{ocst=("state",GHLib.string_of_state,state)},
                         {sopt=("description",description)},{sopt=("due_on",Option.map(GHParse.date_to_string,due_on))}])
    GHLib.api_patch_string("/repos/{user}/{repo}/milestones/{number}", token, json, GHIp.one_milestone)

  delete_milestone(token:string, user:string, repo:string, number:int) =
    GHLib.api_delete_string("/repos/{user}/{repo}/milestones/{number}", token, "", GP.expect_204)

}}
