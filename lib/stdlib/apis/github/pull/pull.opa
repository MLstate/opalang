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

package stdlib.apis.github.pull
import stdlib.apis.github
import stdlib.apis.github.lib

/**
 * GitHub pull request API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */


/* Types returned by API */

type GitHub.merge_result = {
  sha     : string
  merged  : bool
  message : string
}

type GitHub.pull_request_comment = {
  url        : string
  id         : int
  body       : string
  path       : string
  position   : int
  commit_id  : string
  user       : GitHub.short_user
  created_at : Date.date
  updated_at : Date.date
  _links     : GitHub.links
}

@private GHPp = {{

  @private GP = GHParse

  get_pull_req_comment(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("commit_id")
    then
      res = {
        url        = m.str("url")
        id         = GP.get_id(m)
        body       = m.str("body")
        path       = m.str("path")
        position   = m.int("position")
        commit_id  = m.str("commit_id")
        user       = GP.get_short_user(m.record("user"))
        created_at = m.date("created_at")
        updated_at = m.date("updated_at")
        _links     = GP.get_rec(m, "_links", GP.get__links)
       } : GitHub.pull_request_comment
      {some=res}
    else {none}

  get_merge_result(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("sha")
    then
      res = {
        sha     = m.str("sha")
        merged  = m.bool("merged")
        message = m.str("message")
      } : GitHub.merge_result
      {some=res}
    else {none}

  one_pull_request(res) = GP.dewrap_whole_obj(res, GP.get_pull_req)
  multiple_pull_requests(res) = GP.dewrap_whole_list(res, GP.get_pull_req)

  one_pull_request_comment(res) = GP.dewrap_whole_obj(res, get_pull_req_comment)
  multiple_pull_request_comments(res) = GP.dewrap_whole_list(res, get_pull_req_comment)

  one_merge_result(res) = GP.dewrap_whole_obj(res, get_merge_result)

}}

type GitHub.pull_req_input =
   { title : string body : option(string) }
 / { issue : int }

type GitHub.pull_req_create_comment_input =
   { body : string commit_id : string path : string position : int }
 / { body : string in_reply_to : int }

GHPull = {{

  @private GP = GHParse

  list_pull_requests(token:string, user:string, repo:string, state:option(GitHub.state)) =
    GHLib.api_get_full("/repos/{user}/{repo}/pulls", token, GHLib.opt_state(state), GHPp.multiple_pull_requests)

  get_pull_request(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/pulls/{number}", token, [], GHPp.one_pull_request)

  create_pull_request(token:string, user:string, repo:string,
                      head:string, input:GitHub.pull_req_input, base:option(string)) =
    json =
      match input with
      | ~{title body} -> GHLib.mkopts([{sreq=("title",title)},{sopt=("body",body)},{sreq=("head",head)},{sopt=("base",base)}])
      | ~{issue} -> GHLib.mkopts([{sreq=("issue","{issue}")},{sreq=("head",head)},{sopt=("base",base)}])
    GHLib.api_post_string("/repos/{user}/{repo}/pulls", token, json, GHPp.one_pull_request)

  update_pull_request(token:string, user:string, repo:string, number:int,
                      title:option(string), body:option(string), state:option(GitHub.state)) =
    json = GHLib.mkopts([{sopt=("title",title)},{sopt=("body",body)},{ocst=("state",GHLib.string_of_state,state)}])
    GHLib.api_patch_string("/repos/{user}/{repo}/pulls/{number}", token, json, GHPp.one_pull_request)

  list_pull_request_commits(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/pulls/{number}/commits", token, [], GP.multiple_full_commits)

  list_pull_request_files(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/pulls/{number}/files", token, [], GP.multiple_files)

  is_pull_request_merged(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/pulls/{number}/merge", token, [], GP.expect_204_404)

  merge_pull_request(token:string, user:string, repo:string, number:int, commit_message:option(string)) =
    json = GHLib.mkopts([{sopt=("commit_message",commit_message)}])
    GHLib.api_put_string("/repos/{user}/{repo}/pulls/{number}/merge", token, json, GHPp.one_merge_result)

  list_pull_request_comments(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/pulls/{number}/comments", token, [], GHPp.multiple_pull_request_comments)

  get_pull_request_comment(token:string, user:string, repo:string, number:int) =
    GHLib.api_get_full("/repos/{user}/{repo}/pulls/comments/{number}", token, [], GHPp.one_pull_request_comment)

  create_pull_request_comment(token:string, user:string, repo:string, number:int,
                              input:GitHub.pull_req_create_comment_input) =
    json =
      match input with
      | ~{body commit_id path position} ->
          GHLib.mkopts([{sreq=("body",body)},{sreq=("commit_id",commit_id)},{sreq=("path",path)},{ireq=("position",position)}])
      | ~{body in_reply_to} ->
          GHLib.mkopts([{sreq=("body",body)},{sreq=("in_reply_to","{in_reply_to}")}])
    GHLib.api_post_string("/repos/{user}/{repo}/pulls/{number}/comments", token, json, GHPp.one_pull_request_comment)

  edit_comment(token:string, user:string, repo:string, number:int, body:string) =
    json = GHLib.mkopts([{sreq=("body",body)}])
    GHLib.api_patch_string("/repos/{user}/{repo}/pulls/comments/{number}", token, json, GHPp.one_pull_request_comment)

  delete_comment(token:string, user:string, repo:string, number:int) =
    GHLib.api_delete_string("/repos/{user}/{repo}/pulls/comments/{number}", token, "", GP.expect_204)

}}
