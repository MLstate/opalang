/*
    Copyright © 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/*
 * Author    : Nicolas Glondu <nicolas.glondu@mlstate.com>
 **/

/**
 * GitHub gist API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

package stdlib.apis.github.git_data
import stdlib.apis.github
import stdlib.apis.github.lib

/* Types returned by API */


type GitHub.gitdata_blob = {
  content  : string
  encoding : string
  sha      : string
  size     : int
}

type GitHub.gitdata_sha = {
  sha : string
}

type GitHub.gitdata_object = {
  `type` : string
  sha    : string
  url    : string
}

type GitHub.gitdata_reference = {
  ref    : string
  url    : string
  object : option(GitHub.gitdata_object)
}

type GitHub.gitdata_tag = {
  tag     : string
  sha     : string
  url     : string
  message : string
  tagger  : option(GitHub.commit_user)
  object  : option(GitHub.gitdata_object)
}

type GitHub.gitdata_tree_element = {
  path   : string
  mode   : string
  `type` : string
  size   : int
  sha    : string
  url    : string
}

type GitHub.gitdata_tree = {
  sha  : string
  url  : string
  tree : list(GitHub.gitdata_tree_element)
}

type GitHub.gitdata_sha_or_content =
   {sha:string}
 / {content:string}

type GitHub.gitdata_btc = {blob} / {tree} / {commit}

type GitHub.gitdata_tree_element_param = {
  path           : string
  mode           : string
  `type`         : GitHub.gitdata_btc
  sha_or_content : GitHub.gitdata_sha_or_content
}

type GitHub.gitdata_tree_param = list(GitHub.gitdata_tree_element_param)

@private GHGDp = {{

  @private GP = GHParse

  get_gitdata_blob(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("content")
    then
      res = {
        content  = m.str("content")
        encoding = m.str("encoding")
        sha      = m.str("sha")
        size     = m.int("size")
      } : GitHub.gitdata_blob
      {some=res}
    else {none}

  get_gitdata_sha(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("sha")
    then
      res = {
        sha  = m.str("sha")
      } : GitHub.gitdata_sha
      {some=res}
    else {none}

  get_gitdata_object(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("type")
    then
      res = {
        `type`  = m.str("type")
        sha     = m.str("sha")
        url     = m.str("url")
      } : GitHub.gitdata_object
      {some=res}
    else {none}

  get_reference(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("ref")
    then
      res = {
        ref    = m.str("ref")
        url    = m.str("url")
        object = GP.get_rec(m, "object", get_gitdata_object)
      } : GitHub.gitdata_reference
      {some=res}
    else {none}

  get_tag(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("ref")
    then
      res = {
        tag     = m.str("tag")
        sha     = m.str("sha")
        url     = m.str("url")
        message = m.str("message")
        tagger  = GP.get_rec(m, "tagger", GP.get_commit_user_opt)
        object  = GP.get_rec(m, "object", get_gitdata_object)
      } : GitHub.gitdata_tag
      {some=res}
    else {none}

  get_tree_element(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("path")
    then
      res = {
        path    = m.str("path")
        mode    = m.str("mode")
        `type`  = m.str("type")
        size    = m.int("size")
        sha     = m.str("sha")
        url     = m.str("url")
      } : GitHub.gitdata_tree_element
      {some=res}
    else {none}

  get_tree(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("tree")
    then
      res = {
        sha  = m.str("sha")
        url  = m.str("url")
        tree = List.filter_map(get_tree_element,m.list("tree"))
      } : GitHub.gitdata_tree
      {some=res}
    else {none}

  one_blob(res) = GP.dewrap_whole_obj(res, get_gitdata_blob)
  one_sha(res) = GP.dewrap_whole_obj(res, get_gitdata_sha)

  one_commit(res) = GP.dewrap_whole_obj(res, GP.get_commit_opt)

  one_reference(res) = GP.dewrap_whole_obj(res, get_reference)
  multiple_references(res) = GP.dewrap_whole_list(res, get_reference)

  one_tag(res) = GP.dewrap_whole_obj(res, get_tag)

  one_tree(res) = GP.dewrap_whole_obj(res, get_tree)

}}

// TODO:  Big function to update a file in a repo, see http://developer.github.com/v3/git/

GHGitData = {{

  @private GP = GHParse

  get_blob(token:string, user:string, repo:string, sha:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/git/blobs/{sha}", token, [], GHGDp.one_blob)

  create_blob(token:string, encoding:GitHub.encoding, user:string, repo:string, content:string) =
    json = GHLib.mkopts([{sreq=("content",content)},{rcst=("encoding",GHLib.string_of_encoding,encoding)}])
    GHLib.api_post_string("/repos/{user}/{repo}/git/blobs", token, json, GHGDp.one_sha)

  get_commit(token:string, user:string, repo:string, sha:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/git/commits/{sha}", token, [], GHGDp.one_commit)

  @private commit_user_to_json(cu:option(GitHub.commit_user)) =
    match cu with
    | {some=cu} -> [("name","\"{cu.name}\""),("email","\"{cu.email}\""),("date","\"{GHParse.date_to_string(cu.date)}\"")]
    | {none} -> []

  create_commit(token:string, user:string, repo:string,
                message:string, tree:string, parents:list(string),
                author:option(GitHub.commit_user), committer:option(GitHub.commit_user)) =
    json = GHLib.mkopts([{sreq=("message",message)},{obj=("author",commit_user_to_json(author))},
                         {obj=("committer",commit_user_to_json(committer))},{lst=("parents",parents)},
                         {sreq=("tree",tree)}])
    GHLib.api_post_string("/repos/{user}/{repo}/git/commits", token, json, GHGDp.one_commit)

  get_reference(token:string, user:string, repo:string, branch:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/git/refs/heads/{branch}", token, [], GHGDp.one_reference)

  get_all_references(token:string, user:string, repo:string, subname:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/git/refs/{subname}", token, [], GHGDp.multiple_references)

  create_reference(token:string, user:string, repo:string, ref:string, sha:string) =
    json = GHLib.mkopts([{sreq=("ref",ref)},{sreq=("sha",sha)}])
    GHLib.api_post_string("/repos/{user}/{repo}/git/refs", token, json, GHGDp.one_reference)

  update_reference(token:string, user:string, repo:string, ref:string, sha:string, force:bool) =
    json = GHLib.mkopts([{sreq=("sha",sha)},{breq=("force",force)}])
    GHLib.api_post_string("/repos/{user}/{repo}/git/refs/{ref}", token, json, GHGDp.one_reference)

  delete_reference(token:string, user:string, repo:string, ref:string) =
    GHLib.api_delete_string("/repos/{user}/{repo}/git/refs/{ref}", token, "", GP.expect_204)

  get_tag(token:string, user:string, repo:string, sha:string) =
    GHLib.api_get_full("/repos/{user}/{repo}/git/refs/tags/{sha}", token, [], GHGDp.one_tag)

  create_tag(token:string, user:string, repo:string,
             tag:string, message:string, object:string, typ:string, tagger:option(GitHub.commit_user)) =
    json = GHLib.mkopts([{sreq=("tag",tag)},{sreq=("message",message)},{sreq=("object",object)},
                         {sreq=("type",typ)},{obj=("tagger",commit_user_to_json(tagger))}])
    GHLib.api_post_string("/repos/{user}/{repo}/git/tags", token, json, GHGDp.one_tag)

  get_tree(token:string, user:string, repo:string, sha:string, recursively:bool) =
    data = if recursively then [("recursive","1")] else []
    GHLib.api_get_full("/repos/{user}/{repo}/git/trees/{sha}", token, data, GHGDp.one_tree)

  @private string_of_btc(btc:GitHub.gitdata_btc) =
    match btc with
    | {blob} -> "blob"
    | {tree} -> "tree"
    | {commit} -> "commit"

  @private string_of_sha_or_content(soc:GitHub.gitdata_sha_or_content) =
    match soc with
    | {~sha} -> ("sha",sha)
    | {~content} -> ("content",content)

  @private tree_param_to_json(tp:GitHub.gitdata_tree_element_param) =
    GHLib.mksobj([("path",tp.path),
                  ("mode",tp.mode),
                  ("type",string_of_btc(tp.`type`)),
                  string_of_sha_or_content(tp.sha_or_content)])

  create_tree(token:string, user:string, repo:string, base_tree:option(string), tree:GitHub.gitdata_tree_param) =
    json = GHLib.mkopts([{sopt=("base_tree",base_tree)},{lst=("tree",List.map(tree_param_to_json,tree))}])
    GHLib.api_post_string("/repos/{user}/{repo}/git/trees", token, json, GHGDp.one_tree)

}}
