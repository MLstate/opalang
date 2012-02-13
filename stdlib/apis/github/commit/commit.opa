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
 * GitHub commit API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

//package stdlib.apis.github.commit
import stdlib.apis.github
import stdlib.apis.github.lib

/* Types returned by API */

type GitHub.commit = {
  id            : string
  parents       : list(string)
  author        : GitHub.commit_user
  message       : string
  url           : string
  commited_date : Date.date
  authored_date : Date.date
  tree          : string
  committer     : GitHub.commit_user
}

type GitHub.commit_more = {
  base          : GitHub.commit
  modified      : list((string,string))
  removed       : list(string)
}

@private GHCp = {{

  @private GP = GHParse

  get_commit_internal(m) =
    parents = List.filter_map(
      j -> match j:RPC.Json.json with
        | {Record=r} ->
          match List.assoc("id",r) with
          | {some={String=s}} -> some(s)
          | {some=v} -> some(Json.serialize(v))
          | {none} -> none
          end
        | _ -> none,
      m.list("parents"))
    { id            = m.str("id")
      parents       = parents
      author        = GP.get_commit_user(m.record("author"))
      message       = m.str("message")
      url           = m.str("url")
      commited_date = m.date("commited_date")
      authored_date = m.date("authored_date")
      tree          = m.str("tree")
      committer     = GP.get_commit_user(m.record("committer"))
    } : GitHub.commit    

  get_commit(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id") then
      some(get_commit_internal(m))
    else none

  get_commit_more(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id") then
      base = get_commit_internal(m)
      removed = m.list("removed")
       |> List.filter_map(
         x ->
           match x with
           | {String=s} -> some(s)
           | _ -> none,
         _ )
      modified = m.list("modified")
       |> List.fold(
         e, acc ->
           match e:RPC.Json.json with
           | {Record=r} ->
             List.fold(
               (k,v), acc ->
                 vv = match v:RPC.Json.json with
                   | {String=s} -> s
                   | _ -> Json.serialize(v)
                 List.add((k,vv),acc),
               r, acc
             )
           | _ -> acc,
         _, [])
      some(~{base modified removed}:GitHub.commit_more)
    else none

  multiple_commits(res) =
    GP.dewrap_list(res, "commits", get_commit)

  one_commit_more(res) =
    GP.dewrap_obj(res, "commit", get_commit_more)

}}

GHCommit = {{

  @private p(o:string,r:string) = "{o}/{r}"

 /**
  * Gets a list of commits on a branch of a repo
  *
  * @param owner Owner of the repo
  * @param repo Repository to get commits from
  * @param branch Branch to get commits from
  * @param page Page of commits displayed. First page is 1. If a value inferior to 1 is provided, it is considered as 1.
  */
  get_commits(owner, repo, branch:string, page:int) =
    path = "/commits/list/{p(owner,repo)}/{branch}"
    /* Force the min value of page to 1, GitHub also does
       it on its side anyway. */
    page = if page < 1 then 1 else page
    data = [("page",Int.to_string(page))]
    GHLib.api_get(path, data, GHCp.multiple_commits)

 /**
  * Gets a list of commit on a file on a branch of a repo
  * 
  * @param owner Owner of the repo
  * @param repo Repository to get commits from
  * @param branch Branch to get commits from
  * @param file File to follow
  * @param page Page of commits displayed. First page is 1. If a value inferior to 1 is provided, it is considered as 1.
  */
  get_file_commits(owner, repo, branch:string, file:string, page) =
    path = "/commits/list/{p(owner,repo)}/{branch}/{file}"
    /* Force the min value of page to 1, GitHub also does
       it on its side anyway. */
    page = if page < 1 then 1 else page
    data = [("page",Int.to_string(page))]
    GHLib.api_get(path, data, GHCp.multiple_commits)

 /**
  * Gets detailled information about one commit
  */
  get_commit(owner, repo, sha:string) =
    path = "/commits/show/{p(owner,repo)}/{sha}"
    GHLib.api_get(path, [], GHCp.one_commit_more)

}}
