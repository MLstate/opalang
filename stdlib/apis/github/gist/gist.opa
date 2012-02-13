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
 * GitHub gist API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

//package stdlib.apis.github.gist
import stdlib.apis.github
import stdlib.apis.github.lib

/* Types returned by API */

type GitHub.gist_comment = {
  id         : int
  user       : string
  created_at : Date.date
  updated_at : Date.date
  body       : string
}

type GitHub.gist = {
  owner       : string
  public      : bool
  repo        : string
  created_at  : Date.date
  description : string
  files       : list(string)
  comments    : list(GitHub.gist_comment)
}

@private GHGp = {{

  @private GP = GHParse

  get_comment(srcmap) = 
    m = GP.map_funs(srcmap)
    { id         = m.int("id")
      user       = m.str("user")
      created_at = m.date("created_at")
      updated_at = m.date("updated_at")
      body       = m.str("body")
    } : GitHub.gist_comment

  get_gist(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("repo") then
      files = List.map(
        v -> match v:RPC.Json.json with
          | {String=s} -> s
          | _ -> Json.serialize(v),
        m.list("files"))
      comments = List.filter_map(
        v -> match v:RPC.Json.json with
          | {Record=_} -> some(get_comment(v))
          | _ -> none,
        m.list("comments"))
      res = {
        owner       = m.str("owner")
        public      = m.bool("public")
        repo        = m.str("repo")
        created_at  = m.date("created_at")
        description = m.str("description")
        files       = files
        comments    = comments
      } : GitHub.gist
      some(res)
    else none

  multiple_gists(res) =
    GP.dewrap_list(res, "gists", get_gist)

}}

GHGist = {{

  @private host = "https://gist.github.com/api/v1/json"

  get_gist(gist_id:string) =
    path = "{host}/{gist_id}"
    r = GHLib.api_get_full(path, [], GHGp.multiple_gists)
    match r with
    | {some=l} ->
      if l == [] then none else some(List.head(l))
    | _ -> none

  get_user_gists(user:string) =
    path = "{host}/gists/{user}"
    GHLib.api_get_full(path, [], GHGp.multiple_gists)

  get_gist_file(gist_id:string, filename:string) =
    path = "https://raw.github.com/gist/{gist_id}/{filename}"
    GHLib.api_get_full(path, [], some)

}}
