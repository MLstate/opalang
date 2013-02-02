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
 * GitHub gist API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

package stdlib.apis.github.gist
import stdlib.apis.github
import stdlib.apis.github.lib

/* Types returned by API */

type GitHub.gist_comment = {
  id         : int
  url        : string
  body       : string
  user       : GitHub.short_user
  created_at : Date.date
}

/**
 * Type of a GitHub gist provided to [GHGist.list_gists]
 */
type GHGist.t =
    { self : string }   /** Gists of authenticated user (self="" means public gists) */
  / { user : string }   /** Gists of given user */
  / { `public` }        /** All public gists */
  / { starred : string} /** Authenticated user's starred gists */

/**
 * Type of a GitHub gist file update provided to [GHGist.edit_gist]
 */
type GHGist.file_update =
  (string,
     { update : {content  : string} }
   / { modify : {filename : string
                 content  : string } }
   / { delete })


@private GHGp = {{

  @private GP = GHParse

  get_comment(srcmap) = 
    m = GP.map_funs(srcmap)
    if m.exists("id")
    then
      comment = {
        id         = GP.get_id(m)
        url        = m.str("url")
        body       = m.str("body")
        user       = GP.get_rec(m, "user", GP.get_short_user)
        created_at = m.date("created_at")
      } : GitHub.gist_comment
      {some=comment}
    else none

  one_gist(res) = GP.dewrap_whole_obj(res, GP.get_gist)
  multiple_gists(res) = GP.dewrap_whole_list(res, GP.get_gist)

  one_comment(res) = GP.dewrap_whole_obj(res, get_comment)
  multiple_comments(res) = GP.dewrap_whole_list(res, get_comment)

}}

GHGist = {{

  @private GP = GHParse

  @private gist_path(gist:GHGist.t) =
    match gist with
    | {~user} -> ("/users/{user}/gists","")
    | {~self} -> ("/gists",self)
    | {`public`} -> ("/gists/public","")
    | {~starred} -> ("/gists/starred",starred)

  list_gists(gist:GHGist.t) =
    (path, token) = gist_path(gist)
    GHLib.api_get_full(path, token, [], GHGp.multiple_gists)

  get_gist(gist_id:int) =
    path = "/gists/{gist_id}"
    GHLib.api_get_full(path, "", [], GHGp.one_gist)

  create_gist(token:string, description:option(string), public:bool, files:list((string, string))) =
    files = List.to_string_using("\{","}",",",List.map(((fn,cn) -> "\"{fn}\":\{\"content\":\"{cn}\"}"),files))
    json = GHLib.mkopts([{sopt=("description",description)},{breq=("public",public)},{req=("files",files)}])
    GHLib.api_post_string("/gists", token, json, GHGp.one_gist)

  edit_gist(token:string, id:int, description:option(string), files:list(GHGist.file_update)) =
    files = List.to_string_using("\{","}",",",
                                 List.map(((fn,fu) ->
                                           match fu with
                                           | {update=~{content}} ->
                                              "\"{fn}\":\{\"content\":\"{content}\"}"
                                           | {modify=~{filename content}} ->
                                              "\"{fn}\":\{\"filename\":\"{filename}\",\"content\":\"{content}\"}"
                                           | {delete} ->
                                              "\"{fn}\":null}"
                                       ),files))
    json = GHLib.mkopts([{sopt=("description",description)},{req=("files",files)}])
    GHLib.api_patch_string("/gists/{id}", token, json, GHGp.one_gist)

  star_gist(token:string, id:int) = GHLib.api_put_string("/gists/{id}/star", token, "", GP.expect_204)

  unstar_gist(token:string, id:int) = GHLib.api_delete_string("/gists/{id}/star", token, "", GP.expect_204)

  is_gist_starred(token:string, id:int) = GHLib.api_get_full("/gists/{id}/star", token, [], GP.expect_204_404)

  fork_gist(token:string, id:int) = GHLib.api_post_string("/gists/{id}/fork", token, "", GHGp.one_gist)

  delete_gist(token:string, id:int) = GHLib.api_delete_string("/gists/{id}", token, "", GP.expect_204)

  gist_comments(token:string, id:int) = GHLib.api_get_full("/gists/{id}/comments", token, [], GHGp.multiple_comments)

  get_comment(token:string, id:int) = GHLib.api_get_full("/gists/comments/{id}", token, [], GHGp.one_comment)

  create_comment(token:string, gist_id:int, body) =
    json = GHLib.mkopts([{sreq=("body",body)}])
    GHLib.api_post_string("/gists/{gist_id}/comments", token, json, GHGp.one_comment)

  edit_comment(token:string, gist_id:int, body) =
    json = GHLib.mkopts([{sreq=("body",body)}])
    GHLib.api_patch_string("/gists/comments/{gist_id}", token, json, GHGp.one_comment)

  delete_comment(token:string, id:int) = GHLib.api_delete_string("/gists/comments/{id}", token, "", GP.expect_204)

  // TODO: file urls can be found in the gist data
  //get_gist_file(gist_id:string, filename:string) =
  //  path = "https://raw.github.com/gist/{gist_id}/{filename}"
  //  GHLib.api_get_full(path, "", [], some)

}}
