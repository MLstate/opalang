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
 * GitHub commit API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

//package stdlib.apis.github.object
import stdlib.apis.github
import stdlib.apis.github.lib

/* Types returned by API */

type GitHub.tree_meta = {
  sha       : string
  name      : string
  size      : int
  mime_type : string
  mode      : string
  node_type : {tree}/{blob}/{other:string}
}

type GitHub.blob = {
  meta : GitHub.tree_meta
  data : string
}

@private GHOp = {{

  @private GP = GHParse

  get_tree_meta_inner(m) =
    node_type = match m.str("type") with
      | "tree" -> {tree}
      | "blob" -> {blob}
      | other -> ~{other}
    { sha       = m.str("sha")
      name      = m.str("name")
      size      = m.int("size")
      mime_type = m.str("mime_type")
      mode      = m.str("mode")
      node_type = node_type
    } : GitHub.tree_meta

  get_tree_meta(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("sha") then
      some(get_tree_meta_inner(m))
    else none

  get_blob(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("sha") then
      meta = {get_tree_meta_inner(m) with node_type = {blob}}
      res = {
        meta = meta
        data = m.str("data")
      } : GitHub.blob
      some(res)
    else none

  get_blob_meta(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("sha") then
      some({get_tree_meta_inner(m) with node_type = {blob}})
    else none

  get_blobs(srcmap) =
    match srcmap with
    | {Record=r} ->
      res = List.map(
        (k,v) -> match v:RPC.Json.json with
          | {String=s} -> (k,s)
          | _ -> (k,Json.serialize(v)),
        r)
      some(res)
    | _ -> none

  multiple_metas(res) =
    GP.dewrap_list(res, "tree", get_tree_meta)

  one_blob_meta(res) =
    GP.dewrap_obj(res, "blob", get_blob_meta)

  one_blob(res) =
    GP.dewrap_obj(res, "blob", get_blob)

  text_blobs(res) =
    GP.dewrap_obj(res, "blobs", get_blobs)

  full_blobs(res) =
    GP.dewrap_list(res, "blobs", get_blob_meta)

}}

GHObject = {{

  @private p(o:string,r:string) = "{o}/{r}"

  get_tree(owner, repo, tree_sha:string) =
    path = "/tree/show/{p(owner,repo)}/{tree_sha}"
    GHLib.api_get(path, [], GHOp.multiple_metas)

  get_recursive_tree(owner, repo, tree_sha:string) =
    path = "/tree/full/{p(owner,repo)}/{tree_sha}"
    GHLib.api_get(path, [], GHOp.multiple_metas)

  get_blob_meta(owner, repo, tree_sha:string, file:string) =
    path = "/blob/show/{p(owner,repo)}/{tree_sha}/{file}"
    data = [("meta","1")]
    GHLib.api_get(path, data, GHOp.one_blob_meta)

  get_blob(owner, repo, tree_sha:string, file:string) =
    path = "/blob/show/{p(owner,repo)}/{tree_sha}/{file}"
    GHLib.api_get(path, [], GHOp.one_blob)

  get_all_blobs(owner, repo, tree_sha:string) =
    path = "/blob/all/{p(owner,repo)}/{tree_sha}"
    GHLib.api_get(path, [], GHOp.text_blobs)

  get_all_full_blobs(owner, repo, tree_sha:string) =
    path = "/blob/full/{p(owner,repo)}/{tree_sha}"
    GHLib.api_get(path, [], GHOp.full_blobs)

  get_raw_blob(owner, repo, tree:string) =
    path = "/blob/show/{p(owner,repo)}/{tree}"
    GHLib.api_get(path, [], some)

}}
