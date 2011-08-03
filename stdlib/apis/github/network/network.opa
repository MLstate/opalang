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

/**
 * GitHub (secret) network API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

//package stdlib.apis.github.network
import stdlib.apis.github
import stdlib.apis.github.lib

/* Types returned by API */

type GitHub.network_user_head = {
  name : string
  id   : string
}

type GitHub.network_user = {
  name  : string
  repo  : string
  heads : list(GitHub.network_user_head)
}

type GitHub.network_block = {
  name  : string
  start : int
  count : int
}

type GitHub.network_meta = {
  nethash  : string
  focus    : int
  dates    : list(Date.date)
  users    : list(GitHub.network_user)
  blocks   : list(GitHub.network_block)
  spacemap : list(list(list(int)))
}

type GitHub.network_commit = {
  id       : string
  author   : string
  login    : string
  gravatar : string
  date     : Date.date
  time     : int
  space    : int
  parents  : list({index:int}/{hash:string})
}

@private GHNp = {{

  @private get_sublist(m,name,f) =
    List.filter_map(
      v -> match v:RPC.Json.json with
        | {Record=_} -> some(f(v))
        | _ -> none,
      m.list(name))

  @private GP = GHParse

  get_user_head(srcmap) =
    m = GP.map_funs(srcmap)
    { name = m.str("name")
      id   = m.str("id")
    } : GitHub.network_user_head

  get_network_user(srcmap) =
    m = GP.map_funs(srcmap)
    heads = get_sublist(m,"heads",get_user_head)
    { name  = m.str("name")
      repo  = m.str("repo")
      heads = heads
    } : GitHub.network_user

  get_network_block(srcmap) =
    m = GP.map_funs(srcmap)
    { name  = m.str("name")
      start = m.int("start")
      count = m.int("count")
    } : GitHub.network_block

  get_network_meta(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("nethash") then
      dates = List.filter_map(
        v -> match v:RPC.Json.json with
          | {String=s} -> GP.parse_date(s)
          | _ -> none,
        m.list("dates"))
      users = get_sublist(m,"users",get_network_user)
      blocks = get_sublist(m,"blocks",get_network_block)
      spacemap = List.filter_map(
        v0 -> match v0:RPC.Json.json with
          | {List=l0} ->
            elt0 = List.filter_map(
              v1 -> match v1:RPC.Json.json with
                | {List=l1} ->
                  elt1 = List.filter_map(
                    v2 -> match v2:RPC.Json.json with
                      | {Int=i} -> some(i)
                      | _ -> none,
                    l1)
                  some(elt1)
                | _ -> none,
              l0)
            some(elt0)
          | _ -> none,
        m.list("spacemap"))
      res = {
        nethash  = m.str("nethash")
        focus    = m.int("focus")
        dates    = dates
        users    = users
        blocks   = blocks
        spacemap = spacemap
      } : GitHub.network_meta
      some(res)
    else none

  get_network_commit(srcmap) =
    m = GP.map_funs(srcmap)
    if m.exists("id") then
      parents = List.filter_map(
        v -> match v:RPC.Json.json with
          | {String=s} -> some({hash=s})
          | {Int=i}    -> some({index=i})
          | _          -> none,
        m.list("parents"))
      res = {
        id       = m.str("id")
        author   = m.str("author")
        login    = m.str("login")
        gravatar = m.str("gravatar")
        date     = m.date("date")
        time     = m.int("time")
        space    = m.int("space")
        parents  = parents
      } : GitHub.network_commit
      some(res)  
    else none

  one_meta(res) =
    match Json.of_string(res) with
    | {some={Record=r}} ->
      do jlog("{r}")
      get_network_meta({Record=r})
    | _ -> none

  multiple_n_commits(res) =
    GP.dewrap_list(res, "commits", get_network_commit)

}}

GHNetwork = {{

  get_meta(owner:string,repo:string) =
    path = "https://github.com/{owner}/{repo}/network_meta"
    GHLib.api_get_full(path, [], GHNp.one_meta)

  get_data(owner:string, repo:string, nethash:string, start:int, stop:int) =
    path = "https://github.com/{owner}/{repo}/network_data_chunk"
    data = [("nethash",nethash)]
      |> (if start < 0 then identity
          else List.add(("start",Int.to_string(start)), _))
      |> (if stop < 0 then identity
          else List.add(("end",Int.to_string(stop)), _))
    GHLib.api_get_full(path, data, GHNp.multiple_n_commits)

}}
