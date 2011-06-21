/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License as published by the Free
    Software Foundation, either version 3 of the License, or (at your option)
    any later version.

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
 * GitHub library API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination private
 */

//package stdlib.apis.github.lib
import stdlib.apis.common
import stdlib.apis.github

type GHParse.map = {
  str    : string -> string
  date   : string -> string
  int    : string -> int
  bool   : string -> bool
  float  : string -> float
  list   : string -> list(RPC.Json.json)
  record : string -> RPC.Json.json
  exists : string -> bool
}

GHLib = {{

  @private AL = API_libs
  @private host = "https://github.com/api/v2/json"

  generic_build_path(path, options) =
    if options == [] then path
    else "{path}?{API_libs.form_urlencode(options)}"

  add_if(k,v,l) =
    if v == "" then l else List.add((k, v), l)

  /* Temporary duplicate while parse functions are built */

  api_get(path, data, parse_fun) =
    final_path = generic_build_path("{host}{path}", data)
    do jlog(final_path)
    match Uri.of_string(final_path) with
    | {none} -> none
    | {some=uri} ->
      match WebClient.Get.try_get(uri) with
      | {failure=_} -> none
      | {success=s} ->
        do jlog(s.content)
        parse_fun(s.content)
      end

  /**
   * Shortcut for GET requests with only a token
   */
  api_get_logged(path, token, parse_fun) =
    data = if token == "" then []
           else [("access_token", token)]
    api_get(path, data, parse_fun)

  full_post(base,path, data, parse_fun) =
    txtdata = AL.form_urlencode(data)
    do jlog("{base}{path} with {txtdata}")
    match Uri.of_string("{base}{path}") with
    | {none} -> none
    | {some=uri} ->
      match WebClient.Post.try_post(uri,txtdata) with
      | {failure=_} -> none
      | {success=s} ->
        do jlog(s.content)
        parse_fun(s.content)
      end

  api_post = full_post(host, _, _, _)

  /**
   * Shortcut for POST requests with only a token
   */
  api_post_logged(path, token, parse_fun) =
    data = [("access_token", token)]
    api_post(path, data, parse_fun)

}}


GHParse = {{

/* WIP: Date parser
  parse_date(str) =
    n = parser v=([0-9]+) -> Int.of_string(Text.to_string(v))
    tmz = parser
      |
    p = parser
      | y=n "/" m=n "/" d=n " " h=n ":" m=n ":" s=n " " tmz=
*/

  map_funs(srcmap) =
    map = JsonOpa.record_fields(srcmap) ? Map.empty
    str = API_libs_private.map_get_string(_, map)
    date = str
    int = API_libs_private.map_get_int(_, map)
    bool = API_libs_private.map_get_bool(_, map, false)
    float = API_libs_private.map_get_float(_, map)
    list(name) =
      match Map.get(name, map) with
      | {some={List=l}} -> l | _ -> []
    record(name) = Map.get(name, map) ? {Record=[]}
    exists = Map.mem(_, map)
    ~{str date int bool float
      list record exists}:GHParse.map

  obj_list(srcmap, aux) =
    map = JsonOpa.record_fields(srcmap) ? Map.empty
    {some=Map.fold(aux,map,[])}

  dewrap_obj(res, main_key, process) =
    match Json.of_string(res) with
    | {some={Record=r}} ->
      match List.assoc(main_key, r) with
      | {some={Record=o}} -> process({Record=o})
      | _ -> {none}
      end
    | _ -> {none}

  dewrap_list(res, main_key, process) =
    aux(e, acc) =
      match process(e) with
      | {some=s} -> List.add(s,acc) | _ -> acc
    match Json.of_string(res) with
    | {some={Record=r}} ->
      match List.assoc(main_key, r) with
      | {some={List=l}} -> {some=List.fold(aux,l,[])}
      | _ -> {none}
      end
    | _ -> {none}

  multiple_strings(res, main_key) =
    process(e) = match e with
      | {String=s} -> {some=s} | _ -> {none}
    dewrap_list(res, main_key, process)

  one_string(res, main_key) =
    match Json.of_string(res) with
    | {some={Record=r}} ->
      match List.assoc(main_key, r) with
      | {some={String=s}} -> {some=s}
      | _ -> {none}
      end
    | _ -> {none}

  /* Parsers used in several sub-libs */

  get_public_key(srcmap) =
    m = map_funs(srcmap)
    if m.exists("key") then
      res = {
        title = m.str("title")
        id    = m.int("id")
        key   = m.str("key")
      } : GitHub.public_key
      {some=res}
    else {none}

  get_repo(force_some)(srcmap) =
    m = map_funs(srcmap)
    if force_some || m.exists("name") then
      res = {
        name          = m.str("name")
        owner         = m.str("owner")
        homepage      = m.str("homepage")
        url           = m.str("url")
        description   = m.str("description")
        language      = m.str("language")
        created_at    = m.date("created_at")
        pushed_at     = m.date("pushed_at")
        size          = m.int("size")
        private       = m.bool("private")
        fork          = m.bool("fork")
        forks         = m.int("forks")
        watchers      = m.int("watchers")
        has_downloads = m.bool("has_downloads")
        has_wiki      = m.bool("has_wiki")
        has_issues    = m.bool("has_issues")
        open_issues   = m.int("open_issues")
      } : GitHub.repository
      {some=res}
    else {none}

}}
