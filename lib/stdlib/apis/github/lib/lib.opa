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
 * GitHub library API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination private
 */

package stdlib.apis.github.lib
import stdlib.apis.common
import stdlib.apis.github

type GHParse.map = {
  str    : string -> string
  date   : string -> Date.date
  int    : string -> int
  bool   : string -> bool
  float  : string -> float
  list   : string -> list(RPC.Json.json)
  record : string -> RPC.Json.json
  exists : string -> bool
}

GHLib = {{

  @private AL = API_libs
  @private host = "https://api.github.com"

  string_of_encoding(encoding:GitHub.encoding) =
    match encoding with
    | {utf8} -> "utf-8"
    | {base64} -> "base64"

  encoding_of_string(str:string) : GitHub.encoding =
    match str with
    | "utf8" -> {utf8}
    | "base64" -> {base64}
    | _ -> {utf8}

  string_of_state(s:GitHub.state) =
    match s with
    | {open} -> "open"
    | {closed} -> "closed"

  string_of_direction(direction:GitHub.direction) =
    match direction with
    | {asc} -> "asc"
    | {desc} -> "desc"

  generic_build_path(path, options) =
    if options == []
    then path
    else "{path}?{API_libs.form_urlencode(options)}"

  get_final_path(base, path, token, options) =
    match token with
    | "" -> generic_build_path("{base}{path}", options)
    | _ -> generic_build_path("{base}{path}", options++[("access_token",token)])

  userpath(user:GitHub.user_id,what) =
    what = if what == "" then "" else "/{what}"
    match user with
    | ~{self} -> ("/user{what}", [("access_token", self)])
    | ~{login} -> ("/users/{login}{what}", [])

  add_if(k,v,l) =
    if v == "" then l else List.add((k, v), l)

  mkopt(name,raw) = "\"{name}\":{raw}"
  mksopt(name,opt:string) = "\"{name}\":\"{opt}\""
  mkiopt(name,opt:int) = "\"{name}\":{opt}"
  mkbopt(name,opt:bool) = "\"{name}\":{opt}"
  mksub(l:list((string,string))) = String.concat(",",List.map(((n,v) -> mkopt(n,v)),l))
  mkssub(l:list((string,string))) = String.concat(",",List.map(((n,v) -> mksopt(n,v)),l))
  mksobj(l:list((string,string))) = List.to_string_using("\{","}",",",List.map(((n,v) -> mksopt(n,v)),l))
  mklst(l:list(string),mk) = List.to_string_using("[","]",",",List.map((e -> mk(e)),l))
  mkslst(l) = mklst(l,(s -> "\"{s}\""))

  mkopts(opts) =
    do_lst(n,l,mk) =
      match l with
      | [] -> {none}
      | _ -> {some="\"{n}\":[{String.concat(",",List.map((e -> mk(e)),l))}]"}
      end
    do_obj(n,o,mk) =
      match o with
      | [] -> {none}
      | _ -> {some="\"{n}\":\{{String.concat(",",List.map(((n,v) -> mk(n,v)),o))}}"}
      end
    List.to_string_using("\{","}",",",
      List.filter_map((opt ->
        match opt with
        | {req=(n,r)} -> {some=mkopt(n,r)}
        | {sreq=(n,v)} -> {some=mksopt(n,v)}
        | {ireq=(n,v)} -> {some=mkiopt(n,v)}
        | {breq=(n,v)} -> {some=mkbopt(n,v)}
        | {rcst=(n,f,v)} -> {some=mksopt(n,f(v))}
        | {opt=(n,o)} -> Option.map((v -> mkopt(n,v)),o)
        | {sopt=(n,o)} -> Option.map((v -> mksopt(n,v)),o)
        | {iopt=(n,o)} -> Option.map((v -> mkiopt(n,v)),o)
        | {bopt=(n,o)} -> Option.map((v -> mkbopt(n,v)),o)
        | {ocst=(n,f,o)} -> Option.map((v -> mksopt(n,f(v))),o)
        | {lst=(n,l)} -> do_lst(n,l,(v -> v))
        | {slst=(n,l)} -> do_lst(n,l,(s -> "\"{s}\""))
        | {obj=(n,o)} -> do_obj(n,o,mkopt)
       ),opts))

  opt_generic(string_of:'a -> string,name:string,opt:option('a)) : list((string,string)) =
    match opt with
    | {some=opt} -> [(name,string_of(opt))]
    | _ -> []
  opt_string = opt_generic((s -> s),_,_)
  opt_int = opt_generic(Int.to_string,_,_)
  opt_bool = opt_generic(Bool.to_string,_,_)
  opt_state = opt_generic(string_of_state,"state",_)
  opt_direction = opt_generic(string_of_direction,"direction",_)

  /* Temporary duplicate while parse functions are built */

  @private api_get_full_(base, path, token, options, parse_fun) =
    final_path = get_final_path(base, path, token, options)
    do Log.info("github","GET {final_path}")
    match Uri.of_string(final_path) with
    | {none} -> none
    | {some=uri} ->
      match WebClient.Get.try_get(uri) with
      | {failure=_} -> none
      | {success=s} ->
        do Log.info("github","code={s.code} content={if s.content == "" then "none" else s.content}")
        parse_fun(s)
      end
  api_get_full(path, token, options, parse_fun) = api_get_full_(host, path, token, options, parse_fun)
  api_get(path:string, options, parse_fun) = api_get_full_(host, path, "", options, parse_fun)

  mtjson = "application/json"
  mtform = "application/x-www-form-urlencoded"

  @private full_post_string(base, path, token, mimetype, data, parse_fun) =
    final_path = get_final_path(base, path, token, [])
    match Uri.of_string(final_path) with
    | {none} -> none
    | {some=uri} ->
      do Log.info("github","POST {data} on {uri}")
      options = { WebClient.Post.default_options with ~mimetype content={some=data} }
      match WebClient.Post.try_post_with_options(uri,options) with
      | {failure=_} -> none
      | {success=s} ->
        do Log.info("github","code={s.code} content={if s.content == "" then "none" else s.content}")
        parse_fun(s)
      end
  full_post(base, path, data, parse_fun) = full_post_string(base, path, "", mtform, AL.form_urlencode(data), parse_fun)
  full_post_forms(base, path, token, forms, parse_fun) =
    bound = Random.string(20)
    content_type = "multipart/form-data; boundary={bound}"
    forms = List.map((f ->
                      match f with
                      | {form=(name, content)} ->
                         "--{bound}\r\nContent-Disposition: form-data; name=\"{name}\"\r\n\r\n{content}\r\n"
                      | {file=(name, filename, content_type, content)} ->
                         "--{bound}\r\nContent-Disposition: form-data; name=\"{name}\"; filename=\"{filename}\"\r\nContent-Type={content_type}\r\n\r\n{content}\r\n"
                     ),forms)
    content = String.concat("",List.append(forms,["--{bound}--\r\n"]))
    full_post_string(base, path, token, content_type, content, parse_fun)
  api_post = full_post(host, _, _, _)
  api_post_string = full_post_string(host, _, _, mtform, _, _)

  @private full_put_string(base, path, token, data, parse_fun) =
    final_path = get_final_path(base, path, token, [])
    match Uri.of_string(final_path) with
    | {none} -> none
    | {some=uri} ->
      do Log.info("github","PUT {data} on {uri}")
      match WebClient.Put.try_put(uri,data) with
      | {failure=_} -> none
      | {success=s} ->
        do Log.info("github","code={s.code} content={if s.content == "" then "none" else s.content}")
        parse_fun(s)
      end
  full_put(base, path, data, parse_fun) = full_put_string(base, path, "", AL.form_urlencode(data), parse_fun)
  api_put = full_put(host, _, _, _)
  api_put_string = full_put_string(host, _, _, _, _)

  @private full_delete_string(base, path, token, data:string, parse_fun) =
    final_path = get_final_path(base, path, token, [])
    match Uri.of_string(final_path) with
    | {none} -> none
    | {some=uri} ->
      do Log.info("github","DELETE {data} on {uri}")
      // IMPORTANT: this is needed because the node.js client sticks "Transfer-Encoding: chunked"
      //            into the headers for DELETE which confuses the github server.
      options = { default_options("DELETE") with custom_headers=["Transfer-Encoding: identity"] }
      match try_delete_content(uri,data,options) with
      | {failure=_} -> none
      | {success=s} ->
        do Log.info("github","code={s.code} content={if s.content == "" then "none" else s.content}")
        parse_fun(s)
      end
  full_delete(base, path, data, parse_fun) = full_delete_string(base, path, "", AL.form_urlencode(data), parse_fun)
  api_delete = full_delete(host, _, _, _)
  api_delete_string = full_delete_string(host, _, _, _, _)

  // WebClient doesn't have PATCH, yet
  @private default_options(op) : WebClient.Generic.options =
    { operation       = op
      auth            = {none}
      custom_headers  = []
      custom_agent    = {none}
      redirect        = {none}
      timeout_sec     = {some = 36.}
      ssl_key         = {none}
      ssl_policy      = {none}
    }
  @private try_generic(op:string, location:Uri.uri, content:string): WebClient.result(string) =
    try_generic_with_options(op, location, content, default_options(op))
  @private try_generic_with_options(op:string, location:Uri.uri,
                                    content:string, options:WebClient.Generic.options): WebClient.result(string) =
    @callcc(k ->
              on_result(x) = Continuation.return(k, x)
              try_generic_with_options_async(op, location, content, options, on_result))
  @private try_generic_with_options_async(op:string, location:Uri.uri, content:string, options:WebClient.Generic.options,
                                          on_result: WebClient.result(string) -> void): void =
    generic_options = { options with
                          operation      = op
                          custom_headers = ["Content-Length: {String.length(content)}"]++options.custom_headers
                      }
    on_success(x) = on_result({success = x})
    on_failure(x) = on_result({failure = x})
    WebClient.Generic.try_request_with_options_async(location, op, generic_options, {some=content}, on_success, on_failure)

  try_patch(location, content) = try_generic("PATCH", location, content)
  try_delete_content(location, content, options) = try_generic_with_options("DELETE", location, content, options)

  @private full_patch_string(base, path, token, data:string, parse_fun) =
    final_path = get_final_path(base, path, token, [])
    match Uri.of_string(final_path) with
    | {none} -> none
    | {some=uri} ->
      do Log.info("github","PATCH {data} on {uri}")
      match try_patch(uri,data) with
      | {failure=_} -> none
      | {success=s} ->
        do Log.info("github","code={s.code} content={if s.content == "" then "none" else s.content}")
        parse_fun(s)
      end
  full_patch(base, path, data, parse_fun) = full_patch_string(base, path, "", AL.form_urlencode(data), parse_fun)
  api_patch = full_patch(host, _, _, _)
  api_patch_string = full_patch_string(host, _, _, _, _)

}}

GHParse = {{

  parse_date(str) =
    int_of_text(t) = Int.of_string(Text.to_string(t))
    n = parser k=[0-9] -> k
    nn = parser v=(n+) -> int_of_text(v)
    do_shift(forward,h,min) =
      d = { Duration.zero with ~forward ~h ~min }
        |> Duration.of_human_readable
      Date.advance(_, d)      
    shift(forward,h,m) =
      do_shift(forward,int_of_text(h),int_of_text(m))
    tmz = parser
      | "Z" -> identity
      | "-" h=(n n) m=(n n) -> shift(true, h, m)
      | "-" h=(n n) ":" m=(n n) -> shift(true, h, m)
      | "+" h=(n n) m=(n n) -> shift(false, h, m)
      | "+" h=(n n) ":" m=(n n) -> shift(false, h, m)
      | .* -> identity
    p = parser
      | y=nn "/" m=nn "/" d=nn " " h=nn ":" min=nn ":" s=nn " " tmz=tmz ->
        m = Date.Month.of_int(m-1)
        tmz(Date.build({year=y month=m day=d h=h min=min s=s}))
      | y=nn "-" m=nn "-" d=nn "T" h=nn ":" min=nn ":" s=nn tmz=tmz ->
        m = Date.Month.of_int(m-1)
        tmz(Date.build({year=y month=m day=d h=h min=min s=s}))
      | y=nn "-" m=nn "-" d=nn " " h=nn ":" min=nn ":" s=nn ->
        m = Date.Month.of_int(m-1)
        do_shift(true,8,0)(Date.build({year=y month=m day=d h=h min=min s=s}))
      | y=nn "-" m=nn "-" d=nn ->
        m = Date.Month.of_int(m-1)
        do_shift(true,8,0)(Date.build({year=y month=m day=d}))
    Parser.try_parse(p, str)

  @private tzfmt = Date.generate_printer("%z")
  @private fmt = Date.generate_printer("%FT%T")

  date_to_string(d:Date.date) =
    tz = Date.to_formatted_string(tzfmt, d)
    tz =
      match String.length(tz) with
      | 4 -> String.sub(0,2,tz)^":"^String.sub(2,2,tz)
      | 5 -> String.sub(0,3,tz)^":"^String.sub(3,2,tz)
      | _ -> tz
    "{Date.to_formatted_string(fmt, d)}{tz}"

  map_funs(srcmap) =
    map = JsonOpa.record_fields(srcmap) ? Map.empty
    str = API_libs_private.map_get_string(_, map)
    date(field) = (str(field) |> parse_date) ? Date.epoch
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

  add_option(o, l) = match o with | {some=e} -> [e|l] | {none} -> l

  get_str_opt(json) =
    match json with
    | {String=s} -> {some=s}
    | _ -> {none}

  get_rec_opt(m:GHParse.map, name, get) =
    if m.exists(name)
    then
      match m.record(name) with
      | {Record=[]} -> none
      | {Record=r} -> {some=get({Record=r})}
      | _ -> none
    else none

  get_rec(m:GHParse.map, name, get) = get(m.record(name))

  get_list(m:GHParse.map, name, get) = List.fold((r, acc -> add_option(get(r),acc)),m.list(name),[])

  get_raw(m:GHParse.map, name) =
    match m.record(name) with
    | {Record=[]} -> []
    | {Record=r} -> r
    | _ -> []

  xhtml(res:WebClient.success(string)) = {some=Xhtml.of_string(res.content)}

  dewrap_whole_obj(res:WebClient.success(string), process) =
    match Json.of_string(res.content) with
    | {some={Record=r}} -> process({Record=r})
    | _ -> {none}

  // Still needed - see legacy search functions
  dewrap_obj(res:WebClient.success(string), main_key, process) =
    match Json.of_string(res.content) with
    | {some={Record=r}} ->
      match List.assoc(main_key, r) with
      | {some={Record=o}} -> process({Record=o})
      | _ -> {none}
      end
    | _ -> {none}

  dewrap_whole_list(res:WebClient.success(string), process) =
    aux(e, acc) = //List.add(process(e),acc)
      match process(e) with
      | {some=s} -> List.add(s,acc)
      | _ -> acc
    match Json.of_string(res.content) with
    | {some={List=l}} -> {some=List.fold(aux,l,[])}
    | _ -> {none}

  // Still needed - see legacy search functions
  dewrap_list(res:WebClient.success(string), main_key, process) =
    aux(e, acc) =
      match process(e) with
      | {some=s} -> List.add(s,acc) | _ -> acc
    match Json.of_string(res.content) with
    | {some={Record=r}} ->
      match List.assoc(main_key, r) with
      | {some={List=l}} -> {some=List.fold(aux,l,[])}
      | _ -> {none}
      end
    | _ -> {none}

  multiple_strings(res:WebClient.success(string), _) =
    process(e) =
      match e with
      | {String=s} -> {some=s}
      | _ -> {none}
    dewrap_whole_list(res, process)

  list_strings(res:WebClient.success(string)) =
    process(e) =
      match e with
      | {String=s} -> {some=s}
      | _ -> {none}
    dewrap_whole_list(res, process)

  multiple_objects(res:WebClient.success(string), process) =
    process_el(e) =
      match e with
      | {Record=r} -> process({Record=r})
      | _ -> {none}
    dewrap_whole_list(res, process_el)

  one_string(res:WebClient.success(string), main_key) =
    match Json.of_string(res.content) with
    | {some={Record=r}} ->
      match List.assoc(main_key, r) with
      | {some={String=s}} -> {some=s}
      | _ -> {none}
      end
    | _ -> {none}

  /* Parsers used in several sub-libs */

  expect_code(codes:list((int,'a)), res:WebClient.success(string)) : option('a) =
    match List.assoc(res.code, codes) with
    | {some=v} -> {some=v}
    | {none} ->
      do Log.info("github","expect_code: bad code {res.code} (expected {String.concat(",",List.map(((c,_) -> "{c}"),codes))})")
      none

  expect_201 = expect_code([(201,{})],_)
  expect_204 = expect_code([(204,{})],_)
  expect_302 = expect_code([(302,{})],_)
  expect_204_404 = expect_code([(204,true),(404,false)],_)

  get_commit_user(srcmap) =
    m = map_funs(srcmap)
    { name  = m.str("name")
      email = m.str("email")
      date  = m.date("date")
    } : GitHub.commit_user

  get_commit_user_opt(srcmap) = {some=get_commit_user(srcmap)}

  get_id(m) =
    match m.record("id") with
    | {Int=i} -> i
    | {String="user-"} -> 0
    | {String=s} ->
       get_int(s) = Parser.try_parse_opt(parser i=Rule.natural -> {some=i},s)
       Option.default(0,get_int(List.head(List.rev(String.explode("-",s)))))
    | _ -> 0

  get_short_user_opt(srcmap) =
    m = map_funs(srcmap)
    if m.exists("login")
    then
      res = {
        id            = get_id(m)
        login         = m.str("login")
        gravatar_id   = m.str("gravatar_id")
        avatar_url    = m.str("avatar_url")
        url           = m.str("url")
        `type`        = m.str("type")
        contributions = m.int("contributions")
      } : GitHub.short_user
      {some=res}
    else none
  default_short_user = { id=-1; login="unknown user"; url=""; gravatar_id=""; avatar_url=""; `type`="missing"; contributions=-1 }
  get_short_user(srcmap) = Option.default(default_short_user,get_short_user_opt(srcmap))

  get_public_key(srcmap) =
    m = map_funs(srcmap)
    if m.exists("key") then
      res = {
        id       = get_id(m)
        key      = m.str("key")
        title    = m.str("title")
        url      = m.str("url")
        verified = m.bool("verified")
      } : GitHub.public_key
      {some=res}
    else {none}

  get_repo(force_some)(srcmap) =
    m = map_funs(srcmap)
    if force_some || m.exists("name") then
      res = {
        url           = m.str("url")
        html_url      = m.str("html_url")
        clone_url     = m.str("clone_url")
        git_url       = m.str("git_url")
        ssh_url       = m.str("ssh_url")
        svn_url       = m.str("svn_url")
        mirror_url    = m.str("mirror_url")
        id            = get_id(m)
        owner         = get_rec(m, "owner", get_short_user)
        name          = m.str("name")
        full_name     = m.str("full_name")
        description   = m.str("description")
        homepage      = m.str("homepage")
        language      = m.str("language")
        private       = m.bool("private")
        fork          = m.bool("fork")
        forks         = m.int("forks")
        watchers      = m.int("watchers")
        size          = m.int("size")
        master_branch = m.str("master_branch")
        open_issues   = m.int("open_issues")
        pushed_at     = m.date("pushed_at")
        created_at    = m.date("created_at")
        updated_at    = m.date("updated_at")
        organization  = get_rec(m, "organization", get_short_user_opt)
        parent        = get_rec(m, "parent", get_repo(false))
        source        = get_rec(m, "source", get_repo(false))
        has_issues    = if m.exists("has_issues") then {some=m.bool("has_issues")} else {none}
        has_wiki      = if m.exists("has_wiki") then {some=m.bool("has_wiki")} else {none}
        has_downloads = if m.exists("has_downloads") then {some=m.bool("has_downloads")} else {none}
      } : GitHub.repository
      {some=res}
    else {none}

  get_repo_comment(srcmap) = 
    m = map_funs(srcmap)
    if m.exists("id")
    then
      comment = {
        html_url   = m.str("html_url")
        url        = m.str("url")
        id         = get_id(m)
        body       = m.str("body")
        path       = m.str("path")
        position   = m.int("position")
        line       = m.int("line")
        commit_id  = m.str("commit_id")
        user       = get_rec(m, "user", get_short_user)
        created_at = m.date("created_at")
        updated_at = m.date("updated_at")
      } : GitHub.repo_comment
      {some=comment}
    else none

  get_download(srcmap) =
    m = map_funs(srcmap)
    if m.exists("id")
    then
      download = {
        url            = m.str("url")
        html_url       = m.str("html_url")
        id             = get_id(m)
        name           = m.str("name")
        description    = m.str("description")
        size           = m.int("size")
        download_count = m.int("download_count")
        content_type   = m.str("content_type")
      } : GitHub.download
      {some=download}
    else none

  get_gist_file(srcmap) =
    m = map_funs(srcmap)
    if m.exists("filename")
    then
      file = {
        size     = m.int("size")
        filename = m.str("filename")
        raw_url  = m.str("raw_url")
        content  = m.str("content")
      }
      {some=file}
    else none

  get_gist_fork(srcmap) =
    m = map_funs(srcmap)
    if m.exists("user")
    then
      user = get_rec(m, "user", get_short_user)
      fork = {
        user      = user
        url        = m.str("url")
        created_at = m.date("created_at")
      }
      {some=fork}
    else none

  get_gist_change_status(srcmap) =
    m = map_funs(srcmap)
    if m.exists("total")
    then
      cs = {
        deletions = m.int("deletions")
        additions = m.int("additions")
        total     = m.int("total")
      }
      {some=cs}
    else none

  get_gist_hist(srcmap) =
    m = map_funs(srcmap)
    if m.exists("user")
    then
      user = get_rec(m, "user", get_short_user)
      change_status =
        match m.record("change_status") with
        | {Record=r} -> get_gist_change_status({Record=r})
        | _ -> none
      history = {
        user          = user
        url           = m.str("url")
        change_status = change_status
        version       = m.str("version")
        committed_at  = m.date("committed_at")
      }
      {some=history}
    else none

  get_gist(srcmap) =
    m = map_funs(srcmap)
    if m.exists("id")
    then
      user = get_rec(m, "user", get_short_user)
      files =
        match m.record("files") with
        | {Record=r} ->
           List.fold(((filename, file), acc ->
                       match get_gist_file(file) with
                       | {some=file} -> [(filename,file)|acc]
                       | {none} -> acc),r,[])
        | _ -> []
      forks = get_list(m, "forks", get_gist_fork)
      history = get_list(m, "history", get_gist_hist)
      res = {
        id           = get_id(m)
        `public`     = m.bool("public")
        description  = m.str("description")
        user         = user
        url          = m.str("url")
        html_url     = m.str("html_url")
        git_push_url = m.str("git_push_url")
        git_pull_url = m.str("git_pull_url")
        comments     = m.int("comments")
        created_at   = m.date("created_at")
        updated_at   = m.date("updated_at")
        files        = files
        forks        = forks
        history      = history
      } : GitHub.gist
      some(res)
    else none

  get_state(m) =
    match m.str("state") with
    | "closed" -> {closed}
    | "open" | _ -> {open}

  get_label(srcmap) =
    m = map_funs(srcmap)
    if m.exists("name") then
      res = {
        name  = m.str("name")
        url   = m.str("url")
        color = m.str("color")
      } : GitHub.label
      some(res)
    else none

/* milestone
{"open_issues":1,
 "number":1,
 "due_on":null,
 "closed_issues":0,
 "creator":{"avatar_url":"https://sec...",
            "login":"nrs135",
            "gravatar_id":"b0df98244f9039e7546c9d08313cfbf8",
            "id":1378405,
            "url":"https://api.github.com/users/nrs135"},
 "description":null,
 "created_at":"2012-09-19T10:36:32Z",
 "title":"First milestone",
 "id":177762,
 "state":"open",
 "url":"https://api.github.com/repos/nrs135/TestGitHubAPIRepo/milestones/1"
}
*/

  get_milestone(srcmap) =
    m = map_funs(srcmap)
    if m.exists("title") then
      res = {
        url           = m.str("url")
        number        = m.int("number")
        state         = get_state(m)
        title         = m.str("title")
        description   = m.str("description")
        creator       = get_rec(m, "creator", get_short_user)
        open_issues   = m.int("open_issues")
        closed_issues = m.int("closed_issues")
        created_at    = m.date("created_at")
        due_on        = m.date("due_on")
      } : GitHub.milestone
      some(res)
    else none

  get_pull_request(srcmap) =
    m = map_funs(srcmap)
    if m.exists("html_url") then
      res = {
        html_url  = m.str("html_url")
        diff_url  = m.str("diff_url")
        patch_url = m.str("patch_url")
      } : GitHub.pull_request
      some(res)
    else none

  get_issue(srcmap) =
    m = map_funs(srcmap)
    if m.exists("title") then
      res = {
        url          = m.str("url")
        html_url     = m.str("html_url")
        number       = m.int("number")
        state        = get_state(m)
        title        = m.str("title")
        body         = m.str("body")
        user         = get_rec(m, "user", get_short_user)
        labels       = get_list(m, "labels", get_label)
        assignee     = get_rec(m, "assignee", get_short_user)
        milestone    = get_rec(m, "milestone", get_milestone)
        comments     = m.int("comments")
        pull_request = get_rec(m, "pull_request", get_pull_request)
        closed_at    = m.date("closed_at")
        created_at   = m.date("created_at")
        updated_at   = m.date("updated_at")
      } : GitHub.issue
      some(res)
    else none

  get_issue_comment(srcmap) =
    m = map_funs(srcmap)
    if m.exists("id") then
      res = {
        id          = get_id(m)
        url        =  m.str("url")
        body        = m.str("body")
        user        = get_rec(m, "user", get_short_user)
        created_at  = m.date("created_at")
        updated_at  = m.date("updated_at")
      } : GitHub.issue_comment
      some(res)
    else none

  get_ref(srcmap) =
    m = map_funs(srcmap)
    { label      = m.str("label")
      ref        = m.str("ref")
      sha        = m.str("sha")
      repository = m.record("repository") |> get_repo(true) |> Option.get
      user       = get_short_user(m.record("user"))
    } : GitHub.ref

/*
[{"parents":[{"sha":"0136294cf02206767036865c7b9c4bee83372827",
              "url":"https://api.github.com/repos/hbbio/webshell/commits/0136294cf02206767036865c7b9c4bee83372827"}],
  "sha":"df99eaa753bdeb65c735ead68554eb4e67720358",
  "committer":{"avatar_url":"https://secure.gravatar.com/avatar/b0df98244f9039e7546c9d08313cfbf8?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png",
               "login":"nrs135",
               "gravatar_id":"b0df98244f9039e7546c9d08313cfbf8",
               "id":1378405,
               "url":"https://api.github.com/users/nrs135"},
  "commit":{"comment_count":0,
            "tree":{"sha":"21536c9aa218d12272c3d53b91e60d6e46a59827",
                    "url":"https://api.github.com/repos/hbbio/webshell/git/trees/21536c9aa218d12272c3d53b91e60d6e46a59827"},
            "message":"Added Blekko search.",
            "committer":{"date":"2012-02-09T00:17:45-08:00",
                         "email":"norman.scaife@mlstate.com",
                         "name":"Norman Scaife"},
            "url":"https://api.github.com/repos/hbbio/webshell/git/commits/df99eaa753bdeb65c735ead68554eb4e67720358",
            "author":{"date":"2012-02-09T00:17:45-08:00",
                      "email":"norman.scaife@mlstate.com",
                      "name":"Norman Scaife"}},
  "author":{"avatar_url":"https://secure.gravatar.com/avatar/b0df98244f9039e7546c9d08313cfbf8?d=https://a248.e.akamai.net/assets.github.com%2Fimages%2Fgravatars%2Fgravatar-user-420.png",
            "login":"nrs135",
            "gravatar_id":"b0df98244f9039e7546c9d08313cfbf8",
            "id":1378405,
            "url":"https://api.github.com/users/nrs135"},
  "url":"https://api.github.com/repos/hbbio/webshell/commits/df99eaa753bdeb65c735ead68554eb4e67720358"}]
*/

  get_stats(srcmap) =
    m = map_funs(srcmap)
    {
      additions = m.int("additions")
      deletions = m.int("deletions")
      total     = m.int("total")
    } : GitHub.stats

  @private get_full_commit_(m) =
    {
      sha       = m.str("sha")
      commit    = get_rec(m, "commit", get_commit)
      author    = get_rec(m, "author", get_short_user)
      parents   = get_list(m, "parents", get_url_sha_opt)
      url       = m.str("url")
      committer = get_rec(m, "committer", get_short_user)
      stats     = get_rec_opt(m, "stats", get_stats)
      files     = get_list(m, "files", get_file_opt)
    } : GitHub.full_commit

  get_full_commit(srcmap) = get_full_commit_(map_funs(srcmap))

  get_full_commit_opt(srcmap) =
    m = map_funs(srcmap)
    if m.exists("commit")
    then {some=get_full_commit_(m)}
    else none

/*
[{"head":{"user":{"gravatar_id":"f6ce56b7883a131667475cc2f9a17d2a",
                  "login":"Aqua-Ye",
                  "id":85234,
                  "url":"https://api.github.com/users/Aqua-Ye",
                  "avatar_url":"https://secu..."},
          "sha":"c544aa29f1c379a22ca3e131981f183ccaf03c29",
          "ref":"master",
          "label":"Aqua-Ye:master",
          "repo":{"forks_count":0,
                  "has_wiki":true,
                  "html_url":"https://github.com/Aqua-Ye/webshell",
                  "watchers":0,
                  "mirror_url":null,
                  "watchers_count":0,
                  "svn_url":"https://github.com/Aqua-Ye/webshell",
                  "owner":{"gravatar_id":"f6ce56b7883a131667475cc2f9a17d2a",
                           "login":"Aqua-Ye",
                           "id":85234,
                           "url":"https://api.github.com/users/Aqua-Ye",
                           "avatar_url":"https://secur..."}
                 "description":""
                 "pushed_at":"2012-08-29T13:56:49Z"
                 "forks":0
                 "open_issues":0
                 "open_issues_count":0
                 "has_issues":false
                 "created_at":"2012-08-29T09:59:52Z"
                 "clone_url":"https://github.com/Aqua-Ye/webshell.git"
                 "language":"Opa"
                 "has_downloads":true
                 "size":120
                 "fork":true
                 "ssh_url":"git@github.com:Aqua-Ye/webshell.git"
                 "updated_at":"2012-08-29T13:56:50Z"
                 "full_name":"Aqua-Ye/webshell"
                 "name":"webshell"
                 "private":false
                 "id":5598719
                 "homepage":""
                 "git_url":"git://github.com/Aqua-Ye/webshell.git"
                 "url":"https://api.github.com/repos/Aqua-Ye/webshell"}},
      "body":"",
      "user":{"gravatar_id":"f6ce56b7883a131667475cc2f9a17d2a",
              "login":"Aqua-Ye",
              "id":85234,
              "url":"https://api.github.com/users/Aqua-Ye",
              "avatar_url":"https://secu..."},
      "html_url":"https://github.com/hbbio/webshell/pull/6",
      "merged_at":null,
      "_links":{"self":{"href":"https://api.github.com/repos/hbbio/webshell/pulls/6"},
                "issue":{"href":"https://api.github.com/repos/hbbio/webshell/issues/6"},
                "comments":{"href":"https://api.github.com/repos/hbbio/webshell/issues/6/comments"},
                "review_comments":{"href":"https://api.github.com/repos/hbbio/webshell/pulls/6/comments"},
                "html":{"href":"https://github.com/hbbio/webshell/pull/6"}},
      "closed_at":null,
      "created_at":"2012-08-29T13:59:53Z",
      "assignee":null,
      "title":"Fixed compilation and run with Opa 1.0.5",
      "patch_url":"https://github.com/hbbio/webshell/pull/6.patch",
      "diff_url":"https://github.com/hbbio/webshell/pull/6.diff",
      "base":{"user":null,
              "sha":"1f87ee5a9b2a862d24b4f5e17342e7603a4672f2",
              "ref":"master",
              "label":"hbbio:master",
              "repo":{"forks_count":3,
                      "has_wiki":true,
                      "html_url":"https://github.com/hbbio/webshell",
                      "watchers":13,
                      "mirror_url":null,
                      "watchers_count":13,
                      "svn_url":"https://github.com/hbbio/webshell",
                      "owner":{"gravatar_id":"f9492f8c897f46459626d8bd44cc8b9f",
                               "login":"hbbio",
                               "id":808274,
                               "url":"https://api.github.com/users/hbbio",
                               "avatar_url":"https://sec..."},
                      "description":"",
                      "pushed_at":"2012-05-09T10:46:44Z",
                      "forks":3,
                      "open_issues":5,
                      "open_issues_count":5,
                      "has_issues":true,
                      "created_at":"2011-12-15T21:29:00Z",
                      "clone_url":"https://github.com/hbbio/webshell.git",
                      "language":"Opa",
                      "has_downloads":true,
                      "size":124,
                      "fork":false,
                      "ssh_url":"git@github.com:hbbio/webshell.git",
                      "updated_at":"2012-09-13T01:20:05Z",
                      "full_name":"hbbio/webshell",
                      "name":"webshell",
                      "private":false,
                      "id":2990713,
                      "homepage":"",
                      "git_url":"git://github.com/hbbio/webshell.git",
                      "url":"https://api.github.com/repos/hbbio/webshell"}},
  "state":"open",
  "number":6,
  "updated_at":"2012-08-29T13:59:53Z",
  "milestone":null,
  "id":2193062,
  "issue_url":"https://github.com/hbbio/webshell/issues/6",
  "url":"https://api.github.com/repos/hbbio/webshell/pulls/6"}
]
*/

  get_pull_req_int(m) =
    state =
      match m.str("state") with
      | "open"   -> {open}
      | "closed" -> {closed=m.date("closed_at")}
      | x        -> {other=x}
    { 
      url           = m.str("url")
      html_url      = m.str("html_url")
      diff_url      = m.str("diff_url")
      patch_url     = m.str("patch_url")
      issue_url     = m.str("issue_url")
      number        = m.int("number")
      state         = state
      title         = m.str("title")
      body          = m.str("body")
      created_at    = m.date("created_at")
      updated_at    = m.date("updated_at")
      closed_at     = m.date("closed_at")
      merged_at     = m.date("merged_at")
      head          = get_rec(m, "head", get_ref)
      base          = get_rec(m, "base", get_ref)
      _links        = get_rec(m, "_links", get__links)
      user          = get_short_user(m.record("user"))
      merged        = m.bool("merged")
      mergeable     = m.bool("mergeable")
      merged_by     = get_short_user(m.record("merged_by"))
      comments      = m.int("comments")
      commits       = m.int("commits")
      additions     = m.int("additions")
      deletions     = m.int("deletions")
      changed_files = m.int("number")
    } : GitHub.pull_req    

  get_pull_req(srcmap) =
    m = map_funs(srcmap)
    if m.exists("state")
    then some(get_pull_req_int(m))
    else none

  get_plan(srcmap) =
    m = map_funs(srcmap)
    { name          = m.str("name")
      space         = m.int("space")
      private_repos = m.int("private_repos")
    } : GitHub.plan
  get_plan_opt(srcmap) = {some=get_plan(srcmap)}

  get_more(m) =
    if m.exists("total_private_repos") then
      user_more = {
        total_private_repos = m.int("total_private_repos")
        collaborators       = m.int("collaborators")
        disk_usage          = m.int("disk_usage")
        owned_private_repos = m.int("owned_private_repos")
        private_gists       = m.int("private_gists")
        plan                = get_plan(m.record("plan"))
      } : GitHub.user_more
      {some=user_more}
    else {none}

  get_user(srcmap) =
    m = map_funs(srcmap)
    if m.exists("id")
    then
      res = {
        id           = get_id(m)
        login        = m.str("login")
        name         = m.str("name")
        company      = m.str("company")
        gravatar_id  = m.str("gravatar_id")
        created_at   = m.date("created_at")
        location     = m.str("location")
        blog         = m.str("blog")
        public_repos = m.int("public_repos")
        public_gists = m.int("public_gists")
        followers    = m.int("followers")
        following    = m.int("following")
        user_type    = m.str("type")
        avatar_url   = m.str("avatar_url")
        url          = m.str("url")
        html_url     = m.str("html_url")
        more         = get_more(m)
      } : GitHub.user
      {some=res}
    else {none}

  get_url_sha(srcmap) =
    m = map_funs(srcmap)
    { url = m.str("url")
      sha = m.str("sha")
    } : GitHub.url_sha

  get_url_sha_opt(srcmap) = {some=get_url_sha(srcmap)}

  get_commit_internal(m) =
    { url           = m.str("url")
      sha           = m.str("sha")
      author        = get_commit_user(m.record("author"))
      committer     = get_commit_user(m.record("committer"))
      message       = m.str("message")
      tree          = get_url_sha(m.record("tree"))
      //parents       = List.filter_map(get_url_sha_opt,m.list("parents"))
    } : GitHub.commit    

  get_commit(srcmap) =
    m = map_funs(srcmap)
    get_commit_internal(m)

  get_commit_opt(srcmap) =
    m = map_funs(srcmap)
    if m.exists("author")
    then some(get_commit_internal(m))
    else none

  @private get_file_(m) =
    {
      sha       = m.str("sha")
      filename  = m.str("filename")
      status    = m.str("status")
      additions = m.int("additions")
      deletions = m.int("deletions")
      changes   = m.int("changes")
      blob_url  = m.str("blob_url")
      raw_url   = m.str("raw_url")
      patch     = m.str("patch")
    } : GitHub.file

  get_file(srcmap) =
    m = map_funs(srcmap)
    get_file_(m)

  get_file_opt(srcmap) =
    m = map_funs(srcmap)
    if m.exists("filename")
    then {some=get_file_(m)}
    else none

  get_href(srcmap) =
    m = map_funs(srcmap)
    { href = m.str("href") }

  get__links(srcmap) =
    m = map_funs(srcmap)
    {
      self            = get_rec(m, "self", get_href)
      html            = get_rec(m, "html", get_href)
      comments        = get_rec(m, "comments", get_href)
      review_comments = get_rec(m, "review_comments", get_href)
      pull_request    = get_rec(m, "pull_request", get_href)
      git             = get_rec(m, "git", get_href)
    } : GitHub.links

  get__links_no_href(srcmap) =
    m = map_funs(srcmap)
    {
      self            = m.str("self")
      html            = m.str("html")
      comments        = m.str("comments")
      review_comments = m.str("review_comments")
      pull_request    = m.str("pull_request")
      git             = m.str("git")
    } : GitHub.links_no_href

  @private get_id_name_url_(m) =
    {
      url  = m.str("url")
      name = m.str("name")
      id   = get_id(m)
    } : GitHub.id_name_url

  get_id_name_url(srcmap) =
    m = map_funs(srcmap)
    get_id_name_url_(m)

  get_id_name_url_opt(srcmap) =
    m = map_funs(srcmap)
    if m.exists("name")
    then {some=get_id_name_url_(m)}
    else none

  one_short_user(res) = dewrap_whole_obj(res, get_short_user_opt) |> Option.get
  multiple_short_users(res) = dewrap_whole_list(res, get_short_user_opt)

  multiple_id_name_urls(res) = dewrap_whole_list(res, get_id_name_url_opt)

  multiple_files(res) = dewrap_whole_list(res, get_file_opt)

  one_issue(res) = dewrap_whole_obj(res, get_issue)
  multiple_issues(res) = dewrap_whole_list(res, get_issue)

  multiple_full_commits(res) = dewrap_whole_list(res, get_full_commit_opt)

}}
