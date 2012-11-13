/*
    Copyright © 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Dropbox generic API module (v1)
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @author Cedric Soulas, 2012
 * @destination public
 */

import stdlib.apis.common
import stdlib.apis.oauth

/**
 * Dropbox configuration
 *
 * To obtain a credentials, visit:
 *  https://www.dropbox.com/developers/apps
 */
type Dropbox.conf = {
  app_key    : string
  app_secret : string
}

type Dropbox.creds = {
  token  : string
  secret : string
}

type Dropbox.User.status =
   {no_credentials}
 / {request_secret : string; request_token : string}
 / {authenticated : Dropbox.creds}

type Dropbox.metadata_options = {
  file_limit      : int
  hash            : option(string)
  list            : bool
  include_deleted : bool
  rev             : option(int)
}

type Dropbox.thumb_format = {jpeg} / {png}
type Dropbox.thumb_size =
    {small}  // 32x32
  / {medium} // 64x64
  / {large}  // 128x128
  / {s}      // 64x64
  / {m}      // 128x128
  / {l}      // 640x480
  / {xl}     // 1024x768

/* Types returned by API */

type Dropbox.metadata = {
  rev          : string
  thumb_exists : bool
  size         : int // bytes
  size_text    : string
  modified     : option(Date.date)
  path         : string
  icon         : string
  root         : string
  is_deleted   : bool
}

/**
 * Type of an element in a Dropbox folder
 *
 * Note that an empty folder will have its [content] field
 * to [some([])] and that [none] for this field just means
 * that there was no information about the folder files.
 */
type Dropbox.element =
    { file
      metadata  : Dropbox.metadata
      mime_type : string }
  / { folder
      metadata  : Dropbox.metadata
      contents  : option(list(Dropbox.element)) }

type Dropbox.quota_info = {
  shared : int
  normal : int
  total  : int
}

type Dropbox.info = {
  email         : string
  display_name  : string
  referral_link : string
  uid           : int
  country       : string
  quota_info    : Dropbox.quota_info
}

type Dropbox.url = {
  url : string
  expires : Date.date
}

type Dropbox.file = {
  content : binary
  mime_type : string
}

DropboxUser = {{

  @private dropbox_context = UserContext.make( { no_credentials } : Dropbox.User.status)
  get_status() = UserContext.execute(s -> s, dropbox_context)
  is_authenticated() = match get_status() with { authenticated = _ } -> true | _ -> false
  set_status(r) = UserContext.change(_ -> r, dropbox_context)
  set_request(token, secret) = set_status({request_secret = secret; request_token = token})
  set_authenticated(token, secret) = set_status({authenticated = { token = token; secret = secret}})

}}

@private DBParse = {{

  /**
   * Example of date: Fri, 20 Jan 2012 16:18:23 +0000
   */
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
    mon = parser
      | "Jan" -> {january}
      | "Feb" -> {february}
      | "Mar" -> {march}
      | "Apr" -> {april}
      | "May" -> {may}
      | "Jun" -> {june}
      | "Jul" -> {july}
      | "Aug" -> {august}
      | "Sep" -> {september}
      | "Oct" -> {october}
      | "Nov" -> {november}
      | "Dec" -> {december}
    p = parser (!n .)* d=nn " " m=mon " " y=nn " " h=nn ":" min=nn ":" s=nn " " tmz=tmz ->
      tmz(Date.build({year=y month=m day=d h=h min=min s=s}))
    match Parser.try_parse(p, str) with
    | {some=d} -> d
    | {none} ->
      do Log.error("parse_date", "Can't parse '{str}'")
      Date.now()

  build_quota(data) =
    map = JsonOpa.record_fields(data) ? Map.empty
    int(name) = API_libs_private.map_get_int(name, map)
    { shared = int("shared")
      normal = int("normal")
      total  = int("quota")
    } : Dropbox.quota_info

  build_infos(data) =
    f(r) = { failure = r }
    match data.code with
    | 503 -> f({ too_many_requests })
    | 200 ->
      map = API_libs_private.parse_json(data.content)
        |> JsonOpa.record_fields
        |> Option.default(Map.empty, _)
      int(name) = API_libs_private.map_get_int(name, map)
      str(name) = API_libs_private.map_get_string(name, map)
      quota_info =
        StringMap.get("quota_info", map) ? {Record=[]}:RPC.Json.json
        |> build_quota
      { success = { ~quota_info
        email         = str("email")
        referral_link = str("referral_link")
        display_name  = str("display_name")
        uid           = int("uid")
        country       = str("country")
      } : Dropbox.info }
    | code -> f({ unexpected_error = code })

  check_metadata_http_code(code, k) =
    f(r) = { failure = r }
    match code with
    | 503 -> f({ too_many_requests })
    | 507 -> f({ over_storage_quota })
    | 404 -> f({ not_found })
    | 415 -> f({ invalid_image })
    | 411 -> f({ chunked_encoded_not_supported })
    | 403 -> f({ invalid_operation })
    | 406 -> f({ too_many_files })
    | 400 -> f({ bad_input })
    | 304 -> f({ not_modified })
    | 200 -> { success = k() }
    | code -> f({ unexpected_error = code })

  check_file_http_code(code, k) =
    f(r) = { failure = r }
    match code with
    | 503 -> f({ too_many_requests })
    | 404 -> f({ not_found })
    | 200 -> { success = k() }
    | code -> f({ unexpected_error = code })

  build_metadata_internal(elt) : Dropbox.element =
    map = JsonOpa.record_fields(elt) ? Map.empty
    int(name) = API_libs_private.map_get_int(name, map)
    str(name) = API_libs_private.map_get_string(name, map)
    bool(name) = API_libs_private.map_get_bool(name, map, false)
    modified =
      date_str = str("modified")
      if date_str == "" then none
      else some(parse_date(date_str))
    metadata = {
      rev          = str("rev")
      thumb_exists = bool("thumb_exists")
      size         = int("bytes")
      size_text    = str("size")
      modified     = modified
      path         = str("path")
      icon         = str("icon")
      root         = str("root")
      is_deleted   = bool("is_deleted")
    } : Dropbox.metadata
    is_dir = bool("is_dir")
    if is_dir then
      contents : option(list(Dropbox.element)) =
        match StringMap.get("contents", map) with
        | {some={List=l}} ->
          some(List.map(build_metadata_internal, l))
        | _ -> none
      {folder ~metadata ~contents}
    else
      mime_type = str("mime_type")
      {file ~metadata ~mime_type}

  one_metadata(data) =
    k() = parsed = API_libs_private.parse_json(data.content)
          build_metadata_internal(parsed)
    check_metadata_http_code(data.code, k)

  metadata_list(data) =
    k() = match API_libs_private.parse_json(data.content) with
          | {List=l} -> List.map(build_metadata_internal, l)
          | _ -> []
    check_metadata_http_code(data.code, k)

  build_url(data) =
    k() = map = API_libs_private.parse_json(data.content)
          |> JsonOpa.record_fields
          |> Option.default(Map.empty, _)
          str(name) = API_libs_private.map_get_string(name, map)
          { url     = str("url")
            expires = str("expires") |> parse_date
          } : Dropbox.url
    check_file_http_code(data.code, k)

  build_file(data) =
    k() = { content = binary_of_string(data.content)
            mime_type = data.header_get("content-type") ? "text/plain"
          } : Dropbox.file
    check_file_http_code(data.code, k)
}}

@private DBprivate(conf:Dropbox.conf) = {{

  DBOAuth(http_method) = OAuth({
    consumer_key      = conf.app_key
    consumer_secret   = conf.app_secret
    auth_method       = {HMAC_SHA1}
    request_token_uri = "https://api.dropbox.com/1/oauth/request_token"
    authorize_uri     = "https://www.dropbox.com/1/oauth/authorize"
    access_token_uri  = "https://api.dropbox.com/1/oauth/access_token"
    http_method       = http_method
    inlined_auth      = false
    custom_headers    = []
  } : OAuth.parameters)

  wget(host, path, params, creds:Dropbox.creds, parse_fun) =
    uri = "{host}{path}"
    res = DBOAuth({GET}).get_protected_resource_2(uri, params, creds.token, creds.secret)
    match res with
    | {success=s} -> parse_fun(s)
    | {failure=f} -> {failure={ webclient = f}}

  wpost(host, path, params, creds:Dropbox.creds, parse_fun) =
    uri = "{host}{path}"
    res = DBOAuth({POST}).get_protected_resource_2(uri, params, creds.token, creds.secret)
    match res with
    | {success=s} -> parse_fun(s)
    | {failure=f} -> {failure={ webclient = f}}

  wput(host, path, mimetype:string, file:binary, params, creds:Dropbox.creds, parse_fun) =
    uri = "{host}{path}"
    res = DBOAuth({PUT=~{mimetype file}}).get_protected_resource_2(uri, params, creds.token, creds.secret)
    match res with
    | {success=s} -> parse_fun(s)
    | {failure=f} -> {failure={ webclient = f}}

}}

Dropbox(conf:Dropbox.conf) = {{

  // Note: V1 of the API
  @private api_host = "https://api.dropbox.com/1/"
  @private content_host = "https://api-content.dropbox.com/1/"
  @private DBP = DBprivate(conf)

  OAuth = {{

    get_request_token =
      DBP.DBOAuth({GET}).get_request_token

    build_authorize_url(token, callback_url) =
      "{DBP.DBOAuth({GET}).build_authorize_url(token)}&oauth_callback={Uri.encode_string(callback_url)}"

    connection_result =
      DBP.DBOAuth({GET}).connection_result

    get_access_token =
      DBP.DBOAuth({GET}).get_access_token

  }}

  Account = {{

    info(creds) =
      DBP.wget(api_host, "account/info", [], creds, DBParse.build_infos)

  }}

  default_metadata_options = {
    file_limit      = 10000
    hash            = none
    list            = true
    include_deleted = false
    rev             = none
  } : Dropbox.metadata_options

  Files(root:string, file:string) = {{

    @private file_path =
      file =
        if file == "" then "/"
        else if String.sub(0, 1, file) == "/" then file
        else "/{file}"
      "{root}{file}"

    get(rev:option(int), creds) =
      path = "files/{file_path}"
      params = match rev with
        | {none} -> []
        | {some=r} -> [("rev", Int.to_string(r))]
      DBP.wget(content_host, path, params, creds, DBParse.build_file)

    put(mimetype, file:binary, overwrite, parent_rev:option(int), creds) =
      path = "files_put/{file_path}"
      params = [
        ("overwrite", Bool.to_string(overwrite)),
      ] |> (
        match parent_rev with
        | {none} -> identity
        | {some=r} -> List.cons(("parent_rev", Int.to_string(r)), _)
      )
      do ignore(file)
      DBP.wput(content_host, path, mimetype, file, params, creds, DBParse.one_metadata)

    @private format_metadata_options(o:Dropbox.metadata_options) =
      [ ("file_limit", Int.to_string(o.file_limit)),
        ("list", Bool.to_string(o.list)),
        ("include_deleted", Bool.to_string(o.include_deleted)),
      ] |> (
        match o.hash with
          | {none} -> identity
          | {some=h} -> List.cons(("hash", h), _)
      ) |> (
        match o.rev with
          | {none} -> identity
          | {some=r} -> List.cons(("rev", Int.to_string(r)), _)
      )

    metadata(options, creds) =
      path = "metadata/{file_path}"
      params = format_metadata_options(options)
      DBP.wget(api_host, path, params, creds, DBParse.one_metadata)

    /**
     * default: 10 - max: 1000
     */
    revisions(rev_limit:option(int), creds) =
      path = "revisions/{file_path}"
      params = match rev_limit with
        | {none} -> []
        | {some=l} -> [("rev_limit", Int.to_string(l))]
      DBP.wget(api_host, path, params, creds, DBParse.metadata_list)

    restore(rev, creds) =
      path = "restore/{file_path}"
      params = [("rev", Int.to_string(rev))]
      DBP.wpost(api_host, path, params, creds, DBParse.one_metadata)

    /**
     * default and max: 1000
     */
    search(query, include_deleted:bool, file_limit:option(int), creds) =
      path = "search/{file_path}"
      params = [
        ("query", query),
        ("include_deleted", Bool.to_string(include_deleted)),
      ] |> (
        match file_limit with
        | {none} -> identity
        | {some=l} -> List.cons(("file_limit", Int.to_string(l)), _)
      )
      DBP.wget(api_host, path, params, creds, DBParse.metadata_list)

    shares(creds) =
      path = "shares/{file_path}"
      DBP.wpost(api_host, path, [], creds, DBParse.build_url)

    media(creds) =
      path = "media/{file_path}"
      DBP.wpost(api_host, path, [], creds, DBParse.build_url)

    /**
     * Prefer [jpeg] for photos while [png] is better for
     * screenshots and digital art
     */
    thumbnails(format:Dropbox.thumb_format, size:Dropbox.thumb_size, creds) =
      path = "thumbnails/{file_path}"
      format = match format with
        | {jpeg} -> "JPEG"
        | {png} -> "PNG"
      size = match size with
        | {small}  -> "small"
        | {medium} -> "medium"
        | {large}  -> "large"
        | {s}      -> "s"
        | {m}      -> "m"
        | {l}      -> "l"
        | {xl}     -> "xl"
      params = [
        ("format", format),
        ("size", size),
      ]
      DBP.wget(content_host, path, params, creds, DBParse.build_file)

  }}

  FileOps = {{

    copy(root, from_path, to_path, creds) =
      path = "fileops/copy"
      params = [
        ("root", root),
        ("from_path", from_path),
        ("to_path", to_path),
      ]
      DBP.wpost(api_host, path, params, creds, DBParse.one_metadata)

    create_folder(root, path, creds) =
      rpath = "fileops/create_folder"
      params = [
        ("root", root),
        ("path", path),
      ]
      DBP.wpost(api_host, rpath, params, creds, DBParse.one_metadata)

    delete(root, path, creds) =
      rpath = "fileops/delete"
      params = [
        ("root", root),
        ("path", path),
      ]
      DBP.wpost(api_host, rpath, params, creds, DBParse.one_metadata)

    move(root, from_path, to_path, creds) =
      path = "fileops/move"
      params = [
        ("root", root),
        ("from_path", from_path),
        ("to_path", to_path),
      ]
      DBP.wpost(api_host, path, params, creds, DBParse.one_metadata)

  }}

  /**
   * This module helps to request an access to a user Dropbox account.
   * It takes a User module in parameter. You can use the default DropboxUser module
   * which stores tokens in the UserContext.
   *
   * First, use get_login_url to redirect the user to the Dropbox request page.
   * Then, parse the callback url to retrieve the token and negociate the access
   * with this token and the function get_access.
   */
  Auth(User) = {{

    err(v) : outcome(void, string) = { failure = v }

    get_login_url(redirect) =
      match OAuth.get_request_token(redirect)
      | {success = s} -> do User.set_request(s.token, s.secret) : void
                 { success = OAuth.build_authorize_url(s.token, redirect) }
      | {error = error} -> { failure = "Error getting request token: {error}" }

    get_access(raw_token) = pass1(raw_token)

    @private pass1(raw_token) =
      match(User.get_status())
      | {~request_secret ~request_token} -> pass2(request_token, request_secret, raw_token)
      | _ -> err("The current user don't have a request token")

    @private pass2(request_token, request_secret, raw_token) =
      match OAuth.connection_result(raw_token)
      | {success = s} -> pass3(request_token, request_secret, s)
      | {error=error} -> err("The providing arguments are invalid: {error}")

    @private pass3(request_token, request_secret, s) =
      if(s.token == request_token) then
        if(s.verifier == "" && s.secret == "") then
          pass4(s.token, request_secret)
        else
          err("The connection result contains those unexpected values: verifier: '{s.verifier}' and secret: '{s.secret}'")
      else err("The request token of the current user doesn't match provided arguments.")

    @private pass4(token, secret) =
      match OAuth.get_access_token(token, secret, "")
      | {success = s} -> do User.set_authenticated(s.token, s.secret) : void; { success = void }
      | {error=error} -> err("Impossible to retreive an access token: {error}")

    are_valid_creds(creds) =
      match Account.info(creds) with
      | {success=info} -> info.quota_info.total != -1
      | _ -> false


  }}

}}
