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
/**
 * {1 About this module}
 *
 * This module defines the core functions of the CouchDB REST API.
 *
 * {1 Where should I start?}
 *
 * You should obviously start by setting up a couchdb server somewhere (cf.
 * http://couchdb.apache.org/ ). This module will then provide the basic
 * functions to manipulate your databases.
 *
 * {1 What if I need more?}
 *
 * If there is some request you cant express with this API, remember that couchdb
 * has a REST interface and go take a look at [WebClient].
 *
 * {1 Other remarks}
 *
 * The module hierarchy and the function's names are self-explanatory.
 */

import stdlib.core.{web.client, web.core, rpc.core}
import stdlib.crypto

/**
 * {1 The types}
 */

/**
 * A record used to represent the path to a database.
 * @param location The adress of the Couchdb server
 * @param name The name of the table.
 */
type CouchDb.db_infos = {
  location : string
  name : string
}

type CouchDb.doc_id = string
type CouchDb.revision = string

/**
 * The different authentication methods you can use in your request.
 */
type CouchDb.authentication_method = // TODO: extend with OAuth
    {None}
  / {Session : string}
  / {HttpLogin : {
        user : string
        pass : string
      }
    }

/**
 * {3 The different kinds of response you can get when using this module}
 */

type CouchDb.failure =
    {BadLogin}
  / {Communication : WebClient.failure}
  / {DbAlreadyExists : string}
  / {DbDoesNotExists : string}
  / {InvalidJsonDoc}
  / {NotLoggedIn}
  / {Other : string}
  / {UnAuthorized}

type CouchDb.success =
    {RawResponse : WebClient.success(string)}
  / {FormatedJson : (int, option(RPC.Json.json))}
  / {Login : (CouchDb.authentication_method, option(RPC.Json.json))}

type CouchDb.result = outcome(CouchDb.success, CouchDb.failure)

// Remark: some syntactic suger for monadic operations would be nice
//    (in the present case there's a huge overhead on the sequencing of
//    operations returning a outcome('a, 'b).)

/**
 * {1 The API}
 */

CouchDb = {{

  // General helper funs
  @private
  gen_headers(auth : CouchDb.authentication_method, maybe_rev : option(string))
    : option(string) =
    maybe_auth =
      match auth
      | {None} -> ""
      | {HttpLogin = ~{user pass}} ->
          encoded = Crypto.Base64.encode("{user}:{pass}")
          "Authorization: Basic {encoded}\r\n"
      | ~{Session} -> "Cookie: {Session}\r\n"
    tmp = Option.default("", Option.map((rev -> "ETAG: {rev}\r\n"), maybe_rev))
            ^ maybe_auth
    match tmp with
      "" -> {none}
      some -> ~{some}

  @private
  gen_uri(host : string, path : string) : outcome(Uri.uri, CouchDb.failure) =
    match Uri.of_string("http://{host}/{path}")
    | {none} -> {failure = {Other = "Bad formatted base url"}}
    | {some = success} -> ~{success}

  /**
   * Retrieving something from the db
   */
  @private
  get_request(auth, host, path, maybe_rev) =
    match gen_uri(host, path)
    | ~{failure} -> ~{failure}
    | {success = uri} ->
        opts =
          custom_headers = gen_headers(auth, maybe_rev)
          { WebClient.Get.default_options with ~custom_headers }
        match WebClient.Get.try_get_with_options(uri, opts)
        | {failure = Communication} -> {failure = ~{Communication}}
        | {success = RawResponse} -> {success = ~{RawResponse}}

  @private
  really_get(auth, host, path, maybe_rev) : CouchDb.result =
    match get_request(auth, host, path, maybe_rev)
      ~{failure} -> ~{failure}
      {success = {RawResponse = ~{code content ... }}} ->
        response = Json.deserialize(content)
        {success = {FormatedJson = (code, response)}}

  @private
  put_request(auth, host, path, mimetype, content) =
    match gen_uri(host, path)
    | ~{failure} -> ~{failure}
    | {success = uri} ->
        opts =
          custom_headers = gen_headers(auth, {none})
          { WebClient.Put.default_options with ~mimetype ~custom_headers }
        match WebClient.Put.try_put_with_options(uri, content, opts)
        | {failure = Communication} -> {failure = ~{Communication}}
        | {success = RawResponse} -> {success = ~{RawResponse}}

  @private
  put_request_unserialize_result(auth, host, path, mimetype, content) : CouchDb.result =
    match put_request(auth, host, path, mimetype, content)
      ~{failure} -> ~{failure}
      {success = {RawResponse = ~{code content ... }}} ->
        response = Json.deserialize(content)
        {success = {FormatedJson = (code, response)} }

  @private
  delete_request(auth, host, path) : CouchDb.result =
    match gen_uri(host, path)
    | ~{failure} -> ~{failure}
    | {success = uri} ->
        opts =
          custom_headers = gen_headers(auth, {none})
          { WebClient.Delete.default_options with ~custom_headers }
        match WebClient.Delete.try_delete_with_options(uri, opts)
        | {failure = Communication} -> {failure = ~{Communication}}
        | {success = ~{code content ... }} ->
            response = Json.deserialize(content)
            {success = {FormatedJson = (code, response)} }

  /**
   * {2 Manipulating databases}
   */

  Database = {{

    /**
     * Example: [list_all(auth, host)]
     * @param host The location of the couchdb server.
     */
    list_all(auth, host) = really_get(auth, host, "_all_dbs", {none})

    create(auth, the_db : CouchDb.db_infos) : CouchDb.result =
      mime = WebClient.Post.default_options.mimetype
      match put_request(auth, the_db.location, "{the_db.name}/", mime, "")
        ~{failure} -> ~{failure}
        {success = {RawResponse = ~{code content ... }}} ->
          match code
          | 401 -> {failure = {UnAuthorized}}
          | 412 -> {failure = {DbAlreadyExists = the_db.name}}
          | _ -> {success = {FormatedJson = (code, Json.deserialize(content))} }

    delete(auth, the_db : CouchDb.db_infos) : CouchDb.result = // redundant with delete_request
      match gen_uri(the_db.location, "{the_db.name}/")
      | ~{failure} -> ~{failure}
      | {success = uri} ->
          opts =
            custom_headers = gen_headers(auth, {none})
            { WebClient.Delete.default_options with ~custom_headers }
          match WebClient.Delete.try_delete_with_options(uri, opts)
          | {failure = Communication} -> {failure = ~{Communication}}
          | {success = ~{code content ... }} ->
              if code == 404
                then {failure = {DbDoesNotExists = the_db.name}}
                else {success = {FormatedJson = (code, Json.deserialize(content))} }

    infos(auth, the_db : CouchDb.db_infos) : CouchDb.result =
      really_get(auth, the_db.location, the_db.name, {none})

    get_revs_limit(auth, the_db : CouchDb.db_infos) : outcome(int, CouchDb.failure)=
      match get_request(auth, the_db.location, "{the_db.name}/_revs_limit", {none})
        ~{failure} -> ~{failure}
        {success = {RawResponse = ~{content ... }}} ->
          {success = Int.of_string(content)}

    set_revs_limit(auth:CouchDb.authentication_method,the_db:CouchDb.db_infos,nb) : CouchDb.result =
      mime = WebClient.Post.default_options.mimetype
      limit = String.of_int(nb)
      put_request_unserialize_result(auth, the_db.location, "{the_db.name}/_revs_limit", mime, limit)

    // TODO: Handling of bulk doc requests
  }}

  /**
   * {2 Manipulating documents}
   */

  Document = {{

    /**
     * Get the last revision of a document.
     */
    get(auth : CouchDb.authentication_method, the_db : CouchDb.db_infos,
        doc_name : CouchDb.doc_id) : CouchDb.result =
      really_get(auth, the_db.location, "{the_db.name}/{doc_name}", {none})

    /**
     * Same as [get] but with an additionnal parameter to specify a revision.
     */
    get_at_revision(auth : CouchDb.authentication_method,
                    the_db : CouchDb.db_infos,
                    doc_name : CouchDb.doc_id,
                    rev : CouchDb.revision) : CouchDb.result =
      really_get(auth, the_db.location, "{the_db.name}/{doc_name}", {some = rev})

    /**
     * Example: [get_attachment(auth, dbinfos, docid, name)]
     * @param name of the attached file.
     */
    get_attachment(auth, the_db : CouchDb.db_infos, docname : CouchDb.doc_id,
                    name : string) : CouchDb.result =
      get_request(auth, the_db.location, "{the_db.name}/{docname}/{name}", {none})

    head(auth, the_db:CouchDb.db_infos, docid:CouchDb.doc_id) : CouchDb.result =
      match gen_uri(the_db.location, "{the_db.name}/{docid}")
      | ~{failure} -> ~{failure}
      | {success = uri} ->
          opts =
            custom_headers = gen_headers(auth, {none})
            { WebClient.Head.default_options with ~custom_headers }
          match WebClient.Head.try_head_with_options(uri, opts)
          | {failure = Communication} -> {failure = ~{Communication}}
          | {success = RawResponse} ->  {success = ~{RawResponse}}

    /**
     * {3 Pushing something to the db}
     */

    /**
     * Example: [post(auth, dbinfos, content)]
     * @param content The json document to insert in the db
     */
    post(auth, the_db : CouchDb.db_infos, document) : CouchDb.result =
      content = Json.serialize(document)
      match gen_uri(the_db.location, the_db.name)
      | ~{failure} -> ~{failure}
      | {success = uri} ->
          opts =
            { WebClient.Post.default_options with
                    custom_headers = gen_headers(auth, {none})
                    content = {some = content}
                    mimetype = "application/json" }
          match WebClient.Post.try_post_with_options(uri, opts)
          | {failure = Communication} -> {failure = ~{Communication}}
          | {success = ~{code content ...}} ->
              response = Json.deserialize(content)
              {success = {FormatedJson = (code, response)}}

    /**
     * Example: [put(auth, dbinfos, docid, document)]
     * @param document The json document to insert in the db
     *
     * N.B. when updating, the document must contain a "_rev" field set to the
     * last revision number of the document (which you can get with a head
     * request, for example).
     * Otherwise couchdb will say that there's a conflict.
     */
    put(auth, the_db:CouchDb.db_infos, doc_name:CouchDb.doc_id, document) : CouchDb.result =
      path = "{the_db.name}/{doc_name}"
      content = Json.serialize(document)
      put_request_unserialize_result(auth,the_db.location,path,"application/json",content)

    /**
     * Example: [put_attachment_at_revision(auth, dbinfos, docid, rev, name, mimetype, content)]
     * @param name of the attached file
     * @param mimetype of the attached file
     * @param content of the attached file
     */
    put_attachment_at_revision(auth, the_db : CouchDb.db_infos,
                                docname:CouchDb.doc_id, rev:CouchDb.revision,
                                name:string, mimetype, content) : CouchDb.result =
      path = "{the_db.name}/{docname}/{name}?rev={rev}"
      put_request_unserialize_result(auth, the_db.location, path, mimetype, content)

    /**
     * Example: [copy(auth, dbinfos, src, dst)]
     * @param src id of the document to copy
     * @param dst id of the new document
     */
    copy(auth, the_db : CouchDb.db_infos, src:CouchDb.doc_id, dst:CouchDb.doc_id) : CouchDb.result =
      match gen_uri(the_db.location, "{the_db.name}/{src}")
      | ~{failure} -> ~{failure}
      | {success = uri} ->
          opts =
            headers = match gen_headers(auth, {none})
                      | {none} -> {some = "Destination: {dst}\r\n"}
                      | ~{some} -> {some = "{some}Destination: {dst}\r\n"}
            {
              operation        = "COPY"
              auth             = {none}
              redirect         = {none}
              timeout_sec      = {some = 36.}
              custom_agent     = {none}
              custom_headers   = headers
              ssl_key          = {none}
              ssl_policy       = {none}
            }
          do_the_job() =
            @callcc(k ->
                on_result(x) = Continuation.return(k, x)
                on_success(x) = on_result({success = x})
                on_failure(x) = on_result({failure = x})
                WebClient.Generic.try_request_with_options_async(uri, "COPY", opts, {none}, on_success, on_failure)
              )
          match do_the_job()
          | {failure = Communication} -> {failure = ~{Communication}}
          | {success = ~{code content ...}} ->
              response = Json.deserialize(content)
              {success = {FormatedJson = (code, response)}}

    /**
     * {3 Removing something from the db}
     */

    delete(auth, the_db:CouchDb.db_infos, docid:CouchDb.doc_id,
            rev:CouchDb.revision) : CouchDb.result =
      delete_request(auth, the_db.location, "{the_db.name}/{docid}?rev={rev}")

    /**
     * Example: [delete_attachment(auth, dbinfos, docid, rev, name)]
     * @param name of the attachment
     */
    delete_attachment(auth, the_db : CouchDb.db_infos, docid : CouchDb.doc_id,
        rev : CouchDb.revision, name : string) : CouchDb.result =
      delete_request(auth, the_db.location, "{the_db.name}/{docid}/{name}?rev={rev}")

  }}

  /**
   * {2 Miscelleanous tools}
   */

  Misc = {{
    /**
     * Example: [get_root(auth, host)]
     * @param host address of the couchdb server.
     */
    get_root(auth : CouchDb.authentication_method, host) : CouchDb.result =
      really_get(auth, host, "", {none})

    /**
     * Example: [get_active_tasks(auth, host)]
     * @param host address of the couchdb server.
     */
    get_active_tasks(a : CouchDb.authentication_method, h) : CouchDb.result =
      really_get(a, h, "_active_tasks", {none})

    /**
     * Example: [get_log_tail(auth, host)]
     * @param host address of the couchdb server.
     * @param len an option of the number of bytes to retrieve
     */
    get_log_tail(auth : CouchDb.authentication_method, host, len:option(int)) : CouchDb.result =
      path = match len with {none} -> "_log" | ~{some} -> "_log?bytes={some}"
      get_request(auth, host, path, {none})
  }}

  /**
   * {2 Session authentification}
   */

  Session = {{

    /**
     * Example: [log_in(host, username, password)]
     * @param host address of the couchdb server.
     * @param username −
     * @param password −
     * @return [{Login = ({Session = cookie}, jsonCtx)}] when login succeed.
     */
    log_in(host, user:string, pass:string) : CouchDb.result =
      match gen_uri(host, "_session")
      | ~{failure} -> ~{failure}
      | {success = uri} ->
          opts = { WebClient.Post.default_options with
                      content = {some = "name={user}&password={pass}"} }
          match WebClient.Post.try_post_with_options(uri, opts)
          | {failure = Communication} -> {failure = ~{Communication}}
          | {success = ~{code content header_get ... }} ->
              match code
              | 401 -> {failure = {BadLogin}}
              | 200 ->
                  cookie = Option.get(header_get("Set-Cookie"))
                  result = Json.deserialize(content)
                  {success = {Login = ({Session = cookie}, result)}}
              | otherwise -> {failure = {Other = "{otherwise}"}}

    /**
     * Example: [log_out(auth, host)]
     * @param host −
     */
    log_out(auth : CouchDb.authentication_method, host) : CouchDb.result =
      match auth
      | {Session = _} -> delete_request(auth, host, "_session")
      | _ -> {failure = {NotLoggedIn}}

    /**
     * Example: [infos(auth, host]
     * @param host −
     */
    infos(auth : CouchDb.authentication_method, host) : CouchDb.result =
      really_get(auth, host, "_session", {none})
  }}

}}
