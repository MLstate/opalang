/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin server
import stdlib.core.{json, xhtml, rpc.core, map, iter}


/**
 * {1 About this module}
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 */

/**
 * {1 Types defined in this module}
 */

/**
 * The method used by the client to when placing the request to the server.
 *
 * Use this type and the corresponding function [HttpRequest.Generic.get_method] if you are defining a REST service and wish to
 * differentiate requests based on their method.
 *
 * see http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html for a complete definition of methods
 */
type HttpRequest.method =
  { get }    /**A request intended to retrieve information (typically, a web page, an image, etc.) */
/ { head }   /**A request intended to retrieve meta-information (typically, last modification date of a web page, etc.)*/
/ { post }   /**A request intended to transmit information to the server (typically, a form)*/
/ { put }    /**A request intended to store a value on the server*/
/ { delete } /**A request intended to remove a previously stored value from the server*/
/ { trace }  /**A request intended to trigger a form of debugging on the server*/
/ { connect }
/ { options }/**A request intended do determine what form of communication is available on the server*/
/ { other: string }


type HttpRequest.payload = external
type HttpRequest.msg_list = external

type HttpRequest.headers = list({name:string value:string})

/**
 * A multipart request
 */
type HttpRequest.multipart = iter({part:HttpRequest.part headers:HttpRequest.headers})

/**
 * A part of multipart request is :
 * - [~{name value}] : A simple part named [name] and contains [value]
 * - [~{name filename content}] : This part is an uploaded file.
 *   [content] is a dynamic closure, during the file uploading it
 *   returns [~{partial}] when uploading is finished returns the file
 *   [~{content}].
 */
type HttpRequest.part =
  {name : string; value : string} /
  {name : string; filename : string; content : -> HttpRequest.partcontent}

/**
 * Type returned by the [content] fields of [HttpRequest.part] :
 * -[~{partial}] : Corresponding to the size of received data
 * -[~{content}] : Content of uploaded data.
 */
type HttpRequest.partcontent =
  {partial : int} /
  {content : binary}

/**
 * {1 Interface}
 */

HttpRequest = {{

  _of_web_info(winfo) = winfo.http_request

  /**
   * {2 Manipulation of requests}
   */

  Generic = {{

    /**
     * Extract low-level http request information
     */
    get_low_level_request(x: HttpRequest.request): WebInfo.private.native_request =
      x.request

    /**
     * Return the cookie associated to a request.
     */
    get_cookie(x: HttpRequest.request) : option(string) =
        raw = %% BslNet.Requestdef.get_request_cookie %%
        raw(x.request)

    /**
     * Return the user agent associated to a request.
     */
    get_user_agent(x:HttpRequest.request) =
      get_request_ua = %% BslNet.Requestdef.get_request_ua %% :  WebInfo.private.native_request -> string
      request = get_low_level_request(x)
      UserAgentParser.user_compat(get_request_ua(request))

    /**
     * Determine the method used to connect to this server
     */
    get_method(x: HttpRequest.request): HttpRequest.method =
      raw = %% BslNet.Http_server.get_method %% : WebInfo.private.native_request -> string
      request = get_low_level_request(x)
      match raw(request) with
        | "GET"     -> { get }
        | "HEAD"    -> { head }
        | "POST"    -> { post }
        | "PUT"     -> { put }
        | "DELETE"  -> { delete }
        | "TRACE"   -> { trace }
        | "CONNECT" -> { connect }
        | "OPTIONS" -> { options }
        | s         -> { other = s }

    /**
     * Extract the headers of a connexion
     * !!! WARNING !!! all '-' in header name are replaced by '_' in input and output of headers and header_get
     */
    get_headers(x: HttpRequest.request): {headers: list(string); header_get: string -> option(string)} =
      request    = get_low_level_request(x)
      raw_list   = %% BslNet.Http_server.get_header_names %%: WebInfo.private.native_request -> list(string)
      raw_values = %% BslNet.Http_server.get_header_values %%: WebInfo.private.native_request -> (string -> option(string))
      {headers    = raw_list(request)
       header_get = raw_values(request)}

    /**
     * Try to get the "Host" header of a connexion
     */
    get_host(x: HttpRequest.request): option(string) =
      get_headers(x).header_get("Host")

    /**
     * Try to get the "Accept-Language" header of a connexion
     */
    `get_Accept-Language`(x: HttpRequest.request): option(string) =
      get_headers(x).header_get("Accept_Language")

    /**
     * Indicates if the given request is secured
     */
    is_secured(x: HttpRequest.request): bool =
      request    = get_low_level_request(x)
      %% BslNet.Http_server.is_secured %%(request)

    /**
     * Returns the body of a request.
     *
     * Note: The binary data *must* be valid utf8 sequence else the resulted string
     * can be corrupted. [HttpRequest.get_bin_body] is more safe.
     */
    get_body(x: HttpRequest.request): string =
      Binary.to_string(get_bin_body(x))

    /**
     * Return the body of a request as binary.
     */
    get_bin_body(x):binary =
      %%BslNet.Requestdef.get_bin_body%%(get_low_level_request(x))

    /**
     * Returns the form-data of a POST request.
     *
     * Use this function as part of a protocol involving POST
     * or PUT requests to extract the content
     * of the request.
     */
    get_form_data(x: HttpRequest.request): stringmap(string) =
      match Parser.try_parse(UriParser.query_parser, get_body(x))
      | {none} -> StringMap.empty
      | {some=list} ->
        List.fold((a, b), acc ->
          StringMap.add(a, b, acc)
        , list, StringMap.empty)

    /**
     * Return the body of a XML request.
     *
     * Use this function as part of a protocol involving POST
     * or PUT requests (typically, as part of
     * a SOAP protocol) to inspect the content of the request.
     */
    get_xml_body(x: HttpRequest.request): option(xmlns) =
      Xmlns.try_parse(get_body(x))

    /**
     * Return the body of a JSON request.
     *
     * Use this function as part of a protocol involving POST
     * or PUT requests (typically, as part of
     * a REST protocol) to inspect the content of the request.
     */
    get_json_body(x: HttpRequest.request): option(RPC.Json.json) =
      Json.of_string(get_body(x))

    /**
     * Extract the uri of the request
     */
    get_uri(x: HttpRequest.request): string =
      raw = %% BslNet.Http_server.get_uri %%
      raw(x.request)

    /**
     * Extract the uri of the request
     */
    get_url(x: HttpRequest.request): option(Uri.relative) =
      match Uri.of_string(get_uri(x)) with
      | {some={path=_ fragment=_ query=_ is_from_root=_ is_directory=_}} as x -> x
      | {some={~path ~fragment ~query ...}} ->
        {some={~path ~fragment ~query is_from_root=true is_directory=false}}
      | _ -> {none}

    /**
     * Extract low-level ip information
     */
    get_ip(x : HttpRequest.request) : ip =
      WebInfo.get_conn_ip(x.connexion)

    /**
     * Determine if the user has been identified.
     *
     * Note that user identification can be trusted only if
     * your server is running in secure mode.
     * Otherwise, crafty attackers with sufficient resources
     * can forge identification credentials.
     *
     * @return {none} if the user is not identified
     * @return {some = cookie} if the user is identified with a given cookie
     */
    get_user(x: HttpRequest.request): option(user_id) =
      Option.map(
        x -> user_id_of_string(x),
        get_cookie(x)
      )

    /**
     * Like [get_user], but make an error instead of return [none]
     */
    get_user_unsafe(x) =
      get_user(x) ? (
        do Log.error("HttpRequest.Generix", "No user on http request")
        error("[get_user_unsafe] failed")
      )

    /**
     * Return a human-readable string describing the user behind a connexion.
     *
     * This string is meant for debugging and logging purposes, not for identification.
     * For identification, use rather [get_user] or more powerful functions from module
     * [UserContext].
     */
    get_connexion_string(x: HttpRequest.request):string  =
      match get_user(x)
        | {none} -> "<unidentified>"
        | {~some}-> string_of_user_id(some)

    /**
     * {1 Manipulation of multipart request}
     */
    @private
    crlf2 = Binary.of_string("\r\n\r\n")

    @private
    header_parser =
      parser Rule.ws name=((![:].)*) [:] value=((!"\r\n".)*) "\r\n"? ->
        {name = String.lowercase(Text.to_string(name))
         value=Text.to_string(value)}

    @private
    headers_parser =
      parser hlist=header_parser* -> hlist:HttpRequest.headers

    @private Multipart = {{

      form_data(request, boundary) : HttpRequest.multipart =
        body = get_bin_body(request)
        boundary = Binary.of_string("--{boundary}")
        rec aux(parts) =
          match parts with
          | [] | [_] -> {none}
          | [t|q] ->
            i = Binary.indexOf(t, crlf2, 0)
            headers = Binary.get_string(t, 0, i)
            body = Binary.get_binary(t, i+4, Binary.length(t)-i-6)
            match Parser.try_parse(headers_parser, headers)
            | {none} ->
              do Log.error("Multipart", "cannot read headers : '{headers}'")
              aux(q)
            | {some=headers} ->
              match List.find((x -> x.name == "content-disposition"), headers)
              | {none} ->
                do Log.error("Multipart", "content-disposition is not found")
                aux(q)
              | {some=disp} ->
                match Parser.try_parse(
                  parser
                  | [ ]* "form-data;" [ ]* x={
                    file(filename, name) =
                      {part=~{name filename content=-> {content=body}}
                       ~headers}
                    quoted = parser [\"] s=((![\"].)*) [\"] -> Text.to_string(s)
                    parser
                    | "name="name=quoted[;] [ ]* "filename="filename=quoted[;]? ->
                      file(filename, name)
                    | "filename="filename=quoted[;] [ ]* "name="name=quoted[;]? ->
                      file(filename, name)
                    | "name="name=(.*) ->
                      {part={name=Text.to_string(name) value=Binary.to_string(body)}
                      ~headers}
                  } -> x, disp.value    )
                | {none} ->
                  do Log.error("Multipart", "cannot parse content-disposition")
                  aux(q)
                | {some=current} -> {some = (current, {next = -> aux(q)})}
                end
              end
            end
        {next = -> aux(List.tail(Binary.explode(body, boundary)))}

    }}

    get_multipart(request) : option(HttpRequest.multipart) =
      match get_headers(request).header_get("content-type") with
      | {none} -> {none}
      | {some = ctype} ->
        Parser.try_parse(
          parser
            | "multipart/form-data;" [ ]* "boundary="boundary=(.*) ->
              Multipart.form_data(request, boundary)
            | "multipart/"kind=((![;].)*)
              {parser !. ->
                Log.error("Multipart", "multipart/{kind} is not handled by the Opa server.")}
              Rule.fail -> @fail
          , ctype
        )

    /**
     * [fold_multipart(multipart, acc, folder)]. Fold a [multipart]
     * request. A [multipart] request is composed with several part. For
     * each part [fold_multipart] call [folder(part, fold_headers,
     * acc)], [fold_headers] is be able to fold on corresponding [part]
     * headers (first argument is header name, second is header value).
     */
    fold_multipart(mpart:HttpRequest.multipart, acc:'acc, folder:(HttpRequest.part, ('a, (string, string, 'a -> 'a) -> 'a), 'acc -> 'acc)) : 'acc =
      Iter.fold(
      (~{part headers}, acc ->
        folder(part, (a, f -> List.fold(~{name value}, a -> f(name, value, a), headers, a)), acc)
      ), mpart, acc)

  }}

  /**
   * {2 Thread context functions}
   */

  @private apply(f : 'a -> 'b) : option('b) = Option.map(f, thread_context().request)
  @private apply2(f : 'a -> option('b)) : option('b) = Option.bind(f, thread_context().request)

  get_request() = apply(identity)

  get_cookie() = apply2(Generic.get_cookie)

  get_user_agent() = apply(Generic.get_user_agent)

  get_method() = apply(Generic.get_method)

  get_headers() = apply(Generic.get_headers)

  get_host() = apply(Generic.get_host)

  is_secured() = apply(Generic.is_secured)

  get_body() = apply(Generic.get_body)

  get_bin_body() = apply(Generic.get_bin_body)

  get_form_data() = apply(Generic.get_form_data)

  get_xml_body() = apply2(Generic.get_xml_body)

  get_json_body() = apply2(Generic.get_json_body)

  get_uri() = apply(Generic.get_uri)

  get_url() = apply2(Generic.get_url)

  get_ip() = apply(Generic.get_ip)

  get_user() = apply2(Generic.get_user)

  get_user_unsafe() =
    apply(Generic.get_user_unsafe) ? (
      do Log.error("HttpRequest", "No http request")
      error("[get_user_unsafe] failed")
    )

  get_connexion_string() = apply(Generic.get_connexion_string)

  get_multipart() = apply2(Generic.get_multipart)

}}
