/*
    Copyright © 2011-2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import-plugin server
import stdlib.core.{json, xhtml, rpc.core, map}


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
 */
type HttpRequest.part =
  {name : string; value : string} /
  {name : string; filename : string; content : iter(binary)}

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
    @package
    get_low_level_request(x: HttpRequest.request): WebInfo.private.native_request =
      x.request

    /**
     * Return the cookie associated to a request.
     */
    get_cookie(x: HttpRequest.request, name) : option(string) =
        raw = %% BslNet.Requestdef.get_request_cookie %%
        raw(x.request, name)

    /**
     * Return the internal cookie associated to a request.
     */
    get_internal_cookie(x: HttpRequest.request) : option(string) =
      get_cookie(x, "ic")

    /**
     * Try to get the "Accept-Language" header of a connexion
     */
    `get_Accept-Language`(x: HttpRequest.request): option(string) =
      get_headers(x).header_get("accept-language")



    get_user_agent(x:HttpRequest.request) =
      get_request_ua = %% BslNet.Requestdef.get_request_ua %% :  WebInfo.private.native_request -> string
      request = get_low_level_request(x)
      UserAgentParser.user_compat(get_request_ua(request))

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

    get_headers(x: HttpRequest.request): {headers: list(string); header_get: string -> option(string)} =
      request    = get_low_level_request(x)
      raw_list   = %% BslNet.Http_server.get_header_names %%: WebInfo.private.native_request -> list(string)
      raw_values = %% BslNet.Http_server.get_header_values %%: WebInfo.private.native_request -> (string -> option(string))
      {headers    = raw_list(request)
       header_get = s -> raw_values(request)(String.lowercase(s)) }

    get_host(x: HttpRequest.request): option(string) =
      get_headers(x).header_get("host")

    is_secured(x: HttpRequest.request): bool =
      request    = get_low_level_request(x)
      %% BslNet.Http_server.is_secured %%(request)

    get_body(x: HttpRequest.request): string =
      Binary.to_string(get_bin_body(x))

    get_iter_body(x: HttpRequest.request): iter(binary) =
      %%BslNet.Requestdef.get_bin_body%%(get_low_level_request(x))

    get_bin_body(x: HttpRequest.request): binary =
      Binary.of_iter(get_iter_body(x))

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

    get_xml_body(x: HttpRequest.request): option(xmlns) =
      Xmlns.try_parse(get_body(x))

    get_json_body(x: HttpRequest.request): option(RPC.Json.json) =
      Json.of_string(get_body(x))

    get_uri(x: HttpRequest.request): string =
      raw = %% BslNet.Http_server.get_uri %%
      raw(x.request)

    get_url(x: HttpRequest.request): option(Uri.relative) =
      match Uri.of_string(get_uri(x)) with
      | {some={path=_ fragment=_ query=_ is_from_root=_ is_directory=_}} as x -> x
      | {some={~path ~fragment ~query ...}} ->
        {some={~path ~fragment ~query is_from_root=true is_directory=false}}
      | _ -> {none}

    get_ip(x : HttpRequest.request) : ip =
      WebInfo.get_conn_ip(x.connexion)

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
        body = get_iter_body(request)
        boundary = Binary.of_string("--{boundary}")
        blen = Binary.length(boundary)
        rec body_as_value(data, iter:iter(binary), i) =
          match Binary.indexOf(data, boundary, i) with
          | -1 ->
            match iter.next() with
            | {none} -> @fail
            | {some=(data2, iter)} ->
              dlen = Binary.length(data)
              do Binary.add_binary(data, data2)
              body_as_value(data, iter, dlen - blen)
            end
          | s -> (Binary.get_string(data, i, s - i - 2), data, iter, s)
          end
        /* Skip the first boundary */
        rec start(data, iter, i, skipped)
        : option(({part:HttpRequest.part headers:HttpRequest.headers}, HttpRequest.multipart)) =
          dlen = Binary.length(data)
          if (dlen + skipped > i + blen) then
            part(data, iter, blen + i - skipped)
          else match iter.next() with
            | {none} -> {none}
            | {some = (data, iter)} ->
              start(data, iter, 0, -(dlen + skipped))
        /* Analyze a part, starts with data[i] */
        and part(data, iter, i)
        : option(({part:HttpRequest.part headers:HttpRequest.headers}, HttpRequest.multipart)) =
          /* Search for headers */
          match Binary.indexOf(data, crlf2, i) with
          | -1 ->
            /* Headers separator not found */
            match iter.next() with
            | {none} ->
              do if not(Binary.get_string(data, i, 2) == "--") then
                Log.error("Multipart", "Unexpected disposition of datas ")
              {none}
            | {some = (data2, iter)} ->
              do Binary.add_binary(data, data2)
              part(data, iter, i)
            end
          | s ->
            /* Headers separator found at data[s] */
            headers = Binary.get_string(data, i+2/*crlf*/, s-i)
            match Parser.try_parse(headers_parser, headers) with
            | {none} ->
              do Log.error("Multipart", "cannot parse part headers : '{headers}'")
              {none}
            | {some=headers} ->
              match List.find((x -> x.name == "content-disposition"), headers)
              | {none} ->
                do Log.error("Multipart", "content-disposition is not found {headers}")
                {none}
              | {some=disp} ->
                match Parser.try_parse(
                  parser
                  | [ ]* "form-data;" [ ]* x={
                    file(filename, name) =
                      part_file(headers, filename, name, data, iter, s+4/*crlf*2*/)
                    quoted = parser [\"] s=((![\"].)*) [\"] -> Text.to_string(s)
                    parser
                    | "name="name=quoted[;] [ ]* "filename="filename=quoted[;]? ->
                      file(filename, name)
                    | "filename="filename=quoted[;] [ ]* "name="name=quoted[;]? ->
                      file(filename, name)
                    | "name="name=quoted ->
                      (value, data, iter, s) = body_as_value(data, iter, s+4/*crlf*2*/)
                      {some = ({~headers part={~name ~value}},
                               {next = -> start(data, iter, s, 0)})}
                  } -> x, disp.value)
                | {none} ->
                  do Log.error("Multipart", "cannot parse content-disposition")
                  {none}
                | {some=current} -> current
              end
            end
          end

        /* Analyze a part file, starts with data[i] */
        and part_file(headers, filename, name, data, iter:iter(binary), i)
        : option(({part:HttpRequest.part headers:HttpRequest.headers}, HttpRequest.multipart)) =
          barrier = Barrier.make()
          /* This function compute the content iterator, then release the
             barrier to compute the next step of multipart iterator. */
          rec aux(data:binary, iter:iter(binary), i) : option((binary, iter(binary)))=
            match Binary.indexOf(data, boundary, i) with
            | -1 ->
              /* Boundary not found */
              match iter.next() with
              | {none} ->
                do Log.error("Multipart", "end of file is not found")
                @fail
              | {some=(data2, iter)} ->
                dlen = Binary.length(data)
                chunk = Binary.get_binary(data, i, dlen - i - blen)
                do Binary.add_binary(data, data2)
                {some = (chunk, {next = -> aux(data, iter, dlen - blen)}:iter(binary))}
              end
            | s ->
              /* Boundary found, end of current file iterator then next part */
              do Barrier.release(barrier, (data, iter, s+blen))
              {some = (Binary.get_binary(data, i, s-i-2/*crlf*/), {next = -> {none}}:iter(binary))}
            end
          {some = (~{headers
             part=~{name filename content={next = -> aux(data, iter, i)}}},
            {next = ->
              (data, iter, i) = Barrier.wait(barrier)
              start(data, iter, i, blen)
            })}

        {next = -> start(Binary.create(0), body, 0, 0)}

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

  get_internal_cookie() = apply2(Generic.get_internal_cookie)

  get_cookie(name) = apply2(Generic.get_cookie(_, name))

  /**
   * Retuns the user agent of the current client.
   */
  get_user_agent() = apply(Generic.get_user_agent)

  /**
   * Retuns the method of the incomming request.
   */
  get_method() = apply(Generic.get_method)

  /**
   * Retuns the headers of the incomming request.
   */
  get_headers() = apply(Generic.get_headers)

  /**
   * Retuns the "Host" header of the incomming request.
   */
  get_host() = apply(Generic.get_host)

  /**
   * Checks if the incomming request is encrypted (https)
   */
  is_secured() = apply(Generic.is_secured)

  /**
   * Returns the body of the incomming request.
   *
   * Note: The body *must* be valid utf8 sequence else the resulted string
   * can be corrupted. [HttpRequest.get_bin_body] is more safe.
   */
  get_body() = apply(Generic.get_body)

  /**
   * Returns the body of the incomming request as binary.
   */
  get_bin_body() = apply(Generic.get_bin_body)

  /**
   * Returns the body of the incomming request as a binary iterator.
   */
  get_iter_body() = apply(Generic.get_iter_body)

  /**
   * Returns the form-data of the incomming POST request.
   *
   * Use this function as part of a protocol involving POST
   * or PUT requests to extract the content
   * of the request.
   */
  get_form_data() = apply(Generic.get_form_data)

  /**
   * Returns the body of the incomming XML request.
   *
   * Use this function as part of a protocol involving POST
   * or PUT requests (typically, as part of
   * a SOAP protocol) to inspect the content of the request.
   */
  get_xml_body() = apply2(Generic.get_xml_body)

  /**
   * Returns the body of a incomming JSON request.
   *
   * Use this function as part of a protocol involving POST
   * or PUT requests (typically, as part of
   * a REST protocol) to inspect the content of the request.
   */
  get_json_body() = apply2(Generic.get_json_body)

  /**
   * Returns the uri of the incomming request as a string.
   */
  get_uri() = apply(Generic.get_uri)

  /**
   * Returns the uri of the incomming request.
   */
  get_url() = apply2(Generic.get_url)

  /**
   * Returns the ip information of the incomming request
   */
  get_ip() = apply(Generic.get_ip)

  /**
   * Returns the multipart object of a multipart incomming request.
   */
  get_multipart() = apply2(Generic.get_multipart)

}}
