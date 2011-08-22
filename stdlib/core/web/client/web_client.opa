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
    @author David Rajchenbach-Teller
**/

import stdlib.core.{date, web.core, rpc.core, xhtml, security.ssl, parser}

/**
 * A client for web operations GET, POST, HEAD, etc.
 *
 * {1 What is this module for?}
 *
 * This module defines all the operations you need to perform low-level web requests. These requests
 * are generally useful for the following purposes:
 * - downloading or exploring some content from another web server (using operation GET), e.g. to access publicly-visible
 *    files or implement a search engine
 * - using services defined with the REST protocol (using all operations), including WebDav services
 * - simulating the actions of a web browser, e.g. to automatize usage of a web service which doesn't have a clean API
 *    (using all operations)
 *
 * {1 Where should I start?}
 *
 * If you need to {e read} contents, use operation GET, i.e. function [WebClient.Get.try_get] and its companions.
 * If you need to fill forms, use operation POST, i.e. function [WebClient.Post.try_post].
 * This package also defines operations HEAD, PUT and DELETE, in their respective modules.
 *
 * A function [WebClient.get_class_of_result] may help you understand the results returned by distant servers. Note,
 * however, that the actual results depend on how the distant server is implemented, so you may need to
 * read their documentation for more details.
 *
 * {1 What if I need more?}
 *
 * Each module defines several functions with different strategies for concurrency. When time comes for you to
 * optimize your application, explore [try_get_async], [try_get_bg] and the respective functions of other modules.
 *
 * An additional module [WebClient.Generic] defines useful operations which you may wish to use if you need to
 * communicate with 'exotic' services, i.e. services that don't match the definition of GET, POST, HEAD, PUT
 * and DELETE, perhaps because they require custom extensions.
 *
 * @author David Rajchenbach-Teller, 2010
 */

/**
 * {1 Types defined in this module}
 */

/**
 * A failure of the web client
 */
type WebClient.failure =
  {network} /**A network failure, e.g. the distant computer doesn't exist*/
/ {uri:string; reason:{relative_uri} / {incorrect_protocol} / {cannot_parse_redirect}}     /**A uri-related failure, e.g. the URI is relative*/
/ {result_does_not_parse: WebClient.success(string)}
/ {no_redirect}
/ {timeout} /**A timeout failure, e.g. the distant computer could not be reached after repeated attempts*/
/ {ssl}
/ {other:string} /**Any other failure*/

/**
 * A success of the web client
 */
type WebClient.success('content) = {
  mime_type:string/**The content type of the returned value*/
  code: int       /**The result status. Note that this status can signify an error, if the GET/POST request was successful but the server
                     rejected the request politely*/
  content: 'content /**The content of the reply.*/
  headers: list(string) /**All the headers of the reply. Use [header_get] to access their content.*/
  header_get: string -> option(string) /**A mapping from header name to header.*/
}

/**
 * A result of the web client
 */
type WebClient.result('content) =
   {failure: WebClient.failure}
 / {success: WebClient.success('content)}

/**
 * Options for GET operations
 */
type WebClient.Get.options =
 {
   auth:             option(string)
   custom_headers:   option(string)
   custom_agent:     option(string)
   follow_redirects: int /*The maximal number of redirects to follow. By default, 0. Usually a bad idea to set it higher than 5.*/
   timeout_sec:      option(float)
   ssl_key:          option(SSL.private_key)
   ssl_policy:       option(SSL.policy)
 }

/**
 * Options for HEAD operations
 */
type WebClient.Head.options =
 {
   auth: option(string)
   custom_headers: option(string)

   custom_agent:   option(string)
   follow_redirects: int /*The maximal number of redirects to follow. By default, 0. Usually a bad idea to set it higher than 5.*/
   timeout_sec:      option(float)
   ssl_key:          option(SSL.private_key)
   ssl_policy:       option(SSL.policy)
 }

/**
 * Options for DELETE operations
 */
type WebClient.Delete.options =
 {
   auth: option(string)
   custom_headers: option(string)
   custom_agent:   option(string)
   timeout_sec:      option(float)
   ssl_key:          option(SSL.private_key)
   ssl_policy:       option(SSL.policy)
 }

/**
 * Options for POST operations
 *
 * @param The type of content carried by the request itself. Note that function [try_post] and its variants all require
 * ['content] to be [string], so use a conversion function such as [of_xml] or [of_json] to convert your options to this
 * format as appropriate
 */
type WebClient.Post.options('content) =
 {
   mimetype:         string
   content:          option('content)
   auth:             option(string)
   custom_headers:   option(string)
   custom_agent:     option(string)
   redirect_to_get:  option(WebClient.Get.options) /**If [{true}], follow redirections using GET protocol. Otherwise, don't follow redirections.*/
   timeout_sec:      option(float)
   ssl_key:          option(SSL.private_key)
   ssl_policy:       option(SSL.policy)
 }

/**
 * Options for PUT operations
 */
type WebClient.Put.options =
 {
   mimetype:         string
   auth:             option(string)
   custom_headers:   option(string)
   custom_agent:     option(string)
   redirect_to_get:  option(WebClient.Get.options) /**If [{true}], follow redirections using GET protocol. Otherwise, don't follow redirections.*/
   timeout_sec:      option(float)
   ssl_key:          option(SSL.private_key)
   ssl_policy:       option(SSL.policy)
 }

type WebClient.Generic.options =
 {
   operation:        string
   auth:             option(string)
   custom_headers:   option(string)
   custom_agent:     option(string)
   redirect:         option(WebClient.Generic.options)
   timeout_sec:      option(float)
   ssl_key:          option(SSL.private_key)
   ssl_policy:       option(SSL.policy)
 }


/**
 * {1 Interface}
 */

WebClient =
{{

   /**
    * {2 WebClient.Get}
    */

   Get =
   {{

      /**
       * Default options for GET operations
       *
       * By default, no authentication is performed, no custom headers, one redirect is followed,
       * connexions timeout after 36 seconds.
       */
      default_options: WebClient.Get.options =
      {
        auth             = {none}
        timeout_sec      = {some = 36.}
        follow_redirects = 0
        custom_agent     = {none}
        custom_headers   = {none}
        ssl_key          = {none}
        ssl_policy       = {none}
      }

      /**
       * Place a GET request using default options, blocking-style.
       *
       * This function will block your current thread (but not other computations) until the result is computed.
       *
       * Usage suggestion: use [try_get] and [try_get_with_options] when the time spent by the current thread
       * is not critical, e.g. because other threads/users will use the CPU while this thread is waiting, or because
       * your are still in a prototyping phase.
       */
      try_get(location:Uri.uri): WebClient.result(string) = try_get_with_options(location, default_options)
      try_get_with_options(location:Uri.uri, options:WebClient.Get.options): WebClient.result(string) =
           @callcc(k ->
                on_result(x)  = Continuation.return(k, x)
                try_get_with_options_async(location, options, on_result)
           )

      /**
       * Place a GET request using custom options, non-blocking.
       *
       * This function will proceed in the background, without blocking. However, consulting the result for
       * the first time will block your current thread (but not other computations) until the result is computed.
       *
       * [ result = try_get_with_options_bg(my_uri, my_options)
       *   //do something else, while request is being treated
       *   match result() with //if the request hasn't been treated yet, the thread will now block
       *    | ~{success} -> ...
       *    | ~{failure} -> ...
       *   match result() with  //the request has already been treated, so the thread will not block
       *    ...
       * ]
       *
       * Usage suggestion: use [try_get_bg] and [try_get_with_options_bg] to optimize the total execution
       * time of this specific thread.
       */
       try_get_bg(location:Uri.uri): -> WebClient.result(string) = try_get_with_options_bg(location, default_options)
       try_get_with_options_bg(location:Uri.uri, options:WebClient.Get.options): -> WebClient.result(string) =
          result = @spawn({result = try_get_with_options(location, options)})
          -> @wait(result).result

       /**
        * Place a GET request using custom options, call a function when result is available.
        *
        * This function is non-blocking.
        *
        * @param on_result A function called once the result is available
        *
        * Usage suggestion: use [try_get_async] and [try_get_with_options_async] when your code is very concurrent
        * and you intend to send messages on completion of requests.
        */
       try_get_async(location:Uri.uri, options:WebClient.Get.options, on_result: WebClient.result(string) -> void): void =
           try_get_with_options_async(location, options, on_result)
       try_get_with_options_async(location:Uri.uri, options:WebClient.Get.options, on_result: WebClient.result(string) -> void): void =
           on_success(x) = on_result({success = x})
           on_failure(x) = on_result({failure = x})
           Generic.try_request_with_options_async(location, "GET", generic_options_of_get_options(options), {none}, on_success, on_failure)

       generic_options_of_get_options(options: WebClient.Get.options): WebClient.Generic.options =
       (
          Loop = {{
             loop(follow_redirects:int): WebClient.Generic.options =
              {
                operation       = "GET"
                auth            = options.auth
                custom_headers  = options.custom_headers
                custom_agent    = options.custom_agent
                redirect        = if follow_redirects <= 0 then {none} else {some = loop(follow_redirects - 1)}
                timeout_sec     = options.timeout_sec
                ssl_key         = options.ssl_key
                ssl_policy      = options.ssl_policy
              }
          }}
          Loop.loop(options.follow_redirects)
       )
   }}

   /**
    * {2 WebClient.Head}
    */

   Head =
   {{

      /**
       * Default options for HEAD operations
       *
       * By default, no authentication is performed, no custom headers, one redirect is followed,
       * connexions timeout after 36 seconds.
       */
      default_options: WebClient.Head.options =
      {
        auth             = {none}
        timeout_sec      = {some = 36.}
        follow_redirects = 0
        custom_agent     = {none}
        custom_headers   = {none}
        ssl_key          = {none}
        ssl_policy       = {none}
      }

      /**
       * Place a HEAD request using default options, blocking-style.
       *
       * This function will block your current thread (but not other computations) until the result is computed.
       *
       * Usage suggestion: use [try_head] and [try_head_with_options] when the time spent by the current thread
       * is not critical, e.g. because other threads/users will use the CPU while this thread is waiting, or because
       * your are still in a prototyping phase.
       */
      try_head(location:Uri.uri): WebClient.result(string) = try_head_with_options(location, default_options)
      try_head_with_options(location:Uri.uri, options:WebClient.Head.options): WebClient.result(string) =
           @callcc(k ->
                on_result(x)  = Continuation.return(k, x)
                try_head_with_options_async(location, options, on_result)
           )

      /**
       * Place a HEAD request using custom options, non-blocking.
       *
       * This function will proceed in the background, without blocking. However, consulting the result for
       * the first time will block your current thread (but not other computations) until the result is computed.
       *
       * [ result = try_head_with_options_bg(my_uri, my_options)
       *   //do something else, while request is being treated
       *   match result() with //if the request hasn't been treated yet, the thread will now block
       *    | ~{success} -> ...
       *    | ~{failure} -> ...
       *   match result() with  //the request has already been treated, so the thread will not block
       *    ...
       * ]
       *
       * Usage suggestion: use [try_head_bg] and [try_head_with_options_bg] to optimize the total execution
       * time of this specific thread.
       */
       try_head_bg(location:Uri.uri): -> WebClient.result(string) = try_head_with_options_bg(location, default_options)
       try_head_with_options_bg(location:Uri.uri, options:WebClient.Head.options): -> WebClient.result(string) =
          result = @spawn({result = try_head_with_options(location, options)})
          -> @wait(result).result

       /**
        * Place a HEAD request using custom options, call a function when result is available.
        *
        * This function is non-blocking.
        *
        * @param on_result A function called once the result is available
        *
        * Usage suggestion: use [try_head_async] and [try_head_with_options_async] when your code is very concurrent
        * and you intend to send messages on completion of requests.
        */
       try_head_async(location:Uri.uri, options:WebClient.Head.options, on_result: WebClient.result(string) -> void): void =
           try_head_with_options_async(location, options, on_result)
       try_head_with_options_async(location:Uri.uri, options:WebClient.Head.options, on_result: WebClient.result(string) -> void): void =
           on_success(x) = on_result({success = x})
           on_failure(x) = on_result({failure = x})
           Generic.try_request_with_options_async(location, "HEAD", generic_options_of_head_options(options), {none}, on_success, on_failure)

       generic_options_of_head_options(options: WebClient.Head.options): WebClient.Generic.options =
       (
          Loop = {{
             loop(follow_redirects:int): WebClient.Generic.options =
              {
                operation       = "HEAD"
                auth            = options.auth
                custom_headers  = options.custom_headers
                custom_agent    = options.custom_agent
                redirect        = if follow_redirects <= 0 then {none} else {some = loop(follow_redirects - 1)}
                timeout_sec     = options.timeout_sec
                ssl_key         = options.ssl_key
                ssl_policy      = options.ssl_policy
              }
          }}
          Loop.loop(options.follow_redirects)
       )
   }}

   /**
    * {2 WebClient.Delete}
    */

   Delete =
   {{

      /**
       * Default options for DELETE operations
       *
       * By default, no authentication is performed, no custom headers,
       * connexions timeout after 36 seconds.
       */
      default_options: WebClient.Delete.options =
      {
        auth             = {none}
        timeout_sec      = {some = 36.}
        custom_agent     = {none}
        custom_headers   = {none}
        ssl_key          = {none}
        ssl_policy       = {none}
      }

      /**
       * Place a DELETE request using default options, blocking-style.
       *
       * This function will block your current thread (but not other computations) until the result is computed.
       *
       * Usage suggestion: use [try_delete] and [try_delete_with_options] when the time spent by the current thread
       * is not critical, e.g. because other threads/users will use the CPU while this thread is waiting, or because
       * your are still in a prototyping phase.
       */
      try_delete(location:Uri.uri): WebClient.result(string) = try_delete_with_options(location, default_options)
      try_delete_with_options(location:Uri.uri, options:WebClient.Delete.options): WebClient.result(string) =
           @callcc(k ->
                on_result(x)  = Continuation.return(k, x)
                try_delete_with_options_async(location, options, on_result)
           )

      /**
       * Place a DELETE request using custom options, non-blocking.
       *
       * This function will proceed in the background, without blocking. However, consulting the result for
       * the first time will block your current thread (but not other computations) until the result is computed.
       *
       * [ result = try_delete_with_options_bg(my_uri, my_options)
       *   //do something else, while request is being treated
       *   match result() with //if the request hasn't been treated yet, the thread will now block
       *    | ~{success} -> ...
       *    | ~{failure} -> ...
       *   match result() with  //the request has already been treated, so the thread will not block
       *    ...
       * ]
       *
       * Usage suggestion: use [try_delete_bg] and [try_delete_with_options_bg] to optimize the total execution
       * time of this specific thread.
       */
       try_delete_bg(location:Uri.uri): -> WebClient.result(string) = try_delete_with_options_bg(location, default_options)
       try_delete_with_options_bg(location:Uri.uri, options:WebClient.Delete.options): -> WebClient.result(string) =
          result = @spawn({result = try_delete_with_options(location, options)})
          -> @wait(result).result

       /**
        * Place a DELETE request using custom options, call a function when result is available.
        *
        * This function is non-blocking.
        *
        * @param on_result A function called once the result is available
        *
        * Usage suggestion: use [try_delete_async] and [try_delete_with_options_async] when your code is very concurrent
        * and you intend to send messages on completion of requests.
        */
       try_delete_async(location:Uri.uri, options:WebClient.Delete.options, on_result: WebClient.result(string) -> void): void =
           try_delete_with_options_async(location, options, on_result)
       try_delete_with_options_async(location:Uri.uri, options:WebClient.Delete.options, on_result: WebClient.result(string) -> void): void =
           on_success(x) = on_result({success = x})
           on_failure(x) = on_result({failure = x})
           Generic.try_request_with_options_async(location, "DELETE", generic_options_of_delete_options(options), {none}, on_success, on_failure)

       generic_options_of_delete_options(options: WebClient.Delete.options): WebClient.Generic.options =
         {
           operation       = "DELETE"
           auth            = options.auth
           custom_headers  = options.custom_headers
           custom_agent    = options.custom_agent
           redirect        = {none}
           timeout_sec     = options.timeout_sec
           ssl_key         = options.ssl_key
           ssl_policy      = options.ssl_policy
         }
   }}

   /**
    * {2 WebClient.Post}
    */

   Post = {{

      /**
       * Default options for POST operations
       *
       * By default, no authentication is performed, no custom headers,
       * connexions timeout after 36 seconds, mime-type is "application/x-www-form-urlencoded"
       */
      default_options: WebClient.Post.options('content) =
      {
        mimetype         = "application/x-www-form-urlencoded"
        content          = {none}
        auth             = {none}
        timeout_sec      = {some = 36.}
        redirect_to_get  = {none}
        custom_agent     = {none}
        custom_headers   = {none}
        ssl_key          = {none}
        ssl_policy       = {none}
      }

      /**
       * Place a POST request using default options, blocking-style.
       *
       * This function will block your current thread (but not other computations) until the result is computed.
       *
       * Usage suggestion: use [try_post] and [try_post_with_options] when the time spent by the current thread
       * is not critical, e.g. because other threads/users will use the CPU while this thread is waiting, or because
       * your are still in a prototyping phase.
       */
      try_post(location:Uri.uri, content:string): WebClient.result(string) = try_post_with_options(location, {default_options with content = {some = content}})
      try_post_with_options(location:Uri.uri, options:WebClient.Post.options(string)): WebClient.result(string) =
          @callcc(k ->
                on_result(x) = Continuation.return(k, x)
                try_post_with_options_async(location, options, on_result)
           )

      /**
       * Place a POST request using custom options, non-blocking.
       *
       * This function will proceed in the background, without blocking. However, consulting the result for
       * the first time will block your current thread (but not other computations) until the result is computed.
       *
       * [ result = try_post_with_options_bg(my_uri, my_options)
       *   //do something else, while request is being treated
       *   match result() with //if the request hasn't been treated yet, the thread will now block
       *    | ~{success} -> ...
       *    | ~{failure} -> ...
       *   match result() with  //the request has already been treated, so the thread will not block
       *    ...
       * ]
       *
       * Usage suggestion: use [try_post_bg] and [try_post_with_options_bg] to optimize the total execution
       * time of this specific thread.
       */
      try_post_bg(location:Uri.uri, content:string): -> WebClient.result(string) =
          try_post_with_options_bg(location, {default_options with content = {some = content}})
      try_post_with_options_bg(location:Uri.uri, options:WebClient.Post.options(string)): -> WebClient.result(string) =
          result = @spawn({result = try_post_with_options(location, options)})
          -> @wait(result).result

       /**
        * Place a POST request using custom options, call a function when result is available.
        *
        * This function is non-blocking.
        *
        * @param on_result A function called once the result is available
        *
        * Usage suggestion: use [try_post_async] and [try_post_with_options_async] when your code is very concurrent
        * and you intend to send messages on completion of requests.
        */
      try_post_async(location:Uri.uri, content:string, on_result: WebClient.result(string) -> void): void =
          try_post_with_options_async(location, {default_options with content = {some = content}}, on_result)
      try_post_with_options_async(location:Uri.uri, options:WebClient.Post.options(string), on_result: WebClient.result(string) -> void): void =
      (
          length  = match options.content with
             | {none} -> 0
             | ~{some} -> String.length(some)
          post_headers = "Content-Length: {length}\r\nContent-Type: {options.mimetype}\r\n"
          headers = match default_options.custom_headers with
            | {none}  -> post_headers
            | ~{some} -> "{post_headers}{some}"
          generic_options = {
            operation        = "POST"
            auth             = options.auth
            redirect         = Option.map(Get.generic_options_of_get_options(_), options.redirect_to_get)
            timeout_sec      = options.timeout_sec
            custom_agent     = options.custom_agent
            custom_headers   = {some = headers}
            ssl_key          = options.ssl_key
            ssl_policy       = options.ssl_policy

          }
          on_success(x) = on_result({success = x})
          on_failure(x) = on_result({failure = x})
          Generic.try_request_with_options_async(location, "POST", generic_options, options.content, on_success, on_failure)
       )

       /**
        * Normalize a form-like request into a string-based one.
        *
        * @param options A request defined as a list of associations key -> value.
        */
       of_form(options: WebClient.Post.options(list((string, string)))): WebClient.Post.options(string) =
          {options with content =
                   match options.content with
                      {none}  -> {none}
                    | ~{some} ->
                        x = List.map(((x, y) -> "{Uri.encode_string(x)}={Uri.encode_string(y)}"), some)
                       {some = List.to_string_using("", "", "&", x)}
          }

       /**
        * Normalize a XML-based request into a string-based one.
        *
        * @param options A request defined in XML. The content of the request depends on the service you wish to contact.
        */
       of_xml(options: WebClient.Post.options(xmlns)): WebClient.Post.options(string) =
          {options with content =
                   match options.content with
                      {none}  -> {none}
                    | ~{some} -> {some = Xmlns.to_string(some)}
          }

       /**
        * Normalize a JSON-based request into a string-based one.
        *
        * @param options A request defined in JSON. The content of the request depends on the service you wish to contact.
        */
       of_json(options: WebClient.Post.options(RPC.Json.json)): WebClient.Post.options(string) =
          {options with content =
                   match options.content with
                      {none}  -> {none}
                    | ~{some} -> {some = Json.serialize(some)}
                    mimetype = "application/json"
          }

       /**
        * Normalize a SOAP-based request into a string-based one.
        *
        * @param options A request defined in XML. As per SOAP definition, this request {e should} look like
        * [
               <env:Header> some_header </env:Header>
               <env:Body>   some_body </env:Body>
        * ]
        * where [some_header] and [some_body] are replaced by the information expected by the specific service you wish
        * to contact.
        */
        of_soap(options: WebClient.Post.options(xmlns)): WebClient.Post.options(string) =
          {options with content =
                   match options.content with
                       {none} -> {none}
                     |~{some} -> {some = "<env:Envelope xmlns:env=\"http://www.w3.org/2003/05/soap-envelope\" env:encodingStyle=\"http://www.w3.org/2001/12/soap-encoding\"> {Xmlns.to_string(some)} </env:Envelope>"}
                   mimetype = "application/soap+xml"

          }
   }}

   /**
    * {2 WebClient.Put}
    */

   Put = {{

      /**
       * Default options for PUT operations
       *
       * By default, no authentication is performed, no custom headers,
       * connexions timeout after 36 seconds, mime-type is "application/x-www-form-urlencoded"
       */
      default_options: WebClient.Put.options =
      {
        mimetype         = "application/x-www-form-urlencoded"
        auth             = {none}
        timeout_sec      = {some = 36.}
        redirect_to_get  = {none}
        custom_agent     = {none}
        custom_headers   = {none}
        ssl_key          = {none}
        ssl_policy       = {none}
      }

      /**
       * Place a PUT request using default options, blocking-style.
       *
       * This function will block your current thread (but not other computations) until the result is computed.
       *
       * Usage suggestion: use [try_put] and [try_put_with_options] when the time spent by the current thread
       * is not critical, e.g. because other threads/users will use the CPU while this thread is waiting, or because
       * your are still in a prototyping phase.
       */
      try_put(location:Uri.uri, content:string): WebClient.result(string) = try_put_with_options(location, content, default_options)
      try_put_with_options(location:Uri.uri, content:string, options:WebClient.Put.options): WebClient.result(string) =
          @callcc(k ->
                on_result(x) = Continuation.return(k, x)
                try_put_with_options_async(location, content, options, on_result)
           )

      /**
       * Place a PUT request using custom options, non-blocking.
       *
       * This function will proceed in the background, without blocking. However, consulting the result for
       * the first time will block your current thread (but not other computations) until the result is computed.
       *
       * [ result = try_put_with_options_bg(my_uri, my_options)
       *   //do something else, while request is being treated
       *   match result() with //if the request hasn't been treated yet, the thread will now block
       *    | ~{success} -> ...
       *    | ~{failure} -> ...
       *   match result() with  //the request has already been treated, so the thread will not block
       *    ...
       * ]
       *
       * Usage suggestion: use [try_put_bg] and [try_put_with_options_bg] to optimize the total execution
       * time of this specific thread.
       */
      try_put_bg(location:Uri.uri, content:string): -> WebClient.result(string) =
          try_put_with_options_bg(location, content, default_options)
      try_put_with_options_bg(location:Uri.uri, content:string, options:WebClient.Put.options): -> WebClient.result(string) =
          result = @spawn({result = try_put_with_options(location, content, options)})
          -> @wait(result).result

       /**
        * Place a PUT request using custom options, call a function when result is available.
        *
        * This function is non-blocking.
        *
        * @param on_result A function called once the result is available
        *
        * Usage suggestion: use [try_put_async] and [try_put_with_options_async] when your code is very concurrent
        * and you intend to send messages on completion of requests.
        */
      try_put_async(location:Uri.uri, content:string, on_result: WebClient.result(string) -> void): void =
          try_put_with_options_async(location, content, default_options, on_result)
      try_put_with_options_async(location:Uri.uri, content:string, options:WebClient.Put.options, on_result: WebClient.result(string) -> void): void =
          length  = String.length(content)
          put_headers = "Content-Length: {length}\r\nContent-Type: {options.mimetype}\r\n"
          headers = match default_options.custom_headers with
            | {none}  -> put_headers
            | ~{some} -> "{put_headers}{some}"
          generic_options = {
            operation        = "PUT"
            auth             = options.auth
            redirect         = Option.map(Get.generic_options_of_get_options(_), options.redirect_to_get)
            timeout_sec      = options.timeout_sec
            custom_agent     = options.custom_agent
            custom_headers   = {some = headers}
            ssl_key          = options.ssl_key
            ssl_policy       = options.ssl_policy
          }
          on_success(x) = on_result({success = x})
          on_failure(x) = on_result({failure = x})
          Generic.try_request_with_options_async(location, "PUT", generic_options, {some = content}, on_success, on_failure)
   }}

   /**
    * {2 WebClient.Generic}
    */

   /**
    * Generic operations
    */
   Generic = {{
           //Note: Be careful when using this function. If you have a non-[{none}] [content] and you follow redirects, you can end up submitting your data several times
           //Specifications of HTTP (http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html, section 10.3) dictate that following redirections should happen only
           //when redirection is either [GET] or [HEAD]
         try_request_with_options_async(location:Uri.uri, request_kind:string,
                                                          options:WebClient.Generic.options,
                                                          content: option(string),
                                                          on_success:(WebClient.success(string) -> void),
                                                          on_failure:(WebClient.failure -> void)): void =
             raw = %%BslNet.Http_client.place_request%%:
                   /*hostname*/string, /*port*/int, /*path*/string,
                   /*request_kind*/string, /*data*/option(string),
                   /*is_secure*/bool, /*auth*/option(string), /*SSL key*/ option(SSL.private_key), /*SSL policy*/ option(SSL.policy),
                   /*timeout*/option(time_t),
                   /*custom_agent*/option(string), /*more_headers*/option(string),
                   /*success*/(string, int, string, list(string), (string -> option(string)) -> void),
                   /*failure*/(continuation(WebClient.failure))
                   -> void

             Rec = {{
                loop(location:Uri.uri, options:WebClient.Generic.options): void =
                  match location with
                    | {path=_ fragment=_ query=_ is_directory=_ is_from_root=_} -> on_failure({uri = Uri.to_string(location); reason = {relative_uri}})//We cannot handle relative URIs for the time being
                    | {address=_ query=_} -> on_failure({uri = Uri.to_string(location); reason = {incorrect_protocol}})
                    | ~{schema credentials=_ domain port path query fragment is_directory} ->
                      // path = List.cons("", path) //Force initial slash
                      protocol = match schema with
                        | {some = "http"}  -> {some = {false}}
                        | {some = "https"} -> {some = {true} }
                        | {none}           -> {some = {false}}
                        | _       -> {none}
                      match protocol with
                        | {none} -> on_failure({uri = Uri.to_string(location); reason = {incorrect_protocol}})
                        | {some = is_secure} ->
                           cb_failure = Continuation.make(on_failure)
                           cb_success(mime_type:string, code:int, content:string,
                                            headers:list(string), header_get:string -> option(string)) =
                             if (code == 301 || code == 302 || code == 303 || code == 307) then
                                  match options.redirect with
                                     | {some = new_options} ->
                                       match header_get("Location") with
                                        | ~{some} ->
                                          match Uri.of_string(some) with
                                           | {none}  -> on_failure({uri = some; reason = {cannot_parse_redirect}})
                                           | ~{some} -> loop(some, new_options)
                                         end
                                        | {none}  -> on_failure({no_redirect})
                                       end
                                    | {none} -> on_success(~{mime_type code content headers header_get})
                             else on_success(~{mime_type code content headers header_get})
                     port = if is_secure then port?443 else port?80
                      // FIXME, broken date abstraction... [timeout_sec:float] fields in options should be replaced with [timeout:Duration.duration] but there was a problem with value restriction
                     timeout = Option.map((f:float -> Duration.ll_export(Duration.ms(Int.of_float(f * 1000.)))), options.timeout_sec)
                     raw(domain, port, Uri.to_string(~{path query fragment is_directory is_from_root=true}), request_kind, content, is_secure,
                                 options.auth, options.ssl_key, options.ssl_policy, timeout,
                                 options.custom_agent, options.custom_headers,
                                 cb_success, cb_failure)
                  end
             }}
             Rec.loop(location, options)
   }}


   /**
    * {2 WebClient.Result}
    */

   /**
    * Utilities for manipulating the result of a request
    */
   Result =
   {{

     as_xml(result: WebClient.result(string)): WebClient.result(xmlns) =
       match result with
         | ~{failure} -> ~{failure}
         | ~{success} ->
            content = success.content
            match Xmlns.try_parse(content) with
              | {none} -> {failure = {result_does_not_parse = success}}
              |~{some} -> //{success = {success with content = some}}
                {success = {mime_type = success.mime_type
                 code      = success.code
                 content   = some
                 headers   = success.headers
                 header_get= success.header_get}}

     as_json(result: WebClient.result(string)): WebClient.result(RPC.Json.json) =
       match result with
         | ~{failure} -> ~{failure}
         | ~{success} ->
            content = success.content
            match Json.deserialize(content) with
              | {none} -> {failure = {result_does_not_parse = success}}
              |~{some} -> //{success = {success with content = some}}
                {success = {mime_type = success.mime_type
                 code      = success.code
                 content   = some
                 headers   = success.headers
                 header_get= success.header_get}}

     as_form(result: WebClient.result(string)): WebClient.result(list((string, string))) =
       match result with
         | ~{failure} -> ~{failure}
         | ~{success} ->
            content = success.content
            match Parser.try_parse(UriParser.query_parser, content) with
              | {none} -> {failure = {result_does_not_parse = success}}
              |~{some} -> //{success = {success with content = some}}
                {success = {mime_type = success.mime_type
                 code      = success.code
                 content   = some
                 headers   = success.headers
                 header_get= success.header_get}}

     get_class(result: WebClient.success('content)) =
        success = result.code
        if 100 <= success then
           if 200 <= success then
              if 300 <= success then
                 if 400 <= success then
                    if 500 <= success then
                       if 600 <= success then {server_error}
                       else                   {unknown}
                    else                      {client_error}
                 else                         {redirection}
              else                            {success}
            else                              {informational}
        else                                  {unknown}
   }}

}}
