/*
    Copyright © 2011, 2012, 2013 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/*
 *  RemoteLog module
 *
 *  @author: Cedric Soulas, 2011, 2012
 */

/**
 * {1 About this module}
 *
 * Log any kind of events to a remote server, via a simple http GET request.
 * Example: http://hostname:port/?appkey=key&cookie=f8a0qb9sq98fs&event1=v1&event2=v2
 * This remote log server is specified with the "--remote-logs" option.
 *
 * WARNING: this is an experimental feature.
 *
 */

/**
 * {1 Interface}
 */
RemoteLog = {{

  @private
  get_remote_logs_params = %% BslNet.Http_server.get_remote_logs_params %% : -> option((string, int, string))

  @private
  remote_uri_prefix() =
    match get_remote_logs_params() with
    | {none} -> {none}
    | {some=(hostname,port,appkey)} ->
     cookie = match HttpRequest.get_internal_cookie() with
     | {some=cookie} -> "&cookie={cookie}"
     | _ -> ""
     some("http://{hostname}:{port}/?appkey={appkey}{cookie}")

  /*
   * Log a list of events to a remote server.
   * WARNING: this is an experimental feature.
   *
   * @param event_list a list of ("key", "value") events to be sent to the remote log server. The value is encoded, not the key.
   */
  info(event_list) =
    match remote_uri_prefix() with
    | {none} -> void
    | {some=uri} -> (
      uri = List.fold(((key,value), acc ->
        value = Uri.encode_string(value)
        "{acc}&{key}={value}"), event_list, uri)
      match Uri.of_string(uri) with
      | {none} -> Log.error("Remote log server","Bad uri: {uri}")
      | {some=uri} -> WebClient.Get.try_get_async(uri, (_ -> void))
    )

   /*
    * Same as RemoteLog.info but adding a ("error", "1") to the event list.
    */
   error(event_list) = info([("error","1")|event_list])

   /*
    * Same as RemoteLog.info but adding a ("error", "2") to the event list.
    */
   fatal(event_list) = info([("error","2")|event_list])

}}
