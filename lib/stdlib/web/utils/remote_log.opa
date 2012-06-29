/*
    Copyright © 2011, 2012 MLstate

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
     cookie = match HttpRequest.get_cookie() with
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
