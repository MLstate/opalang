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
 * Facebook Dialog API module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

import stdlib.apis.common
import stdlib.apis.facebook
import stdlib.apis.facebook.lib

/**
 * {1 About this module}
 *
 * This module provides an easy way to build Facebbok dialog urls
 * Important :
 *   Any application using this must respect
 *   {{:http://developers.facebook.com/policy}Facebook policy}
 *
 * {1 Where should I start?}
 *
 * You should first {{:https://www.facebook.com/developers/}register an app}
 * to get a application id, an api key and an application secret. You will
 * also have to configure various parameters on your application. Once done,
 * look the module [FbAuth] to obtain an access token from users. You can then use
 * this module to prompt users various dialogs.
 */

/**
 * Display type of a Facebook dialog
 * Default value is [page]
 */
type FbDialog.display =
    { page }            /** Dialog is displayed in a page */
  / { popup }           /** Dialog is displayed in a popup */
  / { iframe : string } /** Dialog is displayed in an iframe, you must provide a valid access token to use this kind of window */
  / { touch }           /** Dialod is displayed in a smartphone (Android phone, iPhone, ...) */
  / { wap }             /** Dialog is displayed in a simple phone (small screen not tactile) */

type FbDialog.custom_filter = {
  name : string
  uids : list(string)
}

type FbDialog.request_filter =
    { all }
  / { app_user }
  / { app_non_user }
  / { custom : FbDialog.custom_filter }

type FbDialog.request_options = {
  title   : string
  to      : string
  data    : string
  filters : list(FbDialog.request_filter)
}

/**
 * Options of OAuth dialogs for [FbDialog.full_oauth]. Default value is
 * [FbDialog.default_oauth].
 * - [scope] List of permissions requested to user
 * - [state] 1 string that will be sent back with the answer. Can be used to
 *   keep a track of the request
 */
type FbDialog.oauth_options = {
  scope : list(Facebook.permission)
  state : string
}

/**
 * Parsed result of an OAuth dialog in case of success.
 * - [code] A code that can be exchanged for an access token using
 *   [FbDialog.get_token_from_code]
 * - [state] If a state was provided in OAuth dialog options, the same string
 */
type FbDialog.oauth_code = {
  code  : string
  state : string
}

/**
 * Parsed result of an OAuth dialog.
 * Note: After a user logs in, he will be redirected to your site with
 * GET data, you need to use [FbDialog.oauth_res] to have this
 * data parsed in something like this.
 */
type FbDialog.oauth_res =
    { code  : FbDialog.oauth_code } /** User has accepted the request */
  / { error : Facebook.error }      /** User has not accepted (access_denied) or other error */

FbDialog(conf:Facebook.config) = {{

  @private dialog_url = "{Facebook.server_url}/dialog"

  @private dialog_display_to_data(p) =
    match p:FbDialog.display with
    { page }    -> [("display","page")]
    { popup }   -> [("display","popup")]
    { ~iframe } -> [("display","iframe"), ("access_token",iframe)]
    { touch }   -> [("display","touch")]
    { wap }     -> [("display","wap")]

  @private common_build_url(method, data, redirect_uri, display) =
    full_data = [ ("app_id",conf.app_id), ("redirect_uri",redirect_uri) ]
        |> List.append(dialog_display_to_data(display),_)
        |> List.append(data,_)
    FbLib.generic_build_path("{dialog_url}/{method}", full_data)

  @private filters_to_json(filters) : RPC.Json.json =
    sub(n) : RPC.Json.json = {String=n}
    aux(x) : RPC.Json.json = match x : FbDialog.request_filter with
    | { all }          -> {String="all"}
    | { app_user }     -> {String="app_user"}
    | { app_non_user } -> {String="app_non_user"}
    | { custom=c }     ->
      {Record=[("name", {String=c.name}:RPC.Json.json),
               ("user_ids", {List=(List.map(sub,c.uids))}:RPC.Json.json)]}
    {List=List.map(aux, filters)}

  full_request(message, options, redirect_uri, display) =
    filters_text =
      if options.filters == [] then ""
      else filters_to_json(options.filters) |> Json.serialize
    FbLib.add_if_filled("title", options.title, [("message",message)])
    |> FbLib.add_if_filled("to", options.to, _)
    |> FbLib.add_if_filled("data", options.data, _)
    |> FbLib.add_if_filled("filters", filters_text, _)
    |> common_build_url("apprequests", _, redirect_uri, display)

  default_request = {
    title   = ""
    to      = ""
    data    = ""
    filters = []
  }

  simple_request(message, redirect_uri) =
    full_request(message, default_request, redirect_uri, {page})

  /**
   * Build an url to prompt the user an OAuth dialog with all options
   * available.
   */
  full_oauth(options, redirect_uri, display) =
    scope = String.of_list(Facebook.permission_to_string, ",",options.scope)
    FbLib.add_if_filled("state", options.state, [("client_id", conf.app_id)])
    |> FbLib.add_if_filled("scope", scope, _)
    |> common_build_url("oauth", _, redirect_uri, display)

  default_oauth = {
    scope = []
    state = ""
  } : FbDialog.oauth_options

  /**
   * Build the url of a simple authentication dialog with no option
   */
  simple_oauth(redirect_uri) = full_oauth(default_oauth, redirect_uri, {page})

  /**
   * Parser for the GET parameters resulting of a OAuth dialog
   */
  oauth_res(rawdata) : FbDialog.oauth_res =
    data = API_libs.get_data(rawdata)
    get_field(name) = API_libs.get_field(data, name)
    if data == [] then
	{ error = Facebook.data_error }
    else if API_libs.has_field(data, "error") then
      { error = Facebook.make_error(get_field("error"), get_field("error_description")) }
    else
      { code = { code = get_field("code")
                 state = get_field("state") } }

  /**
   * Build an URL to prompt an user to add a friend with all options
   */
  full_friends(id, redirect_uri, display) =
    common_build_url("friends", [("id",id)], redirect_uri, display)

  /**
   * Build an URL to prompt an user to add a friend with no option
   */
  simple_friends(id, redirect_uri) =
    common_build_url("friends", [("id",id)], redirect_uri, {page})

  /**
   * Parser for the GET parameters resulting of a friends add dialog
   *
   * @param rawdata String containing all GET parameters
   *
   * @return A boolean set at [true] if user accepted
   */
  friends_res(rawdata) : bool =
    data = API_libs.get_data(rawdata)
    res = API_libs.get_field_def(data, "action", "0")
    if res == "0" then false else true

  /**
   * Build an URL to prompt a user to post a feed about something
   * (your website, for instance)
   */
  full_feed(feed, redirect_uri, display) =
    FbLib.feed_to_data(feed, true)
    |> common_build_url("feed", _, redirect_uri, display)

  /**
   * Build an URL to prompt a user to post a feed about something
   * (your website, for instance) with less options
   */
  simple_feed(feed, redirect_uri) = full_feed(feed, redirect_uri, {page})

  /**
   * Parser for the GET parmeters resulting of a feed dialog
   * Note: If the user refuses to publish the feed, there will be no
   * GET parameter.
   *
   * @param rawdata String containing all GET parameters
   *
   * @return A string containing the id of the newly created feed element
   */
  feed_res(rawdata) : string =
    data = API_libs.get_data(rawdata)
    API_libs.get_field(data, "post_id")

}}
