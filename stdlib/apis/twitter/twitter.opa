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
 * Twitter modules
 *
 * This file provides the implemented methods of Twitter API
 * IMPORTANT NOTE: This API implementation has been updated to use
 * OAuth but the documentation is not yet fully up to date. Please
 * disregard any reference to credentials "formatted as login:password"
 *
 * @category web
 * @author Nicolas Glondu, 2010
 * @destination
 * - Twitter_private : private (and poorly documented)
 * - Twitter : public (and well documented)
 * @stability Work in progress
 */

import stdlib.web.client
import stdlib.apis.common
import stdlib.apis.oauth

/* ---------------- */
/* Type definitions */
/* ---------------- */

/**
 * Twitter configuration
 */
type Twitter.configuration = {
  consumer_key    : string
  consumer_secret : string
}

/**
 * OAuth credentials of a user
 */
type Twitter.credentials = {
  access_token  : string
  access_secret : string
}

type Twitter.user =
    { uid : int } /** Unique identifier of a user */
  / { screen_name : string } /** Screen name of a user */
  / { id : string } /** A screen or a unique identifier of a user - Warning, a screen name can be a valid uid */

/**
 * Options of a Twitter search
 *
 * - [lang] Restricts results to given language
 * - [rpp] Tweets returned per page
 * - [page] Page number (rpp * page < 1500)
 * - [since_id] Returns only tweets which id greater than since_id
 */
type Twitter.search_options = {
  lang     : string
  rpp      : int
  page     : int
  since_id : int
}

/**
 * Options of a timeline request
 *
 * - [count] The number of statuses to return. Default is 20, may not be greater than 200
 * - [page] Number of the page to request. page*number may not be grater tha 3200
 * - [since_id] Allows to get statuses with id greater than this id
 * - [max_id] Allows to get statuses with id smaller than this id
 */
type Twitter.timeline_options = {
  count    : int
  page     : int
  since_id : int
  max_id   : int
}

/**
 * Type of Twitter OAuth parameters
 */
type Twitter.oauth_mode =
    { fast } /** Use [fast] if you want Twitter to skip user authorization if he has already accepted your application once. {{:http://dev.twitter.com/pages/sign_in_with_twitter}Look here} for more information about this. */
  / { full } /** Use [full] if you want Twitter to always follow full OAuth flow, no matter which interaction current user had with your application. */

/* ---------------- */
/*    Data types    */
/* ---------------- */

/**
 * Note for Twitter data types:
 *
 * The end user is not supposed to build object with those types. Those types are defined to structure
 * the data passed back to the user. It is hence necessary that the end user knows which information
 * the contains.
 */

/**
 * A Twitter trend.
 *
 * A trend on Twitter in one of the words mostly used in messages at a given date.
 * Trends are currently [April 2010] available for hours and days.
 */
type Twitter.trend = {
  name : string; /** The name of the trend */
  query : string; /** A query to access messages containing this trend */
}

/**
 * Twitter trends.
 *
 * Trends on Twitter are all the the words mostly used in messages during a given period.
 * Trends are currently [April 2010] available for hours and days.
 */
type Twitter.trends = {
  date : string; /** The date of the trends (the week of the day, according to the precision) */
  tmap : map(string,list(Twitter.trend)); /** A map(string, list(Twitter.trend)) where the value is a list of the trends for a given date which is the key. If the main date is a day, the keys will be precise to an hour, if it is a week, th keys will be precise to a day.*/
}

/**
 * A basic Twitter user.
 *
 * Basic information about a Twitter user.
 */
type Twitter.user_base = {
  user_id     : int; /** The user id of the user. */
  screen_name : string; /** The screen name of the user. */
  pic_url     : string; /** A url to the picture of the user. */
}

/**
 * An extendes Twitter user.
 *
 * Complete information about a Twitter user.
 */
type Twitter.user_extended = {
  user_base       : Twitter.user_base; /** Basic information about the user. */
  true_name       : string; /** The true name [as given to Twitter] of the user. */
  description     : string; /** The description of the user. */
  statuses_count  : int; /** The number of statuses posted by this user. */
  followers_count : int; /** The number of followers of the user. */
  friends_count   : int; /** The number of friends of the user (the number of users he follows). */
  lang            : string; /** The language of the user as an ISO 639-1 code. */
  bg              : string; /** A url to the background picture of the user. */
}


/**
 * A Twitter search result.
 *
 * A result of a Twitter search. It is in fact a simplified representation of a Twitter message.
 */
type Twitter.search_result = {
  date       : string; /** The date of the message. */
  from_user  : Twitter.user_base; /** Basic information about the poster of the message. */
  to_user_id : int; /** If defined (>0), the user to whom the message is addressed. */
  text       : string; /** The text of the message. */
  message_id : int; /** The id of the message. */
  iso_lang   : string; /** The ISO 639-1 code of the language of the poster. */
  source     : string; /** How the message was posted. */
}

/**
 * Twitter search results.
 *
 * The full result of a Twitter search.
 */
type Twitter.search_results = {
  max_id      : int; /** The hightest id present in the search result. Should be used to make another search for newer results. */
  since_id    : int; /** The lowest id present in the search result. Should be used to get older results. */
  rpp         : int; /** The current results per page. */
  page        : int; /** The current page number. */
  duration    : float; /** The time Twitter servers took to process your request. Note the this does not include the process time of you OPA server. */
  query       : string; /** The end of the url of your query. */
  refresh_url : string; /** The end of a url for a "refreshed" result. You can, for instance add "http://search.twitter.com/search" to it to create an external link to the live results. */
  results     : list(Twitter.search_result); /** A list of Twitter.search_result representing the result of the search. */
}

/**
 * A Twitter tweet
 *
 * Also called status or message, a tweet is a message posted in Twitter.
 */
type Twitter.tweet = {
  truncated               : bool; /** A boolean representing wether or not the message has been truncated by Twitter. The maximal length of a Twitter message is 140 characters. */
  created_at              : string; /** The date when the message wax posted. */
  source                  : string; /** How the message was posted. */
  id                      : int; /** The id of the message. */
  text                    : string; /** The text of the message. */
  poster                  : Twitter.user_extended; /** Extended information about the poster. */
  in_reply_to_user_id     : int; /** If the message is a reply to another message, the user id of the poster of this message. */
  in_reply_to_screen_name : string; /** If the message is a reply to another message, the screen name of the poster of this message. */
  in_reply_to_status_id   : int; /** If the message is a reply to another message, the id of this message. */
}

/**
 * A Twitter social graph
 *
 * Social graphs are given only by get_friends and get_followers. They represent a list aof id and two cursors
 * to access the adjacent pages of the graph. Note that it is more a list (that can be huge for some persons,
 * for instance ) than a true graph.
 */
type Twitter.social_graph = {
  previous_cursor : int; /** The cursor to the previous page of the graph. */
  next_cursor     : int; /** The cursor to the next page of the graph. */
  ids             : list(int); /** A list of ids corresponding to the requested social graph. */
}

/**
 * A Twitter social graph
 *
 * Social graphs are given only by get_friends and get_followers. They represent a list aof id and two cursors
 * to access the adjacent pages of the graph. Note that it is more a list (that can be huge for some persons,
 * for instance ) than a true graph.
 */
type Twitter.full_social_graph = {
  previous_cursor : int; /** The cursor to the previous page of the graph. */
  next_cursor     : int; /** The cursor to the next page of the graph. */
  users           : list(Twitter.tweet); /** A list containing the last status of each follower/friend. */
}

/**
 * Twitter rate limits
 *
 * There is a hourly limitation on the number of request that can be sent on Twitter servers. This is the the
 * result of the function asking the remaning hits. The limits can be found at
 * http://help.twitter.com/forums/10711/entries/15364
 * Those limits are associated to two things:
 * - a limit per IP for unauthenticated calls
 * - a limit per user ID for authenticated calls
 * You can be whitelisted by Twitter if you have a very good reason by filling the form available at
 * http://twitter.com/help/request_whitelisting
 */
type Twitter.rate_limit = {
  remaining_hits  : int; /** The remaining hits for the hour ending at reset_time. */
  hourly_limit    : int; /** Current hourly limit. */
  reset_time_secs : int; /** The seconds Unix Epoch time of the reset */
  reset_time      : string; /** A string representing the time of the reset */
}

/**
 * @author Nicolas Glondu, March 2010
 * @category Twitter ?
 * @category External API ?
 * @destination private
 * @stability Quite stable
 */

/**
 * {1 About this module}
 *
 * This module contains the core functions of the Twitter API.
 *
 * {1 Where should I start?}
 *
 * First are the treatment functions. Those functions are mainly json parsers
 * returning Twitter objects from json. Then come the download functions, some
 * constants specific to Twitter and some intermediate functions.
 *
 * {1 What if I need more?}
 *
 * If you want to add a method, you should first identify the type of its response,
 * if it does not exist, you should add it. Then create a json parser for it.
 * Once done, add a function in the Facebook module to acces to your work once
 * OPA compiled.
 */

@private TwitParse = {{

  /* ------------------- */
  /* Treatment functions */
  /* ------------------- */

  _check_date_parser = parser
  | ([0-9][0-9][0-9][0-9]) "-" month=([0-9][0-9]) "-" day=([0-9][0-9]) ->
    month = Int.of_string(Text.to_string(month))
    day = Int.of_string(Text.to_string(day))
    day > 0 && day < 32 && month > 0 && month < 13
  | .* -> false

  _check_date(date:string) = Parser.try_parse(_check_date_parser, date) ? false

  _build_ext_user_from_json(user_map) =
    map = JsonOpa.record_fields(user_map) ? Map.empty
    loc_int(name) = API_libs_private.map_get_int(name, map)
    loc_string(name) = API_libs_private.map_get_string(name, map)
    user_base = { user_id   = loc_int("id");
                  screen_name = loc_string("screen_name");
                  pic_url   = loc_string("profile_image_url");
                }:Twitter.user_base
   { user_base       = user_base;
     true_name       = loc_string("name");
     description     = loc_string("description");
     statuses_count  = loc_int("statuses_count");
     followers_count = loc_int("followers_count");
     friends_count   = loc_int("friends_count");
     lang            = loc_string("lang");
     bg              = loc_string("profile_background_image_url");
   }:Twitter.user_extended

  _build_tweet_from_json_and_poster(jsdata, poster) =
    map = JsonOpa.record_fields(jsdata) ? Map.empty
    loc_int(name) = API_libs_private.map_get_int(name, map)
    loc_string(name) = API_libs_private.map_get_string(name, map)
    { truncated               = API_libs_private.map_get_bool("truncated", map, false);
      created_at              = loc_string("created_at");
      source                  = loc_string("source");
      in_reply_to_user_id     = loc_int("in_reply_to_user_id");
      in_reply_to_status_id   = loc_int("in_reply_to_status_id");
      in_reply_to_screen_name = loc_string("in_reply_to_string_name");
      id                      = loc_int("id");
      text                    = loc_string("text");
      poster                  = poster;
    }:Twitter.tweet

  _build_tweet_from_json(tweet_json) =
    /* In this function, the main map is the tweet
       and the user is in the field "user" */
    map = JsonOpa.record_fields(tweet_json) ? Map.empty
    mapuser = Map.get("user", map) ? { Record = [] }
    poster = _build_ext_user_from_json(mapuser)
    _build_tweet_from_json_and_poster(tweet_json, poster)

  _build_tweet_from_json_2_int(data) =
    /* In this function, the main map is the user
       and the tweet is in the field "status" */
    map = JsonOpa.record_fields(data) ? Map.empty
    poster = _build_ext_user_from_json(data)
    tweetdata = Map.get("status", map) ?  { Record = [] }
    _build_tweet_from_json_and_poster(tweetdata, poster)

  _build_tweet_from_json_2(rawdata) =
    data = API_libs_private.parse_json(rawdata)
    _build_tweet_from_json_2_int(data)

  _build_trend_response(rawtrends) =
    json_2_trend_list(jsonlist) =
      eltlist = JsonOpa.to_list(jsonlist) ? []
      transf(elt) =
        elt = JsonOpa.record_fields(elt) ? Map.empty
        { name  = API_libs_private.map_get_string("name", elt);
          query = API_libs_private.map_get_string("query", elt);
        }:Twitter.trend
      List.map(transf, eltlist)

    trends = API_libs_private.parse_json(rawtrends)
    map = JsonOpa.record_fields(trends) ? Map.empty
    date = Json.to_string(Map.get("as_of", map) ? {String = "Error in date" })
    tmap = JsonOpa.record_fields(Map.get("trends", map) ?  { Record = [] } ) ? Map.empty
    tmap = API_libs_private.remap(json_2_trend_list, tmap)
    { date = date;
      tmap = tmap
    }:Twitter.trends

  _build_search_response(rawresult) =
    json_2_search_result(jsres) =
      map = JsonOpa.record_fields(jsres) ? Map.empty
      loc_int(name) = API_libs_private.map_get_int(name, map)
      loc_string(name) = API_libs_private.map_get_string(name, map)
      user = { user_id   = loc_int("from_user_id");
               screen_name = loc_string("from_user");
               pic_url   = loc_string("profile_image_url");
             }:Twitter.user_base
      { date       = loc_string("created_at");
        from_user  = user;
        to_user_id = loc_int("to_user_id");
        text       = loc_string("text");
        message_id = loc_int("id");
        iso_lang   = loc_string("iso_language_code");
        source     = loc_string("source");
      }:Twitter.search_result

    result = API_libs_private.parse_json(rawresult)
    map = JsonOpa.record_fields(result) ? Map.empty
    loc_int(name) = API_libs_private.map_get_int(name, map)
    loc_string(name) = API_libs_private.map_get_string(name, map)
    reslist = JsonOpa.to_list(Map.get("results", map) ? {List = []}:RPC.Json.json) ? []
    { max_id      = loc_int("max_id");
      since_id    = loc_int("since_id");
      refresh_url = loc_string("refresh_url");
      rpp         = loc_int("results_per_page");
      page        = loc_int("page");
      duration    = API_libs_private.map_get_float("completed_in", map);
      query       = loc_string("query");
      results     = List.map(json_2_search_result, reslist)
    }:Twitter.search_results

  _build_one_status(rawresponse) =
    response = API_libs_private.parse_json(rawresponse)
    _build_tweet_from_json(response)

  _build_timeline_response(rawresponse) =
    response = API_libs_private.parse_json(rawresponse)
    list = JsonOpa.to_list(response) ? []
    List.map(_build_tweet_from_json, list)

  _build_social_graph(rawgraph) =
    loc_to_int(x) = API_libs_private.json_to_int_unsafe(x) ? 0
    jsgraph = Json.of_string(rawgraph) |> Option.get
    map = JsonOpa.record_fields(jsgraph) ? Map.empty
    loc_int(name) = API_libs_private.map_get_int(name, map)
    ids = JsonOpa.to_list(Map.get("ids", map) ? {List = []}:RPC.Json.json) ? []
    ids = List.map(loc_to_int, ids)
    { previous_cursor = loc_int("previous_cursor");
      next_cursor = loc_int("next_cursor");
      ids = ids;
    }:Twitter.social_graph

  _build_full_social_graph(rawgraph) =
    jsgraph = Json.of_string(rawgraph) |> Option.get
    map = JsonOpa.record_fields(jsgraph) ? Map.empty
    loc_int(name) = API_libs_private.map_get_int(name, map)
    users = JsonOpa.to_list(Map.get("users", map) ? {List = []}:RPC.Json.json) ? []
    users = List.map(_build_tweet_from_json_2_int, users)
    { previous_cursor = loc_int("previous_cursor");
      next_cursor = loc_int("next_cursor");
      users = users;
    }:Twitter.full_social_graph

  _build_rate_limit(rawlimit) =
    jslimit = API_libs_private.parse_json(rawlimit)
    map = JsonOpa.record_fields(jslimit) ? Map.empty
    loc_int(name) = API_libs_private.map_get_int(name, map)
    { remaining_hits = loc_int("remaining_hits");
      hourly_limit = loc_int("hourly_limit");
      reset_time_secs = loc_int("reset_time_in_seconds");
      reset_time = API_libs_private.map_get_string("reset_time", map);
    }:Twitter.rate_limit

  _simple_decoder(data) =
    decode_data =
      [ ("\\u00e9", "é"), ("\\u00e8", "è"), ("\\u00e0", "à"),
        ("\\u00c9", "É"), ("\\u00c8", "È"), ("\\u00c0", "À"),
        ("\\u00ea", "ê"), ("\\u00ca", "Ê"), ("\\u00ef", "ï"),
        ("\\u00cf", "Ï"), ("\\u00f4", "ô"), ("\\u00e7", "ç"),
        ("\\u00c7", "Ç"), ("\\u00f9", "ù"), ("\\u00d9", "Ù"),
        ("\\u20ac", "€"), ("\\u00a0", " "), ("\\u00ee", "î") ]
    aux((from, to), acc) = String.replace(from, to, acc)
    List.fold(aux, decode_data, data)

}}

@private Twitter_private(conf:Twitter.configuration) = {{

  /* ----------------------- */
  /* Twitter-specific values */
  /* ----------------------- */

  _api_host = "https://api.twitter.com"

  /* ------------------ */
  /* Download functions */
  /* ------------------ */

  twOAuth(http_method) = OAuth({
    consumer_key      = conf.consumer_key
    consumer_secret   = conf.consumer_secret
    auth_method       = {HMAC_SHA1}
    request_token_uri = "https://api.twitter.com/oauth/request_token"
    authorize_uri     = "https://api.twitter.com/oauth/authorize"
    access_token_uri  = "https://api.twitter.com/oauth/access_token"
    http_method       = http_method
    inlined_auth      = false
  } : OAuth.parameters)


  _wget_generic(path:string, wget_fun, parse_fun) =
    host = _api_host
    do API_libs_private.apijlog("-- Fetching {host} - {path} \n data --")
    (t, res) = Duration.execution_time( -> wget_fun("{host}{path}"))
    do API_libs_private.apijlog("Download: {Duration.in_seconds(t)} seconds")
    (t, res) = Duration.execution_time( -> parse_fun(res))
    do API_libs_private.apijlog("Parsing:  {Duration.in_seconds(t)} seconds")
    res

  _get_res_unlogged(path, params, parse_fun) =
    data = API_libs.form_urlencode(params)
    f(uri) =
      full_uri = if data == "" then uri else "{uri}?{data}"
      match Uri.of_string(full_uri) with
      | {none} -> ""
      | {some=u} ->
        match WebClient.Get.try_get(u) with
        | {failure=_} -> ""
        | {success=s} -> s.content
    _wget_generic(path, f, parse_fun)

  _get_res_logged(path, params, credentials:Twitter.credentials, parse_fun) =
    f(uri) =
      twOAuth({GET}).get_protected_resource(uri,params,
                        credentials.access_token,credentials.access_secret);
    _wget_generic(path, f, parse_fun)

  _get_res(path, params, credentials:option(Twitter.credentials), parse_fun) =
    match credentials with
    | {some=c} -> _get_res_logged(path, params, c, parse_fun)
    | {none}   -> _get_res_unlogged(path, params, parse_fun)

  _post_res(path, params, credentials:Twitter.credentials, parse_fun) =
    f(uri) =
      twOAuth({POST}).get_protected_resource(uri,params,
                        credentials.access_token,credentials.access_secret)
    _wget_generic(path, f, parse_fun)

  /* ---------------------- */
  /* Intermediate functions */
  /* ---------------------- */

  add_if(key, elt, cond) = list ->
    if cond(elt) then List.cons((key, "{elt}"), list)
    else list

  _build_trend_params(exhashtags:bool, date) =
    [] |> add_if("date", date, TwitParse._check_date)
       |> add_if("exclude", "hashtags", (_ -> exhashtags))

  _get_trends_generic(trend_type, params) =
    path = "/1/trends/" ^ trend_type ^ ".json"
    _get_res_unlogged(path, params, TwitParse._build_trend_response)

  _get_generic_timeline(path, p:Twitter.timeline_options, more, credentials) =
    params = more
      |> add_if("count", p.count, (_>0))
      |> add_if("page", p.page, (_>0))
      |> add_if("since_id", p.since_id, (_>0))
      |> add_if("max_id", p.max_id, (_>0))
    _get_res(path, params, credentials, TwitParse._build_timeline_response)

  _get_generic_socgraph(name, id, cursor, graphtype:string, credentials) =
    path = "/1/{graphtype}/ids.json"
    params = [ (if (name == "" && id != 0) then ("user_id","{id}") else ("screen_name",name))
             , (if (cursor == 0) then ("cursor","-1") else ("cursor","{cursor}")) ]
    _get_res(path, params, credentials, TwitParse._build_social_graph)

}}

/**
 * @author Nicolas Glondu, March 2010
 * @category Twitter ?
 * @category External API ?
 * @destination public
 * @stability Quite stable
 */

/**
 * {1 About this module}
 *
 * This module contains the implemented methods for the Twitter API. Quite few aspects
 * of the social aspect of Twitter are implemented. The focus is made on the statuses.
 * A drawback of this is that not all methods are implemented, but the advantage is that
 * the implemented methods return formatted results easier to use.
 *
 * Information about Twitter API can be found here: http://apiwiki.twitter.com/Getting-Started
 *
 * {1 Where should I start?}
 *
 * This implementation was made to be as simple as possible. Simply call the required
 * method and it will return the object indicated in its documentation. You only have
 * to display (or not) the result as you wish.

 * The final_fun parameter of each function must have the type API_libs.answer_fun. If
 * you give a api_fun_html, the function will return a api_html that you should
 * handle with Twitter.get_html. If you give a api_fun_void, it will return a api_void
 * that you should handle with Twitter.get_void. This strange mechanism allows to have
 * the same function returning html (for direct pages) sometimes while returning void
 * (for instance for session communication) at other times.
 *
 * If you already worked with the Facebook OPA API, the mechanism used here
 * is the same.
 *
 * Note: An account on Twitter is required to post messages.
 *
 * {1 What if I need more?}
 *
 * If you want to make a call not implemented, you should use custom_get_request or
 * custom_post_request and build your request and the parser for its answer
 * yourself. Consult http://apiwiki.twitter.com/Twitter-API-Documentation for a
 * list of available methods.
 *
 * An example of path: /1/trends/44418.json (returns the trends in London)
 */

Twitter(conf:Twitter.configuration) = {{

@private c = conf

@private add_if(key, elt, cond) = list ->
  if cond(elt) then List.cons((key, "{elt}"), list)
  else list

  oauth_params(mode:Twitter.oauth_mode) = {
    consumer_key      = conf.consumer_key
    consumer_secret   = conf.consumer_secret
    auth_method       = {HMAC_SHA1}
    request_token_uri = "https://api.twitter.com/oauth/request_token"
    authorize_uri     =
      match mode with
      | {fast} -> "https://api.twitter.com/oauth/authenticate"
      | {full} -> "https://api.twitter.com/oauth/authorize"
    access_token_uri  = "https://api.twitter.com/oauth/access_token"
    http_method       = {POST}
    inlined_auth      = false
  } : OAuth.parameters


/**
 * Custom GET request (advanced)
 *
 * Allows the user to do a custom GET request on Twitter.
 * Returns the API_libs.answer resulting to final_fun applyed to the
 * raw result from Twitter as a string.
 *
 * @param path Path corrsponding to request built according to the Twitter's API
 * @param params (optional) Parameters as a (key, value) list if required
 * @param credentials (optional) Credential if required
 * @param final_fun A API_libs.answer_fun object containing a function taking a string and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
 custom_get_request(path, params, credentials) =
   Twitter_private(c)._get_res(path, params, credentials, (x->x))

/**
 * Custom POST request (advanced)
 *
 * Allows the user to do a custom POST request on Twitter.
 * Returns the API_libs.answer resulting to final_fun applyed to the
 * raw result from Twitter as a string.
 *
 * @param path Path corrsponding to request built according to the Twitter's API
 * @param params (optional) Parameters as a (key, value) list if required
 * @param credentials (optional) Credential if required
 * @param final_fun A API_libs.answer_fun object containing a function taking a string and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
 custom_post_request(path, params, credentials) =
   Twitter_private(c)._post_res(path, params, credentials, (x->x))

  default_search = {
    lang     = ""
    rpp      = 0
    page     = 0
    since_id = 0
  } : Twitter.search_options

/**
 * Search request : http://search.twitter.com/search.json[?q=<search request>&params]
 *
 * Makes a Twitter search of given request.
 * Returns a Twitter.search_results object
 *
 * @param request The request (will be url encoded)
 * @param options
 */
  search(request, options:Twitter.search_options) =
    path = "/search.json"
    params = [("q", request)]
      |> add_if("lang", options.lang, (_!=""))
      |> add_if("rpp", options.rpp, (_!=0))
      |> add_if("page", options.page, (_!=0))
      |> add_if("since_id", options.since_id, (_!=0))
    Twitter_private(c)._get_res_unlogged(path, params, TwitParse._build_search_response)

/**
 * Current trends
 *
 * (The trends on Twitter are the words mostly used in messages during a given period.)
 * This function returns current trends.
 * Returns a Twitter.trends object.
 *
 * @param exhashtags Indicates whether or not hashtags (#abcd) should be excluded from the trends result
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.trends and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_current_trends(exhashtags) =
    params = if (exhashtags) then [("exclude","hashtags")] else []
    Twitter_private(c)._get_trends_generic("current", params)

/**
 * Daily trends
 *
 * (The trends on Twitter are the words mostly used in messages during a given period.)
 * This function returns the trends for each hour (approximately) the given day. If no
 * day is given it returns the trends of /yesterday/.
 * Returns a Twitter.trends object.
 *
 * @param exhashtags Indicates whether or not hashtags (#abcd) should be excluded from the trends result
 * @param date (optional) The day when should start the trend report. Should be formatted YYYY-MM-DD
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.trends and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_daily_trends(exhashtags, date) =
    params = Twitter_private(c)._build_trend_params(exhashtags, date)
    Twitter_private(c)._get_trends_generic("daily", params)

/**
 * Weekly trends
 *
 * (The trends on Twitter are the words mostly used in messages during a given period.)
 * This function returns the trends for each day one week (7 days) starting the given day.
 * If no day is given it returns the trends of the week ending /yesterday/.
 * Returns a Twitter.trends object.
 *
 * @param exhashtags Indicates whether or not hashtags (#abcd) should be excluded from the trends result
 * @param date (optional) The day when should start the trend report. Should be formatted YYYY-MM-DD
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.trends and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_weekly_trends(exhashtags, date) =
    params = Twitter_private(c)._build_trend_params(exhashtags, date)
    Twitter_private(c)._get_trends_generic("weekly", params)

/**
 * Public timeline
 *
 * This function returns the 20 most recent public (=from non protected users who have set a custom
 * picture) statuses update. The answer is cached for 60 seconds so it is useless to
 * call it more with a shorter period.
 * Returns a list of Twitter.tweet objects.
 *
 * @param final_fun A API_libs.answer_fun object containing a function taking a list of Twitter.tweet and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_public_timeline() =
    path = "/1/statuses/public_timeline.json"
    Twitter_private(c)._get_res_unlogged(path, [], TwitParse._build_timeline_response)

  default_timeline = {
    count    = 0
    page     = 0
    since_id = 0
    max_id   = 0
  } : Twitter.timeline_options

/**
 * Home timeline
 *
 * Returns the most recent status updates of current user and of his friends.
 * Will display the same tweets than those displayed in the user's homepage.
 * Returns a list of Twitter.tweet objects.
 *
 * @param count (optional) The number of statuses to return. Default is 20, may not be greater than 200
 * @param page (optional) Number of the page to request. page*number may not be grater tha 3200
 * @param since_id (optional) Allows to get statuses with id greater than this id
 * @param max_id (optional) Allows to get statuses with id smaller than this id
 * @param credentials The credentials of the user formatted as login:password
 * @param final_fun A API_libs.answer_fun object containing a function taking a list of Twitter.tweet and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_home_timeline(params, credentials) =
    path = "/1/statuses/home_timeline.json"
    Twitter_private(c)._get_generic_timeline(path, params, [], credentials)

/**
 * User timeline
 *
 * This function returns the most recent status updates of specified user. Will display the same
 * tweets than those displayed in the user's profile. Required authentication if the
 * requested user is protected. There should only be one or none among id, uid and
 * screen name. If none, requires authentification and returning the user timeline of
 * current user.
 * Returns a list of Twitter.tweet objects.
 *
 * @param user The screen name or the unique id of requested user.
 * @param options Options of the request [Twitter.timeline_options]
 * @param credentials (optional) The credentials of the user formatted as login:password
 */
  get_user_timeline(user:Twitter.user, options, credentials) =
    path = "/1/statuses/user_timeline.json"
    more = match user with
      | {~uid}         -> [("uid","{uid}")]
      | {~id}          -> [("id",id)]
      | {~screen_name} -> [("screen_name",screen_name)]
    Twitter_private(c)._get_generic_timeline(path, options, more, credentials)

/**
 * Get specific message
 *
 * This function returns the message having the specified id. Requires authentication if
 * the author of the message is protected.
 * Returns a Twitter.tweet object.
 *
 * @param id The id of requested message.
 * @param credentials (optional) The credentials of the user formatted as login:password
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.tweet and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_specific_message(id:int, credentials) =
    path = "/1/statuses/show/{id}.json"
    Twitter_private(c)._get_res(path, [], credentials, TwitParse._build_one_status)

/**
 * Get friends
 *
 * This function returns a list of the friends (=the peoples the user follows) of
 * given user. Requires authentication if this user is protected.
 * Returns a Twitter.social_graph object.
 *
 * @param name The screen name of requested user.
 * @param id The user id of requested user.
 * @param cursor Creates page to navigate among the friends. Useful for users with a lot of friends. Starts with -1.
 * @param credentials (optional) The credentials of the user formatted as login:password
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.social_graph and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_friends(name, id, cursor, credentials) =
    Twitter_private(c)._get_generic_socgraph(name, id, cursor, "friends", credentials)

/**
 * Get followers
 *
 * This function returns a list of the followers of given user. Requires
 * authentication if this user is protected.
 * Returns a Twitter.social_graph object.
 *
 * @param name The screen name of requested user.
 * @param id The user id of requested user.
 * @param cursor Creates page to navigate among the friends. Useful for users with a lot of friends. Starts with -1.
 * @param credentials (optional) The credentials of the user formatted as login:password
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.social_graph and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_followers(name, id, cursor, credentials) =
    Twitter_private(c)._get_generic_socgraph(name, id, cursor, "followers", credentials)

/**
 * Get limits
 *
 * This function returns the number of API requests remaining before the hourly limit.
 * This call does not count in this limit. If authentication is provided, returns the
 * limit for logged in user. Else returns the limit for current IP address.
 * Returns a Twitter.rate_limit object.
 *
 * @param credentials (optional) The credentials of the user formatted as login:password
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.rate_limit and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_limit(credentials) =
    Twitter_private(c)._get_res("/1/account/rate_limit_status.json", [], credentials, TwitParse._build_rate_limit)

/**
 * Post status
 *
 * This function updates the status of the authenticated user.
 * Returns a Twitter.tweet object corresponding to the created status.
 *
 * @param message The text of the status update.
 * @param reply_to (optionnal) The ID of an existing message that the message is an update to. Ignored if the body of the message does not contains the name of the poster.
 * @param credentials The credentials of the user formatted as login:password
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.tweet and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  post_status(message, reply_to, credentials) =
    path = "/1/statuses/update.json"
    params = [("status",message)]
      |> add_if("in_reply_to_status_id", reply_to, (_!=""))
    Twitter_private(c)._post_res(path, params, credentials, TwitParse._build_one_status)

/**
 * Delete status
 *
 * This function deletes the status with given if posted by the authenticated user.
 * Returns a Twitter.tweet object corresponding to the deleted status.
 *
 * @param id The ID of the status to delete.
 * @param credentials The credentials of the user formatted as login:password
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.tweet and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  delete_status(id:int, credentials) =
    path = "/1/statuses/destroy/{id}.json"
    Twitter_private(c)._post_res(path, [], credentials, TwitParse._build_one_status)

/**
 * Check credentials
 *
 * This function checks if provided credentials are correct.
 * Returns a Twitter.tweet object corresponding to the last status of the user if the credentials are correct. If incorrect, returns an empty Twitter.tweet object.
 *
 * @param credentials The credentials of the user formatted as login:password
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.tweet and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  check_credentials(credentials) =
    path = "/1/account/verify_credentials.json"
    Twitter_private(c)._get_res_logged(path, [], credentials, TwitParse._build_tweet_from_json_2)

/**
 * Send a private message
 *
 * This function sends a private message to a user
 * from authenticated user.
 *
 * @param user The recipient if the message
 * @param text The message. Like all messages on Twitter, it should not exceed 140 characters
 */
  private_message(user:Twitter.user, message, credentials) =
    path = "/1/direct_messages/new.json"
    params =
      ( match user with
        | ~{uid} -> [("user_id", "{uid}")]
        | ~{screen_name} -> [("screen_name", screen_name)]
        | ~{id} -> [("user", id)]
      ) |> List.cons(("text",message),_)
    Twitter_private(c)._post_res(path, params, credentials, identity)

  Statuses = {{

    @private generic_full_graph(t, user:option(Twitter.user), cursor:int, credentials) =
      path = "/1/statuses/{t}.json"
      params =
       ( match user with
          | {some=~{uid}} -> [("user_id", "{uid}")]
          | {some=~{screen_name}} -> [("screen_name", screen_name)]
          | {some=~{id}} -> [("id", id)]
          | {none} -> []
        ) |> List.cons(("cursor","{cursor}"),_)
      Twitter_private(c)._get_res(path, params, credentials, TwitParse._build_full_social_graph)

    followers(user:option(Twitter.user), cursor:int, credentials) =
      generic_full_graph("followers", user, cursor, credentials)

    friends(user:option(Twitter.user), cursor:int, credentials) =
      generic_full_graph("friends", user, cursor, credentials)
  }}

}}
