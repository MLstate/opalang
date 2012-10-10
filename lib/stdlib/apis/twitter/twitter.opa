package twitter
/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
 * Twitter error
 */
type Twitter.error = {
  code    : int
  message : string
}

type Twitter.failure =
   { twitter : list(Twitter.error) }

type Twitter.outcome('res) = outcome('res,Twitter.failure)

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

type Twitter.result_type = {mixed} / {recent} / {popular}

/**
 * Options of a Twitter search
 *
 * - [geocode] Returns tweets by users located within a given radius of the given latitude/longitude.
 * - [lang] Restricts results to given language
 * - [locale] Restricts tweets to the given language, given by an ISO 639-1 code.
 * - [result_type] Specifies what type of search results you would prefer to receive. (mixed, popular, recent)
 * - [count] Tweets returned per page
 * - [until] Returns tweets generated before the given date.
 * - [since_id] Returns only tweets which id greater than since_id
 * - [max_id] Returns results with an ID less than (that is, older than) or equal to the specified ID.
 * - [include_entities] The entities node will be disincluded when set to false.
 * - [callback] If supplied, the response will use the JSONP format with a callback of the given name.
 */
type Twitter.search_options = {
  geocode          : string
  lang             : string
  locale           : string
  result_type      : option(Twitter.result_type)
  count            : int
  until            : option(Date.date)
  since_id         : int
  max_id           : int
  include_entities : option(bool)
  callback         : string
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
  include_rts : bool
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
 * the type contains.
 */

/**
 * A Twitter trend.
 *
 * A trend on Twitter in one of the words mostly used in messages at a given date.
 */
type Twitter.trend = {
  name             : string; /** The name of the trend */
  query            : string; /** A query to access messages containing this trend */
  url              : string;
  promoted_content : string;
  events           : string;
}

type Twitter.location = {
  name  : string
  woeid : int
}

type Twitter.place_type = {
  code : int
  name : string
}

type Twitter.full_location = {
  country     : string
  countryCode : string
  name        : string
  parentid    : int
  placeType   : Twitter.place_type
  url         : string
  woeid       : int
}

/**
 * Twitter trends.
 *
 * Trends on Twitter are all the the words mostly used in messages during a given period.
 * Trends are currently [April 2010] available for hours and days.
 */
type Twitter.trends = {
  as_of      : string
  created_at : string
  locations  : list(Twitter.location)
  trends     : list(Twitter.trend)
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

type Twitter.metadata = { result_type : string iso_language_code : string }

type Twitter.url = {
  url          : string
  expanded_url : string
  indices      : list(int)
}

type Twitter.urls = {
  urls : list(Twitter.url)
}

type Twitter.hashtag = {
  text    : string
  indices : list(int)
}

type Twitter.entities = {
  url           : Twitter.urls
  description   : Twitter.urls
  hashtags      : list(Twitter.hashtag)
  user_mentions : RPC.Json.json
}

type Twitter.full_user = {
  id : int
  id_str : string
  name : string
  screen_name : string
  location : string
  description : string
  url : string
  entities : Twitter.entities
  protected : bool
  followers_count : int
  friends_count : int
  listed_count : int
  created_at : string
  favourites_count : int
  utc_offset : int
  time_zone : string
  geo_enabled : bool
  verified : bool
  statuses_count : int
  lang : string
  contributors_enabled : bool
  is_translator : bool
  profile_background_color : string
  profile_background_image_url : string
  profile_background_image_url_https : string
  profile_background_tile : bool
  profile_image_url : string
  profile_image_url_https : string
  profile_banner_url : string
  profile_link_color : string
  profile_sidebar_border_color : string
  profile_sidebar_fill_color : string
  profile_text_color : string
  profile_use_background_image : bool
  show_all_inline_media : bool
  default_profile : bool
  default_profile_image : bool
  following : RPC.Json.json
  follow_request_sent : RPC.Json.json
  notifications : RPC.Json.json
}

type Twitter.statuses = {
  message : string
  metadata : Twitter.metadata
  created_at : string
  id  : int
  id_str : string
  text : string
  source : string
  truncated : bool
  in_reply_to_status_id : int
  in_reply_to_status_id_str : string
  in_reply_to_user_id : int
  in_reply_to_user_id_str : string
  in_reply_to_screen_name : string
  user : Twitter.full_user
  geo : RPC.Json.json
  coordinates : RPC.Json.json
  place : RPC.Json.json
  contributors : RPC.Json.json
  retweet_count  : int
  entities : Twitter.entities
  favourited : bool
  retweeted : bool
}

type Twitter.search_metadata = {
  completed_in : float
  max_id : int
  max_id_str : string
  next_results : string
  query : string
  refresh_url : string
  count : int
  since_id : int
  since_id_str : string
}

type Twitter.search_result = {
  statuses        : list(Twitter.statuses)
  search_metadata : Twitter.search_metadata
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
    loc_int(name) = get_int(name, map)
    loc_string(name) = get_string(name, map)
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
    loc_int(name) = get_int(name, map)
    loc_string(name) = get_string(name, map)
    { truncated               = get_bool("truncated", map, false);
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

  get_json_int(json) = match json with | {Int=i} -> i | _ -> -1
  get_unknown(name, map) = Map.get(name, map) ? {String = "missing unknown type" }
  get_int = API_libs_private.map_get_int
  get_float = API_libs_private.map_get_float
  get_string = API_libs_private.map_get_string
  get_bool = API_libs_private.map_get_bool
  get_date(name, map) = Json.to_string(Map.get(name, map) ? {String = "Error in date" })
  get_obj(name, map, get_elt) = get_elt((Map.get(name, map)) ? {Record=[]})
  get_raw_list(json, get_elt) = List.map(get_elt, JsonOpa.to_list(json) ? [])
  get_list(name, map, get_elt) = get_raw_list(Map.get(name, map) ? {List=[]}, get_elt)
  get_map(name, map, get_elt) =
    tmap = JsonOpa.record_fields(Map.get(name, map) ? { Record = [] } ) ? Map.empty
    API_libs_private.remap(get_elt, tmap)

  get_trend_elt(elt) =
    elt = JsonOpa.record_fields(elt) ? Map.empty
    { name  = get_string("name", elt);
      query = get_string("query", elt);
      url = get_string("url", elt);
      promoted_content = get_string("promoted_content", elt);
      events = get_string("events", elt);
    } : Twitter.trend

  get_location_elt(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    { name  = get_string("name", map);
      woeid = get_int("woeid", map)
    } : Twitter.location

  get_place_type(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    { code = get_int("code", map)
      name  = get_string("name", map);
    } : Twitter.place_type

  get_full_location_elt(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    { 
      country  = get_string("country", map);
      countryCode  = get_string("countryCode", map);
      name  = get_string("name", map);
      parentid = get_int("parentid", map)
      placeType = get_obj("placeType", map, get_place_type)
      url  = get_string("url", map);
      woeid = get_int("woeid", map)
    } : Twitter.full_location

  get_trends_elt(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    as_of = get_date("as_of", map)
    created_at = get_date("created_at", map)
    locations = get_list("locations", map, get_location_elt)
    trends = get_list("trends", map, get_trend_elt)
    ~{ as_of; created_at; locations; trends } : Twitter.trends

  get_error_elt(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    { code  = get_int("code", map);
      message = get_string("message", map)
    } : Twitter.error

  _check_errors(json, on_ok) =
    match json with
    | {Record=r} ->
       match List.assoc("errors",r) with
       | {some=errs} -> {failure={twitter=get_raw_list(errs, get_error_elt)}}
       | {none} -> {success=on_ok(json)}
       end
   | _ -> {success=on_ok(json)}

  _build_trend_place_response(s:string) =
    json = API_libs_private.parse_json(s)
    _check_errors(json, get_raw_list(_, get_trends_elt))

  _build_trend_locations_response(s:string) =
    json = API_libs_private.parse_json(s)
    _check_errors(json, get_raw_list(_, get_full_location_elt))

  get_metadata(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      result_type = get_string("result_type", map)
      iso_language_code = get_string("iso_language_code", map)
    } : Twitter.metadata

  get_url(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      url = get_string("url", map)
      expanded_url = get_string("expanded_url", map)
      indices = get_list("indices", map, get_json_int)
    } : Twitter.url

  get_urls(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
       urls = get_list("urls", map, get_url)
    } : Twitter.urls

  get_hashtag(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      text = get_string("text", map)
      indices = get_list("indices", map, get_json_int)
    } : Twitter.hashtag

  get_entities(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      url = get_obj("url", map, get_urls)
      description = get_obj("description", map, get_urls)
      hashtags = get_list("hashtags", map, get_hashtag)
      user_mentions = get_unknown("user_mentions", map)
    } : Twitter.entities

  get_user(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      id = get_int("id", map)
      id_str = get_string("id_str", map)
      name = get_string("name", map)
      screen_name = get_string("screen_name", map)
      location = get_string("location", map)
      description = get_string("description", map)
      url = get_string("url", map)
      entities = get_obj("entities", map, get_entities)
      protected = get_bool("protected", map, false)
      followers_count = get_int("followers_count", map)
      friends_count = get_int("friends_count", map)
      listed_count = get_int("listed_count", map)
      created_at = get_date("created_at", map)
      favourites_count = get_int("favourites_count", map)
      utc_offset = get_int("utc_offset", map)
      time_zone = get_string("time_zone", map)
      geo_enabled = get_bool("get_bool", map, false)
      verified = get_bool("verified", map, false)
      statuses_count = get_int("statuses_count", map)
      lang = get_string("lang", map)
      contributors_enabled = get_bool("contributors_enabled", map, false)
      is_translator = get_bool("is_translator", map, false)
      profile_background_color = get_string("profile_background_color", map)
      profile_background_image_url = get_string("profile_background_image_url", map)
      profile_background_image_url_https = get_string("profile_background_image_url_https", map)
      profile_background_tile = get_bool("profile_background_tile", map, false)
      profile_image_url = get_string("profile_image_url", map)
      profile_image_url_https = get_string("profile_image_url_https", map)
      profile_banner_url = get_string("profile_banner_url", map)
      profile_link_color = get_string("profile_link_color", map)
      profile_sidebar_border_color = get_string("profile_sidebar_border_color", map)
      profile_sidebar_fill_color = get_string("profile_sidebar_fill_color", map)
      profile_text_color = get_string("profile_text_color", map)
      profile_use_background_image = get_bool("profile_use_background_image", map, false)
      show_all_inline_media = get_bool("show_all_inline_media", map, false)
      default_profile = get_bool("default_profile", map, false)
      default_profile_image = get_bool("default_profile_image", map, false)
      following = get_unknown("following", map)
      follow_request_sent = get_unknown("follow_request_sent", map)
      notifications = get_unknown("follow_request_sent", map)
    } : Twitter.full_user

  get_statuses_elt(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      message = get_string("message", map)
      metadata = get_obj("metadata", map, get_metadata)
      created_at = get_date("created_at", map)
      id  = get_int("id", map);
      id_str = get_string("id_str", map)
      text = get_string("text", map)
      source = get_string("source", map)
      truncated = get_bool("truncated", map, false)
      in_reply_to_status_id = get_int("in_reply_to_status_id", map)
      in_reply_to_status_id_str = get_string("in_reply_to_status_id_str", map)
      in_reply_to_user_id = get_int("in_reply_to_user_id", map)
      in_reply_to_user_id_str = get_string("in_reply_to_user_id_str", map)
      in_reply_to_screen_name = get_string("in_reply_to_screen_name", map)
      user = get_obj("user", map, get_user)
      geo = get_unknown("geo", map)
      coordinates = get_unknown("coordinates", map)
      place = get_unknown("place", map)
      contributors = get_unknown("contributors", map)
      retweet_count  = get_int("retweet_count", map)
      entities = get_obj("entities", map, get_entities)
      favourited = get_bool("favourited", map, false)
      retweeted = get_bool("retweeted", map, false)
    } : Twitter.statuses

  get_search_metadata(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
     completed_in = get_float("completed_in", map)
     max_id = get_int("max_id", map)
     max_id_str = get_string("max_id_str", map)
     next_results = get_string("next_results", map)
     query = get_string("query", map)
     refresh_url = get_string("refresh_url", map)
     count = get_int("count", map)
     since_id = get_int("since_id", map)
     since_id_str = get_string("since_id_str", map)
    } : Twitter.search_metadata

  _build_search_response(s) =
    json = API_libs_private.parse_json(s)
    _check_errors(json, (json ->
      map = JsonOpa.record_fields(json) ? Map.empty
      {
       statuses = get_list("statuses", map, get_statuses_elt)
       search_metadata = get_obj("search_metadata", map, get_search_metadata)
      } : Twitter.search_result))

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
    loc_int(name) = get_int(name, map)
    ids = JsonOpa.to_list(Map.get("ids", map) ? {List = []}:RPC.Json.json) ? []
    ids = List.map(loc_to_int, ids)
    { previous_cursor = loc_int("previous_cursor");
      next_cursor = loc_int("next_cursor");
      ids = ids;
    }:Twitter.social_graph

  _build_full_social_graph(rawgraph) =
    jsgraph = Json.of_string(rawgraph) |> Option.get
    map = JsonOpa.record_fields(jsgraph) ? Map.empty
    loc_int(name) = get_int(name, map)
    users = JsonOpa.to_list(Map.get("users", map) ? {List = []}:RPC.Json.json) ? []
    users = List.map(_build_tweet_from_json_2_int, users)
    { previous_cursor = loc_int("previous_cursor");
      next_cursor = loc_int("next_cursor");
      users = users;
    }:Twitter.full_social_graph

  _build_rate_limit(rawlimit) =
    jslimit = API_libs_private.parse_json(rawlimit)
    map = JsonOpa.record_fields(jslimit) ? Map.empty
    loc_int(name) = get_int(name, map)
    { remaining_hits = loc_int("remaining_hits");
      hourly_limit = loc_int("hourly_limit");
      reset_time_secs = loc_int("reset_time_in_seconds");
      reset_time = get_string("reset_time", map);
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
    custom_headers    = []
  } : OAuth.parameters)


  _wget_generic(path:string, wget_fun, parse_fun) =
    do API_libs_private.apijlog("-- Fetching {_api_host} - {path} \n data --")
    do jlog("_wget_generic: path={path}")
    (t, res) = Duration.execution_time( -> wget_fun("{_api_host}{path}"))
    do jlog("_wget_generic: got={res}")
    do API_libs_private.apijlog("Download: {Duration.in_seconds(t)} seconds")
    (t, res) = Duration.execution_time( -> parse_fun(res))
    do jlog("_wget_generic: parsed={res}")
    do API_libs_private.apijlog("Parsing:  {Duration.in_seconds(t)} seconds")
    res

  _get_res(path, params, credentials:Twitter.credentials, parse_fun) =
    f(uri) =
      twOAuth({GET}).get_protected_resource(uri,params,
                        credentials.access_token,credentials.access_secret);
    _wget_generic(path, f, parse_fun)

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

  _get_trends_place(params, credentials) : Twitter.outcome(list(Twitter.trends)) =
    path = "/1.1/trends/place.json"
    _get_res(path, params, credentials, TwitParse._build_trend_place_response)

  _get_trends_locations(trends_type, params, credentials) : Twitter.outcome(list(Twitter.full_location)) =
    path = "/1.1/trends/{trends_type}.json"
    _get_res(path, params, credentials, TwitParse._build_trend_locations_response)

  _get_generic_timeline(path, p:Twitter.timeline_options, more, credentials) =
    params = more
      |> add_if("count", p.count, (_>0))
      |> add_if("page", p.page, (_>0))
      |> add_if("since_id", p.since_id, (_>0))
      |> add_if("max_id", p.max_id, (_>0))
    params =
      if p.include_rts then [("include_rts", "true")|params]
      else params
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
    if cond(elt)
    then List.cons((key, "{elt}"), list)
    else list

  @private add_opt(key, elt, tos) = list ->
    if Option.is_some(elt)
    then List.cons((key, tos(Option.get(elt))), list)
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
    custom_headers    = []
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
 custom_get_request(path, params, credentials, final_fun) =
   Twitter_private(c)._get_res(path, params, credentials, final_fun)

/**
 * Custom POST request (advanced)
 *
 * Allows the user to do a custom POST request on Twitter.
 * Returns the API_libs.answer resulting to final_fun applied to the
 * raw result from Twitter as a string.
 *
 * @param path Path corrsponding to request built according to the Twitter's API
 * @param params (optional) Parameters as a (key, value) list if required
 * @param credentials (optional) Credential if required
 * @param final_fun A API_libs.answer_fun object containing a function taking a string and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
 custom_post_request(path, params, credentials, final_fun) =
   Twitter_private(c)._post_res(path, params, credentials, final_fun)

  default_search = {
    geocode          = ""
    lang             = ""
    locale           = ""
    result_type      = none
    count            = 0
    until            = none
    since_id         = 0
    max_id           = 0
    include_entities = none
    callback         = ""
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
  @private string_of_result_type(rt) =
    match rt with
    | {mixed} -> "mixed"
    | {recent} -> "recent"
    | {popular} -> "popular"
  @private yyyymmdd_of_date =
    printer = Date.generate_printer("%F")
    d -> Date.to_formatted_string(printer,d)
  search(request, options:Twitter.search_options, credentials) =
    path = "/1.1/search/tweets.json"
    do jlog("yyyymmdd={yyyymmdd_of_date(Date.now())}")
    params =
      [("q", request)]
      |> add_if("geocode", options.geocode, (_!=""))
      |> add_if("lang", options.lang, (_!=""))
      |> add_if("locale", options.locale, (_!=""))
      |> add_opt("result_type", options.result_type, string_of_result_type)
      |> add_if("count", options.count, (_!=0))
      |> add_opt("until", options.until, yyyymmdd_of_date)
      |> add_if("since_id", options.since_id, (_!=0))
      |> add_if("max_id", options.max_id, (_!=0))
      |> add_opt("include_entities", options.include_entities, Bool.to_string)
      |> add_if("callback", options.callback, (_!=""))
    Twitter_private(c)._get_res(path, params, credentials, TwitParse._build_search_response)

/**
 * trends/place
 *
 * (The trends on Twitter are the words mostly used in messages during a given period.)
 * This function returns trends/place where id is a WOEID.
 * Returns a Twitter.trends outcome.
 *
 * @param id The place WOEID (eg. France = 23424819)
 * @param exclude Indicates whether or not hashtags (#abcd) should be excluded from the trends result
 * @param credentials Valid access credentials
 */
  get_trends_place(id:int, exclude:bool, credentials) =
    params =
      [("id",Int.to_string(id))]
      |> add_if("exclude", "hashtags", (_ -> exclude))
    Twitter_private(c)._get_trends_place(params, credentials)

/**
 * trends/available
 *
 * (The trends on Twitter are the words mostly used in messages during a given period.)
 * This function returns a list of locations for which trends are available.
 * Returns an outcome of a list of Twitter.full_location objects.
 *
 * @param credentials Valid access credentials
 */
  get_trends_available(credentials) =
    Twitter_private(c)._get_trends_locations("available", [], credentials)

/**
 * trends/closest
 *
 * (The trends on Twitter are the words mostly used in messages during a given period.)
 * This function returns a list of locations which are closest to the given latitude and longitude.
 * Returns an outcome of a list of Twitter.full_location objects.
 *
 * @param lat Optional latitude value
 * @param long Optional longitude value
 * @param credentials Valid access credentials
 */
  get_trends_closest(lat:option(float), long:option(float), credentials) =
    params = [] |> add_opt("lat",lat,Float.to_string) |> add_opt("long",long,Float.to_string)
    Twitter_private(c)._get_trends_locations("closest", params, credentials)

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
  get_public_timeline(credentials) =
    path = "/1/statuses/public_timeline.json"
    Twitter_private(c)._get_res(path, [], credentials, TwitParse._build_timeline_response)

  default_timeline = {
    count    = 0
    page     = 0
    since_id = 0
    max_id   = 0
    include_rts = true
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
    Twitter_private(c)._get_res(path, [], credentials, TwitParse._build_tweet_from_json_2)

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
