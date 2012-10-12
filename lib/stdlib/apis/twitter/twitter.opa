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
  since_id         : string
  max_id           : string
  include_entities : option(bool)
  callback         : string
}

/**
 * Options of a timeline request
 *
 * - [count] The number of statuses to return. Default is 20, may not be greater than 200
 * - [since_id] Allows to get statuses with id greater than this id
 * - [max_id] Allows to get statuses with id smaller than this id
 * - [trim_user] When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID.
 * - [exclude_replies] This parameter will prevent replies from appearing in the returned timeline. (user_timeline and home_timeline only)
 * - [contributor_details] This parameter enhances the contributors element of the status response to include the screen_name of the contributor.
 * - [include_entities] The entities node will be disincluded when set to false.
 * - [include_rts] When set to false, the timeline will strip any native retweets. (user_timeline only)
 */
type Twitter.timeline_options = {
  count               : int
  since_id            : string
  max_id              : string
  trim_user           : option(bool)
  exclude_replies     : option(bool)
  contributor_details : option(bool)
  include_entities    : option(bool)
  include_rts         : option(bool)
}

/**
 * Options for a general statuses resource request.
 *
 * - [trim_user] When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID.
 * - [include_my_retweet] When set to either true, t or 1, any Tweets returned that have been retweeted by the authenticating user will include an additional current_user_retweet node, containing the ID of the source status for the retweet.
 * - [include_entities] The entities node will be disincluded when set to false.
 */
type Twitter.statuses_options = {
  count              : int
  trim_user          : option(bool)
  include_my_retweet : option(bool)
  include_entities   : option(bool)
}

/**
 * Options for statuses/update.
 */
type Twitter.update_options = {
  in_reply_to_status_id : string        /** The ID of an existing status that the update is in reply to. */
  lat                   : option(float) /** The latitude of the location this tweet refers to. */
  long                  : option(float) /** The longitude of the location this tweet refers to. */
  place_id              : string        /** A place in the world. These IDs can be retrieved from GET geo/reverse_geocode. */
  display_coordinates   : option(bool)  /** Whether or not to put a pin on the exact coordinates a tweet has been sent from. */
  trim_user             : option(bool)  /** When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID. */
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

type Twitter.place = {
  name         : string
  country_code : string
  country      : string
  attributes   : RPC.Json.json
  url          : string
  id           : string
  bounding_box : Twitter.coordinates
  full_name    : string
  place_type   : string
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
  user_id     : string; /** The user id of the user. */
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
  user_mentions : list(Twitter.user_mention)
}

type Twitter.full_user = {
  contributors_enabled               : bool /** Indicates that the user has an account with "contributor mode" enabled, allowing for Tweets issued by the user to be co-authored by another account. */
  created_at                         : string /** The UTC datetime that the user account was created on Twitter. */
  default_profile                    : bool /** When true, indicates that the user has not altered the theme or background of their user profile. */
  default_profile_image              : bool /** When true, indicates that the user has not uploaded their own avatar and a default egg avatar is used instead. */
  description                        : string /** Nullable. The user-defined UTF-8 string describing their account. */
  entities                           : Twitter.entities /** Entities which have been parsed out of the url or description fields defined by the user. */
  favourites_count                   : int /** The number of tweets this user has favorited in the account's lifetime. */
  follow_request_sent                : bool /** Nullable. Perspectival. When true, indicates that the authenticating user has issued a follow request to this protected user account. */
//following                          : bool /** Nullable. Perspectival. Deprecated. When true, indicates that the authenticating user is following this user. */
  followers_count                    : int /** The number of followers this account currently has. */
  friends_count                      : int /** The number of users this account is following (AKA their "followings").  */
  geo_enabled                        : bool /** When true, indicates that the user has enabled the possibility of geotagging their Tweets. */
  id                                 : string /** The string representation of the unique identifier for this Tweet. */
  is_translator                      : bool /** When true, indicates that the user is a participant in Twitter's translator community. */
  lang                               : string /** The ISO 639-1 two-letter character code for the user's self-declared user interface language. */
  listed_count                       : int /** The number of public lists that this user is a member of. */
  location                           : string /** Nullable. The user-defined location for this account's profile. */
  name                               : string /** The name of the user, as they've defined it. */
//notifications                      : bool /** Nullable. Deprecated. Indicates whether the authenticated user has chosen to receive this user's tweets by SMS. */
  profile_background_color           : string /** The hexadecimal color chosen by the user for their background. */
  profile_background_image_url       : string /** A HTTP-based URL pointing to the background image the user has uploaded for their profile. */
  profile_background_image_url_https : string /** A HTTPS-based URL pointing to the background image the user has uploaded for their profile. */
  profile_background_tile            : bool /** When true, indicates that the user's profile_background_image_url should be tiled when displayed. */
  profile_banner_url                 : string /** The HTTPS-based URL pointing to the standard web representation of the user's uploaded profile banner. */
  profile_image_url                  : string /** A HTTP-based URL pointing to the user's avatar image. */
  profile_image_url_https            : string /** A HTTPS-based URL pointing to the user's avatar image. */
  profile_link_color                 : string /** The hexadecimal color the user has chosen to display links with in their Twitter UI. */
  profile_sidebar_border_color       : string /** The hexadecimal color the user has chosen to display sidebar borders with in their Twitter UI. */
  profile_sidebar_fill_color         : string /** The hexadecimal color the user has chosen to display sidebar backgrounds with in their Twitter UI. */
  profile_text_color                 : string /** The hexadecimal color the user has chosen to display text with in their Twitter UI. */
  profile_use_background_image       : bool /** When true, indicates the user wants their uploaded background image to be used. */
  protected                          : bool /** When true, indicates that this user has chosen to protect their Tweets. */
  screen_name                        : string /** The screen name, handle, or alias that this user identifies themselves with. */
  show_all_inline_media              : bool /** Indicates that the user would like to see media inline. Somewhat disused. */
  status                             : RPC.Json.json /** Nullable. If possible, the user's most recent tweet or retweet. */
  statuses_count                     : int /** The number of tweets (including retweets) issued by the user. */
  time_zone                          : string /** Nullable. A string describing the Time Zone this user declares themselves within. */
  url                                : string /** Nullable. A URL provided by the user in association with their profile. */
  utc_offset                         : int /** Nullable. The offset from GMT/UTC in seconds. */
  verified                           : bool /** When true, indicates that the user has a verified account. */
  withheld_in_countries              : string /** When present, indicates a textual representation of the two-letter country codes this user is withheld from. */
  withheld_scope                     : string /** When present, indicates whether the content being withheld is the "status" or a "user." */
}

type Twitter.statuses = {
  message                   : string
  metadata                  : Twitter.metadata
  created_at                : string
  id                        : string
  text                      : string
  source                    : string
  truncated                 : bool
  in_reply_to_status_id     : string
  in_reply_to_user_id       : string
  in_reply_to_screen_name   : string
  user                      : Twitter.full_user
  geo                       : Twitter.coordinates
  coordinates               : Twitter.coordinates
  place                     : RPC.Json.json
  contributors              : RPC.Json.json
  retweet_count             : int
  entities                  : Twitter.entities
  favourited                : bool
  retweeted                 : bool
}

type Twitter.search_metadata = {
  completed_in : float
  max_id       : string
  next_results : string
  query        : string
  refresh_url  : string
  count        : int
  since_id     : string
}

type Twitter.search_result = {
  statuses        : list(Twitter.statuses)
  search_metadata : Twitter.search_metadata
}

type Twitter.user_mention = {
  name        : string
  screen_name : string
  indices     : list(int)
  id          : string
}

type Twitter.coordinates = { // TODO: representation of geoJSON objects
  coordinates : RPC.Json.json
  `type`      : string
}

type Twitter.id = {
  id     : string
}

type Twitter.idsn = {
  id          : string
  screen_name : string
}

type Twitter.new_tweet = {
//annotations               : unknown /** Unused. Future/beta home for status annotations. */
  contributors              : list(Twitter.idsn) /** Nullable. An collection of brief user objects (usually only one) indicating users who contributed to the authorship of the tweet, on behalf of the official tweet author. */
  coordinates               : Twitter.coordinates /** Nullable. Represents the geographic location of this Tweet as reported by the user or client application. */
  created_at                : string /** UTC time when this Tweet was created. */
  current_user_retweet      : Twitter.id /** Perspectival. Only surfaces on methods supporting the include_my_retweet parameter, when set to true. */
  entities                  : Twitter.entities /** Entities which have been parsed out of the text of the Tweet. */
  favorited                 : bool /** Nullable. Perspectival. Indicates whether this Tweet has been favorited by the authenticating user. */
//geo                       : Twitter.coordinates /** Deprecated. Nullable. Use the "coordinates" field instead. */
  id                        : string /** The integer representation of the unique identifier for this Tweet. (64-bit int) */
  in_reply_to_screen_name   : string /** Nullable. If the represented Tweet is a reply, this field will contain the screen name of the original Tweet's author. */
  in_reply_to_status_id     : string /** Nullable. If the represented Tweet is a reply, this field will contain the integer representation of the original Tweet's ID. */
  in_reply_to_user_id       : string /** Nullable. If the represented Tweet is a reply, this field will contain the integer representation of the original Tweet's author ID. */
  place                     : Twitter.place /** Nullable. When present, indicates that the tweet is associated (but not necessarily originating from) a Place. */
  possibly_sensitive        : bool /** Nullable. This field only surfaces when a tweet contains a link. */
  scopes                    : RPC.Json.json /** A set of key-value pairs indicating the intended contextual delivery of the containing Tweet. */
  retweet_count             : int /** Number of times this Tweet has been retweeted. */
  retweeted                 : bool /** Perspectival. Indicates whether this Tweet has been retweeted by the authenticating user. */
  source                    : string /** Utility used to post the Tweet, as an HTML-formatted string. */
  text                      : string /** The actual UTF-8 text of the status update. */
  truncated                 : bool /** Indicates whether the value of the text parameter was truncated, for example, as a result of a retweet exceeding the 140 character Tweet length. */
  user                      : Twitter.full_user /** The user who posted this Tweet. */
  withheld_copyright        : bool /** When present and set to "true", it indicates that this piece of content has been withheld due to a DMCA complaint. */
  withheld_in_countries     : string /** When present, indicates a textual representation of the two-letter country codes this content is withheld from. */
  withheld_scope            : string /** When present, indicates whether the content being withheld is the "status" or a "user." */
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
  id                      : string; /** The id of the message. */
  text                    : string; /** The text of the message. */
  poster                  : Twitter.user_extended; /** Extended information about the poster. */
  in_reply_to_user_id     : string; /** If the message is a reply to another message, the user id of the poster of this message. */
  in_reply_to_screen_name : string; /** If the message is a reply to another message, the screen name of the poster of this message. */
  in_reply_to_status_id   : string; /** If the message is a reply to another message, the id of this message. */
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
  ids             : list(string); /** A list of ids corresponding to the requested social graph. */
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

type Twitter.direct_messages_options = {
  since_id         : string
  max_id           : string
  count            : int
  page             : int
  include_entities : option(bool)
  skip_status      : option(bool)
}

type Twitter.message_result = {
  entities              : Twitter.entities
  sender_screen_name    : string
  id                    : string
  created_at            : string
  recipient             : Twitter.full_user
  sender                : Twitter.full_user
  recipient_screen_name : string
  recipient_id          : int
  sender_id             : int
  text                  : string
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

  get_json_int(json) = match json with | {Int=i} -> i | _ -> -1
  get_unknown(name, map) = Map.get(name, map) ? {String = "missing unknown type" }
  get_int = API_libs_private.map_get_int
  get_float = API_libs_private.map_get_float
  get_string = API_libs_private.map_get_string
  get_bool = API_libs_private.map_get_bool
  get_date(name, map) = Json.to_string(Map.get(name, map) ? {String = "Error in date" })
  get_raw_obj(json, get_elt) = get_elt(json)
  get_obj(name, map, get_elt) = get_raw_obj((Map.get(name, map)) ? {Record=[]}, get_elt)
  get_raw_list(json, get_elt) = List.map(get_elt, JsonOpa.to_list(json) ? [])
  get_list(name, map, get_elt) = get_raw_list(Map.get(name, map) ? {List=[]}, get_elt)
  get_map(name, map, get_elt) =
    tmap = JsonOpa.record_fields(Map.get(name, map) ? { Record = [] } ) ? Map.empty
    API_libs_private.remap(get_elt, tmap)

  get_error_elt(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    { code  = get_int("code", map);
      message = get_string("message", map)
    } : Twitter.error

  _check_errors(json, on_ok) =
    match json with
    | {Record=r} ->
       match List.assoc("errors",r) with
       | {some={String=message}} -> {failure={twitter=[{code=-1 ~message}]}}
       | {some=errs} -> {failure={twitter=get_raw_list(errs, get_error_elt)}}
       | {none} -> {success=on_ok(json)}
       end
   | _ -> {success=on_ok(json)}

  _build_ext_user_from_json(user_map) =
    map = JsonOpa.record_fields(user_map) ? Map.empty
    loc_int(name) = get_int(name, map)
    loc_string(name) = get_string(name, map)
    user_base = { user_id     = loc_string("id_str");
                  screen_name = loc_string("screen_name");
                  pic_url     = loc_string("profile_image_url");
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
    loc_str(name) = get_string(name, map)
    { truncated               = get_bool("truncated", map, false);
      created_at              = loc_str("created_at");
      source                  = loc_str("source");
      in_reply_to_user_id     = loc_str("in_reply_to_user_id_str");
      in_reply_to_status_id   = loc_str("in_reply_to_status_id_str");
      in_reply_to_screen_name = loc_str("in_reply_to_string_name");
      id                      = loc_str("id_str");
      text                    = loc_str("text");
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
    json = API_libs_private.parse_json(rawdata)
    _check_errors(json, _build_tweet_from_json_2_int)

  get_user_mention(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      name = get_string("name", map)
      screen_name = get_string("screen_name", map)
      indices = get_list("indices", map, get_json_int)
      id = get_string("id_str", map)
    } : Twitter.user_mention

  get_coordinates(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      coordinates = get_unknown("coordinates", map) // the type varies: list(int), list(list(list(int))) etc.
      `type`      = get_string("type", map)
    } : Twitter.coordinates

  get_place(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      name = get_string("name", map)
      country_code = get_string("country_code", map)
      country = get_string("country", map)
      attributes = get_unknown("attributes", map)
      url = get_string("url", map)
      id = get_string("id", map)
      bounding_box = get_obj("bounding_box", map, get_coordinates)
      full_name = get_string("full_name", map)
      place_type = get_string("place_type", map)
    } : Twitter.place

  get_idsn(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      id = get_string("id_str", map)
      screen_name = get_string("screen_name", map)
    } : Twitter.idsn

  get_id(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      id = get_string("id_str", map)
    } : Twitter.id

  get_new_tweet(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      contributors = get_list("contributors", map, get_idsn)
      coordinates = get_obj("coordinates", map, get_coordinates)
      created_at = get_date("created_at", map)
      current_user_retweet = get_obj("current_user_retweet", map, get_id)
      entities = get_obj("entities", map, get_entities)
      favorited = get_bool("favorited", map, false)
      id = get_string("id_str", map)
      in_reply_to_screen_name = get_string("in_reply_to_screen_name", map)
      in_reply_to_status_id = get_string("in_reply_to_status_id_str", map)
      in_reply_to_user_id = get_string("in_reply_to_user_id_str", map)
      place = get_obj("place", map, get_place)
      possibly_sensitive = get_bool("possibly_sensitive", map, false)
      scopes = get_unknown("scopes", map)
      retweet_count = get_int("retweet_count", map)
      retweeted = get_bool("retweeted", map, false)
      source = get_string("source", map)
      text = get_string("text", map)
      truncated = get_bool("truncated", map, false)
      user = get_obj("user", map, get_full_user)
      withheld_copyright = get_bool("withheld_copyright", map, false)
      withheld_in_countries = get_string("withheld_in_countries", map)
      withheld_scope = get_string("withheld_scope", map)
    } : Twitter.new_tweet

  _build_one_new_tweet(s) =
    json = API_libs_private.parse_json(s)
    _check_errors(json, get_new_tweet)

  _build_new_tweet_response(s) =
    json = API_libs_private.parse_json(s)
    _check_errors(json, get_raw_list(_, get_new_tweet))

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
      user_mentions = get_list("user_mentions", map, get_user_mention)
    } : Twitter.entities

  get_full_user(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      contributors_enabled = get_bool("contributors_enabled", map, false)
      created_at = get_date("created_at", map)
      default_profile = get_bool("default_profile", map, false)
      default_profile_image = get_bool("default_profile_image", map, false)
      description = get_string("description", map)
      entities = get_obj("entities", map, get_entities)
      favourites_count = get_int("favourites_count", map)
      follow_request_sent = get_bool("follow_request_sent", map, false)
      followers_count = get_int("followers_count", map)
      friends_count = get_int("friends_count", map)
      geo_enabled = get_bool("get_bool", map, false)
      id = get_string("id_str", map)
      is_translator = get_bool("is_translator", map, false)
      lang = get_string("lang", map)
      listed_count = get_int("listed_count", map)
      location = get_string("location", map)
      name = get_string("name", map)
      profile_background_color = get_string("profile_background_color", map)
      profile_background_image_url = get_string("profile_background_image_url", map)
      profile_background_image_url_https = get_string("profile_background_image_url_https", map)
      profile_background_tile = get_bool("profile_background_tile", map, false)
      profile_banner_url = get_string("profile_banner_url", map)
      profile_image_url = get_string("profile_image_url", map)
      profile_image_url_https = get_string("profile_image_url_https", map)
      profile_link_color = get_string("profile_link_color", map)
      profile_sidebar_border_color = get_string("profile_sidebar_border_color", map)
      profile_sidebar_fill_color = get_string("profile_sidebar_fill_color", map)
      profile_text_color = get_string("profile_text_color", map)
      profile_use_background_image = get_bool("profile_use_background_image", map, false)
      protected = get_bool("protected", map, false)
      screen_name = get_string("screen_name", map)
      show_all_inline_media = get_bool("show_all_inline_media", map, false)
      status = get_unknown("status", map)
      statuses_count = get_int("statuses_count", map)
      time_zone = get_string("time_zone", map)
      url = get_string("url", map)
      utc_offset = get_int("utc_offset", map)
      verified = get_bool("verified", map, false)
      withheld_in_countries = get_string("withheld_in_countries", map)
      withheld_scope = get_string("withheld_scope", map)
    } : Twitter.full_user

  get_statuses_elt(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      message = get_string("message", map)
      metadata = get_obj("metadata", map, get_metadata)
      created_at = get_date("created_at", map)
      id = get_string("id_str", map)
      text = get_string("text", map)
      source = get_string("source", map)
      truncated = get_bool("truncated", map, false)
      in_reply_to_status_id = get_string("in_reply_to_status_id_str", map)
      in_reply_to_user_id = get_string("in_reply_to_user_id_str", map)
      in_reply_to_screen_name = get_string("in_reply_to_screen_name", map)
      user = get_obj("user", map, get_full_user)
      geo = get_obj("geo", map, get_coordinates)
      coordinates = get_obj("coordinates", map, get_coordinates)
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
     max_id = get_string("max_id_str", map)
     next_results = get_string("next_results", map)
     query = get_string("query", map)
     refresh_url = get_string("refresh_url", map)
     count = get_int("count", map)
     since_id = get_string("since_id_str", map)
    } : Twitter.search_metadata

  _build_search_response(s) =
    json = API_libs_private.parse_json(s)
    _check_errors(json, (json ->
      map = JsonOpa.record_fields(json) ? Map.empty
      {
       statuses = get_list("statuses", map, get_statuses_elt)
       search_metadata = get_obj("search_metadata", map, get_search_metadata)
      } : Twitter.search_result))

  _build_social_graph(rawgraph) = // *Must* use stringify_ids
    //loc_to_int(x) = API_libs_private.json_to_int_unsafe(x) ? 0
    loc_to_str(x) = API_libs_private.json_to_string_unsafe(x) ? ""
    jsgraph = Json.of_string(rawgraph) |> Option.get
    map = JsonOpa.record_fields(jsgraph) ? Map.empty
    loc_int(name) = get_int(name, map)
    ids = JsonOpa.to_list(Map.get("ids", map) ? {List = []}:RPC.Json.json) ? []
    ids = List.map(loc_to_str, ids)
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

  get_message_result(json) =
    map = JsonOpa.record_fields(json) ? Map.empty
    {
      entities = get_obj("entities", map, get_entities)
      sender_screen_name = get_string("sender_screen_name", map)
      id = get_string("id_str", map)
      created_at = get_string("created_at", map)
      recipient = get_obj("recipient", map, get_full_user)
      sender = get_obj("sender", map, get_full_user)
      recipient_screen_name = get_string("recipient_screen_name", map)
      recipient_id = get_int("recipient_id", map)
      sender_id = get_int("sender_id", map)
      text = get_string("text", map)
    }

  _build_message_result(s) =
    json = API_libs_private.parse_json(s)
    _check_errors(json, get_message_result)

  _build_message_results(s) =
    json = API_libs_private.parse_json(s)
    _check_errors(json, get_raw_list(_, get_message_result))

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

  add_opt(key, elt, tos) = list ->
    if Option.is_some(elt)
    then List.cons((key, tos(Option.get(elt))), list)
    else list
  add_sopt(key, elt) = add_opt(key, elt, (s->s))
  add_iopt(key, elt) = add_opt(key, elt, Int.to_string)
  add_bopt(key, elt) = add_opt(key, elt, Bool.to_string)
  add_fopt(key, elt) = add_opt(key, elt, Float.to_string)

  _get_trends_place(params, credentials) : Twitter.outcome(list(Twitter.trends)) =
    path = "/1.1/trends/place.json"
    _get_res(path, params, credentials, TwitParse._build_trend_place_response)

  _get_trends_locations(trends_type, params, credentials) : Twitter.outcome(list(Twitter.full_location)) =
    path = "/1.1/trends/{trends_type}.json"
    _get_res(path, params, credentials, TwitParse._build_trend_locations_response)

  _get_generic_timeline(path, p:Twitter.timeline_options, more, credentials) =
    params = more
      |> add_if("count", p.count, (_>0))
      |> add_if("since_id", p.since_id, (_!=""))
      |> add_if("max_id", p.max_id, (_!=""))
      |> add_bopt("trim_user", p.trim_user)
      |> add_bopt("exclude_replies", p.exclude_replies)
      |> add_bopt("contributor_details", p.contributor_details)
      |> add_bopt("include_entities", p.include_entities)
      |> add_bopt("include_rts", p.include_rts)
    _get_res(path, params, credentials, TwitParse._build_new_tweet_response)

  _get_generic_socgraph(name, id, cursor, graphtype:string, credentials) =
    path = "/1/{graphtype}/ids.json"
    params = [ (if (name == "" && id != "") then ("user_id","{id}") else ("screen_name",name))
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
  @private add_sopt(key, elt) = add_opt(key, elt, (s->s))
  @private add_iopt(key, elt) = add_opt(key, elt, Int.to_string)
  @private add_bopt(key, elt) = add_opt(key, elt, Bool.to_string)
  @private add_fopt(key, elt) = add_opt(key, elt, Float.to_string)

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
 * @param credentials User credentials.
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
 * @param credentials User credentials.
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
    since_id         = ""
    max_id           = ""
    include_entities = none
    callback         = ""
  } : Twitter.search_options

/**
 * Search request : http://search.twitter.com/search.json[?q=<search request>&params]
 *
 * Makes a Twitter search of given request.
 * Returns a Twitter.outcome of a Twitter.search_result object
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
  search(request, options:Twitter.search_options, credentials) : Twitter.outcome(Twitter.search_result) =
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
      |> add_if("since_id", options.since_id, (_!=""))
      |> add_if("max_id", options.max_id, (_!=""))
      |> add_bopt("include_entities", options.include_entities)
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
  get_trends_place(id:string, exclude:bool, credentials) =
    params =
      [("id",id)]
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
    params =
      []
      |> add_fopt("lat",lat)
      |> add_fopt("long",long)
    Twitter_private(c)._get_trends_locations("closest", params, credentials)

  default_timeline = {
    count    = 0
    since_id = ""
    max_id   = ""
    trim_user = none
    exclude_replies = none
    contributor_details = none
    include_entities = none
    include_rts = none
  } : Twitter.timeline_options

/**
 * Mentions timeline
 *
 * Returns the 20 most recent mentions (tweets containing a users's @screen_name) for the authenticating user.
 * Returns a Twitter.outcome of a list of Twitter.tweet objects.
 *
 */
  get_mentions_timeline(params, credentials) =
    path = "/1.1/statuses/mentions_timeline.json"
    Twitter_private(c)._get_generic_timeline(path, params, [], credentials)

/**
 * Home timeline
 *
 * Returns the most recent status updates of current user and of his friends.
 * Will display the same tweets than those displayed in the user's homepage.
 * Returns a list of Twitter.tweet objects.
 *
 * @param params Options of the request [Twitter.timeline_options]
 * @param credentials The user credentials
 */
  get_home_timeline(params, credentials) =
    path = "/1.1/statuses/home_timeline.json"
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
 * @param options Common options of the request [Twitter.timeline_options]
 * @param credentials The user credentials
 */
  get_user_timeline(user:Twitter.user, options, credentials) =
    path = "/1.1/statuses/user_timeline.json"
    more = match user with
      | {~uid}         -> [("uid","{uid}")]
      | {~id}          -> [("id",id)]
      | {~screen_name} -> [("screen_name",screen_name)]
    Twitter_private(c)._get_generic_timeline(path, options, more, credentials)

  default_statuses = {
    count = 0
    trim_user = none
    include_my_retweet = none
    include_entities = none
  } : Twitter.statuses_options

/**
 * Get specific message
 *
 * This function returns the message having the specified id.
 * Returns a Twitter.tweet object.
 *
 * @param id The id of requested message.
 * @param credentials The user credentials
 */
  get_specific_message(id:string, options:Twitter.statuses_options, credentials) =
    path = "/1.1/statuses/show.json"
    params =
      [("id",id)]
      |> add_bopt("trim_user", options.trim_user)
      |> add_bopt("include_my_retweet", options.include_my_retweet)
      |> add_bopt("include_entities", options.include_entities)
    Twitter_private(c)._get_res(path, params, credentials, TwitParse._build_one_new_tweet)

/**
 * Get retweets of a message
 *
 * Returns up to 100 of the first retweets of a given tweet.
 * Returns a Twitter.retweet object.
 *
 * @param id The id of requested message.
 * @param options The command options (count, trim_user).
 * @param credentials The user credentials.
 * @returns List of retweets.
 */
  get_message_retweets(id:string, options:Twitter.statuses_options, credentials) =
    path = "/1.1/statuses/retweets/{id}.json"
    params =
      [("id",id)]
      |> add_if("count", options.count, (_>0))
      |> add_bopt("trim_user", options.trim_user)
    Twitter_private(c)._get_res(path, params, credentials, TwitParse._build_new_tweet_response)

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
 * @param credentials The credentials of the user formatted as login:password
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
 * @param credentials The credentials of the user formatted as login:password
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
 * @param credentials The credentials of the user formatted as login:password
 * @param final_fun A API_libs.answer_fun object containing a function taking a Twitter.rate_limit and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  get_limit(credentials) =
    Twitter_private(c)._get_res("/1/account/rate_limit_status.json", [], credentials, TwitParse._build_rate_limit)

  default_update = {
    in_reply_to_status_id = ""
    lat = none
    long = none
    place_id = ""
    display_coordinates = none
    trim_user = none
  } : Twitter.update_options

/**
 * Post status
 *
 * This function updates the status of the authenticated user.
 * Returns a Twitter.tweet object corresponding to the created status.
 *
 * @param message The text of the status update.
 * @param options Optional arguments for the update.
 * @param credentials The user credentials.
 */
  post_status(message, options:Twitter.update_options, credentials) =
    path = "/1.1/statuses/update.json"
    params =
      [("status",message)]
      |> add_if("in_reply_to_status_id", options.in_reply_to_status_id, (_!=""))
      |> add_fopt("lat", options.lat)
      |> add_fopt("long", options.long)
      |> add_if("place_id", options.place_id, (_!=""))
      |> add_bopt("display_coordinates", options.display_coordinates)
      |> add_bopt("trim_user", options.trim_user)
    Twitter_private(c)._post_res(path, params, credentials, TwitParse._build_one_new_tweet)

/**
 * Post retweet status
 *
 * Retweets a tweet.
 * Returns the original tweet with retweet details embedded.
 *
 * @param id The numerical ID of the desired status.
 * @param trim_user When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID.
 * @param credentials The user credentials.
 */
  retweet_status(id:string, trim_user:option(bool), credentials) =
    path = "/1.1/statuses/retweet/{id}.json"
    params =
      []
      |> add_bopt("trim_user", trim_user)
    Twitter_private(c)._post_res(path, params, credentials, TwitParse._build_one_new_tweet)

/**
 * Delete status
 *
 * This function deletes the status with given if posted by the authenticated user.
 * Returns a Twitter.tweet object corresponding to the deleted status.
 *
 * @param id The ID of the status to delete.
 * @param trim_user When set to either true, t or 1, each tweet returned in a timeline will include a user object including only the status authors numerical ID.
 * @param credentials The user credentials.
 */
  delete_status(id:string, trim_user:option(bool), credentials) =
    path = "/1.1/statuses/destroy/{id}.json"
    params =
      []
      |> add_bopt("trim_user", trim_user)
    Twitter_private(c)._post_res(path, params, credentials, TwitParse._build_one_new_tweet)

// TODO: (unlikely) streaming API

/**
 * Check credentials
 *
 * This function checks if provided credentials are correct.
 * Returns a Twitter.tweet object corresponding to the last status of the user if the credentials are correct. If incorrect, returns an empty Twitter.tweet object.
 *
 * @param include_entities The entities node will not be included when set to false.
 * @param skip_status When set to either true, t or 1 statuses will not be included in the returned user objects.
 * @param credentials The user credentials.
 */
  check_credentials(include_entities:option(bool), skip_status:option(bool), credentials) =
    path = "/1.1/account/verify_credentials.json"
    params =
      []
      |> add_bopt("include_entities", include_entities)
      |> add_bopt("skip_status", skip_status)
    Twitter_private(c)._get_res(path, params, credentials, TwitParse._build_one_new_tweet)

  default_direct_messages = {
    since_id = ""
    max_id = ""
    count = 20
    page = 0
    include_entities = none
    skip_status = none
  } : Twitter.direct_messages_options

/**
 * Get a single direct message
 *
 * @param id The ID of the direct message.
 */
  direct_message(id:string, credentials) =
    path = "/1.1/direct_messages/show.json"
    params = [("id",id)]
    Twitter_private(c)._get_res(path, params, credentials, TwitParse._build_message_result)

/**
 * Get the most recent direct messsages.
 *
 * This function returns the messages received by the authenticated user.
 *
 * @param options The command options.
 */
  direct_messages(options:Twitter.direct_messages_options, credentials) =
    path = "/1.1/direct_messages.json"
    params =
      []
      |> add_if("since_id", options.since_id, (_!=""))
      |> add_if("max_id", options.max_id, (_!=""))
      |> add_if("count", options.count, (_>0))
      |> add_if("page", options.page, (_>0))
      |> add_bopt("include_entities", options.include_entities)
      |> add_bopt("skip_status", options.skip_status)
    Twitter_private(c)._get_res(path, params, credentials, TwitParse._build_message_results)

/**
 * Get the most recent direct messsages sent.
 *
 * This function returns the messages sent by the authenticated user.
 *
 * @param options The command options.
 */
  sent_direct_messages(options:Twitter.direct_messages_options, credentials) =
    path = "/1.1/direct_messages/sent.json"
    params =
      []
      |> add_if("since_id", options.since_id, (_!=""))
      |> add_if("max_id", options.max_id, (_!=""))
      |> add_if("count", options.count, (_>0))
      |> add_if("page", options.page, (_>0))
      |> add_bopt("include_entities", options.include_entities)
      |> add_bopt("skip_status", options.skip_status)
    Twitter_private(c)._get_res(path, params, credentials, TwitParse._build_message_results)

/**
 * Send a direct message
 *
 * This function sends a direct message to a user from the authenticated user.
 *
 * @param user The recipient of the message.
 * @param text The text of your direct message. Keep the message under 140 characters.
 */
  send_direct_message(user:Twitter.user, message, credentials) =
    path = "/1.1/direct_messages/new.json"
    params =
      (match user with
       | ~{uid} -> [("user_id", "{uid}")]
       | ~{screen_name} -> [("screen_name", screen_name)]
       | ~{id} -> [("user", id)])
      |> List.cons(("text",message),_)
    Twitter_private(c)._post_res(path, params, credentials, TwitParse._build_message_result)

/**
 * Delete a direct message
 *
 * This function deletes the direct message with the given ID.
 * Returns the deleted message, if successful.
 *
 * @param id The ID of the message.
 * @param include_entities The entities node will not be included when set to false.
 */
  delete_direct_message(id:string, include_entities:option(bool), credentials) =
    path = "/1.1/direct_messages/destroy.json"
    params =
      [("id",id)]
      |> add_bopt("include_entities", include_entities)
    Twitter_private(c)._post_res(path, params, credentials, TwitParse._build_message_result)

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
