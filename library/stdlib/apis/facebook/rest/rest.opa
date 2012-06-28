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
 * Facebook modules
 *
 * This file provides most methods of Facebook API
 * Note: Don't look for upload functions (photos.upload and video.upload)
 * since they are not here yet ...
 *
 * Important :
 *   Any application using this must respect facebook policy, available here:
 *   http://developers.facebook.com/policy/
 *
 * @category web
 * @author Nicolas Glondu, 2010
 * @destination
 * - private: Facebook_definitions_private, Facebook_private
 * - public: Facebook
 * @stability Work in progress
 */

import stdlib.apis.{common, facebook, facebook.lib}
import stdlib.crypto

/**
 * {1 Types defined in this module}
 */

/**
 * The type of a Facebook session returned by the connection parser
 */
type FbRest.session = {
  session_key :string; /** The session key, representing the session */
  uid         :int; /** The id of the user of the session */
  expires     :int; /** The end of the session */
  secret      :string /** Secret assiciated to the session, used for desktop applications */
}

/**
 * Facebook method
 */
type FbRest.method = {
  req_params :list(string); /** List of required parameters */
  opt_params :list(string) /** List of optional parameters */
}

/**
 * Facebook link
 */
type FbRest.link = {
  text :string; /** text of the link */
  href :string /** url of the link */
}

/**
 * Facebook stream types
 *
 * Useful for the attachment building function made to make
 * attachment building simpler.
 */
type FbRest.stream_ppty =
    { text :string  }
  / { link :FbRest.link }

type FbRest.stream_pic = {
  src  :string;
  href :string
}

type FbRest.stream_flash = {
  swfsrc  :string; /** Url where the swf is located */
  imgsrc  :string; /** Url of a picture representing the flash. This picture will be displayed in the stream and the flash object will be activated only if the user click it. */
  width   :option(string); /** (optional) Width of the picture and of the flash element if no xwidth. */
  height  :option(string); /** (optional) Height of the picture and of the flash element if no xheight. */
  xwidth  :option(string); /** (optional) Width of the flash element once activated. */
  xheight :option(string)  /** (optional) Height of the flash element once activated. */
}

type FbRest.stream_mp3 = {
  src    :string; /** Url where the mp3 is located */
  title  :option(string); /** (optionnal) Title of the mp3. */
  artist :option(string); /** (optionnal) Artist of the mp3. */
  album  :option(string)  /** (optionnal) Album of the mp3. */
}

type FbRest.stream_med =
    { pics  :list(FbRest.stream_pic) } /** Limited to 5 elements. */
  / { flash :FbRest.stream_flash     }
  / { mp3   :FbRest.stream_mp3       }
  / { none }

/**
 * Facebook error
 *
 * Returned by the error parser
 */
type FbRest.error = {
  error_code   :int;
  error_msg    :string;
  request_args :stringmap(string)
}

/**
 * Facebook event types
 *
 * Used by the event building function made to make
 * attachment building simpler.
 */

type FbRest.event_privacy =
    { OPEN }
  / { CLOSED }
  / { SECRET }

type FbRest.event_opt_param =
    { category :int } /** Categories are between 1 and 8 (included). See the documentation of Facebook.build_event for the meaning of each value. */
  / { subcategory :int } /** Subcategories are between 1 and 58 (included). See the documentation of FbRest.build_event for the meaning of each value. */
  / { location :string }
  / { end_time :int }
  / { street :string }
  / { city :string }
  / { phone :string }
  / { email :string }
  / { page_id :int }
  / { description :string }
  / { tagline :string }
  / { host :string }
  / { privacy_type :FbRest.event_privacy }

/**
 * @author Nicolas Glondu, March 2010
 * @category Facebook ?
 * @category External API ?
 * @destination private
 */

/**
 * {1 About this module}
 *
 * This module contains the list of all methods implemented methods of
 * the FbRest module. One FbRest.method here should be associated
 * to at least one function in the FbRest user module. More than one
 * function is used for functions with a lot of optional parameters.
 *
 * Facebook developpers website http://wiki.developers.facebook.com/index.php/API
 * is the best documentation for each method.
 *
 * {1 Where should I start?}
 *
 * Quite simple :
 * - req_params are the required parameters
 * - opt_params are optional parameters
 *
 * _aux_implemented make the transformation
 * list((name, method)) -> Map(name, method)
 *
 * {1 What if I need more?}
 *
 * To add a method, just add it here and it becomes accessible via
 * FbRest.generic_call(method, params, api_key, secret, final_fun)
 * which is an alias to FbRest_private.generic_call explained later
 *
 * You may also create a specific call in the FbRest module to
 * to make the use of the method more user-friendly
 *
 * IMPORTANT NOTE : 'api_key', 'sig', 'call_id' and 'format' are
 * automatically added to all functions
 */

FbRest_definitions_private = {{
  auto_params() =
    [ ("v", "1.0")
    , ("call_id", "{Date.in_milliseconds(Date.now())}")
    , ("format", "JSON")
    ]

  _aux_implemented(elt, acc) = StringMap.add(elt.f1, elt.f2, acc)
  implemented_methods = List.fold(_aux_implemented,
    [ ("admin.banUsers", /*-> boolean */
       { req_params = ["uids"];
         opt_params = [];
       } :FbRest.method)
    , ("admin.getAllocation",
       { req_params = ["integration_point_name"];
       /* notifications_per_day, announcement_notifications_per_week, requests_per_day,
          emails_per_day or email_disable_message_location */
         opt_params = ["user"]; /* ONLY with emails_per_day */
       } :FbRest.method)
    , ("admin.getAppProperties",
       { req_params = ["properties"];
         opt_params = [];
       } :FbRest.method)
    , ("admin.getBannedUsers", /*-> list(int) */
       { req_params = [];
         opt_params = ["uids"]; /* uids acts as a filter */
       } :FbRest.method)
    , ("admin.getMetrics",
       { req_params = ["start_time", "end_time", "period", "metrics"];
       /* supported periods are 86400 (1 day), 604800 (7-days), and 2592000 (30 days) */
         opt_params = [];
       } :FbRest.method)
    , ("admin.getRestrictionInfo",
       { req_params = [];
         opt_params = [];
       } :FbRest.method)
    , ("application.getPublicInfo",
       { req_params = [];
         opt_params = ["application_id", "application_api_key", "application_canvas_name"]; /* MUST have ONE (and only one) of those */
       } :FbRest.method)
    , ("admin.unbanUsers", /*-> boolean */
       { req_params = ["uids"];
         opt_params = [];
       } :FbRest.method)
    , ("admin.setAppProperties", /*-> boolean */
       { req_params = ["properties"];
         opt_params = [];
       } :FbRest.method)
    , ("admin.setRestrictionInfo", /*-> boolean*/
       { req_params = [];
         opt_params = ["restriction_str"];
       } :FbRest.method)
    , ("batch.run",
       { req_params = ["method_feed"];
         opt_params = ["session_key", "serial_only"];
       } :FbRest.method)
    , ("friends.areFriends", /*-> list(stringmap(string)) */
       { req_params = ["session_key", "uids1", "uids2"];
         opt_params = [];
       } :FbRest.method)
    , ("friends.get", /*-> list(int)*/
       { req_params = [];
         opt_params = ["session_key", "flid", "uid"];
         /* flid: ID of the friend list from which the
         friends should be taken  */
       } :FbRest.method)
    , ("friends.getAppUsers", /*-> list(int)*/
       { req_params = ["session_key"];
         opt_params = [];
       } :FbRest.method)
    , ("friends.getLists", /*-> list(int)*/
       { req_params = ["session_key"];
         opt_params = [];
       } :FbRest.method)
    , ("friends.getMutualFriends", /*-> list(int)*/
       { req_params = ["target_uid"];
         opt_params = ["session_key", "source_uid"]; /* MUST have ONE of those */
       } :FbRest.method)
    , ("fql.query", /*-> list(stringmap(string)) */
       { req_params = ["query"];
         opt_params = ["session_key"];
       } :FbRest.method)
    , ("fql.multiquery", /*-> list(stringmap(string)) */
       { req_params = ["queries"];
         opt_params = ["session_key"];
         /* see: http://wiki.developers.facebook.com/index.php/Fql.multiquery
         for more info on multiquery */
       } :FbRest.method)
    , ("notifications.markRead",
        { req_params = ["session_key", "notification_ids"];
          opt_params = [];
        } :FbRest.method)
    , ("notifications.get",
        { req_params = ["session_key"];
          opt_params = [];
        } :FbRest.method)
    , ("notifications.getList",
       { req_params = ["session_key"];
         opt_params = ["start_time", "include_read"];
       } :FbRest.method)
    , ("notifications.sendEmail",
       { req_params = ["recipients", "subject", "text"];
         opt_params = [];
       } :FbRest.method)
    , ("users.getInfo",
       { req_params = ["uids", "fields"];
         opt_params = ["session_key"];
       } :FbRest.method)
    , ("users.getLoggedInUser", /* -> uid of the user */
       { req_params = ["session_key"];
         opt_params = [];
       } :FbRest.method)
    , ("users.getStandardInfo",
       { req_params = ["uids", "fields"];
         opt_params = [];
       } :FbRest.method)
    , ("users.hasAppPermission",
       { req_params = ["ext_perm"];
         opt_params = ["uid"];
       } :FbRest.method)
    , ("users.isAppUser", /* -> bool */
       { req_params = [];
         opt_params = ["session_key","uid"]; /* Must have ONE of these */
       } :FbRest.method)
    , ("users.isVerified",
       { req_params = ["session_key"];
         opt_params = [];
       } :FbRest.method)

    /* --- Auth API Methods --- */
    , ("auth.expireSession",
       { req_params = ["session_key"];
         opt_params = [];
       } :FbRest.method)
    , ("auth.revokeAuthorization",
       { req_params = [];
         opt_params = ["session_key", "uid"];
       } :FbRest.method)
    , ("auth.revokeExtendedPermission",
       { req_params = ["perm"];
         opt_params = ["session_key", "uid"];
       } :FbRest.method)

    /* --- Pages API Methods --- */
    , ("pages.getInfo",
       { req_params = ["fields"];
         opt_params = ["page_ids", "session_key", "uid"];
       } :FbRest.method)
    , ("pages.isAdmin", /* -> bool */
       { req_params = ["session_key", "page_id"];
         opt_params = ["uid"];
       } :FbRest.method)
    , ("pages.isAppAdded", /* -> bool */
       { req_params = [];
         opt_params = ["page_id"];
       } :FbRest.method)
    , ("pages.isFan", /* -> bool */
       { req_params = ["page_id"];
         opt_params = ["session_key", "uid"]; /* Must have at least ONE of these */
       } :FbRest.method)

    /* --- Status API Methods --- */
    , ("status.get",
       { req_params = ["session_key"];
         opt_params = ["uid", "limit"];
       } :FbRest.method)
    , ("users.setStatus",
       { req_params = [];
         opt_params = ["session_key", "status", "clear", "status_includes_verb", "uid"];
       } :FbRest.method)

    /* --- Stream API Methods ---
       - attachment allows to add rich informations to a post
         It is written as a JSON-encoded object
       - actionLink is a short text that invites to react on the comment
         It is a JSON-encoded object containing the fields text and href
         (both string and less than 25 chars)
    */
    , ("stream.addComment", /*-> int (comment_id of the created comment) */
       { req_params = ["post_id", "comment"];
         opt_params = ["session_key", "uid"];
       } :FbRest.method)
    , ("stream.addLike", /*-> bool */
       { req_params = [];
         opt_params = ["session_key", "uid", "post_id"];
       } :FbRest.method)
    , ("stream.publish", /*-> string (post_id of the created stream) */
       { req_params = [];
         opt_params = ["session_key", "message", "attachment", "action_links", "target_id", "uid"];
       } :FbRest.method)
    , ("stream.get",
       { req_params = ["session_key"];
         opt_params = ["viewer_id", "source_ids", "start_time", "end_time", "limit", "filter_key", "metadata"];
       } :FbRest.method)
    , ("stream.getComments",
       { req_params = ["post_id"];
         opt_params = [];
       } :FbRest.method)
    , ("stream.getFilters",
       { req_params = [];
         opt_params = ["session_key", "uid"];
       } :FbRest.method)
    , ("stream.remove", /*-> bool */
       { req_params = ["post_id"];
         opt_params = ["session_key", "uid"];
       } :FbRest.method) /* can only remove streams published by the app */
    , ("stream.removeComment", /*-> bool */
       { req_params = ["comment_id"];
         opt_params = ["session_key", "uid"];
       } :FbRest.method)
    , ("stream.removeLike", /*-> bool */
       { req_params = [];
         opt_params = ["session_key", "uid", "post_id"];
       } :FbRest.method)

    /* --- Groups API Methods --- */
    , ("groups.get",
       { req_params = ["session_key"];
         opt_params = ["gids", "uid"];
       } :FbRest.method)
    , ("groups.getMembers",
       { req_params = ["session_key", "gid"];
         opt_params = [];
       } :FbRest.method)

    /* --- Data API Methods --- */
    , ("data.getCookies",
       { req_params = ["uid"];
         opt_params = ["name"];
       } :FbRest.method)
    , ("data.setCookie",
       { req_params = ["uid", "name", "value"];
         opt_params = ["expires", "path"];
       } :FbRest.method)

    /* --- Comments API Methods --- */
    , ("comments.add",
       { req_params = ["text"];
         opt_params = ["xid", "object_id", "title", "url", "publish_to_stream", "session_key", "uid"];
         /* MUST have either xid or object_id */
       } :FbRest.method)
    , ("comments.get",
       { req_params = ["session_key"];
         opt_params = ["xid", "object_id"]; /* MUST have either xid or object_id */
       } :FbRest.method)
    , ("comments.remove",
       { req_params = ["comment_id"];
         opt_params = ["xid", "object_id", "session_key"];
       } :FbRest.method)

    /* --- Links API Methods --- */
    , ("links.get", /*-> list(stringmap(string)) */
       { req_params = ["session_key"];
         opt_params = ["uid", "link_ids", "limit"]; /* Default limit is 50 */
       } :FbRest.method)
    , ("links.getStats", /*-> list(stringmap(string)) */
       { req_params = ["urls", "session_key"];
         opt_params = [];
       } :FbRest.method)
    , ("links.post", /*-> int (link_id of the created link) */
       { req_params = ["uid", "url", "comment"];
         opt_params = ["session_key", "image"];
         /* image param used if the image detected
         is not the right one (use links.preview to check)*/
       } :FbRest.method)
    , ("links.preview",
       { req_params = ["url"];
         opt_params = ["session_key"];
       } :FbRest.method)

    /* --- Notes API Methods --- */
    , ("notes.create", /*-> int (note_id of the created note) */
       { req_params = ["uid", "title", "content"];
         opt_params = ["session_key"];
       } :FbRest.method)
    , ("notes.delete", /*-> bool */
       { req_params = ["note_id", "title", "content"];
         opt_params = ["session_key", "uid"];
       } :FbRest.method)
    , ("notes.edit", /*-> bool */
       { req_params = ["note_id", "title", "content"];
         opt_params = ["session_key"];
       } :FbRest.method)
    , ("notes.get", /*-> list(stringmap(string)) */
       { req_params = ["session_key"];
         opt_params = ["uid"];
       } :FbRest.method)

    /* --- Events API Methods --- */
    , ("events.cancel", /*-> boolean */
       { req_params = ["eid"];
         opt_params = ["cancel_message", "session_key"];
       } :FbRest.method)
    , ("events.create", /*-> int (eid of the created event) */
       { req_params = ["event_info"];
         opt_params = ["session_key"];
       } :FbRest.method)
    , ("events.edit", /*-> boolean */
       { req_params = ["eid", "event_info"];
         opt_params = ["session_key"];
       } :FbRest.method)
    , ("events.get",
       { req_params = [];
         opt_params = ["uid", "eids", "start_time", "end_time"];
       } :FbRest.method)
    , ("events.getMembers",
       { req_params = ["eid"];
         opt_params = ["session_key"];
       } :FbRest.method)
    , ("events.invite", /*-> boolean */
       { req_params = ["eid", "uids"];
         opt_params = ["personal_message","session_key"];
       } :FbRest.method)
    , ("events.rsvp", /*-> boolean */
       { req_params = ["eid", "rsvp_status"];
         opt_params = ["session_key"];
       } :FbRest.method)

    /* --- Photos API Methods --- */
    , ("photos.addTag",
       { req_params = ["pid", "x", "y"]; /* percentage from 0 to 100 */
         opt_params = ["tag_uid", "tag_text", "session_key", "tags", "owner_uid"]; /* uid (X)OR text required */
       } :FbRest.method)
    , ("photos.upload", /* Does not work ... */
       { req_params = [];
         opt_params = ["session_key", "aid", "caption", "uid"];
       } :FbRest.method)
    , ("photos.createAlbum",
       { req_params = ["name"];
         opt_params = ["session_key", "uid", "location", "description", "visible", "privacy"];
         /* visible = friends, friends-of-friends, networks, everyone
         privacy is a JSON encoded object and should be left to the discretion of the user
         */
       } :FbRest.method)
    , ("photos.get",
       { req_params = ["session_key", "subj_id", "aid", "pids"];
         opt_params = [];
       } :FbRest.method)
    , ("photos.getAlbums",
       { req_params = ["session_key", "uid", "aids"];
         opt_params = [];
       } :FbRest.method)
    , ("photos.getTags",
       { req_params = ["session_key", "pids"];
         opt_params = [];
       } :FbRest.method)
    ], StringMap.empty)
}}

/**
 * @author Nicolas Glondu, March 2010
 * @category Facebook ?
 * @category External API ?
 * @destination private
 */

/**
 * {1 About this module}
 *
 * This module provides the core functions of the Facebook API
 *
 * {1 Where should I start?}
 *
 * All functions are classed in diffrent categories. The names
 * where made to be explicit.
 */


FbRest_private = {{

  _identity(res) = res

  /* ------------------------------ */
  /* Simple JSON building functions */
  /* ------------------------------ */

/**
 * Basic json formatting functions
 */

  /* Might be slow on very big lists - need String.blit for optimization */
  _json_generic_list(l:list(string), delim:string) =
    if l == [] then ""
    else
      aux(elt:string, acc:string) = "{acc},{delim}{elt}{delim}"
      sub_res = List.fold(aux, List.tail(l), "{delim}{List.head(l)}{delim}")
      "[{sub_res}]"

  _json_string_list(l) = _json_generic_list(l, "\"")
  _json_int_list(l) = _json_generic_list(l, "")
  /* Note: ints are already changed in string */

  _json_generic_dictionary(ll:list((string, string)), delim:string) =
    if ll == [] then ""
    else
      aux((k:string,v:string), acc:string) = "{acc},\"{k}\":{delim}{v}{delim}"
      (k, v) = List.head(ll)
      sub_res = "\"{k}\":{delim}{v}{delim}"
      List.fold(aux, List.tail(ll), sub_res)

  _json_string_dictionary(ll) = _json_generic_dictionary(ll, "\"")
  _json_int_dictionary(ll) = _json_generic_dictionary(ll, "")

  _inlined_list(l:list(string)) =
    if l == [] then ""
    else
      aux(elt:string, acc:string) = "{acc},{elt}"
      List.fold(aux, List.tail(l), List.head(l))

  /* ------------------- */
  /* Treatment functions */
  /* ------------------- */

/**
 * Transforms a list of (name, parameters) in a form-urlencoded string
 * format: baseurl?name1=value1&name2=value2 ...
 *
 * Note: if base is empty, returns only name1=value1&name2=value2 ...
 * (This behaviour is used for batch.run method)
 */
  _build_path(baseurl:string, args) =
    aux((pname:string, pval:string), acc:string) =
      pval = API_libs_private.url_encoder(pval)
      if acc == "" then
        if baseurl == "" then "{pname}={pval}"
        else "?{pname}={pval}"
      else "{acc}&{pname}={pval}"
    argstext = List.fold(aux, args, "")
    "{baseurl}{argstext}"

/**
 * Transforms a list of (name, parameters) in a multipart/form-data string
 * format:
 * --boundary
 * Content-Disposition: form-data; name=\"name1\"
 *
 * value1
 * --boundary
 * ...
 * --boundary--
 *
 * Note : here some data is added at the end, intended for (photos|video).upload
 * Http client is not currently capable to send multipart requests,
 * thats's why it is commented
 */
 /* bnd = "-----------------768299458641184676334"

  _build_data(args, fname:string, data:string) =
    aux((pname:string, pval:string), acc:string) =
      pval = API_libs_private.url_encoder(pval)
      "{acc}\r\nContent-Disposition: form-data; name=\"{pname}\"\r\n\r\n{pval}\r\n--{bnd}"
    argstxt = List.fold(aux, args, "")
    endtxt = "Content-Disposition: form-data; name=\"{fname}.jpg\"; filename=\"{fname}.jpg\"\r\nContent-Type: image/jpeg\r\n{data}\r\n--{bnd}--"
    "--{bnd}{argstxt}\r\n{endtxt}" */

/**
 * A parser for Facebook JSON errors
 *
 * Not used yet
 */
  _error_result(raw) =
    map = API_libs_private.parse_json(raw)
    map = JsonOpa.record_fields(map) ? Map.empty
    error_code = Map.get("error_code", map) ? {Int = 0} :RPC.Json.json |> (x -> JsonOpa.to_int(x) ? 0)
    error_msg = Map.get("error_msg", map) ? {String = ""} :RPC.Json.json |> (x -> JsonOpa.to_string(x) ? "")
    request_args = Map.get("request_args", map) ? {List = []} :RPC.Json.json |> (x -> JsonOpa.to_list(x) ? [])
    aux(arg, acc) =
      map = JsonOpa.record_fields(arg) ? Map.empty
      key = Map.get("key", map) ? {String = ""} :RPC.Json.json |> (x -> JsonOpa.to_string(x) ? "")
      value = Map.get("value", map) ? {String = ""} :RPC.Json.json |> (x -> JsonOpa.to_string(x) ? "")
      if (key != "") then StringMap.add(key, value, acc)
      else acc
    request_args = List.fold(aux, request_args, StringMap.empty)
    { error_code = error_code; error_msg = error_msg; request_args = request_args }:FbRest.error

/**
 * Basic explicit to_string functions
 */
  _perm_to_string(p) = Facebook.permission_to_string(p)

  _event_privacy_to_string(ep) =
    match ep :FbRest.event_privacy
    { OPEN }   -> "OPEN"
    { CLOSED } -> "CLOSED"
    { SECRET } -> "SECRET"

  /* ------------------- */
  /* Signature functions */
  /* ------------------- */

/**
 * Function to build the Facebook signature corresponding to given args and secret
 *
 * Probably the most important function in FbRest_private module
 * Facebook signature is built like this :
 * - Sort all name/values couples alphabetically (normally all names are unique)
 * - Form one string whith all names and values directly appended to each others
 * (Should give something like name1value1name2value2 ...)
 * - Append the secret to this string
 * - Return the md5  of the result
 */
  _sign_request(secret:string, args) =
    aux0((pname:string, pval:string), acc:set(string)) = Set.add("{pname}={pval}", acc)
    setorder = List.fold(aux0, args, Set.empty)
    aux1(arg:string, acc:string) = acc^arg
    str_params = Set.fold(aux1, setorder, "")
    Crypto.Hash.md5("{str_params}{secret}")

  /* ----------------- */
  /* Local definitions */
  /* ----------------- */

  host = "https://api.facebook.com"
  base_path = "/restserver.php"

  /* --------------- */
  /* Final functions */
  /* --------------- */

/**
 * This function takes the method name, the "candidate" parameters/values,
 * the api_key and the secret and returns a list of valid parameters/values
 * and the list of missing parameters. If some parameters are missing, the
 * request should not be sent.
 *
 * Note that the method is part of the parameters. The automatic parameters
 * defined in FbRest_definitions_private and the signature are also added here.
 */
  _build_args(method, params, api_key, secret) =
    options = StringMap.get(method, FbRest_definitions_private.implemented_methods) |> Option.get_msg((-> __POSITION__), _)
    req = options.req_params
    opt = options.opt_params

    args = List.append([("api_key", api_key), ("method", method)], FbRest_definitions_private.auto_params())
    res = {args=args; req=req; opt=opt;}

    aux1((key, val), res) =
      if List.mem(key, res.req) then
        {args=List.cons((key, val), res.args); req=List.remove(key, res.req); opt=res.opt;}
      else if List.mem(key, res.opt) then
        {args=List.cons((key, val), res.args); req=res.req; opt=List.remove(key, res.opt);}
      else res
    res = List.fold(aux1, params, res)

    sig = _sign_request(secret, res.args)
    (List.cons(("sig", sig), res.args), res.req)

/**
 * Makes a Facebook call
 *
 * @param method The Facebook method called
 * @param params A (key, value) list of parameters
 * @param api_key Your API key
 * @param secret The secret key of you application OR the session secret, depending on the method called
 * @param final_fun The final handler, must take a string (which will be the result of the function) and return a Resource.
 */
  generic_call(method, params, api_key, secret, final_fun) =
    aux_apply(msg:string) =
      match final_fun :API_libs.answer_fun with
      ~{api_fun_html} ->
        { api_html = api_fun_html(msg) }
      ~{api_fun_void} ->
        do api_fun_void(msg)
        { api_void }:API_libs.answer

    if StringMap.mem(method, FbRest_definitions_private.implemented_methods) then
      (args, missing_req) = _build_args(method, params, api_key, secret)

      if missing_req == [] then
        aux_apply(FbLib.fb_get(host, base_path, args)?"")
      else aux_apply("Required params missing or empty: {_json_int_list(missing_req)}")

    else aux_apply("Not implemented method")

}}

/**
 * @author Nicolas Glondu, March 2010
 * @category Facebook
 * @category External API
 * @destination public
 */

/**
 * {1 About this module}
 *
 * This module contains an alias for all implemented functions.
 * It also contains some helpers to make the contruction of some
 * FbRest objects easier.
 *
 * Facebook developpers website http://wiki.developers.facebook.com/index.php/API
 * is the best documentation for each method.
 *
 * {1 Where should I start?}
 *
 * (Note: Building a Facebook application requires you to have a Facebook account)
 * The first thing to do before building an application is to add the
 * developper application to your Facebbok profile here :
 * http://www.facebook.com/developers/
 * Then add and configure your application (it is quite well explained)
 * You should take your application keys and secret to add them in
 * your application.
 *
 * Once you have an application key, you can create a login link with
 * the integrated helper (FbRest.login_url), then configure your program to
 * give the answer to the connection_result function and store the result
 *
 * The final_fun parameter of each function must have the type API_libs.answer_fun. If
 * you give a api_fun_html, the function will return a api_html that you should
 * handle with API_libs.get_html. If you give a api_fun_void, it will return a api_void
 * that you should handle with API_libs.get_void. This strange mechanism allows to have
 * the same function returning html (for direct pages) sometimes while returning void
 * (for instance for session communication) at other times.
 *
 * If you already worked with the Twitter OPA API, the mechanism used here
 * is the same.
 *
 * You now have everything needed to interact with Facebook. To discover the
 * API, your return function should simply display the raw result of the
 * call. Then you can do whatever you want with it.
 *
 * The help for a given module.method method can generally be found at:
 * http://wiki.developers.facebook.com/index.php/module.method
 *
 * {1 What if I need more?}
 *
 * To add a method, just add it here and it becomes accessible via
 * FbRest.generic_call(method, params, api_key, secret, final_fun)
 * which is an alias to FbRest_private.generic_call explained later
 *
 * You may also create a specific call in the FbRest module to
 * to make the use of the method more user-friendly
 *
 * IMPORTANT NOTE : 'api_key', 'sig', 'call_id' and 'format' are
 * automatically added to all functions
 */

FbRest = {{
  /* -------------------- */
  /* Specific to Facebook */
  /* -------------------- */

/**
 * Facebook time
 *
 * Facebook time is Unix epoch time assuming UTC time is Pacific time
 * (Daylight Savings or Standard, depending on the date ). Which gives
 * epoch minus 7 or 8 hours depending on the date.
 * Citation from the API : "have fun!"
 *
 * Note : This function is approximate on dates close to the time change
 * Note : Might be fixed one day on Facebook
 */
  _time(t : int) =
    tt : Date.date = Date.ll_import(Date.time_t_of_int(t)) // FIXME, replace with Date.from_int?
    wday = Date.get_weekday(tt) |> Date.Weekday.to_int
    day = Date.get_day(tt)
    month = Date.get_month(tt) |> Date.Month.to_int
     // FIXME rewrite 
    decal = if ( (month > 2 && month < 10)
              || (month == 2 && (day - wday) > 7)
              || (month == 10 && (day - wday) < 7) ) then 7 else 8
    t - (decal * 3600)

  _user_type_parser = parser
    | ([0-9]*) -> "uid"
    | .* -> "session_key"

/**
 * Adds the user information (uid or seskey) to provided parameters list
 *
 * The parsing rule is simple : if alphanumeric it is an uid
 * else it is a session key (session keys contain dots and underscores
 * so it is a simple and effective determination)
 *
 * @param uid_or_seskey An uid or a seskey to add
 * @param params The parameters list where the user information should be added
 */
  _add_user(uid_or_seskey, params) =
    user_type = Parser.try_parse(_user_type_parser, uid_or_seskey) ? ""
    List.cons((user_type, uid_or_seskey), params)

  /* -------------------------- */
  /* ---- Parsing functions --- */
  /* -------------------------- */
/**
 * Parsing function for connection result
 *
 * The result given when a user logs in should be given to this function
 * which returns a FbRest.session object and a string list of the
 * permissions allowed (can be empty)
 *
 * @param raw_result The result (from "?") of the user login
 */
  connection_result(raw_result) =
    do API_libs_private.apijlog("****************")
    do API_libs_private.apijlog(raw_result)
    params = Parser.parse(UriParser.query_parser, raw_result)
    session = List.assoc("session", params) ? ""
    perms = List.assoc("perms", params) ? ""

    session = API_libs_private.parse_json(session)
    session = JsonOpa.record_fields(session) ? Map.empty
    loc_int(name) = API_libs_private.map_get_int(name, session)
    loc_string(name) = API_libs_private.map_get_string(name, session)
    session = { session_key = loc_string("session_key");
                uid         = loc_int("uid");
                expires     = loc_int("expires");
                secret      = loc_string("secret");
              } :FbRest.session
    perms = String.explode(",", perms)
    (session, perms)

/**
 * Parser for error_messages
 *
 * This function formats an error message into a FbRest.error
 * object.
 *
 * @param raw_result The raw result of your call
 */
  error_parser(raw_result) =
    FbRest_private._error_result(raw_result)

/**
 * Parser for simple fql result
 *
 * This function returns the result as a StringMap(string)
 * (most fql calls should return a result in this format)
 *
 * @param raw_result The raw result of your FQL call
 */
  fql_result(raw_result) =
    list = API_libs_private.parse_json(raw_result)
    list = JsonOpa.to_list(list) ? []
    aux1(elt) =
      elt = JsonOpa.record_fields(elt) ? Map.empty
      aux2(x) =
        match x : RPC.Json.json
        {~String} -> String
        _ -> Json.to_string(x)
      API_libs_private.remap(aux2, elt)
    List.map(aux1, list)

  /* ---------------------- */
  /* --- Help functions --- */
  /* ---------------------- */
/**
 * Helper for construction of action links
 *
 * The only purpose of this function is to help the user to format his
 * action links.
 * Note: The use of this function is not required, the user can build his action links himself
 *
 * @param action_links A list of FbRest.link
 */
  build_action_links(action_links:list(FbRest.link)) =
    if action_links == [] then ""
    else
      aux(link:FbRest.link, acc:string) = "{acc},\{\"text\":\"{link.text}\",\"href\":\"{link.href}\"}"
      sub_res = List.fold(aux, List.tail(action_links),
       "\{\"text\":\"{List.head(action_links).text}\",\"href\":\"{List.head(action_links).href}\"}")
      "[{sub_res}]"

/**
 * Helper for construction of attachments (used in streams)
 *
 * The only purpose of this function is to help the user to format his
 * attachments.
 * Note: The use of this function is not required, the user can build his attchment himself
 *
 * @param name The name of the stream attachment
 * @param href A link to the stream attachment
 * @param caption A caption for the stream attachment, should contain only a few words
 * @param descr A description for the stream attachment, if it is too long, a "show more" option will be automatically added by Facebook
 * @param properties A list of FbRest.stream_ppty for the stream attachment
 * @param media A FbRest.stream_med joined to the stream
 * @param custom A list of custom (key, value) parameters. These will not be displayed but are stored and given back if the stream is got back with stream.get
 */
  build_attachment(name:string, href:string, caption:string, descr:string, properties:list((string, FbRest.stream_ppty)) , media:FbRest.stream_med, custom:list((string, string))) =
    locformat(name:string, value:string) =
      if value == "" then "" else "\"{name}\":\"{value}\""
    append(str1:string, str2:string) =
      if str1 == "" then str2
      else if str2 == "" then str1
      else "{str1},{str2}"
    rev_append(a, b) = append(b, a)

    t_name = locformat("name", name)
    t_href = locformat("href", href)
    t_caption = locformat("caption", caption)
    t_description = locformat("description", descr)
    t_properties = if properties == [] then ""
      else
        aux((key:string, val), acc:string) = match val :FbRest.stream_ppty
          {~text} -> append(acc, "\"{key}\":\"{text}\"")
          {~link} -> append(acc, "\"{key}\":\{\"text\":\"{link.text}\",\"href\":\"{link.href}\"}")
          _ -> acc
        res = List.fold(aux, properties, "")
        if res == "" then "" else "\"properties\":\{{res}}"
    t_media = match media :FbRest.stream_med
      {~pics} ->
        pics = if List.length(pics) > 5 then API_libs_private.sublist(0, 5, pics)
        else pics

        aux(pic:FbRest.stream_pic, acc:string) =
          "\{\"type\":\"image\",\"src\":\"{pic.src}\",\"href\":\"{pic.href}\"}"
          |> append(acc, _)
        List.fold(aux, pics, "")
      {~flash} ->
        "\"type\":\"flash\",\"swfsrc\":\"{flash.swfsrc}\",\"imgsrc\":\"{flash.imgsrc}\""
        |> rev_append(locformat("width", flash.width?""), _)
        |> rev_append(locformat("height", flash.height?""), _)
        |> rev_append(locformat("expanded_width", flash.xwidth?""), _)
        |> rev_append(locformat("expanded_height", flash.xheight?""), _)
        |> (x:string -> "\{{x}}")
      {~mp3} ->
        "\"type\":\"mp3\",\"src\":\"{mp3.src}\""
        |> rev_append(locformat("title", mp3.title?""), _)
        |> rev_append(locformat("artist", mp3.artist?""), _)
        |> rev_append(locformat("album", mp3.album?""), _)
        |> (x:string -> "\{{x}}")
      _ -> ""
    t_media = if t_media == "" then "" else "\"media\":[{t_media}]"
    aux((key,val), acc) = append(acc, locformat(key, val))
    t_custom = List.fold(aux, custom, "")

    res = t_custom |> append(t_media, _) |> append(t_properties, _) |> append(t_description, _)
      |> append(t_caption, _) |> append(t_href, _) |> append(t_name, _)
    if res == "" then "" else "\{{res}}"

/**
 * Helper for construction of the login link
 *
 * The only purpose of this function is to help the user to format his
 * login link.
 * Note: The use of this function is not required, the user can build his login link himself
 *
 * @param api_key The AI key of you application
 * @param permission A list of Facebook.permissions to request with the login
 * @param ok_url The url where the user should be redirected if he accepts to log in. Must start with the Connect url given to Facebook while registering the application.
 * @param cancel_url The url where the user should be redirected if he refuses to log in. Must start with the Connect url given to Facebook while registering the application.
 */
  login_url(api_key:string, permissions, ok_url:string, cancel_url:string) =
    perms_text = if permissions == [] then ""
    else
      aux(elt, acc:string) = "{acc},{FbRest_private._perm_to_string(elt)}"
      List.fold(aux, List.tail(permissions), "&req_perms={FbRest_private._perm_to_string(List.head(permissions))}")
    "http://www.facebook.com/login.php?api_key={api_key}&extern=1&fbconnect=1&return_session=1{perms_text}&v=1.0&next={ok_url}&cancel_url={cancel_url}"

/**
 * Helper for construction of extend permission link
 *
 * The only purpose of this function is to help the user to format his
 * extend permission link.
 * Note: The use of this function is not required, the user can build his extend permission link himself
 *
 * @param api_key The AI key of you application
 * @param permission A list of Facebook.permissions to request
 * @param next_url The url where the user should be redirected after accepting or not the permissions. Must start with the Connect url given to Facebook while registering the application.
 */
  extend_permissions_url(api_key:string, permissions, next_url:string) =
    perms_text = if permissions == [] then ""
    else
      aux(elt, acc:string) = "{acc},{FbRest_private._perm_to_string(elt)}"
      List.fold(aux, List.tail(permissions), "&ext_perm={FbRest_private._perm_to_string(List.head(permissions))}")
    "http://www.facebook.com/connect/prompt_permissions.php?api_key={api_key}&v=1.0&next={next_url}{perms_text}&enable_profile_selector=1"

/**
 * Helper for construction of event_info
 *
 * event_info is a JSON_encoded object
 * Required parameters :
 * - name (string)
 * - start_time (int) (Timestamp -7 (PDT) or -8 hours (PST) depending on the date)
 * Optionnal parameters :
 * - category (int)
 *   + 1: Party      + 2: Causes     + 3: Education  + 4: Meetings
 *   + 5: Music/Arts + 6: Sports     + 7: Trips      + 8: Other
 * - subcategory (int)
 *   + 1: Birthday Party            + 2: Cocktail Party            + 3: Club Party
 *   + 4: Concert                   + 5: Fraternity/Sorority Party + 6: Business Meeting
 *   + 7: Barbecue                  + 8: Card Night                + 9: Dinner Party
 *   + 10: Holiday Party            + 11: Night of Mayhem          + 12: Movie/TV Night
 *   + 13: Drinking Games           + 14: Bar Night                + 15: LAN Party
 *   + 16: Study Group              + 17: Mixer                    + 18: Slumber Party
 *   + 19: Erotic Party             + 20: Benefit                  + 21: Goodbye Party
 *   + 22: House Party              + 23: Reunion                  + 24: Fundraiser
 *   + 25: Protest                  + 26: Rally                    + 27: Class
 *   + 28: Lecture                  + 29: Office Hours             + 30: Workshop
 *   + 31: Club/Group Meeting       + 32: Convention               + 33: Dorm/House Meeting
 *   + 34: Informational Meeting    + 35: Audition                 + 36: Exhibit
 *   + 37: Jam Session              + 38: Listening Party          + 39: Opening
 *   + 40: Performance              + 41: Preview                  + 42: Recital
 *   + 43: Rehearsal                + 44: Pep Rally                + 45: Pick-Up
 *   + 46: Sporting Event           + 47: Sports Practice          + 48: Tournament
 *   + 49: Camping Trip             + 50: Daytrip                  + 51: Group Trip
 *   + 52: Roadtrip                 + 53: Carnival                 + 54: Ceremony
 *   + 55: Festival                 + 56: Flea Market              + 57: Retail
 *   + 58: Wedding
 * - location (string)
 * - end_time (int) (Same format than start_time)
 * - street (string)
 * - city (string)
 * - phone (string)
 * - email (string)
 * - page_id (int) (to create an event for a group or a page)
 * - description (string)
 * - privacy_type (string) (OPEN, CLOSED or SECRET)
 * - tagline (string)
 * - host (string)
 */
  build_event(name:string, start_time, params) =
    res = "\"name\":\"{name}\",\"start_time\":{_time(start_time)}"
    addint(acc:string, name:string, val:int) = acc ^ ",\"{name}\":{val}"
    addstr(acc:string, name:string, val:string) = acc ^ ",\"{name}\":\"{val}\""
    aux(elt, acc) = match elt :FbRest.event_opt_param
      { ~category }     -> addint(acc, "category", category)
      { ~subcategory }  -> addint(acc, "subcategory", subcategory)
      { ~location }     -> addstr(acc, "location", location)
      { ~end_time }     -> addint(acc, "end_time", _time(end_time))
      { ~street }       -> addstr(acc, "street", street)
      { ~city }         -> addstr(acc, "city", city)
      { ~phone }        -> addstr(acc, "phone", phone)
      { ~email }        -> addstr(acc, "email", email)
      { ~page_id }      -> addint(acc, "page_id", page_id)
      { ~description }  -> addstr(acc, "description", description)
      { ~tagline }      -> addstr(acc, "tagline", tagline)
      { ~host }         -> addstr(acc, "host", host)
      { ~privacy_type } -> addstr(acc, "privacy_type", FbRest_private._event_privacy_to_string(privacy_type))
    res = List.fold(aux, params, res)
    "\{{res}}"

/**
 * Makes a Facebook call
 *
 * The help for a given module.method method can generally be found at:
 * http://wiki.developers.facebook.com/index.php/module.method
 *
 * @param method The Facebook method called
 * @param params A (key, value) list of parameters
 * @param api_key Your API key
 * @param secret The secret key of you application OR the session secret, depending on the method called
 * @param final_fun A API_libs.answer_fun object containing a function taking a string (which will be the result of the function) and returning a resource (if api_fun_html) or void (if api_fun_void).
 */
  generic_call(method, params, api_key, secret, final_fun) =
    FbRest_private.generic_call(method, params, api_key, secret, final_fun)

  _sob(x:bool) = "{x}" /* Is there a cleaner way to do this ? */

/**
 * Note: From here, it is mainly a user-friendly implementation of what was defined
 * in the other methods
 * - Required params are directly passed in the 'params' list
 * - Optional params are integrated through the _add_param(s) functions
 * which filter the empty (empty="" here) ones
 * - Some checking systems are added to ensure correct objects are passed
 * - Since quite a lot of functions take either an uid or a session_key,
 * they are merged in one parameter named "uid_or_seskey" and the the type is
 * added via the function _add_user (uid is an integer while seskey is not)
 *
 * WARNING for editors: 'api_key, secret, final_fun' are passed in all function
 * as 3 last parameters, these parameterss appear only in batch.run because it
 * needs to use those parameters internally
 *
 * final_fun is a API_libs.answer_fun object containing a function taking a string
 * (which will be the result of the function) and returning a resource (if api_fun_html)
 * or void (if api_fun_void).
 */

  _add_opt_param((name, val), acc) = if val == "" then acc else List.cons((name, val), acc)
  _add_opt_params(l, acc) = List.fold(_add_opt_param, l, acc)

  _error_msg(descr:string, list:list(string), _:string, _:string, fun:API_libs.answer_fun) =
    msg = "ERROR: {descr} {FbRest_private._json_int_list(list)}"
    match fun :API_libs.answer_fun with
    ~{api_fun_html} ->
      { api_html = api_fun_html(msg) }:API_libs.answer
    ~{api_fun_void} ->
      do api_fun_void(msg)
      { api_void }:API_libs.answer

  _check_list_correctness(tested, valid) =
    aux(elt, (accbool, acctxt)) =
      subres = List.mem(elt, valid)
      ((accbool && subres), (if subres then acctxt else List.cons(elt, acctxt)))
    List.fold(aux, tested, (true, []))

  Admin = {{
    _valid_app_properties_strings = ["about_url", "app_id", "application_name", "authorize_url",
      "base_domain", "callback_url", "canvas_name", "connect_logo_url", "connect_preview_template",
      "connect_reclaim_url", "connect_url", "contact_email", "creator_uid", "dashboard_url",
      "description", "edit_url", "email", "email_domain", "help_url", "icon_url", "info_changed_url",
      "ip_list", "logo_url", "message_action", "post_authorize_redirect_url", "preload_fql",
      "privacy_url", "profile_tab_url", "publish_action", "publish_self_action", "publish_self_url",
      "publish_url", "support_url", "tab_default_name", "tos_url", "uninstall_url"]
    _valid_app_properties_bools = ["default_column", "desktop", "dev_mode", "iframe_enable_util",
      "ignore_ip_whitelist_for_ss", "installable", "is_mobile", "private_install", "quick_transitions",
      "targeted", "use_iframe", "video_rentals", "wide_mode"]
    _valid_app_properties_array = ["base_domains"]

    generic_ban_action(action:string, uids:list(string)) =
      params = _add_opt_param(("uids", FbRest_private._json_int_list(uids)), [])
      generic_call(action, params, _, _, _)

    banUsers(uids) =
      generic_ban_action("admin.banUsers", uids)

    getAllocation(point) =
      valid_points = ["notifications_per_day", "announcement_notifications_per_week",
                      "requests_per_day", "emails_per_day", "email_disable_message_location"]
      if List.mem(point, valid_points) then
        params = [("integration_point_name", point)]
        generic_call("admin.getAllocation", params, _, _, _)
      else
        _error_msg("Invalid integration point", valid_points, _, _, _)

    getAppProperties(properties) =
      valid_properties = List.append(_valid_app_properties_strings, _valid_app_properties_bools)
        |> List.append(_valid_app_properties_array, _)
      (correct, errors) = _check_list_correctness(properties, valid_properties)

      if correct then
        params = [("properties", FbRest_private._json_string_list(properties))]
        generic_call("admin.getAppProperties", params, _, _, _)
      else
        _error_msg("Invalid properties {FbRest_private._json_int_list(errors)}", valid_properties, _, _, _)

    getBannedUsers(uids) =
      generic_ban_action("admin.getBannedUsers", uids)

    getMetrics(start_time, end_time, period, metrics) =
      valid_periods = [ 86400, 604800, 2592000 ]
      valid_metrics_general = [ "active_users", "api_calls", "unique_api_calls", "canvas_page_views",
        "unique_canvas_page_views", "canvas_http_request_time_avg", "canvas_fbml_render_time_avg",
        "unique_blocks", "unique_unblocks" ]
      valid_metrics_one_day = [ "canvas_page_views_http_code_0", "canvas_page_views_http_code_100",
        "canvas_page_views_http_code_200", "canvas_page_views_http_code_200ND",
        "canvas_page_views_http_code_301", "canvas_page_views_http_code_302",
        "canvas_page_views_http_code_303", "canvas_page_views_http_code_400",
        "canvas_page_views_http_code_401", "canvas_page_views_http_code_403",
        "canvas_page_views_http_code_404", "canvas_page_views_http_code_405",
        "canvas_page_views_http_code_413", "canvas_page_views_http_code_422",
        "canvas_page_views_http_code_500", "canvas_page_views_http_code_502",
        "canvas_page_views_http_code_503", "canvas_page_views_http_code_505" ]
      if List.mem(period, valid_periods) then
        valid_metrics =
          if period == 86400 then List.append(valid_metrics_general, valid_metrics_one_day)
          else valid_metrics_general
        (correct, errors) = _check_list_correctness(metrics, valid_metrics)
        if correct then
          params = [("start_time", "{_time(start_time)}"), ("end_time", "{_time(end_time)}"),
                    ("period", "{Int.to_string(period)}"), ("metrics", FbRest_private._json_string_list(metrics))]
          generic_call("admin.getMetrics", params, _, _, _)
        else
          _error_msg("Invalid metrics {FbRest_private._json_int_list(errors)}", valid_metrics, _, _, _)
      else
        _error_msg("Invalid period", List.map(Int.to_string, valid_periods), _, _, _)

    getRestrictionInfo() =
      generic_call("admin.getRestrictionInfo", [], _, _, _)

    setAppProperties(properties) =
      valid_properties = List.append(_valid_app_properties_strings, _valid_app_properties_bools)
        |> List.append(_valid_app_properties_array, _)
      fst(a) = a.f1
      (correct, errors) = _check_list_correctness(properties |> List.map(fst, _), valid_properties)

      if correct then
        aux(l, a) = List.mem(a.f1, l)
        string_properties = List.filter(aux(_valid_app_properties_strings, _), properties)
          |> FbRest_private._json_string_dictionary(_)
        bool_properties = List.filter(aux(_valid_app_properties_bools, _), properties)
          |> FbRest_private._json_int_dictionary(_)
        array_property = List.filter(aux(_valid_app_properties_array, _), properties)
          |> FbRest_private._json_int_dictionary(_)
        res = string_properties
        res = if res == "" then bool_properties else (res ^ "," ^ bool_properties)
        res = if res == "" then array_property else (res ^ "," ^ array_property)
        res = "\{{res}}"
        params = [("properties", res)]
        generic_call("admin.setAppProperties", params, _, _, _)
      else
        _error_msg("Invalid properties {FbRest_private._json_int_list(errors)}", valid_properties, _, _, _)

    setRestrictionInfo(age, location, age_distribution, typ) =
      aux(name:string, value:string, separator:string, acc:string) =
        if (value == "") then acc
        else if (acc == "") then "\"{name}\":{separator}{value}{separator}"
        else "\"{acc},{name}\":{separator}{value}{separator}"
      restriction_str = aux("age", age, "\"", "")
        |> aux("location", location, "\"", _)
        |> aux("age_distribution", age_distribution, "", _)
        |> aux("type", typ, "\"", _)
        |> (a -> "\{"^a^"}")
      params = [("restriction_str", restriction_str)]
      generic_call("admin.setRestrictionInfo", params, _, _, _)

    unbanUsers(uids) =
      generic_ban_action("admin.unbanUsers", uids)


  }}

  Application = {{
    getPublicInfo(key_type, key) =
      valid_key_types = ["id", "api_key", "canvas_name"]
      if List.mem(key_type, valid_key_types) then
        params = [("application_{key_type}", key)]
        generic_call("application.getPublicInfo", params, _, _, _)
      else
        _error_msg("Invalid key type", valid_key_types, _, _, _)
  }}

  Auth = {{
    expireSession(session_key) =
      params = [("session_key", session_key)]
      generic_call("auth.expireSession", params, _, _, _)

    revokeAuthorization(uid_or_seskey) =
      params = _add_user(uid_or_seskey, [])
      generic_call("auth.revokeAuthorization", params, _, _, _)

    revokeExtendedPermission(perm:Facebook.permission, uid_or_seskey) =
      params = [("perm", FbRest_private._perm_to_string(perm))]
               |>  _add_user(uid_or_seskey, _)
      generic_call("auth.revokeExtendedPermission", params, _, _, _)
  }}

  Batch = {{
/**
 * NOTES:
 *'methods' is a List(Tuple2(string, List(Tuple2(string, string))))
 *     or List(Tuple2(method_name, List(Tuple2(param_name, param_value))))
 * The parameters list should not contain 'api_key', 'call_id', 'v', 'format'
 * since they are still automatically added
 */
    run(methods, serial_only:bool, session_key, api_key, secret, final_fun) =
      if List.length(methods) < 21 then
        /* Step 1: Only keep the existing methods */
        (method_feed, error_feed) = List.fold(
          ( method, (feed, errs) ->
            if StringMap.mem(method.f1, FbRest_definitions_private.implemented_methods) then
              (List.cons(method, feed), errs)
            else (feed, List.cons("Method not implemented: {method.f1}", errs))
          ), methods, ([], []))
        /* Step 2: Verify params and sign all valid methods */
        (method_feed, error_feed) = List.fold(
          ( method, (feed, errs) ->
            (args, missing_req) = FbRest_private._build_args(method.f1, method.f2, api_key, secret)
            if missing_req == [] then (List.cons(args, feed), errs)
            else (feed, List.cons("{method.f1} - required parameters missing or empty: {FbRest_private._json_int_list(missing_req)} ", errs))
          ), method_feed, ([], error_feed))
        /* Last step: Put it all in a JSON string */
        method_feed = FbRest_private._json_string_list( List.map((x -> FbRest_private._build_path("", x)), method_feed) )

        if error_feed == [] then
          params = [("method_feed", method_feed), ("serial_only", _sob(serial_only))]
                   |> _add_opt_params([("session_key", session_key)], _)
          generic_call("batch.run", params, api_key, secret, final_fun)
        else
          _error_msg("batch.run: Error in methods given", error_feed, "", "", final_fun)

      else
        _error_msg("batch.run: More than 20 methods given", [], "", "", final_fun)
  }}

  Comments = {{
    add(text, xid, object_id, title, url, publish_to_stream, uid_or_seskey) =
      params = [("text", text)]
               |> _add_opt_params([("xid", xid), ("object_id", object_id), ("title", title),
                                   ("url", url), ("publish_to_stream", publish_to_stream)], _)
               |> _add_user(uid_or_seskey, _)
      generic_call("comments.add", params, _, _, _)

    add_simple(text, xid, object_id, uid_or_seskey) =
      add(text, xid, object_id, "", "", "", uid_or_seskey)

    get(session_key, xid, object_id) =
      params = [("session_key", session_key)]
               |> _add_opt_params([("xid", xid), ("object_id", object_id)], _)
      generic_call("comments.get", params, _, _, _)

    remove(comment_id, session_key, xid, object_id) =
      params = [("comment_id", comment_id)]
               |> _add_opt_params([("xid", xid), ("object_id", object_id), ("session_key", session_key)], _)
      generic_call("comments.remove", params, _, _, _)
  }}

  Data = {{
    getCookies(uid, name) =
      params = [("uid", uid)]
               |> _add_opt_params([("name", name)], _)
      generic_call("data.getCookies", params, _, _, _)

    setCookies(uid, name, value, expires, path) =
      params = [("uid", uid), ("name", name), ("value", value)]
               |> _add_opt_params([("expires", expires), ("path", path)], _)
      generic_call("data.setCookies", params, _, _, _)
  }}

  Events = {{
    cancel(eid, message, session_key) =
      params = [("eid", eid)]
               |> _add_opt_params([("cancel_message", message), ("session_key", session_key)], _)
      generic_call("events.cancel", params, _, _, _)

    create(event_info, session_key) =
      params = [("event_info", event_info)]
               |> _add_opt_param(("session_key", session_key), _)
      generic_call("events.create", params, _, _, _)

    edit(eid, event_info, session_key) =
      params = [("eid", eid), ("event_info", event_info)]
               |> _add_opt_param(("session_key", session_key), _)
      generic_call("events.edit", params, _, _, _)

    get(eids, start_time, end_time, uid) =
      params = _add_opt_params([("eids", FbRest_private._json_int_list(eids)), ("uid", uid),
                            ("start_time", "{_time(start_time)}"), ("end_time", "{_time(end_time)}")], [])
      generic_call("events.get", params, _, _, _)

    getMembers(eid, session_key) =
      params = [("eid", eid)]
               |> _add_opt_param(("session_key", session_key), _)
      generic_call("events.getMembers", params, _, _, _)

    invite(eid, uids, message, session_key) =
      params = [("eid", eid), ("uids", FbRest_private._json_int_list(uids))]
               |> _add_opt_params([("session_key", session_key), ("personal_message", message)], _)
      generic_call("events.invite", params, _, _, _)

    resvp(eid, rsvp_status, session_key) =
      valid_statuses = ["attending", "unsure", "declined"]
      if List.mem(rsvp_status, valid_statuses) then
        params = [("eid", eid), ("rsvp_status", rsvp_status)]
                 |> _add_opt_param(("session_key", session_key), _)
        generic_call("events.rsvp", params, _, _, _)
      else
        _error_msg("Invalid rsvp status", valid_statuses, _, _, _)
  }}

  Fql = {{
    /* Note: see http://wiki.developers.facebook.com/index.php/FQL for help on FQL */
    query(query, session_key) =
      params = [("query", query)]
               |> _add_opt_param(("session_key", session_key), _)
      generic_call("fql.query", params, _, _, _)

    multiquery(queries, session_key) =
      params = [("queries", "\{{FbRest_private._json_string_dictionary(queries)}}")]
               |> _add_opt_param(("session_key", session_key), _)
      generic_call("fql.multiquery", params, _, _, _)
  }}

  Friends = {{
    areFriends(uids1, uids2, session_key) =
      if List.length(uids1) == List.length(uids2) && List.length(uids1)>0 then
        params = [("uids1", FbRest_private._json_int_list(uids1)), ("uids2", FbRest_private._json_int_list(uids2)),
                  ("session_key", session_key)]
        generic_call("friends.areFriends", params, _, _, _)
      else
        _error_msg("uids1 and uids2 have different lengths", [], _, _, _)

    get(flid, uid_or_seskey) =
      params = _add_opt_params([("flid", flid)], [])
               |> _add_user(uid_or_seskey, _)
      generic_call("friends.get", params, _, _, _)

    getAppUsers(session_key) =
      params = [ ("session_key", session_key) ]
      generic_call("friends.getAppUsers", params, _, _, _)

    getLists(session_key) =
      params = [ ("session_key", session_key) ]
      generic_call("friends.getLists", params, _, _, _)

    getMutualFriends(target_uid, source_uid, session_key) =
      params = [("target_uid", target_uid)]
               |> _add_opt_params([("source_uid", source_uid), ("session_key", session_key)], _)
      generic_call("friends.getMutualFriends", params, _, _, _)
  }}

  Group = {{
    get(session_key, gids, uid) =
      params = [("session_key", session_key)]
               |> _add_opt_params([("gids", FbRest_private._json_int_list(gids)), ("uid", uid)], _)
      generic_call("group.get", params, _, _, _)

    getMembers(session_key, gid) =
      params = [("session_key", session_key), ("gid", gid)]
      generic_call("group.getMembers", params, _, _, _)
  }}

  Links = {{
    get(session_key, uid, link_ids, limit) =
      params = [("session_key", session_key)]
               |> _add_opt_params([("uid", uid), ("link_ids", FbRest_private._json_int_list(link_ids)), ("limit", limit)], _)
      generic_call("links.get", params, _, _, _)

    getStats(urls, session_key) =
      urls_text = List.map(API_libs_private.url_encoder, urls) |> FbRest_private._inlined_list(_)
      params = [("urls", urls_text), ("session_key", session_key)]
      generic_call("links.getStats", params, _, _, _)

    post(uid, url, comment, session_key, image) =
      params = [("uid", uid), ("url", url), ("comment", comment)]
               |> _add_opt_params([("session_key", session_key), ("image", image)], _)
      generic_call("links.post", params, _, _, _)

    preview(url, session_key) =
      params = [("url", url)]
               |> _add_opt_params([("session_key", session_key)], _)
      generic_call("links.preview", params, _, _, _)
  }}

  Notes = {{
    create(uid, title, content, session_key) =
      params = [("uid", uid), ("title", title), ("content", content)]
               |> _add_opt_params([("session_key", session_key)], _)
      generic_call("notes.create", params, _, _, _)

    delete(note_id, title, content, uid_or_seskey) =
      params = [("note_id", note_id), ("title", title), ("content", content)]
               |> _add_user(uid_or_seskey, _)
      generic_call("notes.delete", params, _, _, _)

    edit(note_id, title, content, session_key) =
      params = [("note_id", note_id), ("title", title), ("content", content)]
               |> _add_opt_params([("session_key", session_key)], _)
      generic_call("notes.edit", params, _, _, _)

    get(session_key, uid) =
      params = [("session_key", session_key)]
               |> _add_opt_params([("uid", uid)], _)
      generic_call("notes.create", params, _, _, _)
  }}

  Notifications = {{
    markRead(notification_ids, session_key) =
      ids_text = FbRest_private._inlined_list(notification_ids)
      params = [("notification_ids", ids_text), ("session_key", session_key)]
      generic_call("notifications.markRead", params, _, _, _)

    get(session_key) =
      params = [("session_key", session_key)]
      generic_call("notifications.get", params, _, _, _)

    getList(session_key, include_read:bool, start_time) =
      params = [("session_key", session_key), ("include_read", _sob(include_read)) ]
               |> _add_opt_param(("start_time", start_time), _)
      generic_call("notifications.getList", params, _, _, _)

    sendEmail(recipients, subject, text) =
      recipients = String.implode(identity,",",recipients)
      params = [ ("recipients", recipients),
                 ("subject", subject),
                 ("text", text) ]
      generic_call("notifications.sendEmail", params, _, _, _)
  }}

  Pages = {{
    getInfo(fields, page_ids, uid_or_seskey) =
      params = [("fields", FbRest_private._json_string_list(fields))]
               |> _add_opt_params([ ("page_ids", FbRest_private._json_string_list(page_ids)) ], _)
               |> _add_user(uid_or_seskey, _)
      generic_call("pages.getInfo", params, _, _, _)

    isAdmin(page_id, session_key, uid) =
      params = [("page_id", page_id), ("session_key", session_key)]
               |> _add_opt_params([ ("uid", uid) ], _)
      generic_call("pages.isAdmin", params, _, _, _)

    isAppAdded(page_id) =
      params = [("page_id", page_id)]
      generic_call("pages.isAppAdded", params, _, _, _)

    isFan(page_id, uid_or_seskey) =
      params = [("page_id", page_id)]
               |> _add_user(uid_or_seskey, _)
      generic_call("pages.isFan", params, _, _, _)
  }}

  Photos = {{
    addTag(pid, x, y, tag_uid, tag_text, tags, session_key, owner_uid) =
      if (tag_uid == "" && tag_text == "" && tags == "") || (tag_uid != "" && tag_text == "")
        || (tag_uid == "" && tag_text != "") then
        _error_msg("Invalid tag request", [], _, _, _)
      else
        params = [("pid", pid), ("x", x), ("y", y)]
                 |> _add_opt_params([("tag_uid", tag_uid), ("tag_text", tag_text), ("tags", tags),
                                 ("session_key", session_key), ("owner_uid", owner_uid)], _)
        generic_call("photos.addTag", params, _, _, _)

    createAlbum(name, location, description, visible, privacy, session_key, uid ) =
      valid_visibility = ["friends", "friends-of-friends", "networks", "everyone"]
      if List.mem(visible, valid_visibility) then
        params = [("name", name)]
                 |> _add_opt_params([("location", location), ("description", description),
                                     ("visible", visible), ("visible", visible),
                                     ("session_key", session_key), ("uid", uid),
                                     ("privacy", privacy)], _)
        generic_call("photos.createAlbum", params, _, _, _)
      else
        _error_msg("Invalid visibility", valid_visibility, _, _, _)

    get(subj_id, aid, pids, session_key) =
      params = [("session_key", session_key), ("subj_id", subj_id), ("aid", aid), ("pids", pids)]
      generic_call("photos.get", params, _, _, _)

    getAlbum(uid, aids, session_key) =
      params = [("session_key", session_key), ("uid", uid), ("aids", aids)]
      generic_call("photos.getAlbum", params, _, _, _)

    getTags(pids, session_key) =
      params = [("session_key", session_key), ("pids", pids)]
      generic_call("photos.getAlbum", params, _, _, _)
  }}

  Status = {{
    get(session_key, uid, limit) =
      params = [("session_key", session_key)]
               |> _add_opt_params([("uid", uid), ("limit", limit)], _)
      generic_call("status.get", params, _, _, _)
  }}

  Stream = {{
    addComment(post_id, comment, uid_or_seskey) =
      params = [("post_id", post_id), ("comment", comment)]
               |> _add_user(uid_or_seskey, _)
      generic_call("stream.addComment", params, _, _, _)

    addLike(post_id, uid_or_seskey) =
      params = _add_opt_params([("post_id", post_id)], [])
               |> _add_user(uid_or_seskey, _)
      generic_call("stream.addLike", params, _, _, _)

    get(session_key, viewer_id, source_ids, start_time, end_time, limit, filter_key, metadata) =
      params = [("session_key", session_key)]
               |> _add_opt_params([("viewer_id", viewer_id), ("source_ids", source_ids),
                    ("start_time", start_time), ("end_time", end_time), ("limit", limit),
                    ("filter_key", filter_key), ("metadata", metadata)], _)
      generic_call("stream.get", params, _, _, _)

    getComments(post_id) =
      params = [("post_id", post_id)]
      generic_call("stream.getComments", params, _, _, _)

    getFilters(uid_or_seskey) =
      params = _add_user(uid_or_seskey, [])
      generic_call("stream.getFilters", params, _, _, _)

    publish(message, attachment, action_links, target_id, uid_or_seskey) =
      params = _add_opt_params([("message", message), ("attachment", attachment),
                 ("action_links", action_links), ("target_id", target_id)], [])
               |> _add_user(uid_or_seskey, _)
      generic_call("stream.publish", params, _, _, _)

    remove(post_id, uid_or_seskey) =
      params = [("post_id", post_id)]
               |> _add_user(uid_or_seskey, _)
      generic_call("stream.remove", params, _, _, _)

    removeComment(comment_id, uid_or_seskey) =
      params = [("comment_id", comment_id)]
               |> _add_user(uid_or_seskey, _)
      generic_call("stream.removeComment", params, _, _, _)

    removeLike(post_id, uid_or_seskey) =
      params = _add_opt_params([("post_id", post_id)], [])
               |> _add_user(uid_or_seskey, _)
      generic_call("stream.removeLike", params, _, _, _)
  }}

  Users = {{
    getLoggedInUser(session_key) =
      params = [("session_key", session_key)]
      generic_call("users.getLoggedInUser", params, _, _, _)

    getInfo(uids, fields, session_key) =
      params = [("uids", FbRest_private._json_int_list(uids)), ("fields", FbRest_private._json_string_list(fields))]
               |> _add_opt_param(("session_key", session_key), _)
      generic_call("users.getInfo", params, _, _, _)

    getStandardInfo(uids, fields) =
      params = [("uids", FbRest_private._json_int_list(uids)), ("fields", FbRest_private._json_string_list(fields))]
      generic_call("users.getStandardInfo", params, _, _, _)

    hasAppPermission(ext_perm, uid) =
      params = [("ext_perm", FbRest_private._perm_to_string(ext_perm))]
               |> _add_opt_param(("uid", uid), _)
      generic_call("users.hasAppPermission", params, _, _, _)

    isAppUser(uid_or_seskey) =
      params = _add_user(uid_or_seskey, [])
      generic_call("users.isAppUser", params, _, _, _)

    isVerified(session_key) =
      params = [("session_key", session_key)]
      generic_call("users.isVerified", params, _, _, _)

    setStatus(status, uid_or_seskey, includes_verb:bool, clear:bool) =
      params = [("clear", _sob(clear)), ("status_includes_verb", _sob(includes_verb))]
               |> _add_opt_params([("status", status)], _)
               |> _add_user(uid_or_seskey, _)
      generic_call("users.setStatus", params, _, _, _)
  }}

}}
