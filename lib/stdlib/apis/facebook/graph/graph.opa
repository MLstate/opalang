package facebook
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
 * Facebook Graph API modules
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination public
 */

import stdlib.apis.common
//import stdlib.apis.facebook
//import stdlib.apis.facebook.lib

/**
 * {1 About this module}
 *
 * This module provides an access to Facebbok Graph API.
 * Important :
 *   Any application using this must respect
 *   {{:http://developers.facebook.com/policy}Facebook policy}
 * Some of the documentation is extracted from
 * {{:https://developers.facebook.com}Facebook API reference}
 *
 * {1 Where should I start?}
 *
 * You should first {{:https://www.facebook.com/developers/}register an app}
 * to get a application id, an api key and an application secret. You will
 * also have to configure various parameters on your application. Once done,
 * look the module [FbAuth] to obtain an access token from users. You are now
 * ready to use Facebook's Graph API.
 */

/**
 * Options of an object read request [FbGraph.Read.object]
 * or of a multiple objects request [FbGraph.multiple_objects]
 * - [token] Token of a logged user (required to access some private attributes)
 * - [fields] List of the fields you want. If empty, default fields will be
 *   returned (depending on the type of viewed object and of application
 *   authorizations).
 */
type FbGraph.Read.object_options = {
  token    : string
  fields   : list(string)
  metadata : bool
}

/**
 * Type of a parsed metadata
 * Note: You should avoid asking metadata uselessly because an answer with
 * metada is between 5 and 10 times bigger than the same request without
 * metadata.
 * - [obj_type] Type of associated object. There is no other safe way to be
 *   sure of the type of an object.
 * - [connections] Connections of associated object and links to those.
 * - [fields] Name and descriptions of fields available in object. The field
 *   "id" is always returned.
 */
type FbGraph.Read.metadata = {
  obj_type    : string
  connections : list((string,string))
  fields      : list((string,string))
}

/**
 * Type of a facebook object
 * - [id] Only field present in all elements
 * - [data] List of (name, value) corresponding to each field of an object.
 *   Values are in JSON.
 */
type FbGraph.data = {
   id   : string
   data : list((string,RPC.Json.json))
}

/**
 * Result of an object request
 */
type FbGraph.Read.object =
    { object : FbGraph.data } /** Request successful without metadata. Answer is a list of (string,json) elements corresponding to all fields */
  / { object_metadata : (FbGraph.data,FbGraph.Read.metadata) } /** Tuple containing asked object and metadata. */
  / { error : Facebook.error } /** Request failed */

/**
 * Result of an raw request, just the response content.
 */
type FbGraph.Read.raw =
    { content : string location : string content_type : string}
  / { error : Facebook.error }

/**
 * Result of a multiple object request
 */
type FbGraph.Read.objects =
    { objects : list((string,FbGraph.data)) } /** Request successful without metadata. Answer is a list of (string, object) corresponding to requested ids. */
  / { objects_metadata : list((string,FbGraph.data,FbGraph.Read.metadata)) } /** Request successful with metadata. Answer is a list of (string, object, metadata) corresponding to requested id. */
  / { error : Facebook.error } /** Request failed */

/**
 * Size of a picture
 */
type FbGraph.Read.pic_size =
    { square } /** 50x50 px */
  / { small }  /** 50 pixels wide, variable height */
  / { normal } /** 100 pixels wide, variable height */
  / { large }  /** 200 pixels wide, variable height */

/**
 * Paging options
 * - [limit] Number of elements shown
 * - [offset] Number of elements skipped
 * - [until] Date of newest element returned
 * - [since] Date of oldest element returned
 */
type FbGraph.paging_options = {
  limit  : int
  offset : int
  until  : option(Date.date)
  since  : option(Date.date)
}

/**
 * Paging element returned by Facebook
 * - [next] Paging options of next page
 * - [previous] Paging options of previous page
 */
type FbGraph.paging = {
  next     : FbGraph.paging_options
  previous : FbGraph.paging_options
}

/**
 * Type of most elements returned by Facebook in case of success
 * - [data] List of elements corresponding to what was requested
 * - [paging] Paging parameters of request
 */
type FbGraph.data_paging = {
  data   : list(FbGraph.data)
  paging : FbGraph.paging
}

type FbGraph.data_paging_res =
    { success : FbGraph.data_paging }
  / { error : Facebook.error }

/**
 * Type of a place searched using Facebook's search API
 */
type FbGraph.Search.query_place = {
  longitude : float /** Longitude of the center of the seach */
  latitude  : float /** Latitude of the center of the seach */
  distance  : int   /** Distance of the search from the center */
}

/**
 * Type of objects that can be requested using Facebook's search API
 */
type FbGraph.Search.query_type =
    { post } /** Search among all visible posts */
  / { user } /** Search among users. Requires a valid access_token */
  / { page } /** Search among pages */
  / { event } /** Search among events */
  / { group } /** Search among groups */
  / { place : FbGraph.Search.query_place } /** Search among places. Requires a valid access_token */

/**
 * Location of an event
 * See [FbGraph.event] for more information about events
 */
type FbGraph.event_place = {
  street    : string
  city      : string
  state     : string
  zip       : string
  country   : string
  latitude  : float
  longitude : float
}

/**
 * Privacy of an event
 * See [FbGraph.event] for more information about events
 */
type FbGraph.event_privacy =
  { OPEN } / { CLOSED } / { SECRET }

/**
 * Type of a Facebook event
 */
type FbGraph.event = {
  name        : string
  description : string
  start_time  : Date.date
  end_time    : Date.date
  location    : string
  place       : FbGraph.event_place
  privacy     : FbGraph.event_privacy
}

/**
 * Type of a Facebook achievement(instance)
 */
type FbGraph.achievement = {
  achievement : string
  message     : string
}

/**
 * Type of a Facebook album
 */
type FbGraph.album = {
  name     : string
  message  : string
  location : string
  link     : string
}

/**
 * Type of a Facebook photo
 */
type FbGraph.photo = {
  filename     : string
  content_type : string
  source       : string
  message      : string
}

/**
 * Type of a Facebook video
 */
type FbGraph.video = {
  filename     : string
  content_type : string
  source       : string
  title        : string
  description  : string
}

/**
 * Type of a Facebook picture
 */
type FbGraph.picture = {
  filename     : string
  content_type : string
  source       : string
}

/**
 * Type of a Facebook link
 * Default value is [FbGraph.Post.default_link]
 * - [to] ID or username of the profile that this story will be published
 *   to. If this is unspecified, it defaults to "me".
 * - [message] Message of the link.
 * - [link] Link shared
 * - [picture] URL of a picture attached to this link.used.
 * - [name] Name of the link attachment.
 * - [caption] Caption of the link (appears beneath the link name).
 * - [description] Description of the link (appears beneath the link caption).
 */
type FbGraph.link = {
  to          : string
  message     : string
  link        : string
  picture     : string
  name        : string
  caption     : string
  description : string
}

/**
 * Type of results of posting actions
 */
type FbGraph.Post.result =
    { success : string } /** Id of created element if it has one. "Like", for instance, has no id so the success string will be empty.  */
  / { error : Facebook.error } /** Action failed */

type FbGraph.Insight.period = {day} / {week} / {month} / {lifetime}

/**
 * An insight value content
 */
type FbGraph.Insight.value_t =
    { val_int : int } /** Simple integer value */
  / { val_obj : list((string,RPC.Json.json)) } /** More complex json value - Generally an array with various subvalues */

/**
 * An insight value
 * - [value] The value
 * - [end_time] End time of value measure
 */
type FbGraph.Insight.value = {
  value    : FbGraph.Insight.value_t
  end_time : string /* TODO -> Date.date */
}

/**
 * An insight (or statistic) element
 * - [id] Unique ID of insight
 * - [name] Name of insight
 * - [description] Short description of the insight
 * - [period] Duration of statistics measures
 * - [values] Statistic values
 */
type FbGraph.Insight.t = {
  id          : string
  name        : string
  description : string
  period      : FbGraph.Insight.period
  values      : list(FbGraph.Insight.value)
}

type FbGraph.Insight.insights = {
  insights : list(FbGraph.Insight.t)
  paging   : FbGraph.paging
}

type FbGraph.Insight.insights_res =
    { insights : FbGraph.Insight.insights }
  / { error : Facebook.error }

type FbGraph.property_value0('a) =
    { string : string }
  / { int : int }
  / { bool : bool }
  / { object : list((string,'a)) }
  / { array : list('a) }
type FbGraph.property_value =  FbGraph.property_value0(FbGraph.property_value)

type FbGraph.property = (string,FbGraph.property_value)

type FbGraph.properties = list(FbGraph.property)

type FbGraph.object_type = {user} / {permissions} / {page}

type FbGraph.order_status = {placed} / {settled} / {refunded} / {disputed} / {cancelled}

FbGraph = {{

  /* Static */

  @private graph_url  = "https://graph.facebook.com"
  @private video_url  = "https://graph-video.facebook.com"

  /* Generic private functions */

  @private build_data(all_data:list((string,RPC.Json.json))) =
    id = FbLib.fetch_json_string("id", all_data)
    data = List.filter(((a,_)->a!="id"),all_data)
    { ~id ~data }

  /* Paging */

  /**
   * Default paging element - Will returns default display
   */
  default_paging = {
    limit  = 0
    offset = 0
    until  = none
    since  = none
  } : FbGraph.paging_options

  no_paging = {
    next = default_paging
    previous = default_paging
  } : FbGraph.paging

  @private parse_paging_field(name, data) : FbGraph.paging_options =
    match List.assoc(name, data) with
    | {some={String=s}} ->
      second(l) = List.get(1, l) ? ""
      params = String.explode("?", s) |> second |> API_libs.get_data
      get_int(name) = match List.assoc(name,params) with
        | {some=s} -> Int.of_string(s)
        | {none} -> 0
      get_date(name) = match List.assoc(name,params) with
        | {some=s} -> {some=Date.milliseconds(Int.of_string(s) * 1000)}
        | {none} -> none
      {
        limit  = get_int("limit")
        offset = get_int("offset")
        until  = get_date("until")
        since  = get_date("since")
      }
    | _ -> default_paging

  @private parse_paging(data) : FbGraph.paging =
    match List.assoc("paging", data) with
    | {some={Record=p}} ->
      next = parse_paging_field("next", p)
      previous = parse_paging_field("previous", p)
      { ~next ~previous }
    | _ -> no_paging

  @private prepare_paging(options) =
    sub_int(n) = if n>0 then "{n}" else ""
    FbLib.add_if_filled("limit", sub_int(options.limit), [])
    |> FbLib.add_if_filled("offset", sub_int(options.offset), _)
    |> FbLib.add_if_filled("until", (Option.map(Date.to_string,options.until))?"", _)
    |> FbLib.add_if_filled("since", (Option.map(Date.to_string,options.since))?"", _)

  @private check_for_error(data, on_ok, on_error) =
    match List.assoc("error", data) with
    | {some={Record=r}} ->
       Facebook.make_error(FbLib.fetch_json_string("type", r),
                           FbLib.fetch_json_string("message", r))
       |> on_error
    | _ -> on_ok(data)

  @private build_data_paging(r) : FbGraph.data_paging_res =
    match Json.of_string(r) with
    | {none} -> { error = Facebook.parse_error(r) }
    | {some={Record=r}} ->
      on_ok(x) =
        aux(v,acc) = match v with
          | {Record=o} -> List.add(build_data(o), acc)
          | _ -> acc
        data = match List.assoc("data", x) with
          | {some={List=l}} -> List.fold(aux,l,[])
          | _ -> []
        paging = parse_paging(x)
        { success = { ~data ~paging } }
      check_for_error(r, on_ok, (x -> { error = x }))
    | _ -> { error = Facebook.data_error }

  generic_return(res) : FbGraph.Post.result =
    match res with
    | {none} -> { error = Facebook.network_error }
    | {some=r} ->
      match Json.of_string(r) with
      | {none} -> { error = Facebook.parse_error(r) }
      | {some={Record=r}} ->
        on_ok(x:list((string,RPC.Json.json))) =
          id = FbLib.fetch_json_string("id", x)
          { success = id }
        check_for_error(r, on_ok, (y -> { error = y }))
      | {some={Bool=r}} ->
        if r then { success = "" }
        else { error = Facebook.data_error }
      | {some={Int=number}} -> {success="{number}"}
      | _ -> { error = Facebook.data_error }
      end

  @private @publish generic_action(path, data, token) : FbGraph.Post.result =
    data = List.add(("access_token", token), data)
    generic_return(FbLib.fb_post(graph_url, path, data))

  @private @publish generic_action_multi(path, forms, token) : FbGraph.Post.result =
    forms = List.add({form=("access_token", token)}, forms)
    generic_return(FbLib.fb_post_multi(graph_url, path, forms))

  @private @publish generic_action_multi_base(base, path, forms, token) : FbGraph.Post.result =
    forms = List.add({form=("access_token", token)}, forms)
    generic_return(FbLib.fb_post_multi(base, path, forms))

  @private @publish generic_delete(path, data, token) : FbGraph.Post.result =
    data = List.add(("access_token", token), data)
    generic_return(FbLib.fb_delete(graph_url, path, data))

  string_of_object_type(ot:FbGraph.object_type) =
    match ot with
    | {user} -> "user"
    | {permissions} -> "permissions"
    | {page} -> "page"

  multiuser(id, users, pathname, fieldname) =
    match users with
    | [user_id] -> ("/{id}/{pathname}/{user_id}",[])
    | _ -> ("/{id}/{pathname}",[(fieldname,String.concat(",",users))])

  /* Submodules */

  Delete = {{

    /**
     * Deletes object [id]
     */
    object(id, token) =
      generic_delete("/{id}", [], token)

    /**
     * Delete achievement(instance) object [id]
     */
    achievement(id, token) =
      generic_delete("/{id}/achievements", [], token)

    /**
     * Unlike object [id] (works with various types of id)
     */
    unlike(id, token) =
      generic_delete("/{id}/likes", [], token)

    /**
     * Unban a user from an application.  The token must be an application access token.
     */
    unban(user_id, token) =
      generic_delete("/{user_id}", [], token)

    /** Remove currency association from app. */
    payment_currency(app_id, currency_url, token) =
      generic_delete("/{app_id}", [("currency_url",currency_url)], token)

    /** Remove user role from app. */
    role(app_id, user, token) =
      generic_delete("/{app_id}/roles", [("user",user)], token)

    /** Delete subscriptions. */
    subscriptions(app_id, object:option(FbGraph.object_type), token) =
      data = FbLib.add_if_filled_opt("object", string_of_object_type, object, [])
      generic_delete("/{app_id}/subscriptions", data, token)

    /** Delete translations.
     * The native_hashes actually come from the "Translations" app.
     */
    translations(app_id, native_hashes:list((string,string)), token) =
      data = [("native_hashes",List.to_string_using("[","]",",",List.map(((s,d) -> "\{\"{s}\":\"{d}\"}"),native_hashes)))]
      generic_delete("/{app_id}/translations", data, token)

    /** Delete all scores. */
    scores(app_id, token) = generic_delete("/{app_id}/scores", [], token)

    /** Delete a comment. */
    comment(comment_id, token) = generic_delete("/{comment_id}", [], token)

    /** Uninvite a user. */
    invited(event_id, user_id, token) = generic_delete("/{event_id}/invited/{user_id}", [], token)

    /** Delete picture. */
    picture(id, token) = generic_delete("/{id}/picture", [], token)

    /** Delete friendlist. */
    friendlist = object

    /** Delete members. */
    members(id, users, token) =
      (path, data) = multiuser(id, users, "members", "members")
      generic_delete(path, data, token)

  }}

  Insights = {{

    @private period_from_string(s) : FbGraph.Insight.period =
      match s with
      | "lifetime" -> {lifetime}
      | "month"    -> {month}
      | "week"     -> {week}
      | "day" | _  -> {day}

    @private period_to_string(s) =
      match s:FbGraph.Insight.period with
      | {lifetime} -> "lifetime"
      | {month}    -> "month"
      | {week}     -> "week"
      | {day}      -> "day"

    @private value_from_json(json:RPC.Json.json) : option(FbGraph.Insight.value) =
      match json with
      | {Record=l} ->
        end_time = FbLib.fetch_json_string("end_time", l)
        value = match List.assoc("value", l) with
        | {some={Record=l}} -> {val_obj=l}
        | {some={Int=i}}    -> {val_int=i}
        | _                 -> {val_obj=[]}
        end
        {some = { ~value ~end_time }}
      | _ -> none

    @private build_insight(o:FbGraph.data) : FbGraph.Insight.t =
      aux(v,acc) = match value_from_json(v) with
        | {some=value} -> List.add(value, acc)
        | {none} -> acc
      values = match List.assoc("values", o.data) with
      | {some={List=l}} -> List.fold(aux,l,[])
      | _ -> []
      get(name) = FbLib.fetch_json_string(name, o.data)
      { id = o.id
        name = get("name")
        description = get("description")
        period = period_from_string(get("period"))
        values = values }

    @private build_insights(o) : FbGraph.Insight.insights_res =
      match build_data_paging(o) : FbGraph.data_paging_res with
      | {error=e} -> { error = e }
      | {success=s} ->
        insights = List.rev(List.map(build_insight, s.data))
        { insights = {insights=insights
                      paging=s.paging} }

    @private generic_insights(path, token, paging) : FbGraph.Insight.insights_res =
      data = prepare_paging(paging)
        |> FbLib.add_if_filled("access_token", token, _)
      match FbLib.fb_get(graph_url, path, data) with
      | {none} -> { error = Facebook.network_error }
      | {some=r} -> build_insights(r)

    /**
     * Get all statistics available about [elt_id] element
     * Unlike most other API calls, the token used can be an application token,
     * you can obtain one using [FbAuth.app_login]. You can also use a user token
     * with the permission [manage_pages] and manage this user's elemnts.
     */
    full(elt_id, token, paging) : FbGraph.Insight.insights_res =
      generic_insights("/{elt_id}/insights", token, paging)

    /**
     * Get statistics on given metrics for your application
     * A list of available metrics is available in
     * {{:https://developers.facebook.com/docs/reference/fql/insights/}metrics documentation}
     * Unlike most other API calls, the token used can be an application token,
     * you can obtain one using [FbAuth.app_login]. You can also use a user token
     * with the permission [manage_pages] and manage this user's elemnts.
     */
    metric(metric, elt_id, token, paging) : FbGraph.Insight.insights_res =
      generic_insights("/{elt_id}/insights/{metric}", token, paging)

    /**
     * Get statistics on given metrics and in a given priod for your application
     * A list of available metrics is available at :
     * {{:https://developers.facebook.com/docs/reference/fql/insights/}metrics documentation}
     * Note that not all metrics support all periods. You should refer to the
     * documentation for more information.
     * Unlike most other API calls, the token used can be an application token,
     * you can obtain one using [FbAuth.app_login]. You can also use a user token
     * with the permission [manage_pages] and manage this user's elemnts.
     */
    metric_period(metric, period, elt_id, token, paging) : FbGraph.Insight.insights_res =
      generic_insights("/{elt_id}/insights/{metric}/{period_to_string(period)}", token, paging)

  }}

  Post = {{

    string_of_property_value(pv:FbGraph.property_value) =
      match pv with
      | {~string} -> "\"{string}\""
      | {~int} -> "{int}"
      | {~bool} -> "{bool}"
      | {~object} -> List.to_string_using("\{","}",",",List.map(((n,v) -> "\"{n}\":{string_of_property_value(v)}"),object))
      | {~array} -> List.to_string_using("[","]",",",List.map(string_of_property_value,array))

    options_of_properties(properties:FbGraph.properties) =
      List.map(((n,pv) -> (n,string_of_property_value(pv))),properties)

    generic_properties(id, properties, token) =
      generic_action("/{id}", options_of_properties(properties), token)

    /**
     * Create achievement(instance) for [user_id]
     */
    achievement(user_id, achievement:FbGraph.achievement, token) =
      data = FbGraph_to_string.achievement_to_data(achievement)
      generic_action("/{user_id}/achievements", data, token)

    /**
     * Create migration for [app_id], [name]:[value]
     */
    migration(app_id, name, value:int, token) =
      properties = [("migrations",{object=[(name,{int=(if value == 0 then 0 else 1)})]})]
      generic_properties(app_id, properties, token)

    /**
     * Create restriction for [app_id], [name]:"[value]"
     */
    restriction(app_id, name, value:string, token) =
      properties = [("restrictions",{object=[(name,{string=value})]})]
      generic_properties(app_id, properties, token)

    /**
     * Modify properties for [app_id]
     */
    properties(app_id, properties, token) =
      generic_properties(app_id, properties, token)

    /**
     * Create a test user for [app_id].
     * NOTE: uses the application access token *NOT* the application Page access token.
     */
    test_user(app_id, installed:option(bool), permissions:list(Facebook.permission), name:string, token) =
      data = 
        FbLib.add_if_filled("installed", Option.default("",Option.map(Bool.to_string,installed)), [])
        |> FbLib.add_if_filled("permissions", String.concat(",",List.map(Facebook.permission_to_string,permissions)), _)
        |> FbLib.add_if_filled("name", name, _)
      generic_action("/{app_id}/accounts/test-users", data, token)

    ban(app_id, users, token) =
      data = [("uid",String.concat(",",users))]
      generic_action("/{app_id}/banned", data, token)

    simple_link(app_id, link, message, token) =
      data =
        [("link", link)]
        |> FbLib.add_if_filled("message", message, _)
      generic_action("/{app_id}/feed", data, token)

    payment_currency(app_id, currency_url, token) =
      data = [("currency_url", currency_url)]
      generic_action("/{app_id}/payment_currencies", data, token)

    /**
     * Create a post on an application's profile page.
     * Token needs [publish_stream] permissions.
     * Works on various ids.
     */
    post(id, p:Facebook.post, token) =
      data =
        [("message",p.message),("link", p.link)]
        |> FbLib.add_if_filled("picture", p.picture, _)
        |> FbLib.add_if_filled("name", p.name, _)
        |> FbLib.add_if_filled("caption", p.caption, _)
        |> FbLib.add_if_filled("description", p.description, _)
        |> FbLib.add_array_if_filled("actions", FbLib.actions_to_json, p.actions, _)
        |> FbLib.add_if_filled("privacy", p.privacy, _)
      generic_action("/{id}/feed", data, token)

    /** Add user role for app. */
    role(app_id, user, role, token) =
      data = [("user",user),("role", role)]
      generic_action("/{app_id}/roles", data, token)

    /** Post a status message on an object.  Various objects supported. */
    status(id, message, token) =
      data = [("message",message)]
      generic_action("/{id}/feed", data, token)

    /**
     * Set up a subscription.
     * Needs application page access token.
     */
    subscriptions(app_id, ot:FbGraph.object_type, fields:Facebook.properties, callback_url:string, verify_token:string, token) =
      data =
        [("object",string_of_object_type(ot))]
        |> FbLib.add_json("fields", FbLib.fb_properties_to_json, fields, _)
        |> List.add(("callback_url",callback_url),_)
        |> FbLib.add_if_filled("verify_token", verify_token, _)
      generic_action("/{app_id}/subscriptions", data, token)

    /**
     * Upload application strings for translation.
     * Needs application page access token.
     */
    native_strings_to_json(native_strings:list({text:string; description:string})) : RPC.Json.json =
      aux(ns) : RPC.Json.json =
        {Record=[("text", {String=ns.text}:RPC.Json.json),
                 ("description", {String=ns.description}:RPC.Json.json)]}
      {List=List.map(aux, native_strings)}

    translations(app_id, native_strings:list({text:string; description:string}), token) =
      data = FbLib.add_json("native_strings",native_strings_to_json,native_strings,[])
      generic_action("/{app_id}/translations", data, token)

    /**
     * Requires multipart/form-data forms.
     * NOTE: should also work with USER_ID and EVENT_ID (and maybe others).
     * NOTE: this currently doesn't work because of facebook internal errors...
     */
    photos(album_id, photo, token) =
      forms = FbGraph_to_string.photo_to_form(photo)
      generic_action_multi("/{album_id}/photos", forms, token)

    /**
     * Publish a video to an Application.
     * Also works with event_id.
     * Requires multipart/form-data forms.
     * NOTE: this currently doesn't work because of facebook internal errors...
     */
    videos(page_id, v:FbGraph.video, token) =
      forms =
        [{file=("source", v.filename, v.content_type, v.source)}]
        |> FbLib.add_form_if_filled("title", v.title, _)
        |> FbLib.add_form_if_filled("description", v.description, _)
      generic_action_multi_base(video_url, "/{page_id}/videos", forms, token)

    /**
     * Requires multipart/form-data forms.
     * NOTE: this currently doesn't work because of facebook internal errors...
     */
    picture(id, p:FbGraph.picture, token) =
      forms = [{file=("source", p.filename, p.content_type, p.source)}]
      generic_action_multi("/{id}/picture", forms, token)

    users_generic(id, users:list(string), pathname, fieldname, token) =
      if users == []
      then {error=Facebook.make_error("FbGraph.Post.{pathname}","No users")}
      else
        (path,data) = multiuser(id, users, pathname, fieldname)
        generic_action(path, data, token)

    /**
     * Invite users to an event.
     * Needs [create_event] permission.
     */
    invited(event_id, invited:list(string), token) = users_generic(event_id, invited, "invited", "users", token)

    /**
     * Create a FriendList for a user.
     * Maximum name length is 25 chars.
     * Needs manage_friendlists permission.
     */
    friendlist(profile_id, name, token) =
      if String.length(name) > 25
      then {error=Facebook.make_error("friendlist","Name longer than 25 characters")}
      else
        data = [("name",name)]
        generic_action("/{profile_id}/friendlists", data, token)

    /**
     * Invite users to an event.
     * Needs [create_event] permission.
     */
    members(friendlist_id, members:list(string), token) = users_generic(friendlist_id, members, "members", "members", token)

    string_of_order_status(os:FbGraph.order_status) =
      match os with
      | {placed} -> "placed" 
      | {settled} -> "settled" 
      | {refunded} -> "refunded" 
      | {disputed} -> "disputed" 
      | {cancelled} -> "cancelled"

    /**
     * Update an order.
     * Docs don't say which kind of token/permission is needed.
     * The params value is: "optional JSON-encoded dictionary {'comment' => }", however that is encoded.
     */
    order(id, status:FbGraph.order_status, message:string, params:string, token) =
      data =
        [("status",string_of_order_status(status))]
        |> List.add(("message",message),_)
        |> FbLib.add_if_filled("params", params, _)
      generic_action("/{id}", data, token)

    /**
     * Post a new element in user's feed
     * You can control the target of the feed with [feed.to].
     */
    feed(feed:Facebook.feed, token) =
      to = if feed.to == "" then "me" else feed.to
      data = FbLib.feed_to_data(feed, false)
      generic_action("/{to}/feed", data, token)

    /**
     * Post a comment on [object_id] element on behalf of logged used
     */
    comment(object_id, message, token) =
      data = [("message", message)]
      generic_action("/{object_id}/comments", data, token)

    /**
     * Like [object_id] element on behalf of logger user (works with various types of id)
     */
    like(object_id, token) =
      generic_action("/{object_id}/likes", [], token)

    /**
     * Post a note
     */
    note(profile_id, subject, message, token) =
      data = [("subject", subject), ("message", message)]
      generic_action("/{profile_id}/notes", data, token)

    /**
     * Create a simple link with only the [link] field set
     */
    default_link(link) = {
      to          = ""
      message     = ""
      link        = link
      picture     = ""
      name        = ""
      caption     = ""
      description = ""
    } : FbGraph.link

    /**
     * Post a link
     */
    link(link:FbGraph.link, token) =
      to = if link.to == "" then "me" else link.to
      data = FbGraph_to_string.link_to_data(link)
      generic_action("/{to}/links", data, token)

    empty_place = {
      street    = ""
      city      = ""
      state     = ""
      zip       = ""
      country   = ""
      latitude  = 0.
      longitude = 0.
    } : FbGraph.event_place

    /**
     * Create a simple event with only the [name] field set
     */
    simple_event(name) = {
      name        = name
      description = ""
      start_time  = Date.now()
      end_time    = Date.shift_forward(Date.now(),Duration.h(1))
      location    = ""
      place       = empty_place
      privacy     = { OPEN }
    } : FbGraph.event

    /**
     * Post an event
     */
    event(profile_id, event:FbGraph.event, token) =
      data = FbGraph_to_string.event_to_data(event)
      generic_action("/{profile_id}/events", data, token)

    /**
     * Report current user as attending an event
     */
    rsvp_attending(event_id, token) =
      generic_action("/{event_id}/attending", [], token)

    /**
     * Report current user as maybe attending an event
     */
    rsvp_maybe(event_id, token) =
      generic_action("/{event_id}/maybe", [], token)

    /**
     * Report current user as not attending an event
     */
    rsvp_declined(event_id, token) =
      generic_action("/{event_id}/declined", [], token)

    /**
     * Create a simple album with only the [name] field set
     */
    simple_album(name) = {
      name     = name
      message  = ""
      location = ""
      link     = ""
    }

    /**
     * Post an album
     */
    album(profile_id, album, token) =
      data = FbGraph_to_string.album_to_data(album)
      generic_action("/{profile_id}/albums", data, token)

    /* NOTE: Publishing a Checkin object is deprecated in favor of creating a Post with a location attached.
    checkins(profile_id, checkin)
    */

  }}

  Read = {{

    @private prepare_object_option(o) =
      FbLib.add_if_filled("access_token", o.token, [])
      |> FbLib.add_if_filled("fields", String.concat(",",o.fields), _)
      |> FbLib.add_if_filled("metadata", (if o.metadata then "1" else ""), _)

    empty_metadata = {
      obj_type    = ""
      connections = []
      fields      = []
    } : FbGraph.Read.metadata

    @private extract_metadata(l:list((string,RPC.Json.json))) =
      aux(e:RPC.Json.json,acc:list((string,string))) =
        match e with
        | {Record=r} ->
          name = FbLib.fetch_json_string("name", r)
          if name == "" then acc
          else List.add((name, FbLib.fetch_json_string("description", r)), acc)
        | _ -> acc
      match List.assoc("metadata", l) with
      | {some={Record=m}} ->
        t = FbLib.fetch_json_string("type", m)
        c = match List.assoc("connections", m) with
        | {some={Record=m}} ->
          aux((n,v)) = (n,FbLib.get_json_string(v))
          List.map(aux, m)
        | _ -> []
        end
        f = match List.assoc("fields", m) with
        | {some={List=l}} ->
          List.fold(aux,l,[])
        | _ -> []
        end
        ll = List.filter(((a,_)->a!="metadata"),l)
        (build_data(ll), { obj_type    = t
                           connections = c
                           fields      = f })
      | _ -> (build_data(l), empty_metadata)

    default_object = {
      token    = ""
      fields   = []
      metadata = false
    } : FbGraph.Read.object_options

    /**
     * Get information about the object with given id, results depends of options
     * If no token is provided, only public information is available. Else,
     * fields available will vary according to the permissions of your
     * application.
     * In case of success, if [options.metadata] is true, the result will be
     * [object_metadata]. Else it will be [object].
     */
    object(id, options) : FbGraph.Read.object =
      data = prepare_object_option(options)
      match FbLib.fb_get(graph_url, "/{id}", data) with
      | {none} -> { error = Facebook.network_error }
      | {some=r} ->
        match Json.of_string(r) with
        | {none} -> { error = Facebook.parse_error(r) }
        | {some={Record=r}} ->
          on_ok(x:list((string,RPC.Json.json))) =
            if options.metadata then
              { object_metadata = extract_metadata(x) }
            else { object = build_data(x) }
          check_for_error(r, on_ok, (y -> { error = y }))
        | {some={Bool=b}} ->
          if b then { error = Facebook.data_error }
          else { error = Facebook.access_denied }
        | _ -> { error = Facebook.data_error }
        end

    redirect(id, options) =
      data = prepare_object_option(options)
      FbLib.fb_get_redirect(graph_url, "/{id}", data)

    /**
     * Return the raw content of a request.
     * Eg. profile pictures return jpeg data.
     */
    raw(id, options) : FbGraph.Read.raw =
      data = prepare_object_option(options)
      match FbLib.fb_get_ct(graph_url, "/{id}", data) with
      | {none} -> { error = Facebook.network_error }
      | {some=(content_type,location,content)} -> ~{ content; location; content_type }

    /**
     * Get information about objects with given ids, Results depends of options
     * If no token is provided, only public information is available. Else,
     * fields available will vary according to the permissions of your
     * application.
     * Asked objects do not have to be of the same type. However, if
     * [options.fields] is set, *all fields* must be in *all objects* or the
     * request will fail.
     * In case of success, if [options.metadata] is true, the result will be
     * [objects_metadata]. Else it will be [objects].
     */
    multiple_objects(ids, options) : FbGraph.Read.objects =
      if ids == [] then { error = Facebook.data_error }
      else
        data = prepare_object_option(options)
          |> List.add(("ids",String.concat(",",ids)),_)
        match FbLib.fb_get(graph_url, "", data) with
        | {none} -> { error = Facebook.network_error }
        | {some=r} ->
          match Json.of_string(r) with
          | {none} -> { error = Facebook.parse_error(r) }
          | {some={Record=r}} ->
            aux((k,v),acc) = match v with
              | {Record=r} -> List.add((k,r), acc)
              | _ -> acc
            on_ok(x:list((string,RPC.Json.json))) =
              subres = List.fold(aux,x,[])
              if options.metadata then
                aux((k,v)) =
                  (w,m) = extract_metadata(v)
                  (k,w,m)
                { objects_metadata = List.map(aux,subres) }
              else
                aux((k,v)) = (k, build_data(v))
                { objects = List.map(aux,subres) }
            check_for_error(r, on_ok, (x -> { error = x }))
          | _ -> { error = Facebook.data_error }
          end

    /**
     * Get data about one connection of an element
     * All connections available for an element are available in the
     * metadata of this elelement
     */
    connection(id, connection, token, paging) : FbGraph.data_paging_res =
      data = prepare_paging(paging)
        |> FbLib.add_if_filled("access_token", token, _)
      match FbLib.fb_get(graph_url, "/{id}/{connection}", data) with
      | {none} -> { error = Facebook.network_error }
      | {some=r} -> build_data_paging(r)

    /**
     * Get all permissions available for current app with current user
     */
    permissions(token) : list(Facebook.permission) =
      match connection("me", "permissions", token, default_paging) with
      | {error=_}   -> []
      | {success=s} ->
        if s.data == [] then []
        else
          aux((p,_),acc) =
            match Facebook.permission_of_string(p) with
            | {some=p} -> List.add(p,acc)
            | {none} -> acc
          List.fold(aux,List.head(s.data).data,[])

    /**
     * Simply returns an url to given object with given size.
     */
    picture_url(id, size) : string =
      data = [("type",FbGraph_to_string.pic_size_to_string(size))]
      FbLib.generic_build_path("{graph_url}/{id}/picture", data)

    /**
     * Performs a read on [app_id] with added fields="migrations".
     */
    migrations(app_id, options) = object(app_id, {options with fields=List.add("migrations",options.fields)})

    /**
     * Performs a read on [app_id] with added fields="restrictions".
     */
    restrictions(app_id, options) = object(app_id, {options with fields=List.add("restrictions",options.fields)})

    /**
     * Returns a list of properties for an app.
     */
    properties(app_id, options, properties:list(Facebook.property)) =
      props = String.concat(",",List.map(Facebook.string_of_property,properties))
      object(app_id, {options with fields=List.add(props,options.fields)})

    banned(app_id, options) = object("{app_id}/banned", options)
    is_banned(app_id, user_id, options) = object("{app_id}/banned/{user_id}", options)
    payment_currency(app_id, options) = object("{app_id}/payment_currencies", options)
    roles(app_id, options) = object("{app_id}/roles", options)
    scores(app_id, options) = object("{app_id}/scores", options)
    invited(event_id, options) = object("{event_id}/invited", options)
    is_invited(event_id, user_id, options) = object("{event_id}/invited/{user_id}", options)
    attending(event_id, options) = object("{event_id}/attending", options)
    is_attending(event_id, user_id, options) = object("{event_id}/attending/{user_id}", options)
    maybe(event_id, options) = object("{event_id}/maybe", options)
    is_maybe(event_id, user_id, options) = object("{event_id}/maybe/{user_id}", options)
    declined(event_id, options) = object("{event_id}/declined", options)
    is_declined(event_id, user_id, options) = object("{event_id}/declined/{user_id}", options)
    noreply(event_id, options) = object("{event_id}/noreply", options)
    photos(id, options) = object("{id}/photos", options)
    picture(id, options) = redirect("{id}/photos", options)
    friendlist = object

  }}

  Search = {{

    empty_query_res = {
      data = []
      paging = no_paging
    }

    /**
     * Make a search query for [query] among elements of type [stype]
     * Some types of requests require a valid token.
     */
    query(query, stype, token, paging) : FbGraph.data_paging_res =
      data = prepare_paging(paging)
        |> List.append(FbGraph_to_string.search_query_type_to_data(stype),_)
        |> List.append([("q",query)],_)
        |> FbLib.add_if_filled("access_token", token, _)
      match FbLib.fb_get(graph_url, "/search", data) with
      | {none} -> { error = Facebook.network_error }
      | {some=r} -> build_data_paging(r)

    /**
     * Make a search for [query] in user's home feed.
     */
    home_query(query, token, paging) : FbGraph.data_paging_res =
      data = prepare_paging(paging)
        |> List.append([("q",query)],_)
        |> FbLib.add_if_filled("access_token", token, _)
      match FbLib.fb_get(graph_url, "/me/home", data) with
      | {none} -> { error = Facebook.network_error }
      | {some=r} -> build_data_paging(r)

  }}

  Fql = {{

    /**
     * Make a FQL Query
     */
    query(query:string, token) =
      data = [("access_token", token), ("q", query)]
      match FbLib.fb_get(graph_url, "/fql", data) with
      | {none} -> { error = Facebook.network_error }
      | {some=r} ->
        match Json.of_string(r) with
        | {none} -> { error = Facebook.parse_error(r) }
        | {some={Record=r}} ->
          on_ok(x:list((string,RPC.Json.json))) =
            match List.assoc("data", x) with
            | {none} -> { error = Facebook.data_error }
            | {some=d} -> { success = d }
            end
          check_for_error(r, on_ok, (y -> { error = y }))
        | _ -> { error = Facebook.data_error }
        end
      

  }}

}}

/* to_string functions */
@private FbGraph_to_string = {{

  search_query_type_to_data(t) =
    match t:FbGraph.Search.query_type with
    { post }  -> [("type","post")]
    { user }  -> [("type","user")]
    { page }  -> [("type","page")]
    { event } -> [("type","event")]
    { group } -> [("type","group")]
    { place=p } -> [("type","event"), ("distance","{p.distance}"),
                    ("center","{p.latitude},{p.longitude}")]

  pic_size_to_string(s) =
    match s:FbGraph.Read.pic_size with
    { square } -> "square"
    { small }  -> "small"
    { normal } -> "normal"
    { large }  -> "large"

  privacy_to_string(p) =
    match p:FbGraph.event_privacy with
    { OPEN }   -> "OPEN"
    { CLOSED } -> "CLOSED"
    { SECRET } -> "SECRET"

  place_to_string(p:FbGraph.event_place) =
    if p == FbGraph.Post.empty_place then ""
    else
      f(n,v) = (n, {String=v}:RPC.Json.json)
      g(n,v) = (n, {Float=v}:RPC.Json.json)
      { Record=[ f("street",p.street), f("city",p.city),
                 f("state",p.state), f("zip",p.zip),
                 f("country",p.country), g("latitude",p.latitude),
                 g("longitude",p.longitude) ]
      }:RPC.Json.json |> Json.serialize

  link_to_data(link) =
    FbLib.add_if_filled("message", link.message, [])
    |> FbLib.add_if_filled("link", link.link, _)
    |> FbLib.add_if_filled("picture", link.picture, _)
    |> FbLib.add_if_filled("name", link.name, _)
    |> FbLib.add_if_filled("caption", link.caption, _)
    |> FbLib.add_if_filled("description", link.description, _)

  event_to_data(e:FbGraph.event) =
    aux_date(date) =
      date = Date.in_milliseconds(date) / 1000
      Int.to_string(date)
    [("name", e.name), ("venue", place_to_string(e.place)),
     ("privacy", privacy_to_string(e.privacy))]
    |> FbLib.add_if_filled("description", e.description, _)
    |> FbLib.add_if_filled("location", e.location, _)
    |> FbLib.add_if_filled("start_time", aux_date(e.start_time), _)
    |> FbLib.add_if_filled("end_time", aux_date(e.end_time), _)

  achievement_to_data(a:FbGraph.achievement) =
    [("achievement", a.achievement)]
    |> FbLib.add_if_filled("message", a.message, _)

  album_to_data(a:FbGraph.album) =
    [("name", a.name)]
    |> FbLib.add_if_filled("message", a.message, _)
    |> FbLib.add_if_filled("location", a.location, _)
    |> FbLib.add_if_filled("link", a.link, _)

  photo_to_form(p:FbGraph.photo) =
    [{file=("source", p.filename, p.content_type, p.source)}]
    |> FbLib.add_form_if_filled("message", p.message, _)

}}
