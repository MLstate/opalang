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
 * Author    : Nicolas Glondu <nicolas.glondu@mlstate.com>
 **/

/**
 * Facebook library module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination private
 */

import stdlib.apis.common
import stdlib.apis.facebook

/**
 * {1 About this module}
 *
 * This module contains some generic functions used in all the Facebook modules.
 * I tshould not be used outside of the [facebook] package
 */

FbLib = {{

  generic_build_path(path, options) =
    if options == [] then path
    else "{path}?{API_libs.form_urlencode(options)}"

  /**
   * Make a HTTP GET on [path] at [base] with [data]
   */
  fb_get(base, path, data) =
    final_path = generic_build_path("{base}{path}", data)
    match Uri.of_string(final_path) with
    | {none} -> none
    | {some=uri} ->
      match WebClient.Get.try_get(uri) with
      | {failure=_} -> none
      | {success=s} -> {some=s.content}
      end

  /**
   * Make a HTTP GET on [path] at [base] with [data]
   */
  fb_post(base, path, data) =
    txtdata = API_libs.form_urlencode(data)
    match Uri.of_string("{base}{path}") with
    | {none} -> none
    | {some=uri} ->
      match WebClient.Post.try_post(uri,txtdata) with
      | {failure=_} -> none
      | {success=s} -> {some=s.content}
      end

  /* Generic JSON functions */

  get_json_string(s:RPC.Json.json) =
    match s with
    | {String=s} -> s
    | _ -> ""

  fetch_json_string(name, data) =
    match List.assoc(name, data) with
    | {some={String=s}} -> s
    | _ -> ""

  add_if_filled(name, v, data) =
    if v == "" then data
    else List.add((name, v), data)

 /* Feed to data */

   @private actions_to_json(actions) : RPC.Json.json =
    aux(l:Facebook.feed_link) : RPC.Json.json =
      {Record=[("name", {String=l.text}:RPC.Json.json),
               ("link", {String=l.href}:RPC.Json.json)]}
    {List=List.map(aux, actions)}

  @private properties_to_json(properties) : RPC.Json.json =
    sub(l:Facebook.feed_link) : RPC.Json.json =
      {Record=[("text", {String=l.text}:RPC.Json.json),
               ("href", {String=l.href}:RPC.Json.json)]}
    aux(x) : RPC.Json.json =
      match x : Facebook.feed_property with
      | {simple=s} -> {String=s}
      | {link=l}   -> sub(l)
    {List=List.map(aux, properties)}

  feed_to_data(feed, with_from) =
    properties_text =
      if feed.properties == [] then ""
      else properties_to_json(feed.properties) |> Json.serialize
    actions_text =
      if feed.actions == [] then ""
      else actions_to_json(feed.actions) |> Json.serialize
    ( if with_from then
        add_if_filled("from", feed.from, [])
        |> add_if_filled("to", feed.to, _)
      else [] )
    |> add_if_filled("message", feed.message, _)
    |> add_if_filled("link", feed.link, _)
    |> add_if_filled("picture", feed.picture, _)
    |> add_if_filled("source", feed.source, _)
    |> add_if_filled("name", feed.name, _)
    |> add_if_filled("caption", feed.caption, _)
    |> add_if_filled("description", feed.description, _)
    |> add_if_filled("properties", properties_text, _)
    |> add_if_filled("actions", actions_text, _)

}}
