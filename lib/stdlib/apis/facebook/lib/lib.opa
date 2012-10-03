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
 * Facebook library module
 *
 * @category api
 * @author Nicolas Glondu, 2011
 * @destination private
 */

import stdlib.apis.common
//import stdlib.apis.facebook

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
  @private fb_get_(base, path, data, process) =
    final_path = generic_build_path("{base}{path}", data)
    do jlog("GET {final_path}")
    match Uri.of_string(final_path) with
    | {none} -> none
    | {some=uri} -> process(WebClient.Get.try_get(uri))

  @private content_only(res) =
    match res with
    | {failure=_} -> none
    | {success=s} -> do jlog("HTTP {s.code}\n{s.content}") {some=s.content}

  // header_get doesn't work on node.
  find_header(name,headers) =
    List.fold((h, acc ->
               if String.has_prefix(name,h)
               then
                 len = String.length(name)
                 {some=String.sub(len+2,String.length(h)-(len+2),h)}
               else acc),headers,none)

  @private content_with_type(res) =
    match res with
    | {failure=_} -> none
    | {success=s} ->
       do jlog("hdrs=\n{String.concat("\n",s.headers)}")
       content_type = Option.default("unknown/unknown",find_header("content-type",s.headers))
       location = Option.default("",find_header("location",s.headers))
       do jlog("HTTP {s.code} type={content_type} location={location}")
       {some=(content_type,location,s.content)}

  @private redirect(res) =
    match res with
    | {failure=_} -> none
    | {success=s} ->
       do jlog("HTTP {s.code}")
       if s.code == 302
       then find_header("location",s.headers)
       else none

  fb_get(base, path, data) = fb_get_(base, path, data, content_only)
  fb_get_ct(base, path, data) = fb_get_(base, path, data, content_with_type)
  fb_get_redirect(base, path, data) = fb_get_(base, path, data, redirect)

  /**
   * Make a HTTP POST on [path] at [base] with [data]
   */
  fb_post(base, path, data) =
    txtdata = API_libs.form_urlencode(data)
    do jlog("POST {base}{path}\n{txtdata}\n")
    match Uri.of_string("{base}{path}") with
    | {none} -> none
    | {some=uri} ->
      match WebClient.Post.try_post(uri,txtdata) with
      | {failure=_} -> none
      | {success=s} -> do jlog("HTTP {s.code}\n{s.content}") {some=s.content}
      end

  @private memdump = (%% BslPervasives.memdump %%: string -> string)

  /**
   * Make a HTTP POST on [path] at [base] with [forms] form data
   */
  fb_post_multi(base, path, forms) =
    bound = Random.string(20)
    mimetype = "multipart/form-data; boundary={bound}"
    forms = List.map((f ->
                      match f with
                      | {form=(name, content)} ->
                         "--{bound}\r\nContent-Disposition: form-data; name=\"{name}\"\r\n\r\n{content}\r\n"
                      | {file=(name, filename, content_type, content)} ->
                         "--{bound}\r\nContent-Disposition: form-data; name=\"{name}\"; filename=\"{filename}\"\r\nContent-Type={content_type}\r\n\r\n{content}\r\n"
                     ),forms)
    content = some(String.concat("",List.append(forms,["--{bound}--\r\n"])))
    options = ~{ WebClient.Post.default_options with mimetype content }
    do jlog("POST {base}{path}\n{memdump(Option.get(content))}\n")
    match Uri.of_string("{base}{path}") with
    | {none} -> none
    | {some=uri} ->
      match WebClient.Post.try_post_with_options(uri,options) with
      | {failure=_} -> none
      | {success=s} -> do jlog("HTTP {s.code}\n{s.content}") {some=s.content}
      end

  /**
   * Make a HTTP DELETE on [path] at [base] with [data]
   */
  fb_delete(base, path, data) =
    final_path = generic_build_path("{base}{path}", data)
    do jlog("DELETE {final_path}")
    match Uri.of_string(final_path) with
    | {none} -> none
    | {some=uri} ->
      match WebClient.Delete.try_delete(uri) with
      | {failure=_} -> none
      | {success=s} -> do jlog("HTTP {s.code}\n{s.content}") {some=s.content}
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
    if v == ""
    then data
    else List.add((name, v), data)

  add_if_filled_opt(name, tostr, v, data) =
    if Option.is_none(v)
    then data
    else List.add((name, tostr(Option.get(v))), data)

  add_if_filled_generic(name, tostr, v, data) =
    str = tostr(v)
    if str == ""
    then data
    else List.add((name, str), data)

  add_bool_if_filled(name, v:option(bool), data) = add_if_filled_opt(name, Bool.to_string, v, data)

  add_int_if_filled(name, v:option(int), data) = add_if_filled_opt(name, Int.to_string, v, data)

  add_array_if_filled(name, tojson:list('a)->RPC.Json.json, v:list('a), data) =
    if v == []
    then data
    else List.add((name, Json.serialize(tojson(v))), data)

  add_json(name, tojson:'a->RPC.Json.json, v:'a, data) =
    List.add((name, Json.serialize(tojson(v))), data)

  add_form_if_filled(name, v, data) =
    if v == ""
    then data
    else List.add({form=(name, v)}, data)

 /* Feed to data */

  fb_properties_to_json(properties:Facebook.properties) : RPC.Json.json =
    {List=List.map((p -> {String=Facebook.string_of_property(p)}),properties)}

  actions_to_json(actions) : RPC.Json.json =
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
