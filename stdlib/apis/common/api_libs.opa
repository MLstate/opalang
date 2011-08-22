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
 * Common API libraries module
 *
 * This file provides some methods used by the API libraries
 *
 * @category web
 * @author Nicolas Glondu, 2010
 * @destination private
 * @stability Work in progress
 */

import stdlib.web.client

API_libs_private = {{

 /**
  * Alias to easily manage the jlog of API modules
  */
  apijlog(text:string) =
    do jlog(text)
    void

  /* -------------- */
  /* Json functions */
  /* -------------- */

/**
 * A wrapper for Json parser. Size limit removed because Facebook
 * can give HUGE json objects (espacially on fql queries) and
 * Facebook is considered as a reliable source here.
 *
 * A timer for performance measure is added.
 */
  parse_json(rawdata) =
  raw_data_parser = (parser
  | [a-z]* !. -> {String = rawdata}
  | .* ->
    parse() = Json.of_string(rawdata) |> Option.get_msg((-> "{rawdata} does not parse ({__POSITION__})"), _)
    (t, data) = Duration.execution_time(parse)
    do apijlog("Json_parser : {Duration.in_seconds(t)} seconds")
    data)
  Parser.parse(raw_data_parser, rawdata)

/**
 * Similar to JsonOpa.to_string but does not return an error if the type does not match
 * Twitter is a trusted source here.
 */
  json_to_string_unsafe(j) = match j: RPC.Json.json
  | {~String} -> some(String)
  | _ -> none
  end

/**
 * Similar to JsonOpa.to_int but does not return an error if the type does not match
 * Twitter is a trusted source here.
 */
  json_to_int_unsafe(j) = match j :RPC.Json.json
  | {~Int}    -> some(Int)
  | {~String} -> some(Int.of_string(String))
  | {Float=f} -> some(Float.to_int(f))
  | _         -> none
   end

/**
 * Similar to JsonOpa.to_float but does not return an error if the type does not match
 * Twitter is a trusted source here.
 */
  json_to_float_unsafe(j) = match j :RPC.Json.json
  | {~Float} -> some(Float)
  | {~Int}   -> some(float_of_int(Int))
  | _        -> none
  end

/**
 * Similar to JsonOpa.to_bool but does not return an error if the type does not match
 * Twitter is a trusted source here.
 */
  json_to_bool_unsafe(j) = match j :RPC.Json.json
  | {~Bool} -> some(Bool)
  | _       -> none
  end

/**
 * Shortcup to get a string in a map(string, json)
 */
  map_get_string(eltname, map) =
    sub = Map.get(eltname, map) ? {String = ""}:RPC.Json.json
    json_to_string_unsafe(sub) ? ""

/**
 * Shortcup to get an integer in a map(string, json)
 */
  map_get_int(eltname, map) =
    sub = Map.get(eltname, map) ? {Int = -1}:RPC.Json.json
    json_to_int_unsafe(sub) ? 0

/**
 * Shortcup to get a float in a map(string, json)
 */
  map_get_float(eltname, map) =
    sub = Map.get(eltname, map) ? {Int = -1}:RPC.Json.json
    json_to_float_unsafe(sub) ? float_of_int(0)

/**
 * Shortcup to get a boolean in a map(string, json)
 */
  map_get_bool(eltname, map, default) =
    sub = Map.get(eltname, map) ? {Bool = default}:RPC.Json.json
    json_to_bool_unsafe(sub) ? default

  /* --------------- */
  /* Other functions */
  /* --------------- */

/**
 * Returns a sublist starting at start and of length len of given list l
 * if l is smaller than len, returns l
 */
  sublist(start, len, l) =
    rec sub(start, len, l, acc) =
      match l :list with
      | { nil } -> acc
      | { ~hd ~tl } ->
        if start > 0 then sub(start-1, len, tl, acc)
        else if len > 0 then sub(0, len-1, tl, List.cons(hd, acc))
        else acc
    sub(start, len, l, [])

/**
 * Alias for a Map.map
 */
  remap(fun, map) =
    sub(key, elt, acc) = Map.add(key, fun(elt), acc)
    Map.fold(sub, map, Map.empty)


 /* Simple (and maybe incomplete) url encoder */
  url_encoder(text) = Uri.encode_string(text)

}}

/**
 * Public functions common to all APIs
 *
 */

/**
 * The type of an API callback
 */
type API_libs.answer_fun('a) =
    { api_fun_html :('a->resource) }
  / { api_fun_void :('a->void) }

/**
 * The type of an API answer
 */
type API_libs.answer =
    { api_html :resource }
  / { api_void }


API_libs = {{
/**
 * Extracts the result from an API answer
 *
 * @param res Result of your API call
 */
  get_html(res) =
    match res:API_libs.answer with
    | ~{api_html} -> api_html
    | _           -> error("API error: incorrect type")

/**
 * Extracts the result from an API answer
 *
 * Useful even for a void, because with 'a -> void' the result of 'a' is not calculated
 *
 * @param res Result of your API call
 */
  get_void(res) =
    match res:API_libs.answer with
    | {api_void} -> void
    | _           -> error("API error: incorrect type")

/**
 * Urlencode a list of couples (key, value)
 *
 * Build a string respecting application/x-www-form-urlencoded
 * from a (string * string) list as (key * value) list.
 * Values are url-encoded
 *
 * @param args List of args
 */
  form_urlencode(args) =
    aux((pname:string, pval:string), acc:string) =
      pval = API_libs_private.url_encoder(pval)
      if pval == "" then acc
      else if acc == "" then "{pname}={pval}"
      else "{acc}&{pname}={pval}"
    List.fold(aux, args, "")

  /**
   * Transforms data url-encoded in a list of (key, value) pairs
   */
  get_data(rawdata) =
    /* Add the '?' at the beginning if it was removed */
    rawdata =
      if String.substring(0,1,rawdata) == "?" then rawdata
      else "?{rawdata}"
    Parser.try_parse(UriParser.query, rawdata) ? []

  has_field(data, name) = List.assoc(name, data) |> Option.is_some
  get_field(data, name) = List.assoc(name, data) ? ""
  get_field_def(data, name, def) = List.assoc(name, data) ? def

}}
