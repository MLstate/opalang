/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// Needed by compiler

/**
 * {1 About this module}
 *
 * Serialization and unserialization of json values.
 *
    {1 How to start}
    This module provide 2 important functions :

    - serialize_opt for serialize a json value
    - deserialize_opt for unserialize a json value

    The serialization and unserialization doesn't respect exactly JSON
    format. Because client native json parser can crash if the json
    to parse is too deep. Perhaps the server parser is also limited,
    but it isn't tested. Therefore the exchange client -> server
    respect JSON format, but the server -> client exchange doesn't
    respect exactly JSON format. This modified JSON format will be
    called cut JSON.

    Server side :
      - serialize_opt : Returns cut JSON
      - deserialize_opt : Get normal JSON

    Client side :
      - serialize_opt : Returns normal JSON
      - deserialize_opt : Get cut JSON


    {1 What is the cut JSON?}

    If JSON value is not too deep the generated string respect exactly
    JSON format which doesn't contains null value.

    Else it's a special format. This special format begin by $
    character. This indicates that the JSON is cut. Then it's a
    sequence of : integer followed by JSON. Integer indicates the size
    of JSON which follows. This following JSON may contains null
    value. This null value indicates that it must be replaced by the
    following value. Cut JSON format is informally described bellow.

    Cut JSON format
      JSON
      $ (size JSON)+

    Simple example on list (with depth limit = 2) :
      $44{"hd":"toto0","tl":{"hd":"toto1","tl":null}}44{"hd":"toto2","tl":{"hd":"toto3","tl":null}}44{"hd":"toto4","tl":{"hd":"toto5","tl":null}}...

    {1 Important notes}

    Functions defined in this module (must) don't change order of json
    objects. For example :
    [Json.serialize_opt({Record = [("a",1), ("b",2)]}) == "{a:1,b:2}"]
    [Json.serialize_opt({Record = [("a",1), ("b",2)]}) != "{b:2,a:1}"]

*/


import stdlib.core.{js, web.core, map}

/**
 * {1 Types defined in this module}
 */

/**
 * The type of json. All json object can be represented by this
 * type.
 */
type RPC.Json.json0('json) =
    { Int: int }
  / { Float: float }
  / { String: string }
  / { Bool: bool }
  / { List: list('json) }
  / { Record: list((string, 'json)) }

@opacapi
type RPC.Json.json = RPC.Json.json0(RPC.Json.json)

type RPC.Json.js_code =
  RPC.Json.json0(RPC.Json.js_code) / { Direct : string}

/** Those types are used for translation OPA Json to/from low level Json,
 * ie native implementation on javascript, and corresponding implementation in OCaml.
  */
type RPC.Json.private.native = external
type ll_json_list_repr = external
type ll_json_record_repr = external


@both Json = {{

  /**
   * {1 Public API}
   */

  /**
   * {2 Communication with non-OPA world}
   *
   * These functions produce or consume standard JSON. If you wish to communicate with the rest of the
   * world, these are probably what you're looking for.
   */

  /**
   * Serialize a JSON value to a standard representation as a character string.
   */
  serialize(j: RPC.Json.json): string =
    Text.to_string(to_text(j,Text.cons(""), {false}))

  /**
   * Deserialize a JSON value from a standard representation as a character string.
   */
  deserialize(s:string): option(RPC.Json.json) =
    of_string(s)

 /**
  * {2 Communications inside OPA}
  *
  * These functions produce or consume "cut" JSON, i.e. an optimized representation of JSON that
  * can be used to improve chances of legacy browsers not throwing an exception during deserialization.
  * Only OPA understands "cut" JSON.
  */

  /**
   * Serialize a JSON value to a "cut" character string
   */
  serialize_opt(j: RPC.Json.json): string =
    Text.to_string(to_text(j,Text.cons(""), {true}))

  /**
   * Deserialize a valid character string (possibly "cut") to a JSON value
   */
  deserialize_opt(s: string): option(RPC.Json.json) =
    of_string(s)

 /**
  * {2 Insertion in JS}
  */

  /**
   * Serialize a JSON value to a string that may directly be inserted in JS
   */
  serialize_to_js(j/*: json*/) =
    Text.to_string(to_text_in_js(j))

  serialize_float(f) =
    // WHAT ABOUT PRINTING PRECISIONS
    if Math.is_normal(f) then Float.to_string(f)
    else if Math.is_NaN(f) then "NaN"
    else if f<0.0 then "-Infinity" else "Infinity"

  /**
   * {2 Utils on JSON structure}
   */

  /**
   * Sort JSON records as the order specifies.
   */
  sort(json:RPC.Json.json, order:order(string, 'a)):RPC.Json.json =
    sort_field_content( (name,json) ) = (name,sort(json,order))
    cmp_field( (name1, _), (name2, _) ) = Order.compare(name1, name2, order)
    match json with
    | { List = list } -> { List = List.map(sort(_,order), list) }
    | { Record = rc } ->
      { Record =
        rc
        |> List.map(sort_field_content, _)
        |> List.sort_with(cmp_field, _)
      }
    | _ -> json

/**
 * {1 Private stuff}
 */

  /** Limit of depth of json value (tested uniquely on client).
      Some test find these values for reached stack overflow of
      native client parser :
        Firefox 3.6 = 317
        Opera 10.10 = 272
        TODO : test with IE
  */
  @private depth_limit = 100




  /** Parse a string and construct the corresponding json value.
      @param str The string to parse.
      @return An option contains the corresponding json value.
  */
  of_string(str) =
    bp= %% BslJson.Json.of_string%%
    bp(str)


  /** This function creates a OPA Json object from a low level Json object
   * it takes the same parameters than [native_to_json], except the last,
   * which is a low level Json object
   *
   * @return A OPA Json object
   */
  from_ll_json(elem) =
    aux= %% BslJson.Json.of_json_repr%%
    aux(elem)

  /** This function deconstruct a OPA JSon object and create a low level Json object
   *
   * @param a OPA Json object
   * @return a low level Json object (native javascript for the client, Json implementation for the server)
   */
  to_ll_json(json_elem) =
    to_int = %%BslJson.Json.json_repr_int%%
    to_float = %%BslJson.Json.json_repr_float%%
    to_string = %%BslJson.Json.json_repr_string%%
    to_bool = %%BslJson.Json.json_repr_bool%%
    to_array = %%BslJson.Json.json_repr_array%%
    to_record = %%BslJson.Json.json_repr_record%%
    json_list_empty = %%BslJson.Json.json_list_empty%%
    json_list_cons = %%BslJson.Json.json_list_cons%%
    json_record_empty = %%BslJson.Json.json_record_empty%%
    json_record_cons = %%BslJson.Json.json_record_cons%%

    rec aux(elem:RPC.Json.json) =
        match elem with
        | {Int=i} -> to_int(i)
        | {Float=f} -> to_float(f)
        | {String=s} -> to_string(s)
        | {Bool=b} -> to_bool(b)
        | {List=l} ->
            fun((x/*: json*/), (a:(ll_json_list_repr))) =
                t : RPC.Json.private.native = aux(x)
                json_list_cons(t,a)

            tmp : ll_json_list_repr = List.fold_backwards( fun ,
                         l, json_list_empty())
            to_array(tmp)
        | {Record=r} ->
            to_record(
              List.fold(
                ((ch, x), a ->
                  json_record_cons(ch, aux(x), a)),
                  r, json_record_empty()
              )
            )

     aux(json_elem)


  /**
      Serialize a json value in a format described above for runs with
      deep object. This serialization is added at right of given text.
      @param j A json value to serialize.
      @param tx A text for store serialization.
      @param cut If [true], construct "cut" JSON, understood only by OPA. Otherwise, construct standard JSON.
      @return The text which was concatenate the serialization of json
        value.
  */
  @private to_text(j:RPC.Json.json, tx, cut:bool) =
    `++`(tx, e) = Text.insert_right(tx,e)
    cons(str) = Text.cons(str)
    concat(e1, e2) = Text.concat(e1, e2)
    length(tx) = Text.length(tx)
    for_string(tx, str) = tx ++ "\"{String.escape_non_utf8_special(str)}\""
    rec aux(j:RPC.Json.json, tx, n) =
      match j with
      | {~Int} -> (tx ++ "{Int}", [])
      | {~Float} -> (tx ++ serialize_float(Float), [])
      | {~String} -> (for_string(tx, String), [])
      | {~Bool} -> (tx ++ "{Bool}", [])
      | {~Record} ->
         if n == 0 && WebUtils.is_server() && cut then (tx ++ "null", [j])
         else
           tx = tx ++ "\{"
           (tx, ls, _) = List.fold(
             ((key, val), (tx, ls, beg) ->
               tx = if beg then tx else tx ++ ","
               (tx, lsr) = aux(val, for_string(tx, key) ++ ":", n-1)
               (tx, List.append(ls, lsr), false)
             ),
             Record, (tx, [], true)
           )
           (tx ++ "}", ls)
      | {List=l} ->
         tx = tx ++ "["
         (tx, ls, _) = List.fold(
            (item, (tx, ls, beg) ->
              tx = if beg then tx else tx ++ ","
              (tx, lsr) = aux(item, tx, n)
              (tx, List.append(ls, lsr), false)
            ),
            l, (tx, [], true)
          )
         (tx ++ "]", ls)
    and aux2(j, tx, ls, res) =
      (txr, lsr) = aux(j, cons(""), depth_limit)
      ls = List.append(ls, lsr)
      if List.is_empty(ls) then
        if List.is_empty(res) then txr
        else
          List.fold_backwards(
            (elt, acc ->
              concat(acc ++ Int.to_string(length(elt)), elt)),
            List.add(txr, res), tx ++ "$")
      else
        j = List.head(ls)
        aux2(j, tx, List.tail(ls), List.add(txr, res))
    aux2(j, tx, [], [])


  /** Same as to_text but returns a string.
      @param j A json value to serialize.
      @return The serialized json value.
  */
  to_string(j/*: json*/):string = serialize_opt(j)

  /**
   * Transform a json value on js_code value
   */
  to_js_code(j : RPC.Json.json) =
    j <: RPC.Json.js_code

  // FOR JAVASCRIPT FILE
  /**
      Serialize a json value, this serialized value must be inserted
      on a javascript file only. This generated string can be
      evaluated natively by browsers (It's javascript code).

      TODO : Test if depth_limit should be different.

      WARNING : Don't use this function for network transfers.
      not too deep.

      @param j A json value to serialize.
      @param tx A text for store serialization.
      @return The serialized json value can be evaluated natively by
      browsers.
  */
  to_text_in_js_ll(j : RPC.Json.js_code ) =
    `++`(tx, e) = Text.insert_right(tx,e)
    for_string(tx, str) = tx ++ "\"{String.escape_non_utf8_special(str)}\""
    rec aux(j : RPC.Json.js_code, tx : text, n, nid) =
      match j with
      | {~Int} -> (tx ++ "{Int}", [], nid)
      | {~Float} -> (tx ++ serialize_float(Float), [], nid)
      | {~String} -> (for_string(tx, String), [], nid)
      | {~Bool} -> (tx ++ "{Bool}", [], nid)
      | {~Direct} -> (tx ++ Direct, [], nid)
      | {~Record} ->
         if n == 0 && not(WebUtils.is_client()) then
           (tx ++ "{nid}", [(nid,j)], String.next(nid))
         else
           tx = tx ++ "\{"
           (tx, ls, nid,  _) = List.fold(
             ((key, val), (tx, ls, nid, beg) ->
               tx = if beg then tx else tx ++ ","
               (tx, lsr, nid) =
                 aux(val, for_string(tx, key) ++ ":", n-1, nid)
               (tx, List.append(ls, lsr), nid, false)
             ),
             Record, (tx, [], nid, true)
           )
           (tx ++ "}", ls, nid)
      | {List=l} ->
         tx = tx ++ "["
         (tx, ls, nid, _) = List.fold(
            (item, (tx, ls, nid, beg) ->
              tx = if beg then tx else tx ++ ","
              (tx, lsr, nid) = aux(item, tx, n-1, nid)
              (tx, List.append(ls, lsr), nid, false)
            ),
            l, (tx, [], nid, true)
          )
         (tx ++ "]", ls, nid)

    rec aux2(j : RPC.Json.js_code, ls : list((string, RPC.Json.js_code)), res : list(text), current_id, last_id, cut) =
      (txr, lsr, last_id) = aux(j, Text.cons(""), depth_limit, String.next(last_id))
      ls = List.append(ls, lsr)
      match ls with
      | [] ->
        txr = if cut then Text.insert_left(txr, "var "^current_id^" = ") ++ ";"
              else txr
        (List.fold(
          (elt, acc -> Text.concat(acc, elt)),
          List.add(txr, res), Text.cons("")
        ), cut)
      | [ (new_id, j) | tl ] ->
        txr = Text.insert_left(txr, "var "^current_id^" = ") ++ ";"
        aux2(j, tl, List.add(txr, res), new_id, String.next(last_id), true)

    (txr, cut) = aux2(j, [], [], "a", "a", false)
    if cut then
       Text.insert_left(txr,"(function()\{") ++ "return a;})()"
    else
       txr

  /**
   * Like [to_text_in_js_ll] but for json value
   */
  to_text_in_js(j/*: json*/) =
    to_text_in_js_ll(to_js_code(j))


  /** Same as to_text_in_js but returns a string.
      @param j A json value to serialize.
      @return The serialized json value.
  */
  to_string_in_js(j/*: json*/) =
    Text.to_string(to_text_in_js(j))
  : string


  get_type(j) =
    match j : RPC.Json.json
     {Int=_} -> "Int"
     {Float=_} -> "Float"
     {String=_} -> "String"
     {Bool=_} -> "Bool"
     {Record=_} -> "Record"
     {List=_} -> "List"

}}

// Json conversion functions (OPA specific)
JsonOpa =
{{

  to_int(j) = match j : RPC.Json.json
    | {~Int} -> some(Int)
    | _      -> do Log.warning("JsonOpa", "expected an int, found {Json.to_string(j)}. Aborting."); none
  end

  to_string(j) = match j : RPC.Json.json
    | {~String} -> some(String)
    | _ -> do Log.warning("JsonOpa", "expected a string, found {Json.to_string(j)}. Aborting."); none
  end

  to_float(j) = match j : RPC.Json.json
    | {~Float} -> some(Float)
    | {~Int}   -> some(float_of_int(Int))
    | _        -> do Log.warning("JsonOpa", "expected a float, found {Json.to_string(j)}. Aborting."); none
  end

  to_bool(j) = match j : RPC.Json.json
    | {~Bool} -> some(Bool)
    | _       -> do Log.warning("JsonOpa", "expected a bool, found {Json.to_string(j)}. Aborting."); none
  end

  to_void(j) = match j : RPC.Json.json
    | {Record=[]} -> some(void)
    | {Record=_} -> do Log.warning("JsonOpa", "found {Json.to_string(j)}; assuming void."); some(void)
    | _ -> do Log.warning("JsonOpa", "expected a void, found {Json.to_string(j)}. Aborting."); none
  end

  list_to_records(l) = match l: list(RPC.Json.json)
    | [] -> {Record = [("nil", {Record = []})]} : RPC.Json.json
    | [hd | tl] -> {Record= [("hd", hd), ("tl", list_to_records(tl))]}

  records_to_list(r) = match r: RPC.Json.json
    | { Record = [("hd", hd), ("tl", tl)] } ->
       Option.map( (x -> [hd | x] ), records_to_list(tl))
    | { Record = [("nil", { Record = {nil} } : RPC.Json.json)] } -> some([])
    | _ -> do Log.warning("JsonOpa", "records_to_list:  {Json.to_string(r)}"); none
  : option(list(RPC.Json.json))

  record_fields(j) = match j : RPC.Json.json
    | {~Record} -> some(StringMap.From.assoc_list(Record))
    | {~List}-> record_fields(list_to_records(List))
    | _ -> do Log.warning("JsonOpa", "expected a record, found {Json.to_string(j)}. Aborting."); none
  end

  to_list(j) = match j : RPC.Json.json with
    | {~List} -> some(List)
    | {Record=_} -> records_to_list(j)
    | _ -> do Log.warning("JsonOpa", "expected a list, found {Json.to_string(j)}. Aborting."); none
  end
}}
