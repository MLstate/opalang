/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/



package stdlib.apis.mongo


/**
* {1 About this module}

* This module provides full-text search functionalities
* Create an index from a db name
* Index a value, using tf-idf weight for each word of its representation
* Search values from a list of words, using a special syntaxe to build queries
*/

/**
* {1 Types defined in this module}
*/

type key = string

type indexation =
  { _score: float // tf_idf weight
  ; lexem: string // original lexem before it has been stemmed, to be able to search with exact matching
  ; key: key }

type index_element =
  {_id: string
  ; values: list(indexation) }

type index_count =
  {_id : string
  ; count : int }

type index = Mongo.collection(index_element)
type count = Mongo.collection(index_count)


@private
SearchDbUtils = {{

  create_select(r) =
    b = Bson.opa2doc(r)
    MongoSelect.create(b)

  create_update(r) =
    b = Bson.opa2doc(r)
    MongoUpdate.create(b)

  batch_from_list(v) =
    MongoCollection.Batch.of_list(v)

  insert(c, v) =
    MongoCollection.insert(c, v)

  insert_batch(c, b) =
    MongoCollection.insert_batch(c, b)

  update(c, s, v) =
    MongoCollection.update(c, create_select(s), create_update(v))

  delete(c, s) =
    MongoCollection.delete(c, create_select(s))

  find_one(c, r) =
    MongoCommon.result_to_option(
      MongoCollection.find_one(c, create_select(r))
    )

  find_all(c, r) =
    MongoCommon.results_to_list(
      MongoCollection.find_all(c, create_select(r))
    )

  elements(c, set) =
    find_all(c, {_id = {`$in` = StringSet.To.list(set)}})
    |> List.fold(
      element, acc -> StringMap.add(element._id, element.values, acc)
    , _, StringMap.empty)

}}

@private
SearchValueTransform = {{

  @private
  string_of_record(record, row) =
    if List.is_empty(row) then ""
    else
      OpaValue.Record.fold_with_fields(
        field, field_type, field_value, acc ->
          field_value = @unsafe_cast(field_value)
          str = string_of_value(field_value, field_type)
          field = OpaValue.Record.name_of_field(field) |> Option.default("", _)
          "{acc}{str} {field} "
      , record, row, "")

  @private
  string_of_sum(records, rows) =
    rows = List.fold(
      list, acc -> List.append(list, acc)
    , rows, [])
    List.fold(
      record, acc -> "{acc}{string_of_record(record, rows)} "
    , records, "")

  /**
  * Return a string from a value of type alpha
  * The following cases are not handled by indexation : Functions, Quantifiers
  */
  rec string_of_value(value : 'a, vtype: OpaType.ty) : string =
    match vtype with
    | {TyConst = {TyInt}} -> @unsafe_cast(value) : int |> int_to_string
    | {TyConst = {TyFloat}} -> @unsafe_cast(value) : float |> float_to_string
    | {TyConst = {TyString}} -> @unsafe_cast(value) : string
    | {TyVar = var} -> var
    | {TyRecord_row = row} -> string_of_record(value, row)
    | {TyRecord_row = row; TyRecord_rowvar = _} -> string_of_record(value, row)
    | {TySum_col = col} -> string_of_sum([value], col)
    | {TySum_col = col; TySum_colvar = _} -> string_of_sum([value], col)
    | {TyName_args = l; TyName_ident = r} -> string_of_value(value, OpaType.type_of_name(r, l))
    | _ -> ""

}}


@private
SearchCache = {{

  /**
  * cache mechanisme to store temporary index before writing it on db
  * user can choose the size of the cache
  * more the cache is small, more we need to write on db
  */
  cache = Mutable.make(StringMap.empty) : Mutable.t(stringmap(list(indexation)))
  cache_limit = Mutable.make(500) : Mutable.t(int)

  /**
   * Write the index on db
   */
  finalize(index) =
    current_cache = cache.get()
    start_time = Date.now() |> Date.in_milliseconds(_)
    set = StringMap.fold(
      lexem, _, acc -> StringSet.add(lexem, acc)
    , current_cache, StringSet.empty)
    elements = SearchDbUtils.elements(index, set)
    (to_remove, batch) = StringMap.fold(
      lexem, values, (to_remove, acc) ->
         (to_remove, values) = match StringMap.get(lexem, elements) with
         | {some=l} -> (List.cons(lexem, to_remove), List.append(l, values))
         | _ -> (to_remove, values)
         element = {_id = lexem; values = values} : index_element
        (to_remove, List.cons(element, acc))
     , current_cache, ([], []))
   match batch with
   | [] -> void
   | l ->
     batch = SearchDbUtils.batch_from_list(l)
     _ = SearchDbUtils.delete(index, {_id = {`$in` = to_remove}})
     _ = SearchDbUtils.insert_batch(index, batch)
     do cache.set(StringMap.empty)
     delta = (Date.now() |> Date.in_milliseconds(_)) - start_time
     Log.notice("SEARCH", "INDEX WRITEN ON DB IN {delta} MS")

  add_to_index(index, lexem, value) =
    current_cache = cache.get()
    limit = cache_limit.get()
    values = match StringMap.get(lexem, current_cache) with
    | {some = values} -> List.cons(value, values)
    | _ -> [value]
    do cache.set(StringMap.add(lexem, values, current_cache))
    if StringMap.size(cache.get()) >= limit then finalize(index) else void

  remove_from_index(lexem, key) =
    current_cache = cache.get()
    match StringMap.get(lexem, current_cache) with
    | {some=values} ->
      map = match List.remove_p(value -> value.key == key, values) with
       | [] -> StringMap.remove(lexem, current_cache)
       | l -> StringMap.add(lexem, l, current_cache)
       cache.set(map)
    | {none} -> void

}}


@private
SearchUtils = {{

  /**
  * Return the number of time a given word appears in a given string
  */
  @private
  count_occurrence(lexem: string, words: list(string)) =
    rec aux(l, acc) = match l with
      | [] -> acc
      | [hd | tl] ->
        if hd == lexem then aux(tl, acc+1)
        else aux(tl, acc)
    aux(words, 0)

  /**
  * Compue tf-idf weight for a given word given a document
  */
  compute_tf_idf(index: index, count: count, lexem: string, words: list(string)) =
    tf =
      if List.is_empty(words) then 0.00
      else int_to_float(count_occurrence(lexem, words)) / int_to_float(List.length(words))
    idf =
      nb_docs = match SearchDbUtils.find_one(count, {_id = "count"}) with
      | {some = count} -> int_to_float(count.count)
      | {none} -> 0.0
      context_count =
        match StringMap.get(lexem, SearchCache.cache.get()) with
        | {some=l} -> 1 + List.length(l) |> int_to_float
        | {none} -> 1.00
      db_count =
        match SearchDbUtils.find_one(index, {_id = lexem}) with
        | {some=elt: index_element} -> 1 + List.length(elt.values) |> int_to_float
        | {none} -> 1.00
      nb_docs_with_lexem = context_count + db_count
      if (nb_docs_with_lexem == 0.0 || nb_docs == 0.0)
      then 0.0
      else Math.ln(nb_docs / nb_docs_with_lexem)
    tf * idf


   /* SEARCHING */

  build_list(index: index, lexem: string, exact: bool) : list(index_element) =
    context_search =
      cache = SearchCache.cache.get()
      if exact then
        match StringMap.get(lexem, cache) with
        | {some=values} -> [{_id = lexem; ~values}]
        | _ -> []
      else StringMap.fold(
        lex, values, acc ->
          if String.contains(lex, lexem) then
            List.cons({_id = lexem; ~values}, acc)
          else acc
        , cache, [])
    db_search =
      if exact then
        match (SearchDbUtils.find_one(index, {_id = lexem})) with
        | {some = elt} -> [elt]
        | {none}-> []
      else
        SearchDbUtils.find_all(index, {_id = {`$regex` = ".*{lexem}.*"}})
    List.append(context_search, db_search)

  check_lexem(index: index, lexem: string, exact: bool) : list(key) =
    match (build_list(index, lexem, exact)) with
      | [] -> []
      | l ->
        List.fold(
          elt, acc ->
            List.fold(
              v, acc ->
                if List.mem(v, acc) then acc else List.cons(v, acc)
              , elt.values, acc)
         , l, [])
        |> List.sort_by(indexation -> indexation._score, _)
        |> List.rev_map(indexation -> indexation.key, _)

  @private
  parse_exact(query: string) =
    p = parser
    | "\"" s=Rule.alphanum_string "\"" -> (s, true)
    | s=Rule.alphanum_string -> (s, false)
  Parser.parse(p, query)

  parse_query(query: string) =
    query = String.replace(" + ", "+", query)
      |> String.replace(" +", "+", _)
      |> String.replace("+ ", "+", _)
    list = String.explode(" ", query)
    List.fold(
      lexems, acc ->
        elt = String.explode("+", lexems) |> List.map(elt -> parse_exact(elt), _)
        List.cons(elt, acc)
    , list, [])

  create_col(db_name: string, col_name: string) =
    mongo = MongoConnection.openfatal("default")
    clone =  MongoConnection.clone(mongo)
    { db = MongoConnection.namespace(clone, db_name, col_name) }

}}



/**
 * {1 Interface}
 */

MongoSearch = {{

  notice(s) = Log.notice("SEARCH: ", s)

  /**
   * Index the given value
   * Exemple: [add_to_index index value key]
   * @param index: index
   * @param value: value to be indexed, of type 'a
   * @param key: the key from the main database to store in the index, so it can resend after a search query
   */
  @server_private
  add_to_index(index: (index, count), value: 'a, key: key) =
    do notice("add value {value} at key {key}")
    (index, count) = index

    // update count
    _ = SearchDbUtils.update(count, {_id = "count"}, {`$inc` = {count = 1}})

    // compute a list of words from the given value
    words = SearchValueTransform.string_of_value(value, OpaValue.typeof(value)) |> String.explode(" ", _)

    // compute a set to avoid indexing more than one time the same word
    set = StringSet.From.list(words)

    Set.iter(
      lexem ->
        // fix a limit to avoid 'key too large to index' error message from mongoDB
        if (String.length(lexem) <= 512) then
          // compute tf-idf score for current lexem
          tf_idf = SearchUtils.compute_tf_idf(index, count, lexem, words)
          value = {_score =  tf_idf; lexem = lexem; key = key}
          SearchCache.add_to_index(index, lexem, value)
        else void
     , set)

  /**
   * Desindex the given value
   * Exemple: [remove_from_index index value key]
   * @param index: index
   * @param value: value to be desindexed
   * @param key: the key from the main database from where to remove value
   */
  @server_private
  remove_from_index(index: (index, count), value: 'a, key: key) =
    do notice("remove value {value} from key {key}")
    (index, count) = index

    // update count
    _ = SearchDbUtils.update(count, {_id = "count"}, {`$inc` = {count = -1}})

    // compute a list of words from the given value
    words = SearchValueTransform.string_of_value(value, OpaValue.typeof(value)) |> String.explode(" ", _)

    // compute a set to avoid desindexing more than one time the same word
    set = StringSet.From.list(words)

    (to_update, to_remove) = Set.fold(
      lexem, (to_update, to_remove) ->

      // clean the cache
        do SearchCache.remove_from_index(lexem, key)

        match (SearchDbUtils.find_one(index, {_id = lexem})) with
        | {none} -> (to_update, to_remove) // lexem not found on db
        | {some=elt} ->
          match (List.remove_p(value -> value.key == key, elt.values)) with
          | [] ->  (to_update, List.cons(lexem, to_remove)) // lexem with no more values => to be removed
          | l ->
          elt = {_id = lexem; values = l}
          (List.cons(elt, to_update), to_remove) // lexem with some remaining values => to be updated
    , set, (List.empty, List.empty))
    _  = match to_update with
    | [] -> void
    | l ->
      _ = SearchDbUtils.insert_batch(index, SearchDbUtils.batch_from_list(l))
      void
    match to_remove with
    | [] -> void
    | l ->
      _ = SearchDbUtils.delete(index, {_id = {`$in` = l}})
      void

  /**
   * Search all documents containing the query words
   * Exemple: [search index query]
   * "toto" => search for exact lexem "toto"
   * toto => search for lexems containing "toto"
   * toto titi => search for lexems "toto" or "titi"
   * toto + titi => search for lexems "toto" and "titi"
   * @param index: index
   * @param query: string containing words to search
   * @return a list of db key representing the keys where to the found values
   */
  @server_private
  search(index: (index, count), query:string) : list(key) =
    do notice("search for query {query}")
    (index, _) = index
    res = List.fold(
      lexem_list, acc ->
        tmp_res = List.fold(
          (lexem, exact), acc ->
            res = SearchUtils.check_lexem(index, lexem, exact)
            match acc with
            | {none} -> {some = res}
            | {some=l} -> {some = List.filter(elt -> List.mem(elt, res), l)}
        , lexem_list, {none})
    |> Option.default([], _)
    List.append(tmp_res, acc)
    , SearchUtils.parse_query(query), [])
    List.unique_list_of(res)


  /**
   * Create an index as a MongoDb collection
   * Exemple: [create_index db_name]
   * @param db_name: string the name of the main database
   * @param cache_limit: int the size of the internal cache; a limit set at 0 implies a db write for each word to index
   * @return an index as a MongoDb collection
   */
  @server_private
  create_index(db_name, cache_limit) =
    index = SearchUtils.create_col(db_name, "index")
    count = SearchUtils.create_col(db_name, "count")
    _ = SearchDbUtils.insert(count, {_id = "count"; count = 0})
    do SearchCache.cache_limit.set(cache_limit)
    (index, count)

}}
