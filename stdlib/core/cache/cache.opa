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
    @author David Rajchenbach-Teller
**/

import stdlib.core.{map, date}

/**
 * General-purpose caching of function results.
 *
 * {1 What is this module for?}
 *
 * This module provides a general-purpose cache for functions, also known as a memoizer. You can use it whenever
 * you have a function whose evaluation is long or memory consuming, and you wish to store the result of the function
 * in memory.
 *
 * This is an extremely useful optimization tool in numerous cases:
 * - if you are writing an application that needs to access a distant web server to read information, where the information
 *   changes rather slowly (e.g. today's weather conditions in Paris, today's exchange rate for Pesos, a map of Antakya, ...)
 * - if you are writing an application that needs to perform heavy computations but can reuse the results for a time
 *   (e.g. a usage statistics graph, the high scores of a game, ...)
 * - if you are writing an application in which the browser needs to frequently read information from the server (e.g. schedule
 *   manager for many users, a turn-by-turn game, ...)
 * - ...
 *
 *
 * {1 Where should I start?}
 *
 * A good way to determine whether caching a function can be useful is to use function [Cache.simple].
 *
 *
 * {1 What if I need more?}
 *
 * Module {!Cache.Negociator} and function {!Cache.make} are designed to give you fine control upon the cache. Use these to
 * set a maximal size for the cache, or to determine how long results should be kept in the cache, or how they should be
 * stored, or how new results should be added to the cache, or to set the concurrency level, or if you wish to invalidate
 * cache results manually, or to add results to the cache manually.
 *
 *
 * @author David Rajchenbach-Teller, 2011
 * @stability Experimental
 */

type Cache.options('key, 'value, 'order) =
{
   size_limit: option(int)                                         /**If set, determine the maximal number of items to store in the cache. Otherwise, no limit.*/
   age_limit:  option(Duration.duration)                           /**If set, items older than a given duration are discarded from the cache.*/
   storage:    {default}                                           /**Store to local RAM, using default strategy to differenciate ['key]*/
           /   {ordering: order('key, 'order)}                     /**Store to local RAM, using custom strategy to differenciate ['key] */
   strategy:   {minimize_overhead}                                 /**Optimistic concurrency: optimize assuming little concurrency.
                                                                      If several requests are placed to the cache concurrently, some results will not be stored
                                                                      into the cache.
                                                                      In the current version of OPA, this is always the best strategy on the client*/
          / {balance_transactions}                                //Ignored for the moment -- mutexes around [get] and [set]
          / {minimize_transactions}                               //Ignored for the moment -- serialize all requests, might cause deadlocks
          / {default}                                              /**Choose a reasonable strategy for the platform.*/
   //In the future: additional options (e.g. handling disconnect, strategy for asynchronicity)?
}

/**
 * @param 'a The type of keys
 * @param 'b The type of return values
 * @param 'signature The type of signatures, e.g. elements which may be used to decide whether a value needs to be recomputed (typically, expiration date, hash, etc.)
 */
type Cache.sync('a, 'b, 'signature) =
{
        get:        'a -> 'b                    /**Get a cached response if available -- compute one otherwise.*/
        invalidate: 'a -> void                  /**Remove a response from the cache.*/
        put:        'a,'b,'signature -> void    /**Artificially put a new response in the cache.*/
        read:       'a -> option('b)            /**Read the current contents of the cache, without any side-effect.*/
        fetch:      list('a) -> void            /**Ask the cache to fetch several values in one request*/
}

/**
 * @param 'a The type of keys
 * @param 'b The type of return values
 * @param 'signature The type of signatures, e.g. elements which may be used to decide whether a value needs to be recomputed (typically, expiration date, hash, etc.)
 */
type Cache.async('a, 'b, 'signature) =
{
        get:        'a, ('b -> void) -> void    /**Get a cached response if available -- compute one otherwise.*/
        invalidate: 'a -> void                  /**Remove a response from the cache.*/
        put:        'a,'b,'signature -> void    /**Artificially put a new response in the cache.*/
        read:       'a -> option('b)            /**Read the current contents of the cache, without any side-effect.*/
        fetch:      list('a) -> void            /**Ask the cache to fetch several values in one request*/
}

/**
 * An object used to determine if a result has indeed changed.
 *
 * Example: accessing a resource on a distant web server (with dates)
 *
 * The typical behavior of a web client using a cache is to keep track of the latest date at which
 * a given information was downloaded. Whenever the user asks the client to access the information
 * from the server, the client sends to the server the latest modification date, and the server
 * may decide to either return the complete data or to reply that the data hasn't changed in the
 * meantime. This may be implemented as a [Cache.negociator(url, WebClient.result(string), Date.date)],
 * where the [Date.date] is the last date at which the information was downloaded.
 *
 * Example: accessing a resource on a distant web server (with etags)
 *
 * Another possible behavior for web clients using a cache is to keep a signature for the resource
 * downloaded (typically, as a MD5, but in fact, any data provided by the server fits). Whenever
 * the user asks the client to access the information from the server, the client sends the signature
 * to the server, and the server may decide to either return the complete data (e.g. if the signature
 * doesn't fit) or to reply that the data hasn't changed in the meantime. This may be implemented as a
 * [Cache.negociator(url, WebClient.result(string), string)].
 *
 * Other examples include accessing a database, in which case the signature can be a revision number.
 */
@abstract type Cache.negociator('a, 'b, 'signature, 'diff) = ({
    get_always:        'a -> ('signature, 'b)                 //Note: typically, located on the server
    get_diff:          'signature, 'a -> ('signature, 'diff)  //Note: typically, located on the server
    get_many:          list((option('signature), 'a)) -> list(('signature, 'a, {init:'b} / {diff: 'diff}))//Note: typically, located on the server
    decode_diff:       ('b,'diff) -> 'b                       //Note: typically, located on the client
})

@both Cache =
{{

     Negociator =
     {{
        /**
         * Produce a 'dumb' negociator, that always negociates to perform the computation
         */
        always_necessary(f: 'a -> 'b): Cache.negociator('a, 'b, void, 'b) =
          {
             get_always(x) = (void, f(x))
             get_diff(_,x) = (void, f(x))
             get_many(l)   = List.map(((maybe_sig, key) ->
                 match maybe_sig with | {none}    -> (void, key, {init = f(key)})
                                      | {some = _}-> (void, key, {diff = f(key)})
             ), l)
             decode_diff((_x,diff) : ('b,'b) ) : 'b = diff
          }

        /**
         * Deprecated
         */
        make(~{get_always:        'a -> ('signature, 'b)
               get_maybe:         'signature, 'a -> ('signature, option('b))}): Cache.negociator('a, 'b, 'signature, option('b)) =
        (
             get_many = make_default_get_many(get_always, get_maybe)
             decode_diff((x,diff)) = match diff with
                                        | ~{some} -> some
                                        | {none} -> x
             x = {~get_always get_diff=get_maybe ~decode_diff ~get_many}
             x
        )

        /**
         * Produce a negociator interested only in signatures, not in decoding replies
         */
        make_sig(~{get_always:        'a -> ('signature, 'b)
                   get_diff:          'signature, 'a -> ('signature, option('b))}): Cache.negociator('a, 'b, 'signature, option('b)) =
             get_many = make_default_get_many(get_always, get_diff)
             decode_diff((x,diff)) = match diff with
                                        | ~{some} -> some
                                        | {none} -> x
             x = {~get_always ~get_diff ~decode_diff ~get_many}
             x

        make_no_diff(~{get_always: 'a -> ('signature, 'b)
                      get_maybe:  'signature, 'a -> ('signature, option('b))
                      get_many: list((option('signature), 'a)) -> list(('signature, 'a, {init:'b} / {diff: 'diff}))
                     }) : Cache.negociator('a, 'b, 'signature, option('b)) =
             decode_diff((x,diff)) = match diff with
                                        | ~{some} -> some
                                        | {none} -> x
             x = {~get_always get_diff=get_maybe ~decode_diff ~get_many}
             x


        @private make_default_get_many(get_always, get_diff) =
           l -> List.map(((maybe_sig, key) ->
                 match maybe_sig with | {none}  -> (sig, value) = get_always(key)
                                                   (sig, key, {init = value})
                                      | ~{some} -> (sig, diff) = get_diff(some, key)
                                                   (sig, key, ~{diff})
             ), l)
     }}

     /**
      * Perform simple caching for a function.
      *
      * @param f A function
      * @return A new function, with the same behavior as [f], but where results are cached forever, without any size
      * or age limit, using the default strategy.
      */
     simple(f: 'a -> 'b): 'a -> 'b =
        make(Negociator.always_necessary(f), default_options).get

     /**
      * A default set of options. You may customize it for finer control on your caches.
      */
     default_options: Cache.options('key, 'value, 'order) =
     {
        size_limit= {none}
        age_limit=  {none}
        storage=    {default}
        strategy=   {default}
     }


     /**
      * Create a cache for a function
      */
     make(get_manager:Cache.negociator('a/*string*/, 'b/*float*/, 'signature/*void*/, 'diff/*dom*/),
          options:    Cache.options('a/*string*/, 'b/*float*/, 'order)): Cache.sync('a/*string*/, 'b/*float*/, 'signature/*void*/) =
     (
        Storage : Map('a/*string*/, _) = match options.storage with
           | {default}  -> Map
           |~{ordering} -> Map_make(ordering)
        init = {
          storage    = Storage.empty:ordered_map('a/*string*/, Cache.private.storage.entry('b/*float*/, 'signature/*void*/), 'order)
          lru        = IntMap.empty: Cache.private.lr('a/*string*/)
          generation = 0
          size       = 0
        }

        {get = getter set = setter} = Mutable.make(init:Cache.private.content('a/*string*/, 'b/*float*/, 'order, 'signature/*void*/))

       /**
        * {3 Adapting to option [size_limit]}
        */

       /**
        * Update the age of an information in the [lru]             O(ln(n))
        *
        * (noop when [size_limit] is not set)
        */
       touch_lr = match options.size_limit with
          | {none} -> (x, _, _, _ -> x)
          | {some = _} ->
            action(lru:Cache.private.lr('a/*string*/), old_generation:int, new_generation:int, key:'a/*string*/):Cache.private.lr('a/*string*/) =
               lru = IntMap.remove(old_generation, lru)
               lru = IntMap.add(new_generation, {index=new_generation ~key}, lru)
               lru
            action


        /**
         * Find and remove oldest element from lru
         *
         * Assume lru is not empty
         */
        extract_oldest(lru: Cache.private.lr('a/*string*/)): (Cache.private.lr('a/*string*/), 'a/*string*/) =
           (lru, binding) = IntMap.extract_min_binding(lru)
           match binding with
             | {none} -> error("Internal error: attempting to remove oldest elements of an already empty cache")
             |~{some} -> (lru, some.f2.key)

        trim_to_size = match options.size_limit with
          | {none} -> (x -> x)
          | {some = max_size} ->
              max_size = max(1, max_size) //If [max_size <= 0], well, round up to 1
          (data:Cache.private.content('a/*string*/, 'b/*float*/, 'order, 'signature/*void*/)) ->
             ~{storage lru generation size} = data
             //do jlog("[trim_to_size] starting -- size is {size}, max_size is {max_size}")
             if size <= max_size then
                //do jlog("[trim_to_size] no need to remove")
                data
             else
                //do jlog("[trim_to_size] removing one element")
                (lru, key) = extract_oldest(lru)
                storage = Storage.remove(key, storage)
                ~{storage lru generation size=(size-1)}


        /**
         * {3 Adapting to option [age_limit]}
         */

       /**
        * Determine whether some information is stale, based on [options.age_limit]
        *
        * (noop when [age_limit] is not set)
        */
       is_still_valid = match options.age_limit with
           | {none} -> (_ -> {true})
           | ~{some} ->
           (birth ->
             now = Date.now()
             Order.is_smallereq(Date.between(birth, now), some, Duration.order))

        get_date = match options.age_limit with
           | {none} ->  -> Date.epoch
           | {some = _} -> Date.now

       /**
        * {3 Handle [invalidate]}
        */

       /**
        * Remove a value from the cache
        */
       invalidate_in(key:'a/*string*/, cache:Cache.private.content('a, 'b, 'order, 'signature/*void*/)): option(Cache.private.content('a, 'b, 'order, 'signature/*void*/)) =
       (
          //do jlog("invalidating")
          ~{storage lru generation size} = cache
          match Storage.get(key, storage) with
           | {none} -> {none}
           | ~{some} ->
              lru     = IntMap.remove(some.index, lru)
              storage = Storage.remove(key, storage)
              {some = ~{storage lru generation size=(size-1)}}
       )

       /**
        * Perform invalidation (public API).
        */
       do_invalidate(key:'a/*string*/):void =
       (
          match invalidate_in(key, getter()) with
            | {none} -> void         //No need to update, the value wasn't there in the first place
            |~{some} -> setter(some) //Perform update, possibly erasing result of concurrent computations
       )

       /**
        * {3 Handling [put]}
        */

       /**
        * Insert a value in the cache. Do not update size. Do not increment generation.
        */
       insert_in(key, value, storage, lru, size, generation, signature) =
       (
           //do jlog("[cache] inserting")
           now     = get_date()
           lru     = IntMap.add(generation, {index=generation ~key}, lru)
           storage = Storage.add(key, {index=generation ~value updated = now ~signature}, storage)
           ~{value data = ~{storage lru generation size}}
       )

       /**
        * Compute a new version of the value of [f(key)] and insert it in the cache
        * If the [if_necessary] determines that the value doesn't need to be inserted in the cache, reuse the previous value
        */
       put_newer_version_in(key:'a/*string*/, previous_value:'b/*float*/, storage, lru, size, generation:int, signature:'signature/*void*/) =
       (
           get_diff =  get_manager.get_diff//TODO: => share dereference
           decode_diff=get_manager.decode_diff//TODO: => share dereference
           (signature, diff_value) = get_diff(signature, key)

             //message = match maybe_value with {none} -> "old value is still valud" | {some = _} -> "replacing with new value"
             //do jlog("[put_newer_version_in] {message}")
           value   = decode_diff((previous_value,diff_value))
           insert_in(key, value, storage, lru, size + 1, generation, signature)
       )

       /**
        * Compute for the first time a version of the value of [f(key)] and insert it in the cache
        */
       put_first_version_in(key:'a/*string*/, storage, lru, size, generation:int) =
       (
           get_always =  get_manager.get_always
           (signature, value) = get_always(key)
           insert_in(key, value, storage, lru, size + 1, generation, signature)
       )

       /**
        * Manual insertion in the cache
        */
       do_put(key:'a/*string*/, value:'b/*float*/, signature:'signature/*void*/): void =
       (
           cache = getter()
           cache = match invalidate_in(key, cache) with
             | {none} -> cache
             | ~{some}-> some
           ~{storage lru generation size} = cache
           cache = insert_in(key, value, storage, lru, size + 1, generation + 1, signature).data
           cache = trim_to_size(cache)
           do setter(cache)
           void
       )


       /**
        * {3 Handle [get]}
        */
       do_get(key:'a/*string*/):'b/*float*/ =
       (
          //do jlog("[do_get] starting")
          content = getter()
          ~{storage lru generation size} = content
          generation = generation + 1
          //do jlog("[do_get] ready to search")
          match Storage.get(key, storage) with                                     //Search for [key] in current storage                               O(ln(n))
            |  {some = ~{index value updated signature}} ->                        //If [key] was found
                  //do jlog("[do_get] found key")
                  if is_still_valid(updated) then                                  // and if the data is not too old                                   O(1)
                     //do jlog("[do_get] data is still valid")
                     stored  = {index=generation ~value ~updated ~signature}       //     Mark value as having been seen recently                      O(ln(n))
                     storage = Storage.add(key, stored, storage)
                     lru = touch_lr(lru, index, generation, key)
                     do setter(~{storage lru generation size})                     //     Update cache (possibly erasing result of competing threads)  O(1)
                     value                                                         //     Return value
                  else                                                             // otherwise, data may be obsolete, we need to renegociate
                     //do jlog("[do_get] data is old, need to check whether it's obsolete")
                     ~{storage lru generation size} = Option.get(invalidate_in(key, content))
                                                                                   //     Invalidate value, decreasing size                           O(ln(n))
                     ~{value data} = put_newer_version_in(key, value, storage, lru, size, generation, signature)
                                                                                   //     Refresh value, reincreasing size                            O(ln(n))
                     data = trim_to_size(data)
                     do setter(data)                                               //     Update cache (possibly erasing result of competing threads)  O(1)
                     value                                                         //     Return value
            |  {none} ->
                     //do jlog("[do_get] key not found")
                     ~{value data} = put_first_version_in(key, storage, lru, size, generation)
                     data = trim_to_size(data)
                     do setter(data)
                     value
        )


       do_fetch(keys:list('a)/*string*/): void =
       (
          //do jlog("[do_get] starting")
          content = getter()
          //do jlog("[do_get] ready to search")
          (to_update, content) = List.fold((key, (acc_to_update, content) ->
            ~{storage lru generation size} = content
            match Storage.get(key, storage) with                                     //Search for [key] in current storage                               O(ln(n))
              |  ~{some} ->                                                          //If [key] was found
                    ~{index value updated signature} = some
                    //do jlog("[do_get] found key")
                    if is_still_valid(updated) then                                  // and if the data is not too old                                   O(1)
                         //do jlog("[do_get] data is still valid")
                       generation = generation + 1
                       stored  = {index=generation ~value ~updated ~signature}       //     Mark value as having been seen recently                      O(ln(n))
                       storage = Storage.add(key, stored, storage)
                       lru = touch_lr(lru, index, generation, key)
                       (acc_to_update, ~{storage lru generation size})
                    else                                                             // otherwise, data may be obsolete, we need to renegociate
                       //do jlog("[do_get] data is old, need to check whether it's obsolete")
                       content = Option.get(invalidate_in(key, content))
                                                                                     //     Invalidate value, decreasing size                            O(ln(n))
                       ([ ({renegociate=signature previous=value}, key) | acc_to_update ], content)      //     Ask for renegociation
              |  {none} ->
                       ([ ({none}, key) | acc_to_update ], content)                  //     Ask for initial negociation
           ), keys, ([], content))
           instructions_to_transmit = List.map(((detail, key) ->
                                    match detail with
                                          | {none} -> ({none}, key)
                                          | {renegociate=signature previous=_} -> ({some = signature}, key)
                                    ), to_update)//TODO: Should be an array
           updates = get_manager.get_many(instructions_to_transmit)
           decoder = get_manager.decode_diff
           content = List.fold2(((signature, key, reply), instruction, content ->
             ~{storage lru generation size} = content
             generation = generation+1
             {data=content value=_} = match reply with
               | ~{diff} ->
                  previous_value =
                    match instruction.f1 with
                      | {renegociate=_ ~previous} -> previous
                      | {none} -> error("Internal error in cache: renegociation indicated that previous version was still valid, but there is no previous version")
                    end
                  new_value = decoder((previous_value,diff))
                  insert_in(key, new_value, storage, lru, size + 1, generation, signature)
               | ~{init} ->
                  insert_in(key, init, storage, lru, size + 1, generation, signature)
             content
            ), updates, to_update, content)
           setter(content)                                                           //     Store result, possibly erasing result of competing threads  O(1)
          )

        /**
         * {3 Handle [do_read]}
         */
       do_read(key:'a/*string*/):option('b/*float*/) =
       (
          //do jlog("[do_read] starting")
          storage = getter().storage
          //do jlog("[do_read] ready to search")
          match Storage.get(key, storage) with                       //Search for [key] in current storage                               O(ln(n))
            | ~{some} -> /*do jlog("[do_read] found")*/ {some = some.value}
            | {none}  -> /*do jlog("[do_read] not found")*/ {none}
       )


       {get=do_get invalidate=do_invalidate put=do_put read=do_read fetch=do_fetch}
     )


     //TODO: make_async(...) = ...
}}


/**
 * {1 Private section}
 */

type Cache.private.lr.entry('a) =
{
        index: int
        key:   'a
}

type Cache.private.lr('a) = intmap(Cache.private.lr.entry('a))

type Cache.private.storage.entry('b, 'signature) =
{
        index: int
        value: 'b
        updated: Date.date
        signature: 'signature
}

type Cache.private.content('a, 'b, 'order, 'signature) =
{
          storage:     ordered_map('a, Cache.private.storage.entry('b, 'signature), 'order)
          lru    :     Cache.private.lr('a)
          generation:  int
          size:        int
}
