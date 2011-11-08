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
 * MongoDB binding for OPA.
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * [MongoUtils] is a set of utility functions to make programming
 * using the MongoDB driver easier.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

MongoUtils = {{

   /** A safe operation checking the error (still have to check if the
    *  last error is really the last error, using eg. findAndModify).
    **/
   @private
   safe_(c:Mongo.collection('value),f:'a->bool,a:'a,msg:string): bool =
     if not(f(a))
     then (do println("{msg}: Fatal error message not sent to server") false)
     else
       (match MongoConnection.getLastError(c.db) with
        | {~success} ->
           (match Bson.find_string(success, "err") with
            | {some=""} | {none} -> true
            | {some=err} -> do println("{msg}: {err}") false)
        | {~failure} -> do println("{msg}: fatal error {MongoDriver.string_of_failure(failure)}") false)

   safe_insert(c,v) = safe_(c,((c,v) -> MongoCollection.insert(c,v)),(c,v),"Collection.insert")
   safe_insert_batch(c,b) = safe_(c,((c,b) -> MongoCollection.insert_batch(c,b)),(c,b),"Collection.insert_batch")
   safe_update(c,s,v) = safe_(c,((c,s,v) -> MongoCollection.update(c,s,v)),(c,s,v),"Collection.update")
   safe_delete(c,s) = safe_(c,((c,s) -> MongoCollection.delete(c,s)),(c,s),"Collection.delete")

    // It's easier to deal with options
    find_result_to_opt(result) : option('a) =
       match result with
       | {success=v} -> {some=v}
       | _ -> {none}
   
    // Idem with list and empty list
    find_all_result_to_list(result) : list('a) =
       match result with
       | {success=v} -> v
       | _ -> []

    find(c,r) = find_result_to_opt(MongoCollection.find_one(c,MongoSelect.unsafe_make(r)))
    find_all(c,r) = find_all_result_to_list(MongoCollection.find_all(c,MongoSelect.unsafe_make(r)))

    // Delete by id by default
    delete(c,id) = MongoCollection.delete(c,MongoSelect.unsafe_make({_id = id}))

}}

// End of file utils.opa

