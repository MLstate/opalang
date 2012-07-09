/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/**
 * {1 About this module}
 *
 * This module defines some high level functions to manipulate a couchdb database.
 * It's build on top of [CouchDb].
 *
 * {1 Where should I start?}
 *
 * You should obviously start by setting up a couchdb server somewhere (cf.
 * http://couchdb.apache.org/ ) and having a look at the couchdb official
 * documentation.
 *
 * {1 What if I need more?}
 *
 * The [CouchDb] module provides numerous "low-level" functions to interract
 * with CouchDb, you will probably find what you need there.
 *
 */

import stdlib.core.{web.client, web.core, rpc.core}
import stdlib.apis.couchdb

/**
 * {1 The types}
 */

type Opacouch.success('a) =
    {FormatedRecord : (int, option('a))}
  / CouchDb.success

type Opacouch.result('a) = outcome(Opacouch.success('a), CouchDb.failure)

 /**
 * {1 The API}
 */

Opacouch = {{

    @private embed_rev(document, rev) =
      match document
      | {Record = fields} ->
          match List.assoc("_rev", fields)
          | {some = ~{String}} ->
              if String == rev
                then {success = document}
                else {failure = {Other = "The _rev field of the document doesn't match the last revision in the db"}}
          | _ -> {success = {Record = [("_rev", {String = rev} : RPC.Json.json) | fields]}}
          end
      | _ -> {failure = {InvalidJsonDoc}}

    update_doc(auth, the_db, docid, document) =
      doc = OpaSerialize.Json.serialize(document)
      rev = Option.get(get_revision(auth, the_db, docid))
      match embed_rev(doc, rev)
      | ~{failure} -> ~{failure}
      | ~{success} -> CouchDb.Document.put(auth, the_db, docid, success)

    create_doc(auth, the_db, docid, document) =
      doc = OpaSerialize.Json.serialize(document)
      CouchDb.Document.put(auth, the_db, docid, doc)

    insert_doc(auth, the_db, docid, document) =
      doc = OpaSerialize.Json.serialize(document)
      rev = Option.default(dummy_revision, get_revision(auth, the_db, docid))
      match embed_rev(doc, rev)
      | ~{failure} -> ~{failure}
      | ~{success} -> CouchDb.Document.put(auth, the_db, docid, success)

    dummy_revision = "0-0" : CouchDb.revision

    retrieve_doc(auth, the_db, docid) : Opacouch.result('a) =
      match CouchDb.Document.get(auth, the_db, docid)
      | {success = {FormatedJson = (code, {some = json_doc})}} ->
          // we unserialize only when the HTTP code is 2XX (i.e. the request
          // succeeded) since we don't know what structure will have the error
          // message.
          //
          // TODO: check if it's always {error : string , reason : string}
          //  if it is, we COULD return :
          //    option('a / {error : string , reason : string })
          if (code >= 200 && code < 300) then
            {success = { FormatedRecord = (code, OpaSerialize.Json.unserialize_unsorted(json_doc)) }}
          else {success = {FormatedJson = (code, {some = json_doc})}}
      | ~{failure} -> ~{failure}
      | _ -> do @assert(false) {failure = {Other = "This should never happen."}}

    /**
     * Get the revision of a document.
     * @return [none] if the document doesn't exists.
     */
    get_revision(auth, the_db, docid) : option(CouchDb.revision) =
      match CouchDb.Document.head(auth, the_db, docid)
      | {success = {RawResponse = {code=200 ~header_get ... }}} ->
          match header_get("ETag")
          | {none} -> {none} // This should never happen.
          | {some = rev} ->
              some = String.drop_right(1, String.drop_left(1, rev))
              ~{some}
          end
      | _ -> {none}

}}
