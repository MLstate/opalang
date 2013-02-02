/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package stdlib.apis.mongo

/**
 * MongoDB binding for OPA.
 *
 * @destination public
 * @stabilization work in progress
 **/

/**
 * {1 About this module}
 *
 * Module [MongoCommon] has common routines for handling responses from MongoDB driver functions.
 *
 * This module also has routines for dealing with tags and for accessing the [Mongo.reply] external type.
 *
 * {1 Where should I start?}
 *
 * {1 What if I need more?}
 *
 **/

/** Type of concurrency handling **/
type Mongo.concurrency = {pool} / {cell} / {singlethreaded}

/**
 * Mongo success status, just a document.
 **/
type Mongo.success = Bson.document
type Mongo.successes = list(Bson.document)

/**
 * A Mongo driver result value is either a valid document
 * or a [Mongo.failure] value.
 **/
type Mongo.result = outcome(Mongo.success, Mongo.failure)
type Mongo.results = outcome(Mongo.successes, Mongo.failure)

/**
 * Many Mongo driver functions return OPA types or failures.
 **/
type Mongo.valresult('a) = outcome('a,Mongo.failure)
type Mongo.valresults('a) = outcome(list('a),Mongo.failure)

/**
 * A Mongo error is either an error value which is an OPA
 * value containing the error information from a [Bson.document]
 * or a [Mongo.failure] value.
 **/
type Mongo.error = outcome(Bson.error, Mongo.failure)

/* Flag tags */

/** OP_INSERT **/
type Mongo.insert_tag =
  {ContinueOnError}

/** OP_UPDATE **/
type Mongo.update_tag =
  {Upsert} /
  {MultiUpdate}

/** OP_QUERY **/
type Mongo.query_tag =
  {TailableCursor} /
  {SlaveOk} /
  {OplogReplay} /
  {NoCursorTimeout} /
  {AwaitData} /
  {Exhaust} /
  {Partial}

/** OP_DELETE **/
type Mongo.delete_tag =
  {SingleRemove}

/** OP_REPLY **/
type Mongo.reply_tag =
  {CursorNotFound} /
  {QueryFailure} /
  {ShardConfigStale} /
  {AwaitCapable}

/** Tags for indices **/
type Mongo.index_tag =
  {Unique} /
  {DropDups} /
  {Background} /
  {Sparse}

/**
 *  We wrap the tags so that we can tell if it is an insert tag,
 *  query tag etc.  We don't want to send SingleRemove to an update.
 **/
type Mongo.mongo_tag =
  {itag:Mongo.insert_tag} /
  {utag:Mongo.update_tag} /
  {qtag:Mongo.query_tag} /
  {dtag:Mongo.delete_tag} /
  {rtag:Mongo.reply_tag} /
  {xtag:Mongo.index_tag}

@server_private
MongoCommon = {{

  @private ML = MongoLog
  @private H = Bson.Abbrevs
  @private WP = WireProtocol

  /** Code for query operations */
  _OP_QUERY = 2004

  /** Return new Bson Object ID */
  new_oid = WP.new_oid

  /** Get OID from string */
  oid_of_string = WP.oid_of_string

  /** Get string from OID */
  oid_to_string = WP.oid_to_string

  /** Generate a driver error message outcome **/
  failErr(msg:string): outcome('a,Mongo.failure) = {failure={Error=msg}}

  /** Map either a success function or a failure function over an outcome **/
  map_outcome(outcome:outcome('s,'f), sfn:'s->'t, ffn:'f->'g): outcome('t,'g) =
    match outcome with
    | {~success} -> {success=sfn(success)}
    | {~failure} -> {failure=ffn(failure)}

  /** Map a function over a success, leave failures untouched **/
  map_success(outcome, sfn) = map_outcome(outcome, sfn, (f -> f))

  /** Map a function over a failure, leave successes untouched **/
  map_failure(outcome, ffn) = map_outcome(outcome, (s -> s), ffn)

  /** Map a pair of convergent functions over an outcome **/
  outcome_map(outcome:outcome('s,'f), sfn:'s->'r, ffn:'f->'r): 'r =
    match outcome with
    | {~success} -> sfn(success)
    | {~failure} -> ffn(failure)

  /* Map a list of outcomes onto an outcome of a list. */
  Outcome_list(f:'a->outcome('b,'c), l:list('a)): outcome(list('b),'c) =
    rec aux(l) =
      match l with
      | [a|t] ->
         (match f(a) with
          | {success=b} ->
             (match aux(t) with
              | {success=l} -> {success=[b|l]}
              | {~failure} -> {~failure})
          | {~failure} -> {~failure})
      | [] -> {success=[]}
    aux(l)

  /** Same as [outcome_map] but coerced to string **/
  string_of_outcome = (outcome_map:outcome('s,'f), ('s->string), ('f->string) -> string)

  /** Turn a result into an option **/
  result_to_option(result:outcome('a,'b)) : option('a) = match result with | {success=v} -> {some=v} | _ -> {none}

  /** Turn result into a list **/
  results_to_list(result:outcome(list('a),'b)) : list('a) = match result with | {success=v} -> v | _ -> []

  /** Turn a result into a [Mongo.error] value **/
  error_of_result(result:Mongo.result): Mongo.error = map_success(result, Bson.error_of_document)

  /** Make a readable string out of a [Mongo.error] value **/
  string_of_error(error:Mongo.error): string = outcome_map(error, Bson.string_of_error, string_of_failure)

  /** String representation of a [Mongo.failure] value **/
  string_of_failure(failure:Mongo.failure): string =
    match failure with
    | {Error=str} -> str
    | {DocError=doc} -> Bson.string_of_doc_error(doc)
    | {OK} -> "Ok"
    | {Incomplete} -> "Incomplete"
    | {NotFound} -> "NotFound"
    | {SlaveOK} -> "SlaveOK"

  /** Make an error report string out of a [Mongo.result] value, will print "<ok>" if no error. **/
  string_of_result(result:Mongo.result): string = outcome_map(result, Bson.string_of_doc_error, string_of_failure)

  /** Same for a list of results. **/
  string_of_results(results:Mongo.results): string =
    outcome_map(results, (l -> List.list_to_string(Bson.string_of_doc_error,l)), string_of_failure)

  /** Similar to [string_of_result] but the success value is user-defined. **/
  string_of_value_or_failure(result:outcome('a,Mongo.failure), success_to_str:'a->string): string =
    string_of_outcome(result, success_to_str, (failure -> "\{failure={string_of_failure(failure)}\}"))

  /** Either pretty-print the document or generate a failure string. **/
  pretty_of_result(result:Mongo.result): string = string_of_value_or_failure(result,Bson.to_pretty)

  /** Same as [pretty_of_result] but for a list of results. **/
  pretty_of_results(results:Mongo.results): string =
    string_of_value_or_failure(results,(l -> List.list_to_string(Bson.to_pretty,l)))

  /** Predicate for error status of a [Mongo.result] value. **/
  is_error(result:Mongo.result): bool = outcome_map(result, Bson.is_error, (_ -> true))
  is_not_master(result:Mongo.result): bool = outcome_map(result, Bson.is_not_master, (_ -> false))

  /** Predicate for error status of a [Mongo.result] value. **/
  isError(error:Mongo.error): bool = outcome_map(error, Bson.isError, (_ -> true))

  /**
   * Validate a BSON document by turning it into a [Mongo.result] value.
   * If [ok] is not 1 then turn it into a [Mongo.failure] value.
   * Note that if there is no "ok" element then we assume success.
   **/
  check_ok(doc:Bson.document): Mongo.result =
    match Bson.find_int(doc,"ok") with
    | {some=ok} ->
       if ok == 1
       then {success=doc}
       else {failure={DocError=doc}}
    | _ -> {success=doc}

  /**
   * Outcome-wrapped versions of Bson.find_xxx etc.
   **/
  @private
  result_(result:Mongo.result, key:string, find:(Bson.document, string -> option('a))): option('a) =
    match result with
    | {success=doc} -> find(doc,key)
    | {failure=_} -> {none}

  result_bool(result:Mongo.result,key:string): option(bool) = result_(result, key, Bson.find_bool)
  result_int(result:Mongo.result,key:string): option(int) = result_(result, key, Bson.find_int)
  result_float(result:Mongo.result,key:string): option(float) = result_(result, key, Bson.find_float)
  result_string(result:Mongo.result,key:string): option(string) = result_(result, key, Bson.find_string)
  result_doc(result:Mongo.result,key:string): option(Bson.document) = result_(result, key, Bson.find_doc)

  /**
   * Same as outcome-wrapped versions but allowing dot notation.
   **/
  @private
  dotresult_(result:Mongo.result,key:string,find:(Bson.document, string -> option('a))): option('a) =
    match result with
    | {success=doc} -> Bson.find_dot(doc,key,find)
    | {failure=_} -> {none}

  dotresult_bool(result:Mongo.result,key:string): option(bool) = dotresult_(result, key, Bson.find_bool)
  dotresult_int(result:Mongo.result,key:string): option(int) = dotresult_(result, key, Bson.find_int)
  dotresult_float(result:Mongo.result,key:string): option(float) = dotresult_(result, key, Bson.find_float)
  dotresult_string(result:Mongo.result,key:string): option(string) = dotresult_(result, key, Bson.find_string)
  dotresult_doc(result:Mongo.result,key:string): option(Bson.document) = dotresult_(result, key, Bson.find_doc)

  /**
   * If a result is success then return an OPA value from the
   * document using [Bson.doc2opa].  Must be cast at point of call.
   **/
  result_to_opa(result:Mongo.result): option('a) =
    match result with
    | {success=doc} -> (Bson.doc2opa(doc):option('a))
    | {failure=_} -> {none}

  /**
   * Same as [result_to_opa] but returning an outcome instead of an option.
   **/
  resultToOpa(result:Mongo.result): outcome('a,Mongo.failure) =
    match result with
    | {success=doc} ->
       (match (Bson.doc2opa(doc):option('a)) with
        | {some=a} -> {success=a}
        | {none} -> failErr("MongoDriver.resultToOpa: document conversion failure"))
    | {~failure} -> {~failure}

  /** Flag bitmasks **/

  /* OP_INSERT */
  ContinueOnErrorBit  = 0x00000001

  /* OP_UPDATE */
  UpsertBit           = 0x00000001
  MultiUpdateBit      = 0x00000002

  /* OP_QUERY */
  TailableCursorBit   = 0x00000002
  SlaveOkBit          = 0x00000004
  OplogReplayBit      = 0x00000008
  NoCursorTimeoutBit  = 0x00000010
  AwaitDataBit        = 0x00000020
  ExhaustBit          = 0x00000040
  PartialBit          = 0x00000080

  /* OP_DELETE */
  SingleRemoveBit     = 0x00000001

  /* OP_REPLY */
  CursorNotFoundBit   = 0x00000001
  QueryFailureBit     = 0x00000002
  ShardConfigStaleBit = 0x00000004
  AwaitCapableBit     = 0x00000008

  /* Flags used by the index routines. */
  UniqueBit     = 0x00000001
  DropDupsBit   = 0x00000002
  BackgroundBit = 0x00000004
  SparseBit     = 0x00000008

  /**
   *  [flag_of_tag]:  Turn a list of tags into a bit-wise flag suitable
   *  for sending to MongoDB.  We have an extra layer of types to allow
   *  forcing of tags to belong to a particular operation.
   **/
  flag_of_tag(tag:Mongo.mongo_tag): int =
    match tag with
      /* OP_INSERT */
    | {itag={ContinueOnError}} -> ContinueOnErrorBit

      /* OP_UPDATE */
    | {utag={Upsert}} -> UpsertBit
    | {utag={MultiUpdate}} -> MultiUpdateBit

      /* OP_QUERY */
    | {qtag={TailableCursor}} -> TailableCursorBit
    | {qtag={SlaveOk}} -> SlaveOkBit
    | {qtag={OplogReplay}} -> OplogReplayBit
    | {qtag={NoCursorTimeout}} -> NoCursorTimeoutBit
    | {qtag={AwaitData}} -> AwaitDataBit
    | {qtag={Exhaust}} -> ExhaustBit
    | {qtag={Partial}} -> PartialBit

      /* OP_DELETE */
    | {dtag={SingleRemove}} -> SingleRemoveBit

      /* OP_REPLY */
    | {rtag={CursorNotFound}} -> CursorNotFoundBit
    | {rtag={QueryFailure}} -> QueryFailureBit
    | {rtag={ShardConfigStale}} -> ShardConfigStaleBit
    | {rtag={AwaitCapable}} -> AwaitCapableBit

      /* Index tags */
    | {xtag={Unique}} -> UniqueBit
    | {xtag={DropDups}} -> DropDupsBit
    | {xtag={Background}} -> BackgroundBit
    | {xtag={Sparse}} -> SparseBit

  /**
   * Turn a list of tags into a single MongoDB-compatible int.
   **/
  flags(tags:list(Mongo.mongo_tag)): int =
    List.fold_left((flag, tag -> Bitwise.land(flag,flag_of_tag(tag))),0,tags)

  /**
   *  Extract the tags from a given bit-wise flag.  These are specific
   *  to each operation, you need to know which operation the flag was for/from
   *  before you can give meaning to the bits.
   **/
  insert_tags(flag:int): list(Mongo.mongo_tag) =
    if Bitwise.land(flag,ContinueOnErrorBit) != 0 then [{itag={ContinueOnError}}] else []

  update_tags(flag:int): list(Mongo.mongo_tag) =
    tags = if Bitwise.land(flag,UpsertBit) != 0 then [{utag={Upsert}}] else []
    if Bitwise.land(flag,MultiUpdateBit) != 0 then [{utag={MultiUpdate}}|tags] else tags

  query_tags(flag:int): list(Mongo.mongo_tag) =
    tags = if Bitwise.land(flag,TailableCursorBit) != 0 then [{qtag={TailableCursor}}] else []
    tags = if Bitwise.land(flag,SlaveOkBit) != 0 then [{qtag={SlaveOk}}|tags] else tags
    tags = if Bitwise.land(flag,OplogReplayBit) != 0 then [{qtag={OplogReplay}}|tags] else tags
    tags = if Bitwise.land(flag,NoCursorTimeoutBit) != 0 then [{qtag={NoCursorTimeout}}|tags] else tags
    tags = if Bitwise.land(flag,AwaitDataBit) != 0 then [{qtag={AwaitData}}|tags] else tags
    tags = if Bitwise.land(flag,ExhaustBit) != 0 then [{qtag={Exhaust}}|tags] else tags
    if Bitwise.land(flag,PartialBit) != 0 then [{qtag={Partial}}|tags] else tags

  delete_tags(flag:int): list(Mongo.mongo_tag) =
    if Bitwise.land(flag,SingleRemoveBit) != 0 then [{dtag={SingleRemove}}] else []

  reply_tags(flag:int): list(Mongo.mongo_tag) =
    tags = if Bitwise.land(flag,CursorNotFoundBit) != 0 then [{rtag={CursorNotFound}}] else []
    tags = if Bitwise.land(flag,QueryFailureBit) != 0 then [{rtag={QueryFailure}}|tags] else tags
    tags = if Bitwise.land(flag,ShardConfigStaleBit) != 0 then [{rtag={ShardConfigStale}}|tags] else tags
    if Bitwise.land(flag,AwaitCapableBit) != 0 then [{rtag={AwaitCapable}}|tags] else tags

  index_tags(flag:int): list(Mongo.mongo_tag) =
    tags = if Bitwise.land(flag,UniqueBit) != 0 then [{xtag={Unique}}] else []
    tags = if Bitwise.land(flag,DropDupsBit) != 0 then [{xtag={DropDups}}|tags] else tags
    tags = if Bitwise.land(flag,BackgroundBit) != 0 then [{xtag={Background}}|tags] else tags
    if Bitwise.land(flag,SparseBit) != 0 then [{xtag={Sparse}}|tags] else tags

  /**
   * A string representation of a [Mongo.mongo_tag] value.
   **/
  string_of_tag(tag:Mongo.mongo_tag): string =
    match tag with
    | {itag={ContinueOnError}} -> "ContinueOnError"
    | {utag={Upsert}} -> "Upsert"
    | {utag={MultiUpdate}} -> "MultiUpdate"
    | {qtag={TailableCursor}} -> "TailableCursor"
    | {qtag={SlaveOk}} -> "SlaveOk"
    | {qtag={OplogReplay}} -> "OplogReplay"
    | {qtag={NoCursorTimeout}} -> "NoCursorTimeout"
    | {qtag={AwaitData}} -> "AwaitData"
    | {qtag={Exhaust}} -> "Exhaust"
    | {qtag={Partial}} -> "Partial"
    | {dtag={SingleRemove}} -> "SingleRemove"
    | {rtag={CursorNotFound}} -> "CursorNotFound"
    | {rtag={QueryFailure}} -> "QueryFailure"
    | {rtag={ShardConfigStale}} -> "ShardConfigStale"
    | {rtag={AwaitCapable}} -> "AwaitCapable"
    | {xtag={Unique}} -> "Unique"
    | {xtag={DropDups}} -> "DropDups"
    | {xtag={Background}} -> "Background"
    | {xtag={Sparse}} -> "Sparse"

  /** String of a list of tags. **/
  string_of_tags(tags:list(Mongo.mongo_tag)): string = List.list_to_string(string_of_tag,tags)

  /** Access components of the reply value **/
  reply_messageLength = WP.reply_messageLength
  reply_requestId = WP.reply_requestId
  reply_responseTo = WP.reply_responseTo
  reply_opCode = WP.reply_opCode
  reply_responseFlags = WP.reply_responseFlags
  reply_cursorID = WP.reply_cursorID
  reply_startingFrom = WP.reply_startingFrom
  reply_numberReturned = WP.reply_numberReturned

  /** Return the n'th document attached to the reply **/
  reply_document = WP.reply_document_pos

  /** Null cursor value **/
  null_cursorID(_) = Int64.zero

  /** Return a string representation of a cursor (it's an int64) **/
  string_of_cursorID(cid) = Int64.to_string_radix(cid,16)

  /** Predicate for end of query, when the cursorID is returned as zero **/
  is_null_cursorID(cid) = Int64.op_eq(cid,Int64.zero)

  /**
   * Extract a document from a reply.
   *
   * Example: [reply_to_result(from, n, reply_opt)]
   *
   * @param from A string included in failure messages.
   * @param n The number of the document required (base 0).
   * @param reply_opt The optional reply (this is what most query operations return).
   **/
  reply_to_result(from:string, n:int, reply_opt: option(Mongo.reply)): Mongo.result =
    match reply_opt with
    | {some=reply} ->
       numberReturned = reply_numberReturned(reply)
       if numberReturned <= n
       then failErr("{from}: insufficient number of documents {numberReturned} vs {n}")
       else
         (match reply_document(reply,n) with
          | {some=doc} -> {success=doc}
          | {none} -> failErr("{from}: no document in reply"))
    | {none} -> failErr("{from}: no reply")

  reply_is_not_master(reply_opt: option(Mongo.reply)): bool =
    match reply_opt with
    | {some=reply} ->
       numberReturned = reply_numberReturned(reply)
       if numberReturned != 1
       then false
       else
         (match reply_document(reply,0) with
          | {some=doc} -> Bson.is_not_master(doc)
          | {none} -> false)
    | {none} -> false

  /**
   * Extract all documents from a reply.
   *
   * Example: [reply_to_results(from, reply_opt)]
   *
   * @param from A string included in failure messages.
   * @param reply_opt The optional reply (this is what most query operations return).
   **/
  reply_to_results(from:string, reply_opt: option(Mongo.reply)): Mongo.results =
    match reply_opt with
    | {some=reply} ->
       Outcome_list((n ->
                     match reply_document(reply,n) with
                     | {some=doc} -> {success=doc}
                     | {none} -> failErr("{from}: no document in reply")),
                    List.init((n -> n),reply_numberReturned(reply)))
    | {none} -> failErr("{from}: no reply")

}}

//End of file common.opa

