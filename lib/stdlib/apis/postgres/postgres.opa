/*
    Copyright © 2011-2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
/**
 * Postgres low-level driver
 *
 * This file provides the implemented methods of the Postgres API.
 *
 * @category web
 * @author Norman Scaife, 2012
 * @destination public
 * @stability Work in progress
 */

import stdlib.apis.apigenlib
import stdlib.crypto

/**
 * {1 About this module}
 *
 * This module provides basic, low-level support for connecting to a PosgreSQL server.
 * Routines are provided to send a subset of the PostgreSQL commands and a single input
 * routine is provided to handle incoming messages.  Note that some of the messages sent
 * do not expect a reply whereas others do.
 *
 * The connection object includes data gathered during the send and receive operations
 * but mostly the driver operates by calling callbacks, of which there are three: a row
 * callback which is called when a data row is received from the server, a finalize callback
 * which is called when an operation terminates and an error callback which is called
 * if an error was received from PostgreSQL.
 *
 * {1 Where should I start?}
 *
 * Take a look at the example code (test_pg.opa).  This shows how to perform basic queries
 * and handle the results.  Currently, only the raw data is returned, functions may be
 * provided in future to convert the result data into Opa format.
 *
 * {1 What if I need more?}
 *
 * Study the auto-generated module Pg which shows how the underlying code works.  You can
 * add more functionality exterior to this module by copying the code shown there.
 *
 */

/**
 * {1 Types defined in this module}
 */

/** A message (error or notice) from the PostgreSQL server.
 */
type Postgres.msg = list((int,string))

/** Either a PostgreSQL error or notice.
 */
type Postgres.en =
   { error : Postgres.msg }
 / { notice : Postgres.msg }

/** Types of replies to [Authentication] message replies.
 */
type Postgres.auth_reply =
     { Ok }
   / { MD5Password: binary }
   / { GSSContinue: binary }
   / { SSPI }
   / { GSS }
   / { SCMCredential }
   / { CleartextPassword }
   / { KerberosV5 }

/** Type of a reply from PostgreSQL.
 */
type Postgres.reply =
   { ReadyForQuery: string }
 / { ErrorResponse: Postgres.msg }
 / { NoticeResponse: Postgres.msg }
 / { DataRow: list(binary) }
 / { RowDescription: list(tuple_7(string, int, int, int, int, int, int)) }
 / { EmptyQueryResponse }
 / { BackendKeyData: tuple_2(int, int) }
 / { ParameterStatus: tuple_2(string, string) }
 / { Authentication: Postgres.auth_reply }
 / { PortalSuspended }
 / { ParameterDescription: list(int) }
 / { NotificationResponse: tuple_3(int, string, string) }
 / { NoData }
 / { FunctionCallResponse: binary }
 / { CopyBothResponse: tuple_2(int, list(int)) }
 / { CopyOutResponse: tuple_2(int, list(int)) }
 / { CopyDoneB }
 / { CopyDataB: binary }
 / { CloseComplete }
 / { BindComplete }
 / { CommandComplete: string }
 / { ParseComplete }

/** Type of errors from the Postgres driver, includes [Apigen] failures as
 * well as errors from the PostgreSQL server and driver internal errors.
 */
type Postgres.failure =
   { api_failure : Apigen.failure }
 / { postgres : Postgres.en }
 / { bad_reply : Postgres.reply }
 / { bad_row : (list(Postgres.rowdesc),list(binary)) }
 / { bad_format : int }
 / { no_key }
 / { not_found }

/** Failure return for some Postgres module function (it includes the connection itself).
 */
type Postgres.unsuccessful = (Postgres.connection,Postgres.failure)

/** Standard success value is just an updated connection.
 */
type Postgres.success = Postgres.connection

/** Standard result value.
 */
type Postgres.result = outcome(Postgres.success,Postgres.unsuccessful)

/** Row description, value returned from the PostgreSQL server.
 */
type Postgres.rowdesc = {
  name : string
  table_id : int
  table_attribute_number : int
  type_id : int
  data_type_size : int
  type_modifier : int
  format_code : int
}

/** List of row descriptions.
 */
type Postgres.rowdescs = list(Postgres.rowdesc)

/** PostgreSQL raw data can be text or binary.
 */
type Postgres.data = {text:string} / {binary:binary}

/** A row is just a list of binary values.  You need the [format_code] values
 * to tell if each one is text or binary.
 */
type Postgres.row = list(binary)

/** A row value passed to the row continuation.
 * Includes the row number (in the command return values), the row description and the raw row data.
 */
type Postgres.full_row = (int,Postgres.rowdescs,Postgres.row)

/** A complete set of rows, one row description and multiple rows.
 */
type Postgres.rows = (list(Postgres.rowdesc),list(Postgres.row))

/** A Postgres driver connection object.
 */
type Postgres.connection = {
  name          : string /** A name for the connection */
  secure        : option(SSL.secure_type) /** Optional SSL security information */
  conn          : ApigenLib.connection /** The underlying ApigenLib connection object */
  major_version : int /** The major version for the protocol */
  minor_version : int /** The minor version for the protocol */
  dbase         : string /** The name of the database to connected to */
  params        : stringmap(string) /** Map of parameter values returned during authentication */
  processid     : int /** The processid for the connection at the server */
  secret_key    : int /** The connection's secret data (used in cancel requests) */
  query         : string /** A note of the last query command */
  status        : string /** The last received status code from the server */
  suspended     : bool /** Whether an execution was suspended or not */
  error         : option(Postgres.failure) /** The last received error value (driver internal) */
  empty         : bool /** Whether the last query returned an empty reply */
  completed     : list(string) /** List of [CommandComplete] messages received */
  rowdescs      : Postgres.rowdescs /** A note of the most recent row descriptions */
  rows          : int /** A list of the rows received from the last query */
  paramdescs    : list(int) /** List of the last-received parameter descriptions */
  rowcc         : continuation(Postgres.full_row) /** The row continuation */
  errcc         : continuation(Postgres.unsuccessful) /** The error continuation */
}

/** Defines whether an operation is for a prepared statement or a portal */
type Postgres.sp = {statement} / {portal}

/**
 * {1 Interface}
 */
Postgres = {{

  @private bindump = (%% BslPervasives.bindump %%: binary -> string)

  @private to_rowdesc((name,table_id,table_attribute_number,type_id,data_type_size,type_modifier,format_code)) =
    ~{name table_id table_attribute_number type_id data_type_size type_modifier format_code} : Postgres.rowdesc 

  @private string_of_sp(sp:Postgres.sp) =
    match sp with
    | {statement} -> "S"
    | {portal} -> "P"

  /** The default host for a local PostgreSQL server: localhost:5432 */
  default_host = Pg.default_host

  /** The default major version of the protocol, you shouldn't ever have to set this */
  default_major_version = Pg.default_major_version

  /** The default minor version of the protocol, you shouldn't ever have to set this */
  default_minor_version = Pg.default_minor_version

  /** The ApigenLib connection module, contains send and receive functions. */
  Conn = Pg.Conn

  /** Create connection object.
   *
   * This function returns a fresh [Postgres.connection] object.
   * All internal parameters are reset and the object contains all the information
   * to manage the connection but doesn't actually open a connection to the server
   * until an operation is performed on the connection.
   *
   * We keep the continuations for rows and errors in the connection to make the
   * interface simpler, these probably won't change during the lifetime of the connection.
   * The finalisation continuation is provided at the point of call.
   *
   * @param name The name of the connection, this is only relevant if you have connections to multiple PostgreSQL servers.
   * @param secure Optional SSL security type.
   * @param dbase The name of the PostgreSQL database to connect to.
   * @param rowcc The row continuation.
   * @param errcc The error continuation.
   * @returns An outcome of either a connection object or an [Apigen.failure] value.
   */
  connect(name:string, secure:option(SSL.secure_type),
          dbase:string, rowcc, errcc, endcc:continuation(outcome(Postgres.connection,Apigen.failure))) =
    match Pg.connect(name,secure) with
    | {success=conn} ->
      Continuation.return(endcc,
                          {success={ ~name ~secure ~conn ~dbase
                                     major_version=default_major_version minor_version=default_minor_version
                                     params=StringMap.empty processid=-1 secret_key=-1
                                     query="" status="" suspended=false error=none
                                     empty=true completed=[] rowdescs=[] rows=0 paramdescs=[]
                                     ~rowcc ~errcc }})
    | {~failure} ->
      Continuation.return(endcc,{~failure})

  /** Close a connection object.
   *
   * This routine closes the connection to the server and releases any associated resources.
   *
   * @param conn The connection object.
   * @param terminate If true, a [Terminate] message will be sent to the server before closing the connection.
   * @returns An outcome of the updated connection or an [Apigen.failure] value.
   */
  close(conn:Postgres.connection, terminate:bool, endcc:continuation(outcome(Postgres.connection,Apigen.failure))) =
    _ =
      if terminate
      then Pg.terminate({success=conn.conn})
      else {success={}}
    match Pg.close({success=conn.conn}) with
    | {success=c} -> Continuation.return(endcc,{success={conn with conn=c processid=-1 secret_key=-1}})
    | {~failure} -> Continuation.return(endcc,{~failure})

  /** Search for PostgreSQL parameter.
   *
   * During authentication, the driver accumulates all the parameter values
   * returned by the server.  This routine allows you to search for a parameter.
   * Note that you can get at the raw [StringMap] from [conn.params].
   *
   * @param conn The connection object.
   * @param name The name of the parameter.
   * @returns An optional string value for the parameter.
   */
  param(conn:Postgres.connection, name:string) : option(string) =
    StringMap.get(name, conn.params)

  /** Return the stored [processid] and [secret_key] values.
   *
   * During authentication the driver stores the identifiers for the connection to
   * be used in cancellation requests.  This returns those values but they are
   * only useful for these cancellation requests.
   *
   * @param conn The connection object.
   * @returns An optional object with the key data.
   */
  keydata(conn:Postgres.connection) : option({processid:int; secret_key:int}) =
    if conn.processid != -1 && conn.secret_key != -1
    then {some={processid=conn.processid; secret_key=conn.secret_key}}
    else {none}

  /** Return last status value.
   *
   * At the end of a command cycle the server returns a [ReadyForQuery] message
   * which contains a status value (I=idle, T=in transaction, E=in failed transaction).
   * This function returns the last received status value but note that it may not be
   * present during a sequence of extended commands.
   *
   * @param conn The connection object.
   * @returns An optional status character.
   */
  status(conn:Postgres.connection) : option(string) =
    if conn.status == "" then {none} else {some=conn.status}

  /** Set the row continuation.
   *
   * @param conn The connection object.
   * @param rowcc A continuation which takes a [Postgres.full_row] object.
   * @returns An updated connection object.
   */
  set_rowcc(conn:Postgres.connection, rowcc:continuation(Postgres.full_row)) : Postgres.connection = {conn with ~rowcc}

  /** Set the error continuation.
   *
   * @param conn The connection object.
   * @param errcc A connection which takes a [Postgres.unsuccessful] object.
   * @returns An updated connection object.
   */
  set_errcc(conn:Postgres.connection, errcc:continuation(Postgres.unsuccessful)) : Postgres.connection = {conn with ~errcc}

  /** Set the protocol major version number.
   *
   * This is used in startup messages and should reflect the version of the protocol implemented by the driver.
   * Should probably never be necessary.
   *
   * @param conn The connection object.
   * @param major_version The major version number (default is currently 3).
   * @returns An updated connection object.
   */
  set_major_version(conn:Postgres.connection, major_version:int) : Postgres.connection = {conn with ~major_version}

  /** Set the protocol minor version number.
   *
   * @param conn The connection object.
   * @param minor_version The minor version number (default is currently 0).
   * @returns An updated connection object.
   */
  set_minor_version(conn:Postgres.connection, minor_version:int) : Postgres.connection = {conn with ~minor_version}

  /* Implementation note, the PostgreSQL docs recomment using a single input
   * point for all incoming messages from PostgreSQL.  This is how we implement
   * this driver.  Only the authentication, query, sync and flush commands call this routine.
   */
  @private loop(conn:Postgres.connection, k:continuation(Postgres.result)) : void =
    get_result(conn,status) =
      match conn.error with
      | {some=failure} -> {failure=(conn,failure)}
      | {none} -> {success={conn with ~status}}
    match Pg.reply({success=conn.conn}) with
    | {success={Authentication={Ok}}} -> loop(conn,k)
    | {success={Authentication={CleartextPassword}}} ->
       match (Pg.password({success=conn.conn},conn.conn.conf.password)) with
       | {success={}} -> loop(conn,k)
       | ~{failure} -> loop({conn with error={some={api_failure=failure}}},k)
       end
    | {success={Authentication={MD5Password=salt}}} ->
       inner = Crypto.Hash.md5(conn.conn.conf.password^conn.conn.conf.user)
       outer = Crypto.Hash.md5(inner^(%%bslBinary.to_encoding%%(salt,"binary")))
       md5password = "md5"^outer
       match (Pg.password({success=conn.conn},md5password)) with
       | {success={}} -> loop(conn,k)
       | ~{failure} -> loop({conn with error={some={api_failure=failure}}},k)
       end
    | {success={ParameterStatus=(n,v)}} -> loop({conn with params=StringMap.add(n,v,conn.params)},k)
    | {success={BackendKeyData=(processid,secret_key)}} -> loop({conn with ~processid ~secret_key},k)
    | {success={~CommandComplete}} -> loop({conn with completed=[CommandComplete|conn.completed]},k)
    | {success={PortalSuspended}} -> Continuation.return(k,get_result({conn with suspended=true},""))
    | {success={EmptyQueryResponse}} -> loop({conn with empty=true},k)
    | {success={~RowDescription}} -> loop({conn with rowdescs=List.map(to_rowdesc,RowDescription)},k)
    | {success={~DataRow}} ->
       do Continuation.return(conn.rowcc,(conn.rows,conn.rowdescs,DataRow))
       loop({conn with rows=conn.rows+1},k)
    | {success={~ParameterDescription}} -> loop({conn with paramdescs=ParameterDescription},k)
    | {success={NoData}} -> loop({conn with paramdescs=[]},k)
    | {success={ParseComplete}} -> loop(conn,k)
    | {success={BindComplete}} -> loop(conn,k)
    | {success={CloseComplete}} -> loop(conn,k)
    | {success={~NoticeResponse}} ->
       do Continuation.return(conn.errcc,(conn,{postgres={notice=NoticeResponse}}))
       loop(conn,k)
    | {success={~ErrorResponse}} ->
       do Continuation.return(conn.errcc,(conn,{postgres={error=ErrorResponse}}))
       loop({conn with error={some={postgres={error=ErrorResponse}}}},k)
    | {success={ReadyForQuery=status}} -> Continuation.return(k,get_result(conn,status))
    | {success=reply} -> Continuation.return(k,{failure=(conn,{bad_reply=reply})})
    | ~{failure} -> Continuation.return(k,{failure=(conn,{api_failure=failure})})
    end

  @private init(conn:Postgres.connection, query) : Postgres.connection =
    {conn with error=none; empty=false; suspended=false; completed=[]; rowdescs=[]; rows=0; paramdescs=[]; ~query}

  /** Authenticate with the PostgreSQL server.
   *
   * This function sends a [StartupMessage] message which includes the user and database names.
   * The server then responds with an authentication request (currently only MD5 password and
   * clear-text passwords are supported) which the driver responds to.
   * The driver then reads back a large number of reply messages including the server parameters
   * and the connection id ([processid] and [secret_key]).
   *
   * @param conn The connection object.
   * @param endcc The finalization continuation.
   * @returns An updated connection with connection data installed.
   */
  authenticate(conn:Postgres.connection, endcc:continuation(Postgres.result)) =
    conn = init(conn, "authentication")
    version = Bitwise.lsl(Bitwise.land(conn.major_version,0xffff),16) + Bitwise.land(conn.minor_version,0xffff)
    match Pg.start({success=conn.conn}, (version, [("user",conn.conn.conf.user),("database",conn.dbase)])) with
    | {success={}} -> loop(conn,endcc)
    | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))

  /** Issue simple query command and read back responses.
   *
   * The simple query protocol is a simple command to which the server will reply
   * with the results of the query.  It is similar to doing [Parse]/[Bind]/[Describe]/[Execute]/[Sync].
   * If any row data are returned, the row continuation will be called, likewise for errors.
   *
   * Note that this routine also returns an updated connection object upon failure since the effects
   * of previously successful messages received from the server may be retained.
   *
   * @param conn The connection object.
   * @param query The query string (can contain multiple queries).
   * @param endcc The finalization continuation.
   * @returns An outcome of an updated connection object or a failure plus a connection object.
   */
  query(conn:Postgres.connection, query, endcc:continuation(Postgres.result)) =
    conn = init(conn, query)
    match Pg.query({success=conn.conn},query) with
    | {success={}} -> loop(conn,endcc)
    | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))
    end

  /** Extract the raw data for a named column from a [Postgres.full_row] object.
   *
   * This is a routine for handling the raw row data from the server which may be
   * either binary or text according to the [format_code] value in the row description.
   * The type of the data is embodied in the row description which has the object id for
   * the type.  This will then need to be decoded to generate, for example ints or dates,
   * from the raw data.
   *
   * @param name The name of the column.
   * @param row The row data.
   * @returns The row description plus the data.
   */
  get_row_value_by_column_name(name:string, row:Postgres.full_row) : outcome((Postgres.rowdesc,Postgres.data),Postgres.failure) =
    rec aux(rds, r) =
      match (rds, r) with
      | ([rd|rds],[el|r]) ->
         if rd.name == name
         then
           match rd.format_code with
           | 0 -> {success=(rd,{text=string_of_binary(el)})}
           | 1 -> {success=(rd,{binary=el})}
           | n -> {failure={bad_format=n}}
         else aux(rds, r)
      | ([],[]) -> {failure={not_found}}
      | _ -> {failure={bad_row=(row.f2, row.f3)}}
    aux(row.f2, row.f3)

  /** Send a parse query message.
   *
   * This routine causes a prepared statement to be placed in the server.
   * This routine only sends the message, if you want to see the result
   * immediately, you have to then send a flush message to force an early reply.
   *
   * @param conn The connection object.
   * @param name The name of the prepared statement (empty means the unnamed prepared statement).
   * @param query The query string (may contain placeholders for later data, eg. "$1" etc.).
   * @param oids The object ids for the types of the parameters (may be zero to keep the type unspecified).
   * @param endcc The finalization continuation.
   * @returns An updated connection object or failure.
   */
  parse(conn:Postgres.connection, name, query, oids, endcc:continuation(Postgres.result)) =
    conn = init(conn, "Parse({query},{name})")
    match Pg.parse({success=conn.conn},(name,query,oids)) with
    | {success={}} -> Continuation.return(endcc, {success=conn})
    | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))
    end

  /** Bind parameters to a prepared statement and a portal.
   *
   * @param conn The connection object.
   * @param portal The name of the destination portal (empty means the unnamed portal).
   * @param name The name of the source prepared statement.
   * @param codes The parameter format codes (0=text, 1=binary).
   * @param params The list of parameters.
   * @param result_codes A list of the result column return codes (0=text, 1=binary).
   * @param endcc The finalization continuation.
   * @returns The original connection object or failure.
   */
  bind(conn:Postgres.connection, portal, name, codes, params, result_codes, endcc:continuation(Postgres.result)) =
    conn = init(conn, "Bind({name},{portal})")
    match Pg.bind({success=conn.conn},(portal,name,codes,params,result_codes)) with
    | {success={}} -> Continuation.return(endcc, {success=conn})
    | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))
    end

  /** Execute named portal.
   *
   * @param conn The connection object.
   * @param portal The name of the portal to execute (empty means the unnamed portal).
   * @param rows_to_return The number of rows to return (zero means unlimited).
   * @param endcc The finalization continuation.
   * @returns The original connection object or failure.
   */
  execute(conn:Postgres.connection, portal, rows_to_return, endcc:continuation(Postgres.result)) =
    conn = init(conn, "Execute({portal})")
    match Pg.execute({success=conn.conn},(portal,rows_to_return)) with
    | {success={}} -> Continuation.return(endcc, {success=conn})
    | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))
    end

  /** Describe portal or statement.
   *
   * This triggers the return of row description data, it can be used to type the
   * values passed to or returned from prepared statements or portals.
   *
   * @param conn The connection object.
   * @param sp Statement or portal flag
   * @param name The name of the prepared statement or portal.
   * @param endcc The finalization continuation.
   * @returns The original connection object or failure.
   */
  describe(conn:Postgres.connection, sp:Postgres.sp, name, endcc:continuation(Postgres.result)) =
    conn = init(conn, "Describe({string_of_sp(sp)},{name})")
    match Pg.describe({success=conn.conn},(string_of_sp(sp),name)) with
    | {success={}} -> Continuation.return(endcc, {success=conn})
    | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))
    end

  /** Close a prepared statement or portal.
   *
   * Note that this does not close the connection, it is used to release resources
   * associated with prepared statements and portals on the server, otherwise they
   * persist (apart from the unnamed statement/portal which can be overwritten and in fact
   * are destroyed by simple query commands).
   *
   * @param conn The connection object.
   * @param sp Statement or portal flag
   * @param name The name of the prepared statement or portal.
   * @param endcc The finalization continuation.
   * @returns The original connection object or failure.
   */
  closePS(conn:Postgres.connection, sp:Postgres.sp, name, endcc:continuation(Postgres.result)) =
    conn = init(conn, "Close({string_of_sp(sp)},{name})")
    match Pg.closePS({success=conn.conn},(string_of_sp(sp),name)) with
    | {success={}} -> Continuation.return(endcc, {success=conn})
    | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))
    end

  /** Send [Sync] command and read back response data.
   *
   * This command is the normal way to terminate an extended query.
   * Upon receiving this command the server will flush out any pending messages
   * and end with a [ReadyForQuery] message.
   *
   * @param conn The connection object.
   * @param endcc The finalization continuation.
   * @returns An updated connection object or failure.
   */
  sync(conn:Postgres.connection, endcc:continuation(Postgres.result)) =
    conn = init(conn, "Sync")
    match Pg.sync({success=conn.conn}) with
    | {success={}} -> loop(conn,endcc)
    | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))
    end

  /** Send a [Flush] command and read back response data.
   *
   * This requests the server to flush any pending messages.
   *
   * @param conn The connection object.
   * @param endcc The finalization continuation.
   * @returns An updated connection object or failure.
   */
  flush(conn:Postgres.connection, endcc:continuation(Postgres.result)) =
    conn = init(conn, "Flush")
    match Pg.flush({success=conn.conn}) with
    | {success={}} -> loop(conn,endcc)
    | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))
    end

  /** Send cancel request message on secondary channel.
   *
   * This operates differently from the other commands.
   * A second connection to the server is opened but instead of
   * performing authentication an immediate [CancelRequest] message
   * is sent which contains the connection id received during the authentication
   * of the connection passed in here.  The original connection is
   * untouched by this operation but we return it for consistency with
   * the structure of the other functions in this API.  Note that the only
   * way of knowing if the cancel succeeded is to monitor the original connection.
   *
   * @param conn The connection object.
   * @param endcc The finalization continuation.
   * @returns An outcome of a void or an [Apigen.failure] object.
   */
  cancel(conn:Postgres.connection, endcc:continuation(Postgres.result)) =
    if conn.processid == -1 || conn.secret_key == -1
    then Continuation.return(conn.errcc,(conn,{no_key}))
    else
      match @callcc(k -> connect(conn.name, conn.secure, conn.dbase, conn.rowcc, conn.errcc, k)) with
      | {success=conn2} ->
         match Pg.cancel({success=conn2.conn},(conn.processid,conn.secret_key)) with
         | {success={}} ->
            _ = @callcc(k -> close(conn2,false,k))
            Continuation.return(endcc, {success=conn})
         | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))
         end
      | ~{failure} -> Continuation.return(conn.errcc, (conn,{api_failure=failure}))

}}

// End of file postgres.opa
