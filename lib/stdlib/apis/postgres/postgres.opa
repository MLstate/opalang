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

import stdlib.io.socket
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
 * The connection object includes some status information gathered during the send and receive operations
 * but mostly the driver operates by calling the listener callback.  This performs three functions:
 * it is passed row and row description data as it is returned from the server, it is passed
 * error and notice messages retrned from the server (note that an SQL operation can generate
 * more than one of these) and a final message called once all data has been received and the
 * server is ready for more queries.  The final message will receive either a success outcome
 * of an updated connection object or a copy of the *last* failure message.
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
 / { RowDescription: list((string, int, int, int, int, int, int)) }
 / { EmptyQueryResponse }
 / { BackendKeyData: (int, int) }
 / { ParameterStatus: (string, string) }
 / { Authentication: Postgres.auth_reply }
 / { PortalSuspended }
 / { ParameterDescription: list(int) }
 / { NotificationResponse: (int, string, string) }
 / { NoData }
 / { FunctionCallResponse: binary }
 / { CopyBothResponse: (int, list(int)) }
 / { CopyOutResponse: (int, list(int)) }
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
 / { bad_ssl_response : string }
 / { bad_type : string }
 / { no_key }
 / { not_found }

/** The type of a value passed to the listener callback.
 */
type Postgres.listener_arg =
   { final: Postgres.result }
 / { rowdescs: Postgres.rowdescs }
 / { DataRow: list(binary) }
 / { ErrorResponse: Postgres.msg }
 / { NoticeResponse: Postgres.msg }

/** The type of a Postgres listener */
type Postgres.listener = (Postgres.connection, Postgres.listener_arg -> void)

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
  type_id : Postgres.type_id
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
  ssl_accepted  : bool /** The server has accepted an SSL connection */
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
  rows          : int /** The number of rows received during a query */
  rowdescs      : Postgres.rowdescs /** Stored value of the last row description received */
  paramdescs    : list(int) /** List of the last-received parameter descriptions */
  handlers      : intmap((OpaType.ty,Postgres.abstract_handler)) /** Handlers for unknown data types */
}

/** Defines whether an operation is for a prepared statement or a portal */
type Postgres.sp = {statement} / {portal}

/** Listener definition, functions for each event generated by the postgres driver.
 */
type Postgres.listener_def = {
  on_success  : option(Postgres.connection -> void)
  on_failure  : option(Postgres.connection, Postgres.failure -> void)
  on_rowdescs : option(Postgres.connection, Postgres.rowdescs -> void)
  on_row      : option(Postgres.connection, Postgres.row -> void)
  on_error    : option(Postgres.connection, Postgres.msg -> void)
  on_notice   : option(Postgres.connection, Postgres.msg -> void)
}

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
   * @param name The name of the connection, this is only relevant if you have connections to multiple PostgreSQL servers.
   * @param secure Optional SSL security type.
   * @param dbase The name of the PostgreSQL database to connect to.
   * @returns An outcome of either a connection object or an [Apigen.failure] value.
   */
  connect(name:string, secure:option(SSL.secure_type), dbase:string) =
    preamble = if Option.is_some(secure) then {some=request_ssl} else none
    match Pg.connect(name,secure,preamble) with
    | {success=conn} ->
      {success={ ~name ~secure ~conn ~dbase ssl_accepted=false
                 major_version=default_major_version minor_version=default_minor_version
                 params=StringMap.empty processid=-1 secret_key=-1
                 query="" status="" suspended=false error=none
                 empty=true completed=[] paramdescs=[] rows=0 rowdescs=[]
                 handlers=IntMap.empty }}
    | {~failure} -> {~failure}

  /** Close a connection object.
   *
   * This routine closes the connection to the server and releases any associated resources.
   *
   * @param conn The connection object.
   * @param terminate If true, a [Terminate] message will be sent to the server before closing the connection.
   * @returns An outcome of the updated connection or an [Apigen.failure] value.
   */
  close(conn:Postgres.connection, terminate:bool) =
    _ =
      if terminate
      then Pg.terminate({success=conn.conn})
      else {success={}}
    match Pg.close({success=conn.conn}) with
    | {success=c} -> {success={conn with conn=c processid=-1 secret_key=-1}}
    | {~failure} -> {~failure}

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

  /** Install a handler for a given Postgres type id.
   *
   * The type_id is specific to a given PostgreSQL server so we attach the handler
   * to the connection.  See the PostgresTypes module for the use of handlers.
   * You can also install a handler for an ENUM type using the [create_enum] function.
   *
   * @param conn The connection object.
   * @param type_id The PostgreSQL type id number, can be read from the server.
   * @param handler A handler function, should return an option of an opa value corresponding to the type.
   * @returns An updated connection with the handler installed.
   */
  install_handler(conn:Postgres.connection, type_id:Postgres.type_id, handler:Postgres.handler('a)) : Postgres.connection =
    {conn with handlers=IntMap.add(type_id,(PostgresTypes.name_type(@typeval('a)),@unsafe_cast(handler)),conn.handlers)}

  /** Set the protocol minor version number.
   *
   * @param conn The connection object.
   * @param minor_version The minor version number (default is currently 0).
   * @returns An updated connection object.
   */
  set_minor_version(conn:Postgres.connection, minor_version:int) : Postgres.connection = {conn with ~minor_version}

  @private again(conn:Postgres.connection, result:Postgres.listener_arg, k:Postgres.listener) : Postgres.connection =
    do k(conn, result)
    loop(conn, k)

  @private final(conn:Postgres.connection, result:Postgres.listener_arg, k:Postgres.listener) : Postgres.connection =
    do k(conn, result)
    conn

  @private error(conn:Postgres.connection, failure:Postgres.failure, k:Postgres.listener) : Postgres.connection =
    do k(conn, {final={failure=(conn, failure)}})
    conn

  /* Implementation note, the PostgreSQL docs recomment using a single input
   * point for all incoming messages from PostgreSQL.  This is how we implement
   * this driver.  Only the authentication, query, sync and flush commands call this routine.
   */
  @private loop(conn:Postgres.connection, k:Postgres.listener) : Postgres.connection =
    get_result(conn,status) : Postgres.listener_arg =
      conn = {conn with ~status}
      {final=
        match conn.error with
        | {some=failure} -> {failure=(conn,failure)}
        | {none} -> {success=conn}}
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
    | {success={PortalSuspended}} -> final(conn, get_result({conn with suspended=true},""),k)
    | {success={EmptyQueryResponse}} -> loop({conn with empty=true},k)
    | {success={~RowDescription}} -> rowdescs = List.map(to_rowdesc,RowDescription) again({conn with ~rowdescs},{~rowdescs},k)
    | {success={~DataRow}} -> again({conn with rows=conn.rows+1},~{DataRow},k)
    | {success={~ParameterDescription}} -> loop({conn with paramdescs=ParameterDescription},k)
    | {success={NoData}} -> loop({conn with paramdescs=[]},k)
    | {success={ParseComplete}} -> loop(conn,k)
    | {success={BindComplete}} -> loop(conn,k)
    | {success={CloseComplete}} -> loop(conn,k)
    | {success={~NoticeResponse}} -> again(conn,~{NoticeResponse},k)
    | {success={~ErrorResponse}} -> again({conn with error={some={postgres={error=ErrorResponse}}}}, ~{ErrorResponse},k)
    | {success={ReadyForQuery=status}} -> final(conn, get_result(conn,status),k)
    | {success=reply} -> error(conn, {bad_reply=reply},k)
    | ~{failure} -> error(conn, {api_failure=failure},k)
    end

  @private init(conn:Postgres.connection, query) : Postgres.connection =
    {conn with error=none; empty=false; suspended=false; completed=[]; rows=0; rowdescs=[]; paramdescs=[]; ~query}

  /* Failed attempt to get SSL connection.  We do everything the docs say,
   * we send the SSLRequest message, we get back "S" and then we call
   * tls.connect with the old socket value.  But it just hangs... (EINPROGRESS).
   *
   * This function is a preamble function which SocketPool inserts between
   * an open insecure socket and a secure reconnect with the SSL options.
   */
  @private request_ssl(conn:Socket.t) : SocketPool.result =
    do jlog("request_ssl")
    timeout = 60*60*1000
    match Socket.binary_write_with_err_cont(conn.conn, timeout, Pg.packed_SSLRequest) with
    | {success=cnt} ->
      if cnt == Binary.length(Pg.packed_SSLRequest)
      then
        do jlog("sent ssl request")
        match Socket.read_fixed(conn.conn, timeout, 1, conn.mbox) with
        | {success=mbox} ->
          match Mailbox.sub(mbox, 1) with
          | {success=(mbox,binary)} ->
             if Binary.length(binary) == 1
             then
               match Binary.get_string(binary,0,1) with
               | "S" -> do jlog("SSL accepted") {success={conn with ~mbox}}
               | "N" -> do jlog("SSL rejected") {failure="request_ssl: Server does not accept SSL connections"}
               | code -> {failure="request_ssl: Unknown code, expected 'S' or 'N', got '{code}'"}
               end
             else
               {failure="request_ssl: Wrong length, expected 1 byte got {Binary.length(binary)}"}
          | {~failure} -> {~failure}
          end
        | {~failure} -> {~failure}
        end
      else {failure="request_ssl: Socket write failure (didn't send whole data)"}
    | ~{failure} -> ~{failure}
    end

  /** Authenticate with the PostgreSQL server.
   *
   * This function sends a [StartupMessage] message which includes the user and database names.
   * The server then responds with an authentication request (currently only MD5 password and
   * clear-text passwords are supported) which the driver responds to.
   * The driver then reads back a large number of reply messages including the server parameters
   * and the connection id ([processid] and [secret_key]).
   *
   * @param conn The connection object.
   * @param listener A Postgres listener callback.
   * @returns An updated connection with connection data installed.
   */
  authenticate(conn:Postgres.connection, k:Postgres.listener) : Postgres.connection =
    conn = init(conn, "authentication")
    version = Bitwise.lsl(Bitwise.land(conn.major_version,0xffff),16) + Bitwise.land(conn.minor_version,0xffff)
    match Pg.start({success=conn.conn}, (version, [("user",conn.conn.conf.user),("database",conn.dbase)])) with
    | {success={}} -> loop(conn,k)
    | ~{failure} -> error(conn,{api_failure=failure},k)

  /** Issue simple query command and read back responses.
   *
   * The simple query protocol is a simple command to which the server will reply
   * with the results of the query.  It is similar to doing [Parse]/[Bind]/[Describe]/[Execute]/[Sync].
   * If any row data are returned, the listener will be called, likewise for errors and notices.
   *
   * Note that this routine also returns an updated connection object upon failure since the effects
   * of previously successful messages received from the server may be retained.
   *
   * @param conn The connection object.
   * @param query The query string (can contain multiple queries).
   * @param listener A Postgres listener callback.
   * @returns An outcome of an updated connection object or a failure plus a connection object.
   */
  query(conn:Postgres.connection, query, k:Postgres.listener) : Postgres.connection =
    conn = init(conn, query)
    match Pg.query({success=conn.conn},query) with
    | {success={}} -> loop(conn,k)
    | ~{failure} -> error(conn,{api_failure=failure},k)
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
   * @param listener A Postgres listener callback.
   * @returns An updated connection object or failure.
   */
  parse(conn:Postgres.connection, name, query, oids, k:Postgres.listener) : Postgres.connection =
    conn = init(conn, "Parse({query},{name})")
    match Pg.parse({success=conn.conn},(name,query,oids)) with
    | {success={}} -> final(conn,{final={success=conn}},k)
    | ~{failure} -> error(conn,{api_failure=failure},k)
    end

  /** Bind parameters to a prepared statement and a portal.
   *
   * @param conn The connection object.
   * @param portal The name of the destination portal (empty means the unnamed portal).
   * @param name The name of the source prepared statement.
   * @param codes The parameter format codes (0=text, 1=binary).
   * @param params The list of parameters.
   * @param result_codes A list of the result column return codes (0=text, 1=binary).
   * @param listener A Postgres listener callback.
   * @returns The original connection object or failure.
   */
  bind(conn:Postgres.connection, portal, name, codes, params, result_codes, k:Postgres.listener) : Postgres.connection =
    conn = init(conn, "Bind({name},{portal})")
    match Pg.bind({success=conn.conn},(portal,name,codes,params,result_codes)) with
    | {success={}} -> final(conn,{final={success=conn}},k)
    | ~{failure} -> error(conn,{api_failure=failure},k)
    end

  /** Execute named portal.
   *
   * @param conn The connection object.
   * @param portal The name of the portal to execute (empty means the unnamed portal).
   * @param rows_to_return The number of rows to return (zero means unlimited).
   * @param listener A Postgres listener callback.
   * @returns The original connection object or failure.
   */
  execute(conn:Postgres.connection, portal, rows_to_return, k:Postgres.listener) : Postgres.connection =
    conn = init(conn, "Execute({portal})")
    match Pg.execute({success=conn.conn},(portal,rows_to_return)) with
    | {success={}} -> final(conn,{final={success=conn}},k)
    | ~{failure} -> error(conn,{api_failure=failure},k)
    end

  /** Describe portal or statement.
   *
   * This triggers the return of row description data, it can be used to type the
   * values passed to or returned from prepared statements or portals.
   *
   * @param conn The connection object.
   * @param sp Statement or portal flag
   * @param name The name of the prepared statement or portal.
   * @param listener A Postgres listener callback.
   * @returns The original connection object or failure.
   */
  describe(conn:Postgres.connection, sp:Postgres.sp, name, k:Postgres.listener) : Postgres.connection =
    conn = init(conn, "Describe({string_of_sp(sp)},{name})")
    match Pg.describe({success=conn.conn},(string_of_sp(sp),name)) with
    | {success={}} -> final(conn,{final={success=conn}},k)
    | ~{failure} -> error(conn,{api_failure=failure},k)
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
   * @param listener A Postgres listener callback.
   * @returns The original connection object or failure.
   */
  closePS(conn:Postgres.connection, sp:Postgres.sp, name, k:Postgres.listener) : Postgres.connection =
    conn = init(conn, "Close({string_of_sp(sp)},{name})")
    match Pg.closePS({success=conn.conn},(string_of_sp(sp),name)) with
    | {success={}} -> final(conn,{final={success=conn}},k)
    | ~{failure} -> error(conn,{api_failure=failure},k)
    end

  /** Send [Sync] command and read back response data.
   *
   * This command is the normal way to terminate an extended query.
   * Upon receiving this command the server will flush out any pending messages
   * and end with a [ReadyForQuery] message.
   *
   * @param conn The connection object.
   * @param listener A Postgres listener callback.
   * @returns An updated connection object or failure.
   */
  sync(conn:Postgres.connection, k:Postgres.listener) : Postgres.connection =
    conn = init(conn, "Sync")
    match Pg.sync({success=conn.conn}) with
    | {success={}} -> loop(conn,k)
    | ~{failure} -> error(conn,{api_failure=failure},k)
    end

  /** Send a [Flush] command and read back response data.
   *
   * This requests the server to flush any pending messages.
   *
   * @param conn The connection object.
   * @param listener A Postgres listener callback.
   * @returns An updated connection object or failure.
   */
  flush(conn:Postgres.connection, k:Postgres.listener) : Postgres.connection =
    conn = init(conn, "Flush")
    match Pg.flush({success=conn.conn}) with
    | {success={}} -> loop(conn,k)
    | ~{failure} -> error(conn,{api_failure=failure},k)
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
   * @returns An outcome of a void or an [Apigen.failure] object.
   */
  cancel(conn:Postgres.connection) : Postgres.result =
    if conn.processid == -1 || conn.secret_key == -1
    then {failure=(conn,{no_key})}
    else
      match connect(conn.name, conn.secure, conn.dbase) with
      | {success=conn2} ->
         match Pg.cancel({success=conn2.conn},(conn.processid,conn.secret_key)) with
         | {success={}} ->
            _ = close(conn2,false)
            {success=conn}
         | ~{failure} -> {failure=(conn,{api_failure=failure})}
         end
      | ~{failure} -> {failure=(conn,{api_failure=failure})}

  // Some support code

  /* Turn a Postgres message into a string */
  string_of_msg(msg:Postgres.msg) : string = String.concat("\n  ",List.map(((c, m) -> "{c}: {m}"),msg))

  /**
   * A simple listener, does nothing except count rows and print out
   * error and notice messages.
   */
  default_listener_def : Postgres.listener_def =
    { on_success=none; on_failure=none; on_rowdescs=none; on_row=none; on_error=none; on_notice=none }

  /**
   * Make a listener from a listener definition.
   * @param def A [Postgres.listener_def] value with handler functions for specific actions.
   * @returns A valid listener function.
   */
  make_listener(def:Postgres.listener_def) : Postgres.listener =
    (conn:Postgres.connection, arg:Postgres.listener_arg ->
      match arg with
      | {~final} ->
        match final with
        | {success=conn} ->
          match def.on_success with
          | {some=on_success} -> on_success(conn)
          | {none} -> void
          end
        | {failure=(conn,failure)} ->
          match def.on_failure with
          | {some=on_failure} -> on_failure(conn,failure)
          | {none} -> Log.error("Postgres.listener({conn.query})","{failure}")
          end
        end
      | {~rowdescs} ->
        match def.on_rowdescs with
        | {some=on_rowdescs} -> on_rowdescs(conn,rowdescs)
        | {none} -> void
        end
      | {~DataRow} ->
        match def.on_row with
        | {some=on_row} -> on_row(conn,DataRow)
        | {none} -> void
        end
      | {~ErrorResponse} ->
          match def.on_error with
          | {some=on_error} -> on_error(conn,ErrorResponse)
          | {none} -> Log.error("Postgres.listener({conn.query})","\n  {string_of_msg(ErrorResponse)}")
          end
      | {~NoticeResponse} ->
          match def.on_notice with
          | {some=on_notice} -> on_notice(conn,NoticeResponse)
          | {none} -> Log.notice("Postgres.listener({conn.query})","\n  {string_of_msg(NoticeResponse)}")
          end
      end)

  /** A default listener built from [default_listener_def]. */
  default_listener : Postgres.listener = make_listener(default_listener_def)

  /** Get the [type_id] value from the server for the named type.
   *
   * Performs a "SELECT oid from pg_type WHERE typname='name'" query on the
   * server and returns the [oid] value returned.
   *
   * @param conn The connection object.
   * @param name The name of the type.
   * @returns A tuple of the updated connection (maybe with and [error] value) and the type_id (-1=not found).
   */
  get_type_id(conn:Postgres.connection, name:string) : (Postgres.connection,int) =
    type_id = ServerReference.create(-1)
    conn = query(conn,"SELECT oid from pg_type WHERE typname='{name}'",
                 make_listener({default_listener_def with
                                 on_row={some=(conn, row ->
                                                match StringMap.get("oid",PostgresTypes.getRow(conn.rowdescs,row)) with
                                                | {some={Int=tid}} -> ServerReference.set(type_id,tid)
                                                | _ -> void)}}))
    (conn,ServerReference.get(type_id))

  /** Create a new [ENUM] type on the server from an Opa type and install a handler for it.
   *
   * For a type such as [type mood = {happy} or {ok} or {sad}] the record labels
   * are turned into an ENUM type at the server, the [type_id] is read back and
   * a handler is installed for the Opa type.  A notice is posted if the operation succeeds.
   *
   * @param conn The connection object.
   * @param dummy A dummy optional value of the Opa type (used to analyse the field names).
   * @returns A Postgres result type with updated connection plus possible failure code.
   */
  create_enum(conn:Postgres.connection, _:option('a)) : Postgres.result =
    raw_ty = @typeval('a)
    match raw_ty with
    | {TyName_args=[]; TyName_ident=name} ->
      ty = PostgresTypes.name_type(raw_ty)
      match ty with
      | {TySum_col=col ...} ->
        (names,err) =
          List.fold((row, (names,err) -> 
                      match err with
                      | {some=_} -> (names,err)
                      | {none} ->
                        match row with
                        | [~{label; ty}] ->
                           if OpaType.is_void(ty)
                           then ([label|names],err)
                           else (names,{some="create_enum: Label {label} is not void ({ty})"})
                        | _ -> (names,{some="create_enum: Bad type for enum {row}"})
                        end),col,([],none))
        match err with
        | {none} ->
           match
             match get_type_id(conn, name) with
             | (conn,-1) ->
                qnames = String.concat(", ",List.map((name -> "'{name}'"),names))
                conn = query(conn,"CREATE TYPE {name} AS ENUM ({qnames})",default_listener)
                if Option.is_none(conn.error)
                then get_type_id(conn, name)
                else (conn,-1)
             | (conn,type_id) -> (conn,type_id)
             end
           with
           | (conn,-1) -> {failure=(conn,{bad_type="create_enum: can't create enum for type {ty}"})}
           | (conn,type_id) ->
              generic_handler(h_type_id:Postgres.type_id, h_ty:OpaType.ty, val:Postgres.data) : option(void) =
                if type_id == h_type_id && ty == h_ty
                then
                  val = match val with ~{text} -> text | ~{binary} -> string_of_binary(binary)
                  if List.mem(val,names)
                  then
                    match OpaValue.Record.field_of_name(val) with
                    | {none} -> none
                    | {some=field} -> {some=@unsafe_cast(OpaValue.Record.make_simple_record(field))}
                    end
                  else none
                else none
              do Log.notice("Postgres.create_enum","installing handler for type_id {type_id} type {OpaType.to_pretty(raw_ty)}")
              {success={conn with handlers=IntMap.add(type_id,(ty,@unsafe_cast(generic_handler)),conn.handlers)}}
           end
        | {some=err} -> {failure=(conn,{bad_type=err})}
        end
      | _ -> {failure=(conn,{bad_type="create_enum: bad type {ty}"})}
      end
    | _ -> {failure=(conn,{bad_type="create_enum: bad type {raw_ty}"})}

}}

// End of file postgres.opa
