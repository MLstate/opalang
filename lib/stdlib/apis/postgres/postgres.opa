/*
    Copyright © 2011 MLstate

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

type Postgres.msg = list((int,string))

type Postgres.en =
   { error : Postgres.msg }
 / { notice : Postgres.msg }

type Postgres.auth_reply =
     { Ok }
   / { MD5Password: binary }
   / { GSSContinue: binary }
   / { SSPI }
   / { GSS }
   / { SCMCredential }
   / { CleartextPassword }
   / { KerberosV5 }

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

type Postgres.failure =
   { api_failure : Apigen.failure }
 / { postgres : Postgres.en }
 / { bad_reply : Postgres.reply }
 / { bad_row : (list(Postgres.rowdesc),list(binary)) }
 / { bad_format : int }

type Postgres.unsuccessful = (Postgres.connection,Postgres.failure)

type Postgres.success = Postgres.connection

type Postgres.result = outcome(Postgres.success,Postgres.unsuccessful)

type Postgres.rowdesc = {
  name : string
  table_id : int
  table_attribute_number : int
  type_id : int
  data_type_size : int
  type_modifier : int
  format_code : int
}

type Postgres.rowdescs = list(Postgres.rowdesc)

type Postgres.data = {text:string} / {binary:binary}

type Postgres.row = list(binary)

type Postgres.rows = (list(Postgres.rowdesc),list(Postgres.row))

type Postgres.connection = {
  conn : ApigenLib.connection
  dbase : string
  user : string
  password : string
  params : stringmap(string)
  processid : int
  secret_key : int
  query : string
  status : string
  error : option(Postgres.failure)
  empty : bool
  completed : list(string)
  rowdescs : Postgres.rowdescs
  rows : int
  rowcc : continuation((int,Postgres.rowdescs,Postgres.row))
  endcc : continuation(void)
  errcc : continuation(Postgres.unsuccessful)
}

Postgres = {{

  @private bindump = (%% BslPervasives.bindump %%: binary -> string)

  @private to_rowdesc((name,table_id,table_attribute_number,type_id,data_type_size,type_modifier,format_code)) =
    ~{name table_id table_attribute_number type_id data_type_size type_modifier format_code} : Postgres.rowdesc 

  default_host = Pg.default_host

  Conn = Pg.Conn

  @private rowcc = Continuation.make(row -> Log.error("Postgres","missing row callback row={row}"))
                 : continuation((int,Postgres.rowdescs,Postgres.row))
  @private endcc = Continuation.make(_ -> Log.error("Postgres","missing finalize callback"))
                 : continuation(void)
  @private errcc = Continuation.make((_,err) -> Log.error("Postgres","missing error callback err={err}"))
                 : continuation(Postgres.unsuccessful)

  connect(name:string, dbase:string, user:string, password:string) : outcome(Postgres.connection,Apigen.failure) =
    match Pg.connect(name) with
    | {success=conn} ->
      {success={ ~conn ~dbase ~user ~password
                 params=StringMap.empty processid=-1 secret_key=-1 query="" status="" error=none
                 empty=true completed=[] rowdescs=[] rows=0
                 ~rowcc ~endcc ~errcc
               }}
    | {~failure} -> {~failure}

  close(conn:Postgres.connection) : outcome(Postgres.connection,Apigen.failure) =
    match Pg.close({success=conn.conn}) with
    | {success=c} -> {success={conn with conn=c}}
    | {~failure} -> {~failure}

  param(conn:Postgres.connection, name:string) : option(string) =
    StringMap.get(name, conn.params)

  keydata(conn:Postgres.connection) : option({processid:int; secret_key:int}) =
    if conn.processid != -1 && conn.secret_key != -1
    then {some={processid=conn.processid; secret_key=conn.secret_key}}
    else {none}

  status(conn:Postgres.connection) : option(string) =
    if conn.status == "" then {none} else {some=conn.status}

  @private flush(conn:Postgres.connection, k:continuation(Postgres.result)) : void =
    match Pg.Conn.rcv({success=conn.conn}) with
    | {success=binary} -> 
      match (Pg.unpack_PostgresReply({~binary; pos=0})) with
      | {success=(_,{Authentication={Ok}})} ->
         flush(conn,k)
      | {success=(_,{Authentication={MD5Password=salt}})} ->
         inner = Crypto.Hash.md5(conn.password^conn.user)
         outer = Crypto.Hash.md5(inner^(%%bslBinary.to_encoding%%(salt,"binary")))
         md5password = "md5"^outer
         match (Pg.md5pass({success=conn.conn},md5password)) with
         | {success={}} -> flush(conn,k)
         | ~{failure} -> flush({conn with error={some={api_failure=failure}}},k)
         end
      | {success=(_,{ParameterStatus=(n,v)})} ->
         flush({conn with params=StringMap.add(n,v,conn.params)},k)
      | {success=(_,{BackendKeyData=(processid,secret_key)})} ->
         flush({conn with ~processid ~secret_key},k)
      | {success=(_,{~CommandComplete})} ->
         flush({conn with completed=[CommandComplete|conn.completed]},k)
      | {success=(_,{EmptyQueryResponse})} ->
         flush({conn with empty=true},k)
      | {success=(_,{~RowDescription})} ->
         flush({conn with rowdescs=List.map(to_rowdesc,RowDescription)},k)
      | {success=(_,{~DataRow})} ->
         do Continuation.return(conn.rowcc,(conn.rows,conn.rowdescs,DataRow))
         flush({conn with rows=conn.rows+1},k)
      | {success=(_,{~NoticeResponse})} ->
         do Continuation.return(conn.errcc,(conn,{postgres={notice=NoticeResponse}}))
         flush(conn,k)
      | {success=(_,{~ErrorResponse})} ->
         do Continuation.return(conn.errcc,(conn,{postgres={error=ErrorResponse}}))
         flush({conn with error={some={postgres={error=ErrorResponse}}}},k)
      | {success=(_,{ReadyForQuery=status})} ->
         result =
           match conn.error with
           | {some=failure} -> {failure=(conn,failure)}
           | {none} -> {success={conn with ~status}}
         Continuation.return(k,result)
      | {success=(_,reply)} ->
         Continuation.return(k,{failure=(conn,{bad_reply=reply})})
      | {~failure} ->
         Continuation.return(k,{failure=(conn,{api_failure={pack=failure}})})
      end
    | ~{failure} -> Continuation.return(k,{failure=(conn,{api_failure=failure})})
    end

  authenticate(conn:Postgres.connection, errcc) =
    conn = {conn with error=none; empty=false; completed=[]; rowdescs=[]; rows=0; query="authentication"; ~rowcc; ~endcc; ~errcc}
    match Pg.start({success=conn.conn}, (196608, [("user",conn.user),("database",conn.dbase)])) with
    | {success={}} -> @callcc(k -> flush(conn,k))
    | ~{failure} -> {failure=(conn,{api_failure=failure})}

  query(conn:Postgres.connection, query, rowcc, errcc) =
    conn = {conn with error=none; empty=false; completed=[]; rowdescs=[]; rows=0; ~query; ~rowcc; ~errcc}
    match Pg.query({success=conn.conn},query) with
    | {success={}} -> @callcc(k -> flush(conn,k))
    | ~{failure} -> {failure=(conn,{api_failure=failure})}
    end

}}
