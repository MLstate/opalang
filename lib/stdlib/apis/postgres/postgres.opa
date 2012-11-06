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
 / { ErrorResponse: list(tuple_2(int, string)) }
 / { NoticeResponse: list(tuple_2(int, string)) }
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
 / { postgres : list((int,string)) }
 / { bad_reply : Postgres.reply }
 / { bad_row : (list(Postgres.rowdesc),list(binary)) }
 / { bad_format : int }

type Postgres.success = Postgres.connection

type Postgres.result = outcome(Postgres.success,Postgres.failure)

type Postgres.rowdesc = {
  name : string
  table_id : int
  table_attribute_number : int
  type_id : int
  data_type_size : int
  type_modifier : int
  format_code : int
}

type Postgres.data = {text:string} / {binary:binary}

type Postgres.rows = (list(Postgres.rowdesc),list(list(Postgres.data)))

type Postgres.connection = {
  conn : ApigenLib.connection
  dbase : string
  user : string
  password : string
  params : stringmap(string)
  processid : int
  secret_key : int
  status : string
  error : option(Postgres.result)
  empty : bool
  completed : list(string)
  rowdescs : list((string,int,int,int,int,int,int))
  rows : list(list(binary))
}

Postgres = {{

  @private bindump = (%% BslPervasives.bindump %%: binary -> string)

  default_host = Pg.default_host

  Conn = Pg.Conn

  connect(name:string, dbase:string, user:string, password:string) : outcome(Postgres.connection,Apigen.failure) =
    match Pg.connect(name) with
    | {success=conn} ->
      {success={ ~conn ~dbase ~user ~password
                 params=StringMap.empty processid=-1 secret_key=-1 status="" error=none
                 empty=true completed=[] rowdescs=[] rows=[] }}
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

  @private flush(conn:Postgres.connection) : Postgres.result =
    match Pg.Conn.rcv({success=conn.conn}) with
    | {success=binary} -> 
      match (Pg.unpack_PostgresReply({~binary; pos=0})) with
      | {success=(_,{Authentication={Ok}}:Postgres.reply)} -> flush(conn)
      | {success=(_,{Authentication={MD5Password=salt}}:Postgres.reply)} ->
         inner = Crypto.Hash.md5(conn.password^conn.user)
         outer = Crypto.Hash.md5(inner^(%%bslBinary.to_encoding%%(salt,"binary")))
         md5password = "md5"^outer
         match (Pg.md5pass({success=conn.conn},md5password)) with
         | {success={}} -> flush(conn)
         | ~{failure} -> flush({conn with error={some={failure={api_failure=failure}}}})
         end
      | {success=(_,{ParameterStatus=(n,v)}:Postgres.reply)} -> flush({conn with params=StringMap.add(n,v,conn.params)})
      | {success=(_,{BackendKeyData=(processid,secret_key)}:Postgres.reply)} -> flush({conn with ~processid ~secret_key})
      | {success=(_,{~CommandComplete}:Postgres.reply)} -> flush({conn with completed=[CommandComplete|conn.completed]})
      | {success=(_,{EmptyQueryResponse}:Postgres.reply)} -> flush({conn with empty=true})
      | {success=(_,{~RowDescription}:Postgres.reply)} -> flush({conn with rowdescs=RowDescription})
      | {success=(_,{~DataRow}:Postgres.reply)} -> flush({conn with rows=[DataRow|conn.rows]})
      | {success=(_,{~NoticeResponse}:Postgres.reply)} ->
        do Log.notice("Postgres/Notices","{String.concat("\n",List.map(((c,n) -> "{c}: {n}"),NoticeResponse))}")
        flush(conn)
      | {success=(_,{ErrorResponse=msgs}:Postgres.reply)} -> flush({conn with error={some={failure={postgres=msgs}}}})
      | {success=(_,{ReadyForQuery=status}:Postgres.reply)} -> Option.default({success={conn with ~status}},conn.error)
      | {success=(_,reply:Postgres.reply)} -> {failure={bad_reply=reply}}
      | {~failure} -> {failure={api_failure={pack=failure}}}
      end
    | ~{failure} -> {failure={api_failure=failure}}
    end

  authenticate(conn:Postgres.connection) =
    match Pg.start({success=conn.conn}, (196608, [("user",conn.user),("database",conn.dbase)])) with
    | {success={}} -> flush(conn)
    | ~{failure} -> {failure={api_failure=failure}}

  query(conn:Postgres.connection, q) =
    conn = {conn with error=none; empty=false; completed=[]; rowdescs=[]; rows=[]}
    match Pg.query({success=conn.conn},q) with
    | {success={}} -> flush(conn)
    | ~{failure} -> {failure={api_failure=failure}}
    end

  to_rowdesc((name,table_id,table_attribute_number,type_id,data_type_size,type_modifier,format_code)) : Postgres.rowdesc =
    ~{name table_id table_attribute_number type_id data_type_size type_modifier format_code}

  query_row(row:list(binary), rows:outcome(Postgres.rows,Postgres.failure)) =
    match rows with
    | {success=(rowdescs,rows)} ->
      if List.length(rowdescs) == List.length(row)
      then
        aux2(rowdesc,bin,row) =
          match row with
          | {success=row} ->
            aux(el) = {success=[el|row]}
            match rowdesc.format_code with
            | 0 -> aux({text=string_of_binary(bin)})
            | 1 -> aux({binary=bin})
            | n -> {failure={bad_format=n}}
            end
          | {~failure} -> {~failure}
        aux3(row) =
          match row with
          | {success=row} -> {success=(rowdescs,[List.rev(row)|rows])}
          | {~failure} -> {~failure}
        aux3(List.fold2(aux2,rowdescs,row,{success=[]}))
      else {failure={bad_row=(rowdescs,row)}}
    | {~failure} -> {~failure}

  query_rows(conn:Postgres.connection) =
    if conn.empty
    then {success=([],[])}
    else List.fold(query_row,conn.rows,{success=(List.map(to_rowdesc,conn.rowdescs),[])})

}}
