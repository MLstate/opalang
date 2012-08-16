/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/
package stdlib.apis.mongo

import stdlib.core.queue

type SocketPool.result = outcome((bool,Socket.connection),Mongo.failure)

@private type SocketPool.state = {
  host: Mongo.mongo_host;
  max: int;
  sockets: list(Socket.connection);
  allocated: list(Socket.connection);
  cnt: int;
  log: bool;
  queue: Queue.t(continuation(SocketPool.result));
  slaveok: bool;
  open_connections: intset;
  monitor_connections: bool;
}

@private type SocketPool.msg =
    {get : continuation(SocketPool.result)}
  / {release : Socket.connection}
  / {reconnect : Mongo.mongo_host}
  / {gethost : continuation(Mongo.mongo_host)}
  / {setslaveok : bool}
  / {getslaveok : continuation(bool)}
  / {close}
  / {stop}

@abstract type SocketPool.t = channel(SocketPool.msg)

@server @private
SocketPool = {{

  @private ML = MongoLog

  @private
  Socket_close(conn, state) =
    conn_id = Socket.conn_id(conn)
    do if state.log then ML.debug("SocketPool.handler","close {conn_id}",void)
    do Socket.close(conn)
    {state with open_connections=IntSet.remove(conn_id,state.open_connections)}

  @private monitor(from, state) =
    if state.monitor_connections
    then
     do ML.debug("SocketPool.handler({from})",
                 "open={List.list_to_string(Int.to_string,IntSet.To.list(state.open_connections))}",void)
     do ML.debug("SocketPool.handler({from})",
                 "sockets={List.list_to_string(Int.to_string,List.map(Socket.conn_id,state.sockets))}",void)
     do ML.debug("SocketPool.handler({from})",
                 "allocated={List.list_to_string(Int.to_string,List.map(Socket.conn_id,state.allocated))}",void)
     void

  @private pool_handler(state:SocketPool.state, msg:SocketPool.msg): Session.instruction(SocketPool.state) =
    match msg with
    | {release=connection} ->
       do monitor("release", state)
       conn_id = Socket.conn_id(connection)
       if List.exists((c -> Socket.conn_id(c) == conn_id),state.allocated)
       then
         (match Queue.rem(state.queue) with
          | ({none}, _) ->
            do if state.log then ML.debug("SocketPool.handler","socket back in pool {Socket.conn_id(connection)}",void)
            sockets = connection +> state.sockets
            conn_id = Socket.conn_id(connection)
            allocated = List.filter((s -> Socket.conn_id(s) != conn_id),state.allocated)
            {set={state with ~sockets; ~allocated}}
          | ({some=waiter}, queue) ->
            do if state.log then ML.debug("SocketPool.handler","reallocate socket {Socket.conn_id(connection)}",void)
            do Continuation.return(waiter, {success=(state.slaveok,connection)})
            {set={state with ~queue}})
       else
         do if state.log then ML.debug("SocketPool.handler","drop socket {Socket.conn_id(connection)}",void)
         state = Socket_close(connection, state)
         {set=state}
    | {get=k} ->
       do monitor("get", state)
       (match state.sockets with
        | [conn|sockets] ->
          do if state.log then ML.debug("SocketPool.handler","reuse open socket {Socket.conn_id(conn)}",void)
          do Continuation.return(k, {success=(state.slaveok,conn)})
          allocated = conn +> state.allocated
          {set={state with ~sockets; ~allocated}}
        | [] ->
          if state.cnt >= state.max
          then
            do if state.log then ML.debug("SocketPool.handler","queue caller",void)
            {set={state with queue=Queue.add(k, state.queue)}}
          else
            (match Socket.binary_connect_with_err_cont(state.host.f1,state.host.f2) with
             | {success=conn} ->
                state = {state with open_connections=IntSet.add(Socket.conn_id(conn),state.open_connections)}
                do if state.log then ML.debug("SocketPool.handler","successfully opened socket {Socket.conn_id(conn)}",void)
                do Continuation.return(k, {success=(state.slaveok,conn)})
                allocated = conn +> state.allocated
                {set={state with cnt=state.cnt+1; ~allocated}}
             | {failure=str} ->
                do if state.log then ML.debug("SocketPool.handler","open socket failure",void)
                do Continuation.return(k, {failure={Error="Got exception {str}"}})
                {unchanged}))
    | {reconnect=host} ->
       do monitor("reconnect", state)
       do if state.log then ML.debug("SocketPool.handler","reconnect({host.f1}.{host.f2})",void)
       state = List.fold(Socket_close,state.sockets,state)
       state = List.fold(Socket_close,state.allocated,state)
       {set={state with ~host; cnt=0; sockets=[]; allocated=[]}}
    | {gethost=k} ->
       do Continuation.return(k, state.host)
       {unchanged}
    | {setslaveok=tf} ->
       {set={state with slaveok=tf}}
    | {getslaveok=k} ->
       do Continuation.return(k, state.slaveok)
       {unchanged}
    | {close} ->
       do if state.log then ML.debug("SocketPool.handler","close socket pool",void)
       state = List.fold(Socket_close,state.sockets,state)
       state = List.fold(Socket_close,state.allocated,state)
       {set=state}
    | {stop} ->
       do monitor("stop", state)
       do if state.log then ML.debug("SocketPool.handler","stop socket pool",void)
       state = List.fold(Socket_close,state.sockets,state)
       _state = List.fold(Socket_close,state.allocated,state)
       {stop}

  make(host:Mongo.mongo_host, max:int, _log:bool): SocketPool.t =
    do ML.debug("SocketPool.make","{host}",void)
    Session.make(({~host; ~max; log=true; cnt=0; sockets=[]; allocated=[]; queue=Queue.empty;
                   slaveok=false; open_connections=IntSet.empty; monitor_connections=true}:SocketPool.state),
                 pool_handler)

  get(pool:SocketPool.t) : SocketPool.result =
    @callcc(k -> Session.send(pool, {get=k}))

  release(pool:SocketPool.t, connection:Socket.connection) : void =
    Session.send(pool, {release=connection})

  reconnect(pool:SocketPool.t, host:Mongo.mongo_host) : void =
    Session.send(pool, {reconnect=host})

  gethost(pool:SocketPool.t) : Mongo.mongo_host =
    @callcc(k -> Session.send(pool, {gethost=k}))

  setslaveok(pool:SocketPool.t, setslaveok:bool) : void =
    Session.send(pool, {~setslaveok})

  getslaveok(pool:SocketPool.t) : bool =
    @callcc(k -> Session.send(pool, {getslaveok=k}))

  close(pool:SocketPool.t) : void =
    Session.send(pool, {close})

  stop(pool:SocketPool.t) : void =
    Session.send(pool, {stop})

}}

// End of file socketpool.opa
