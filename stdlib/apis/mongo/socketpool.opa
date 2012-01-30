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
package stdlib.apis.mongo

import stdlib.queue

type SocketPool.result = outcome(Socket.connection,Mongo.failure)

@private type SocketPool.state = {
  host: Mongo.mongo_host;
  max: int;
  sockets: list(Socket.connection);
  allocated: list(Socket.connection);
  cnt: int;
  log: bool;
  queue: Queue.t(continuation(SocketPool.result));
}

@private type SocketPool.msg =
    {get : continuation(SocketPool.result)}
  / {release : Socket.connection}
  / {reconnect : Mongo.mongo_host}
  / {gethost : continuation(Mongo.mongo_host)}
  / {close}
  / {stop}

@abstract type SocketPool.t = channel(SocketPool.msg)

@server @private
SocketPool = {{

  @private ML = MongoLog

  @private
  Socket_close(log)(conn) =
    do if log then ML.debug("SocketPool.handler","close {Socket.conn_id(conn)}",void)
    Socket.close(conn)

  @private pool_handler(state:SocketPool.state, msg:SocketPool.msg): Session.instruction(SocketPool.state) =
    match msg with
    | {release=connection} ->
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
            do Continuation.return(waiter, {success=connection})
            {set={state with ~queue}})
       else
         do if state.log then ML.debug("SocketPool.handler","drop socket {Socket.conn_id(connection)}",void)
         do Socket_close(state.log)(connection)
         {unchanged}
    | {get=k} ->
       (match state.sockets with
        | [conn|sockets] ->
          do if state.log then ML.debug("SocketPool.handler","reuse open socket {Socket.conn_id(conn)}",void)
          do Continuation.return(k, {success=conn})
          allocated = conn +> state.allocated
          {set={state with ~sockets; ~allocated}}
        | [] ->
          if state.cnt >= state.max
          then
            do if state.log then ML.debug("SocketPool.handler","queue caller",void)
            {set={state with queue=Queue.add(k, state.queue)}}
          else
            (match Socket.connect_with_err_cont(state.host.f1,state.host.f2) with
             | {success=conn} ->
                do if state.log then ML.debug("SocketPool.handler","successfully opened socket {Socket.conn_id(conn)}",void)
                do Continuation.return(k, {success=conn})
                allocated = conn +> state.allocated
                {set={state with cnt=state.cnt+1; ~allocated}}
             | {failure=str} ->
                do if state.log then ML.debug("SocketPool.handler","open socket failure",void)
                do Continuation.return(k, {failure={Error="Got exception {str}"}})
                {unchanged}))
    | {reconnect=host} ->
       do if state.log then ML.debug("SocketPool.handler","reconnect({host.f1}.{host.f2})",void)
       do List.iter(Socket_close(state.log),state.sockets)
       do List.iter(Socket_close(state.log),state.allocated)
       {set={state with ~host; cnt=0; sockets=[]; allocated=[]}}
    | {gethost=k} ->
       do Continuation.return(k, state.host)
       {unchanged}
    | {close} ->
       do if state.log then ML.debug("SocketPool.handler","close socket pool",void)
       do List.iter(Socket_close(state.log),state.sockets)
       do List.iter(Socket_close(state.log),state.allocated)
       {unchanged}
    | {stop} ->
       do if state.log then ML.debug("SocketPool.handler","stop socket pool",void)
       do List.iter(Socket_close(state.log),state.sockets)
       do List.iter(Socket_close(state.log),state.allocated)
       {stop}

  make(host:Mongo.mongo_host, max:int, log:bool): SocketPool.t =
    Session.make(({~host; ~max; ~log; cnt=0; sockets=[]; allocated=[]; queue=Queue.empty;}:SocketPool.state), pool_handler)

  get(pool:SocketPool.t) : SocketPool.result =
    @callcc(k -> Session.send(pool, {get=k}))

  release(pool:SocketPool.t, connection:Socket.connection) : void =
    Session.send(pool, {release=connection})

  reconnect(pool:SocketPool.t, host:Mongo.mongo_host) : void =
    Session.send(pool, {reconnect=host})

  gethost(pool:SocketPool.t) : Mongo.mongo_host =
    @callcc(k -> Session.send(pool, {gethost=k}))

  close(pool:SocketPool.t) : void =
    Session.send(pool, {close})

  stop(pool:SocketPool.t) : void =
    Session.send(pool, {stop})

}}

// End of file socketpool.opa
