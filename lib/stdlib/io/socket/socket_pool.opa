/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.core.queue

/**
 * Authentication information.
 * We should really parametrise the socket pool with this
 * type but that would propagate for miles and miles.
 * For now we just provide two general-purpose ints and a bool
 * (see Postgres for the use of the ints).
 */
type SocketPool.auth = { authenticated:bool; i1:int; i2:int }

/**
 * A SocketPool.result is either a socket or a string which describes the socket error.
 */
type SocketPool.result = outcome(Socket.t, string)

/**
 * A SocketPool result with an authorisation flag.
 */
type SocketPool.auth_result = outcome((Socket.t,SocketPool.auth), string)

/**
 * Some protocols have a preamble to trigger SSL authentication (see Postgres).
 */
type SocketPool.preamble = Socket.t -> SocketPool.result

/**
 * Configuration of socket pool.
 */
type SocketPool.conf = {
  /** Indicates if the pool should output a trace of its action */
  verbose : bool;

  /** The default hint of the allocated mailboxes */
  hint    : int;

  /** The maximum number of allocated socket */
  max     : int;
}

@private type SocketPool.waiter =
   {waiter:continuation(SocketPool.result)}
 / {auth_waiter:continuation(SocketPool.auth_result)}

@private type SocketPool.state = {
  host : Socket.host;
  secure_type : option(SSL.secure_type);
  preamble : option(SocketPool.preamble);
  conf : SocketPool.conf;
  sockets: list(Socket.t);
  auths: list(SocketPool.auth);
  allocated: list(Socket.t);
  cnt: int;
  queue: Queue.t(SocketPool.waiter);
  open_connections: intset;
}

@private type SocketPool.msg =
    {get : continuation(SocketPool.result)}
  / {get_auth : continuation(SocketPool.auth_result)}
  / {release : Socket.t}
  / {release_auth : (Socket.t,SocketPool.auth)}
  / {reconnect : Socket.host}
  / {getconf : continuation(SocketPool.conf)}
  / {updconf : SocketPool.conf -> SocketPool.conf}
  / {gethost :  continuation(Socket.host)}
  / {close}
  / {stop}

@abstract type SocketPool.t = channel(SocketPool.msg)

@server_private
SocketPool = {{

  @private
  Log = {{

    @private
    @expand
    gen(f, m, fn:string, msg) =
      if m.conf.verbose then f("SocketPool({m.host}).{fn}", msg)
      else void

    @expand
    info(m, fn, msg) = gen(@toplevel.Log.info, m, fn, msg)

    @expand
    debug(m, fn, msg) = gen(@toplevel.Log.debug, m, fn, msg)

    @expand
    error(m, fn, msg) = gen(@toplevel.Log.error, m, fn, msg)

  }}

  @private
  socket_close(conn, state) =
    conn_id = Socket.conn_id(conn.conn)
    do Log.debug(state, "SocketPool.handler", "close {conn_id}")
    do Socket.close(conn.conn)
    _ = Mailbox.reset(conn.mbox)
    {state with open_connections=IntSet.remove(conn_id,state.open_connections)}

  @private @expand monitor(__from, __state) =
    #<Ifstatic:SOCKET_POOL_DEBUG>
     do Log.debug(__state, "handler({__from})",
                 "open={List.list_to_string(Int.to_string,IntSet.To.list(__state.open_connections))}")
     do Log.debug(__state, "handler({__from})",
                 "sockets={List.list_to_string(Int.to_string,List.map((id -> Socket.conn_id(id.conn)),__state.sockets))}")
     do Log.debug(__state, "handler({__from})",
                 "allocated={List.list_to_string(Int.to_string,List.map((id -> Socket.conn_id(id.conn)),__state.allocated))}")
     void
    #<Else>
     void
    #<End>

  @private release_socket(state:SocketPool.state, connection:Socket.t, authenticated:SocketPool.auth): Session.instruction(SocketPool.state) =
    do monitor("release", state)
    conn_id = Socket.conn_id(connection.conn)
    if List.exists((c -> Socket.conn_id(c.conn) == conn_id),state.allocated)
    then
      match Queue.rem(state.queue) with
      | ({none}, _) ->
        do Log.debug(state, "handler","socket back in pool {Socket.conn_id(connection.conn)}")
        sockets = connection +> state.sockets
        auths = authenticated +> state.auths
        conn_id = Socket.conn_id(connection.conn)
        allocated = List.filter((s -> Socket.conn_id(s.conn) != conn_id),state.allocated)
        {set={state with ~sockets; ~auths; ~allocated}}
      | ({some={~waiter}}, queue) ->
        do Log.debug(state, "handler","reallocate socket {Socket.conn_id(connection.conn)}")
        do Continuation.return(waiter, {success=connection})
        {set={state with ~queue}}
      | ({some={~auth_waiter}}, queue) ->
        do Log.debug(state, "handler","reallocate socket {Socket.conn_id(connection.conn)}")
        do Continuation.return(auth_waiter, {success=(connection,authenticated)})
        {set={state with ~queue}}
      end
    else
      do Log.debug(state, "handler","drop socket {Socket.conn_id(connection.conn)}")
      state = socket_close(connection, state)
      {set=state}

  @private socket_return(k:SocketPool.waiter, connection, authenticated:SocketPool.auth) : void =
    match k with
    | {waiter=k} -> Continuation.return(k, {success=connection})
    | {auth_waiter=k} -> Continuation.return(k, {success=(connection,authenticated)})
    end

  @private socket_error(k:SocketPool.waiter, failure:{failure:string}) : void =
    match (k,failure) with
    | ({waiter=k},{~failure}) -> Continuation.return(k, ~{failure})
    | ({auth_waiter=k},{~failure}) -> Continuation.return(k, ~{failure})
    end

  @private get_socket(state:SocketPool.state, k:SocketPool.waiter): Session.instruction(SocketPool.state) =
    do monitor("get", state)
    match (state.sockets,state.auths) with
    | ([connection|sockets],[authenticated|auths]) ->
      do Log.debug(state, "handler","reuse open socket {Socket.conn_id(connection.conn)}")
      do socket_return(k, connection, authenticated)
      allocated = connection +> state.allocated
      {set={state with ~sockets; ~auths; ~allocated}}
    | (_,_) ->
      if state.cnt >= state.conf.max
      then
        do Log.debug(state, "handler", "queue caller")
        {set={state with queue=Queue.add(k, state.queue)}}
      else
        match
          match state.secure_type with
          | {some=secure_type} ->
             match state.preamble with
             | {some=preamble} ->
                //match Socket.binary_secure_connect_with_err_cont(state.host.f1,state.host.f2,false,secure_type) with
                match Socket.binary_connect_with_err_cont(state.host.f1,state.host.f2) with
                | {success=conn} ->
                   match preamble({~conn; mbox=Mailbox.create(state.conf.hint)}) with
                   | {success=conn} ->
                      Socket.binary_secure_reconnect_with_err_cont(conn.conn,state.host.f1,state.host.f2,secure_type)
                   | ~{failure} -> ~{failure}
                   end
                | ~{failure} -> ~{failure}
                end
             | {none} -> Socket.binary_secure_connect_with_err_cont(state.host.f1,state.host.f2,true,secure_type)
             end
          | {none} -> Socket.binary_connect_with_err_cont(state.host.f1,state.host.f2)
          end
        with
        | {success=conn} ->
           connection = {~conn; mbox=Mailbox.create(state.conf.hint)}
           state = {state with open_connections=IntSet.add(Socket.conn_id(connection.conn),state.open_connections)}
           do Log.debug(state, "handler", "successfully opened socket {Socket.conn_id(connection.conn)}")
           do socket_return(k, connection, {authenticated=false; i1=-1; i2=-1})
           allocated = connection +> state.allocated
           {set={state with cnt=state.cnt+1; ~allocated}}
        | {failure=str} ->
           do Log.debug(state, "handler", "open socket failure: {str}")
           do socket_error(k, {failure=str})
           {unchanged}
        end

  @private pool_handler(state:SocketPool.state, msg:SocketPool.msg): Session.instruction(SocketPool.state) =
    match msg with
    | {release=connection} -> release_socket(state, connection, {authenticated=false; i1=-1; i2=-1})
    | {release_auth=(connection,authenticated)} -> release_socket(state, connection, authenticated)
    | {get=k} -> get_socket(state, {waiter=k})
    | {get_auth=k} -> get_socket(state, {auth_waiter=k})
    | {reconnect=host} ->
       do monitor("reconnect", state)
       do Log.debug(state, "handler", "reconnect({host.f1}.{host.f2})")
       state = List.fold(socket_close,state.sockets,state)
       state = List.fold(socket_close,state.allocated,state)
       {set={state with ~host; cnt=0; sockets=[]; auths=[]; allocated=[]}}
    | {getconf=k} ->
       do Continuation.return(k, state.conf)
       {unchanged}
    | {updconf=f} ->
       {set = {state with conf = f(state.conf)}}
    | {gethost=k} ->
       do Continuation.return(k, state.host)
       {unchanged}
    | {close} ->
       do Log.debug(state, "handler","close socket pool")
       state = List.fold(socket_close,state.sockets,state)
       state = List.fold(socket_close,state.allocated,state)
       {set={state with auths=[]}}
    | {stop} ->
       do monitor("stop", state)
       do Log.debug(state, "handler","stop socket pool")
       state = List.fold(socket_close,state.sockets,state)
       _state = List.fold(socket_close,state.allocated,state)
       {stop}

  @private
  initial_state(host, conf, secure_type, preamble) = {
    ~host; ~conf; ~secure_type; ~preamble;
     cnt=0;
     sockets=[];
     auths=[];
     allocated=[];
     queue=Queue.empty;
     open_connections=IntSet.empty;
  }

  /**
   * Create a pool of sockets which will return sockets connected to the [host].
   * @param host The host to connect
   * @param conf The initial configuration of the socket pool
   * @return A pool of sockets
   */
  make(host:Socket.host, conf:SocketPool.conf): SocketPool.t =
    state = initial_state(host, conf, none, none)
    do Log.debug(state, "make","{host}")
    Session.make(state, pool_handler)

  /**
   * Create a pool of sockets which will return sockets securely connected to the [host].
   * @param host The host to connect
   * @param conf The initial configuration of the socket pool
   * @param secure_type The optional SSL security information, none is the same as an insecure connection
   * @param preamble An optional function to call upon an insecure socket before SSL handshake
   * @return A pool of sockets
   */
  make_secure(host:Socket.host, conf:SocketPool.conf,
              secure_type:option(SSL.secure_type), preamble:option(SocketPool.preamble)): SocketPool.t =
    state = initial_state(host, conf, secure_type, preamble)
    do Log.debug(state, "make","{host}")
    Session.make(state, pool_handler)

  /**
   * Get a socket from the [pool]. If the maximum of allocated sockets are
   * reached wait until a socket is released (see [SocketPool.conf]).
   * @param pool The pool of socket
   * @return A socket connected to the current [host] of the pool
   */
  get(pool:SocketPool.t) : SocketPool.result =
    @callcc(k -> Session.send(pool, {get=k}))

  /**
   * Same as get but also returns an authentication value associated with the socket.
   * This is intended, for example, to attach an "authenticated" status to the socket
   * for connections requiring external authentication.
   */
  get_auth(pool:SocketPool.t) : SocketPool.auth_result =
    @callcc(k -> Session.send(pool, {get_auth=k}))

  /**
   * Same as get but the result is returned to the [callback]
   * @param pool The pool of socket
   * @param callback The callback which receives the result
   */
  get_async(pool:SocketPool.t, callback) =
    @callcc(k ->
      do Continuation.return(k, void)
      callback(get(pool))
    )

  /**
   * Same as get_async but with the authentication flag.
   */
  get_auth_async(pool:SocketPool.t, callback) =
    @callcc(k ->
      do Continuation.return(k, void)
      callback(get_auth(pool))
    )

  /**
   * Release [socket] into the pool. If the socket not coming from the [pool]
   * then the socket is just close and the pool stay unchanged.
   * @param socket The socket to release
   * @param pool The pool of socket
   */
  release(pool:SocketPool.t, socket:Socket.t) : void =
    Session.send(pool, {release=socket})

  /**
   * Same as release but returns the authentication flag along with the socket.
   */
  release_auth(pool:SocketPool.t, socket:Socket.t, authenticated:SocketPool.auth) : void =
    Session.send(pool, {release_auth=(socket,authenticated)})

  /**
   * Change the [host] where sockets of the pool are connected.
   * @param pool The pool of socket
   * @param host The new host to set
   */
  reconnect(pool:SocketPool.t, host:Socket.host) : void =
    Session.send(pool, {reconnect=host})

  /**
   * Close all allocated socket into the socket [pool]
   * @param pool The pool of socket
   */
  close(pool:SocketPool.t) : void =
    Session.send(pool, {close})

  /**
   * Stop the pool of socket, no more socket will be allocated.
   * @param pool The pool of socket
   */
  stop(pool:SocketPool.t) : void =
    Session.send(pool, {stop})


  @private getconf_map(pool, f) = f(getconf(pool))

  /**
   * Returns the configuration of the [pool] of socket
   * @param pool The pool of socket
   * @return The configuration of the [pool]
   */
  getconf(pool:SocketPool.t) : SocketPool.conf =
    @callcc(k -> Session.send(pool, {getconf=k}))

  /**
   * Returns the host of the [pool] of socket
   * @param pool The pool of socket
   * @return The host of the [pool]
   */
  gethost(pool:SocketPool.t) : Socket.host =
    @callcc(k -> Session.send(pool, {gethost=k}))

  /**
   * Returns the maximum number of allocated socket of the [pool] of socket
   * @param pool The pool of socket
   * @return The maximum number of allocated socket of the [pool]
   */
  getmax(pool:SocketPool.t) : int =
    getconf_map(pool, _.max)

  /**
   * Returns the default hint of allocated mailboxes of the [pool] of socket
   * @param pool The pool of socket
   * @return The default hint of allocated mailboxes  of the [pool]
   */
  gethint(pool:SocketPool.t) : int =
    getconf_map(pool, _.hint)

  /**
   * Returns the verbosity  of the [pool] of socket
   * @param pool The pool of socket
   * @return The verbosity of the [pool]
   */
  getverbose(pool:SocketPool.t) : bool =
    getconf_map(pool, _.verbose)

  @private setconf_map(pool, f) =
    Session.send(pool, {updconf=f})

  /**
   * Set the configuration of the [pool] of socket
   * @param pool The pool of socket
   * @param The new configuration of the [pool]
   */
  setconf(pool:SocketPool.t, conf:SocketPool.conf) =
    setconf_map(pool, (_ -> conf))

  /**
   * Set the maximum number of allocated socket of the [pool] of socket
   * @param pool The pool of socket
   * @param max The new maximum number of allocated socket of the [pool]
   */
  setmax(pool:SocketPool.t, max) =
    setconf_map(pool, (conf -> {conf with ~max}))

  /**
   * Set the default hint of allocated mailboxes of the [pool] of socket
   * @param pool The pool of socket
   * @param hint The new default hint of allocated mailboxes  of the [pool]
   */
  sethint(pool:SocketPool.t, hint:int) =
    setconf_map(pool, (conf -> {conf with ~hint}))

  /**
   * Set the verbosity of the [pool] of socket
   * @param pool The pool of socket
   * @param verbose
   */
  setverbose(pool:SocketPool.t, verbose) =
    setconf_map(pool, (conf -> {conf with ~verbose}))

}}

// End of file socketpool.opa
