/*
    Copyright © 2011, 2012 MLstate

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

#<Ifstatic:OPA_FULL_DISPATCHER>

type PingRegister.msg = RPC.Json.json

@private
type PingRegister.entry =
  {ajax:web_info nb:int key:Scheduler.key}
/ {msgs:list(PingRegister.msg)}

@private
type PingRegister.pang_result = {nb:int result:string}

@server_private
PingRegister = {{

  @private
  debug = Log.debug("PING", _)

  @private
  error = Log.error("PING", _)

  @package
  Utils = {{

    @private
    hash_client(~{client page}:ThreadContext.client) =
      "{client}_{page}"

    make_tctbl(): Hashtbl.t(ThreadContext.client, 'a) =
      Hashtbl.make(hash_client, (_, _ -> true), 1024)

  }}

  /**
   * This module manages the communications between the server and the clients.
   * It handles ping and pang request, buffurize messages sent to client, etc.
   */
  @private
  Entry = {{

    @private
    hash_client(~{client page}:ThreadContext.client) =
      "{client}_{page}"

    @private
    entries : Hashtbl.t(ThreadContext.client, PingRegister.entry) =
      Utils.make_tctbl()

    @private
    pangtbl : Hashtbl.t(ThreadContext.client, list(PingRegister.pang_result)) =
      Utils.make_tctbl()

    @private
    ping_delay = 30 * 1000

    /**
     * Associate the [entry] to the [client].
     */
    @private
    add(client, entry) =
      //do @assert(not(Hashtbl.mem(entries, client)))
      Hashtbl.add(entries, client, entry)

    /**
     * As add but overwrite the previous association if already exists.
     */
    @private
    replace(client, entry) =
      Hashtbl.replace(entries, client, entry)

    /**
     * Remove the entry association to the [client]
     */
    @private
    remove(client) =
      Hashtbl.remove(entries, client)

    /**
     * Add the [nb]th pang [result] to the pang table.
     */
    @private
    add_pang(client, nb, result) =
      @atomic(
        match Hashtbl.try_find(pangtbl, client) with
        | {none} -> Hashtbl.add(pangtbl, client, [~{result nb}])
        | {some = pangs} -> Hashtbl.replace(pangtbl, client, [~{result nb} | pangs])
      )

    /**
     * Send a msg on the [winfo].
     */
    @private
    send_response(winfo, msg) =
      json = match msg with
        | {pong}      -> {Record = [("type", {String = "pong"})]}
        | {break}     -> {Record = [("type", {String = "break"})]}
        | ~{msgs}      -> {Record = [("type", {String = "msgs"}),
                                     ("body", {List = List.rev(msgs)})]}
        | ~{result nb} -> {Record = [("type", {String = "result"}),
                                     ("body", {String = result}),
                                     ("id"  , {Int = nb})]}
      WebInfo.simple_reply(winfo, Json.to_string(json), {success})

    /**
     * Reply a pong to the [pnb]th ping.
     */
    @private
    pong(client, pnb) =
      #<Ifstatic:MLSTATE_PING_DEBUG>
      do debug("PONG({pnb}, {client}) sending")
      #<End>
      match @atomic(
        match Hashtbl.try_find(entries, client) with
        | {some = ~{ajax nb key=_}} ->
          do remove(client)
          if pnb == nb then {~ajax pong}
          else {}
        | _ -> {}
      ) with
      | {~ajax pong} ->
        #<Ifstatic:MLSTATE_PING_DEBUG>
        do debug("PONG({pnb}, {client}) really sending")
        #<End>
        send_response(ajax, {pong})
      | {} -> void

    /**
     * Handles the ping request :
     * - reply stored messages if exists
     * - hangs the request and program a pong else
     */
    @private
    iping(crush, client, nb, winfo) =
      // Should be programmed only in needed case but is not compatible with
      // atomic. Thus we abort on non needed case.
      //
      // Note : They doesn't have (real) good reason for non atomicity because
      // asleep is an atomic cps-bypass. We should have a better @atomic and
      // update the following code.
      apong =
        #<Ifstatic:MLSTATE_PING_DEBUG>
        do debug("PONG({nb}, {client}) is programmed")
        #<End>
        Scheduler.asleep(ping_delay, -> pong(client, nb))
      match @atomic(
        match Hashtbl.try_find(entries, client) with
        | {none} ->
          do add(client, {ajax=winfo ~nb key=apong})
          {}
        | {some = {msgs=[]}} ->
          do replace(client, {ajax=winfo ~nb key=apong})
          {}
        | {some = {msgs=_} as e} ->
          do Scheduler.abort(apong)
          do remove(client)
          e
        | {some = {ajax=owinfo nb=onb key=okey}} ->
          if crush then
            do replace(client, {ajax=winfo ~nb key=apong})
            do Scheduler.abort(okey)
            {winfo=owinfo break}
          else
            do Scheduler.abort(apong)
            {error already=onb}
      ) with
      | {} -> void
      | {error ~already} ->
        do error("PING({nb}, {client}) not registered PING({already}) already present")
        send_response(winfo, {break})
      | {~winfo break} -> send_response(winfo, {break})
      | {msgs=_} as e -> send_response(winfo, e)

    /**
     * As [iping] plus handles pang mecanism.
     */
    ping(crush, client, nb, winfo) =
     #<Ifstatic:MLSTATE_PING_DEBUG>
     do debug("PING({nb}, {client}) is received")
     #<End>
      match @atomic(
        match Hashtbl.try_find(pangtbl, client) with
        | {none}
        | {some = []} -> {}
        | {some = [pang|pangs]} ->
          do Hashtbl.replace(pangtbl, client, pangs)
          do match pangs with
             | [] -> Hashtbl.remove(pangtbl, client)
             | _ -> void
          ~{winfo pang}
      ) with
      | {} -> iping(crush, client, nb, winfo)
      | ~{winfo pang} -> send_response(winfo, @opensums(pang))

    /**
     * Handles a pang request, it's like a synchronous ping.
     */
    pang(client, nb, winfo) =
      #<Ifstatic:MLSTATE_PING_DEBUG>
      do debug("PANG({nb}, {client}) is received")
      #<End>
      ping(true, client, nb, winfo)

    /**
     * Returns the [nb]th pang to the [client]
     */
    return(client, nb, result) =
      #<Ifstatic:MLSTATE_PING_DEBUG>
      do debug("PANG({nb}, {client}) return ")
      #<End>
      match @atomic(
        match Hashtbl.try_find(entries, client) with
        | {some = ~{ajax key ...}} ->
          do remove(client)
          do Scheduler.abort(key)
          ~{ajax}
        | {some = _}
        | {none} ->
          do add_pang(client, nb, result)
          {}
      ) with
      | ~{ajax} -> send_response(ajax, ~{result nb})
      | {} -> void

    send(client, msg:PingRegister.msg) =
      #<Ifstatic:MLSTATE_PING_DEBUG>
      do debug("SEND({client}) message {msg}")
      #<End>
      match @atomic(
        match Hashtbl.try_find(entries, client) with
        | {some = ~{ajax key ...}} ->
          do Hashtbl.remove(entries, client)
          do Scheduler.abort(key)
          ~{ajax}
        | {some = ~{msgs}} ->
          do Hashtbl.replace(entries, client, {msgs = [msg | msgs]})
          {}
        | {none} ->
          do Hashtbl.add(entries, client, {msgs = [msg]})
          {}
      ) with
      | ~{ajax} ->
        #<Ifstatic:MLSTATE_PING_DEBUG>
        do debug("SEND({client}) really")
        #<End>
        send_response(ajax, {msgs = [msg]})
      | {} -> void

  }}

  @private @expand
  `?|>`(a,f) =
    match a with
    | {none} -> @fail
    | {some = x} -> f(x)

  @private
  werror(winfo, msg) =
    do error(msg)
    do WebInfo.simple_reply(winfo,
      #<Ifstatic:MLSTATE_PING_DEBUG>msg#<Else>""#<End>,
      {bad_request}
    )
    Scheduler.stop()

  send(client, msg) =
    do Entry.send(client, msg)
    true

  parser_(winfo) =
    request = winfo.http_request.request
    jbody() =
      json = Json.of_string(%%BslNet.Requestdef.get_request_message_body %%(request))
      do if Option.is_none(json) then werror(winfo, "bad formatted json")
      json
    client(active) =
      client = ThreadContext.Client.get_opt({current}) ? werror(winfo, "No client context : {ThreadContext.get_opt({current})}")
      do ClientEvent.touch(client, active)
      client
    parser
    | "ping" -> jbody() ?|>
      | {Int = nb} ->
        client = client(false)
        do Entry.ping(nb == 1, client, nb, winfo)
        {none}
      | json -> werror(winfo, "bad formatted ping : {json}")
      end

    | "pang" -> jbody() ?|>
      | {Record = [("ping", {Int = nb}),
                   ("uri", {String = uri}),
                   ("body", {String = body}),
                  ]} ->
        client = client(true)
        do Entry.pang(client, nb, winfo)
        (request, cont) = %%BslNet.Requestdef.request_with_cont%%(request, uri, body, Entry.return(client, nb, _))
        {some = {winfo with http_request.request = request ~cont}}
      | {Int = nb} ->
        client = client(false)
        do Entry.pang(client, nb, winfo)
        {none}
      | json -> werror(winfo, "bad formatted json : {json}")
      end

}}

#<End>
