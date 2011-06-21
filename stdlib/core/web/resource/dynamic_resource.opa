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

import stdlib.core.{fresh, qos}

/**
 * Management of dynamic resources
 *
 * @category WEB
 * @author Mathieu Barbin, David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability UNTESTED
**/

/*
  Version using QOS resource system for collecting dynamic resources associations
*/

/**
 * {1 About this module}
 *
 * This module is meant to manipulate and publish dynamic and volatile resources.
 *
 * {1 Where do I start?}
 *
 * To publish a dynamic resource, you need to call the function {!DynamicResource.publish}
 * and specify some parameters about the details of publication mode. (expiration date,
 * number of consumption, clients visibility, etc.)
 *
 * {1 What if I need more?}
 *
 * If you need to customize the accessibility policy of a resource, you can use
 * the function custom_publish, and provide your own consumation function, e.g. sending
 * message to some external sessions, etc.
**/

/**
 * The default parameters for configuring the dynamic publication of a resource.
**/
type DynamicResource.parameters = {
  expiration : DynamicResource.parameters.expiration ;
  consumption : DynamicResource.parameters.consumption ;
  visibility : DynamicResource.parameters.visibility ;
}

/**
 * Expiration of a publication. Can be a timeout (relative)
 * or a absolute expiration date.
 * In case of a timeout, you have the choice between a timeout starting
 * from the creation of the resource (timeout) or a timeout starting
 * from the first time the resource is consumed (grace_delay)
**/
type DynamicResource.parameters.expiration =
   { grace_delay : Duration.duration }
 / { timeout : Duration.duration }
 / { expiration : Date.date }
 / { none } //TODO: Map this to a URI defined with the server UID

/**
 * Number of consumption allowed.
**/
type DynamicResource.parameters.consumption =
   { limited : int }
 / { unlimited }

/**
 * Visibility of a resource. Can be shared between all clients,
 * or visible only by a specific client, from a thread_context.
**/
type DynamicResource.parameters.visibility =
   { current_context }
 / { thread_context : ThreadContext.t }
 / { shared }

/**
 * The state, traduced from standard parameters, and updated
 * as long as the resource gets consumed.
 * @private
**/
type DynamicResource.parameters.state = {

  /**
   * Absolute expiration
  **/
  expiration : DynamicResource.parameters.expiration ;

  consumption : DynamicResource.parameters.consumption ;

  /**
   * The field 'client' of the thread context
  **/
  visibility : option(string) ;
}

/**
 * Url configuration, can be optional (see {!DynamicResource.publish_extend})
 */
type DynamicResource.config = {
  /** Means corresponding generated url will be optionaly prefixed. */
  prefix : option(string)

  /** Means corresponding generated url will be optionaly sufixed. */
  sufix : option(string)

  /** A callback that be called at each url access. */
  onaccess : option(-> void)
}

/**
 * @private
**/
type DynamicResource.manager = ResourceTracker.manager(ThreadContext.t, bool)

/**
 * @private
**/
type DynamicResource.resource = {
  resource : resource
  manager : DynamicResource.manager
}

/**
 * @private
**/
type DynamicResource.key = string

/**
 * @private
**/
type DynamicResource.message =
   { add : DynamicResource.key ; dynresource : DynamicResource.resource }
 / { get : DynamicResource.key }
 / { remove : DynamicResource.key }

@server DynamicResource = {{

  /**
   * Interaction with the global state of the dynamic resources.
  **/
  @private on_message(~{map default}, message) =
    match message with
    | { get = key } ->
      //do jlog("GET key:{key}")
      return =
        match StringMap.get(key, map) with
        | { some = ~{ resource manager } } ->
          // !! Warning, if the semantic of cellule changes, it wont work anymore
          // currently, cell are using dynamic session, so this context is the context
          // of the sender. If this propertie change, replace the cellule by a dynamic cellule,
          // or provide the context of the sender via the message
          if ResourceTracker.call(manager, ThreadContext.get({current}))
          then some(resource)
          else
            do Log.error("DynamicUrl[{key}]", "Access denied")
            some(default({access_denied}))
        | { none } ->
          do Log.error("DynamicUrl[{key}]", "Not found")
          some(default({not_found}))
        end
      instruction = {unchanged}
      ~{ return instruction }

    | { add = key ; ~dynresource } ->
      //do jlog("ADD key:{key}")
      map = StringMap.add(key, dynresource, map)
      instruction = { set = ~{map default} }
      { return = none ; ~instruction }

    | { remove = key } ->
      //do jlog("REMOVE key:{key}")
      map = StringMap.remove(key, map)
      instruction = { set = ~{map default} }
      { return = none ; ~instruction }
    | ~{ default } ->
      instruction = { set = ~{map default} }
      { return = none ; ~instruction }

  @private resourceCell =
    state = {map = StringMap.empty : stringmap(DynamicResource.resource);
             default = _ -> Resource.default_error_page({wrong_address})}
    // CF remark about thread context, we may want to use a dynamic cellule instead.
    Cell.make(state, on_message)

  @private find_resource(key) =
    Option.get(Cell.call(resourceCell, { get = key }))

  @private next_key =
    fresh = Fresh.server(i->i)
    ((prefix:string), (sufix:string) ->
      id = fresh()
      random = Random.string(32)
      "{prefix}{id}_{random}{sufix}"
    )

  @private dynamic_resource_path = "dynamic_resource"

  @private url_of_key(key) = "/_internal_/{dynamic_resource_path}/{key}"

  set_default(default) = ignore(Cell.call(resourceCell, ~{default}))

  /**
   * Publishing a dynamic resource with a customized consumption management.
   *
   * @param namespace An optional namespace used to organize resources.
  **/
  custom_publish(
    resource : resource,
    config : DynamicResource.config,
    state : 'state,
    access : ('state -> ('state, bool)),
    expire : ('state -> bool)
    ) =
    access = Option.switch((onaccess -> (state -> do onaccess() access(state))),
                           access, config.onaccess)
    on_message(state, context) = @with_thread_context(context, access(state))
    key = next_key(match config.prefix with {none} -> "" | ~{some} -> "{some}/",
                   match config.sufix with {none} -> "" | ~{some} -> "/{some}")
    expire(state) = if expire(state) then some(ResourceTracker.Signal.EXPIRATION) else none
    collect(_, _) =
      _ = Cell.call(resourceCell, { remove = key })
      void
    manager = ResourceTracker.create(state, on_message, expire, collect)
    dynresource = ~{ resource manager }
    _ = Cell.call(resourceCell, { add = key ; ~dynresource })
    url = url_of_key(key)
    url

  @private get_visibility(thread_context) =
    match thread_context.key with
    | { client = { ~client ; page = _ } } -> some(client)
    | _ -> none

  /**
   * Transforming parameters into a parameters_state (publish).
  **/
  @private state_of_parameters(parameters: DynamicResource.parameters) =
    match parameters : DynamicResource.parameters with
    | ~{ expiration consumption visibility } ->
      expiration =
        match expiration with
        | { ~grace_delay } -> ~{ grace_delay }
        | { ~timeout } ->
          now = Date.now()
          expiration = Date.advance(now, timeout)
          ~{ expiration }
        | { ~expiration } -> ~{ expiration }
        | { none } -> { none }
      visibility =
        match visibility with
        | { current_context } -> get_visibility(thread_context())
        | { thread_context = context } -> get_visibility(context)
        | { shared } -> none
      ~{ expiration consumption visibility } : DynamicResource.parameters.state

  /**
   * The standard expire of resources (argument of custom_publish).
  **/
  @private expire_parameters_state(parameters_state) : bool =
    match parameters_state : DynamicResource.parameters.state with
    | { ~expiration ~consumption visibility = _ } ->
      (
        match expiration with
        | { ~expiration } -> Date.compare(Date.now(), expiration) == {gt}
        | _ -> false
      )
      || (
        match consumption with
        | { limited = i } -> i <= 0
        | { unlimited } -> false
      )

  /**
   * The standard access of resources (argument of custom_publish).
  **/
  @private access_parameters_state(parameters_state) : (DynamicResource.parameters.state, bool) =
    match parameters_state : DynamicResource.parameters.state with
    | ~{ expiration consumption visibility } ->
      if (
        match expiration with
        | { ~expiration } -> Date.compare(Date.now(), expiration) == {lt}
        | _ -> true
      )
      && (
        match consumption with
        | { limited = i } -> i > 0
        | { unlimited } -> true
      )
      && (
        match visibility with
        | { none } -> true
        | { some = filter_client } ->
          do jlog("filter_client of resource is {filter_client}")
          match thread_context().key with
          | { client = { ~client ; page = _ } } ->
            do jlog("accessor of client is {client}")
            String.equals(client, filter_client)
          | _ ->
            do jlog("no client context there !!")
            false
      )
      then (
        parameters_state =
          expiration =
            match expiration with
            | { ~grace_delay } ->
              now = Date.now()
              expiration = Date.advance(now, grace_delay)
              ~{ expiration }
            | _ -> expiration
            end
          consumption =
            match consumption with
            | { limited = i } -> { limited = pred(i) }
            | _ -> consumption
            end
          ~{ parameters_state with expiration consumption }
        (parameters_state, true)
      )
      else
        (parameters_state, false)
    end

  /**
   * The standard collectable of resources (publish).
  **/
  @private collectable_parameters_state(_parameters_state) = false

  @private get_executable_id    = %% BslInit.get_executable_id %%: -> string
  @private executable_path      = "{get_executable_id()}/"
  @private some_executable_path = {some = executable_path}

  /**
   * A default url configuration.
   */
  default_config : DynamicResource.config = {
    prefix = none
    sufix = none
    onaccess = none
  }

  /**
   * Publishing a dynamic resource with the standard parameters.
  **/
  publish(resource, parameters: DynamicResource.parameters) =
    publish_extend(resource, parameters, default_config)

  /**
   * Like {!DynamicResource.publish} but you can provide more configuration.
   * see type {!DynamicResource.config}
   */
  publish_extend(resource, parameters, config) =
    parameters_state = state_of_parameters(parameters)
    custom_publish(resource, config, parameters_state, access_parameters_state, expire_parameters_state)

  /**
   * The parser for dynamic resources.
   * Automatically added to internal part of the url dispatcher.
   * @see server_private.opa
   * Not for casual user, used internally only. (in a different package)
  **/
  parser_() =
    parser
      | {Rule.of_string(dynamic_resource_path)} "/" t=(.*) ->
        key = Text.to_string(t)
        find_resource(key)
}}
