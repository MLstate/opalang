/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.core.{rpc.core, map, web.core}


/**
 * Manipulating UserContext.
 *
 * This file provides the primitives for defining and modifying user_context.
 *
 * @author David Rajchenbach-Teller, 2009-2010
 * @author Hugo Heuzard
 * @destination public
 * @stability stable
 */

/**
 * {1 About this module}
 *
 * This module provides the necessary functions to define and interact with UserContext.
 * UserContext are the high-level mechanism used to associate values to each client (based on cookie)
 *
 * User's data stored in the UserContext can only be accessed and modified by the user owning this
 * data (based on thread context)
 *
 * {1 Where do I start?}
 *
 * For most of your needs, you only need to use functions [UserContext.make] to create a UserContext
 * with a default value, [UserContext.change] to change the current state of the UserContext for this
 * user and [UserContext.remove] to remove the current value.
 *
 * {1 What if I need more?}
 */


/**
 * {1 Types defined in this module}
 */


@private
type UserContext.messages('state) = {exec: 'state -> (-> void)}
                                  / {set: 'state -> option('state)}
                                  / {remove: void}
                                  / {set_default: 'state}
@private
type UserContext.private.message('state) = {exec: 'state -> (-> void)}
                                         / {set: 'state -> option('state)}
/**
 * The type of UserContext parametrized by the type of value stored
 */
@abstract
type UserContext.t('state) =
    Cell.cell(UserContext.messages('state),
              {none} / {some: -> void} / {error:string})
@private
type UserContext.private.t('state) = Cell.cell(UserContext.private.message('state), option( ->void))

/**
 * {1 Interface}
 */

UserContext =
{{

  @private  UserContextMap = StringMap

  /**
   * {2 Creating UserContext}
   */

  /**
   * [make(default)] Create an empty context from a default value
   *
   * @param default the default value of the UserContext
   *
   * @return A UserContext
   */
  @server
  make_generic(get_key: -> option(string), default : 'state) =
    init = (UserContextMap.empty: stringmap(UserContext.private.t('state)),default)
    rec val ctx = Cell.make(init, aux): UserContext.t('state)
    and aux2(state,msg) = match msg with
      | {~exec} -> {return=some(exec(state)) instruction={unchanged}}
      | {~set} -> match set(state) with
        | {~some} -> {return=none instruction={set=some}}
        | {none} ->
          do remove(ctx)
          {return=none instruction={stop}}
        end
    and aux((state,default), msg) =
      get_info(client, state) =
        match UserContextMap.get(client, state) with
          | {some=c} -> (c,false)
          | {none} -> (Cell.make(default,aux2),true)
      match get_key() with
      | {none} -> {return={error="UserContext cannot get key"} instruction={unchanged}}
      | {some = client} ->
      match msg with
      | {~exec} ->
        (c,new) = get_info(client, state)
        instruction =
          if new
          then {set=(UserContextMap.add(client,c,state),default)}
          else {unchanged}
        {return={some = -> Cell.call(c, {~exec}) |> Option.get(_)()}
            ~instruction}
      | {~set} ->
        (c,new) = get_info(client, state)
        instruction =
          if new
          then {set=(UserContextMap.add(client,c,state),default)}
          else {unchanged}
        f() = ignore(Cell.call(c, {~set}))
        {return={some = f} ~instruction}
      | {remove} ->
        {return={none}
         instruction={set = (UserContextMap.remove(client, state),default)}}
      | {~set_default} ->
        {return={none}
         instruction={set=(state,set_default)}}
    ctx

  make(default:'state) =
    client() = match thread_context().key with
      | {nothing} | {server=_server} ->
        do Log.warning("UserContext","Cannot identify user. This execution is not attached to any user")
        none
      | {~client} -> some(client.client)
    make_generic(client, default)

  @private
  call(context, msg) =
    match Cell.call(context, msg)
    | ~{error} -> @fail(error)
    | {none} as e | {some = _} as e -> e : option

  /**
   * [change(f, context)] Change the state of a UserContext
   *
   * @param f a function that take the old value associated with a user as parameter and return the new value
   * @param context a UserContext
   *
   * @return void
   */
  @server
  change(change : ('state -> 'state), context : UserContext.t('state)) : void =
    (call(context, {set=(x -> some(change(x)))}) |> Option.get(_))()

  /**
   * [change_or_destroy(f, context)] Like [change(f, context)] but [f]
   * returns a [option('state)]. If returned value equals to [none]
   * state is removed.
   */
  @server
  change_or_destroy(change : ('state -> option('state)), context : UserContext.t('state)) =
    (call(context, {set=change}) |> Option.get(_))()

  /**
   * [remove(context)] Remove the state associated with a user from a UserContext (the default value will be used instead)
   *
   * @param context a UserContext
   *
   * @return void
   */
  @server
  remove(context : UserContext.t('state)) : void =
    ignore(call(context, {remove}: UserContext.messages('state)))

  /**
   * [execute(f, context)] Return a value computed from the state of the UserContext
   * the f function is executed inside the UserContext to prevent concurrency issue.
   *
   * @param  f a function that take the value associated with a user as parameter and return a new value
   * this does not change the state of UserContext
   * @param context UserContext
   *
   * @return a value computed by f
   */
  @server
  execute(action : 'state -> 'b, context : UserContext.t('state)) : 'b =
    k(cont)=
      f(state:'state) : ( -> void) =
        return = action(state)
        -> Continuation.return(cont, return)
      (call(context, {exec=f}) |> Option.get(_))()
    @callcc(k)


  /**
   * [get(context)] returns the state associated with [context] (for the current
   * user).
   */
  get(context : UserContext.t('state)) : 'state =
    execute(identity, context)

  /**
   * [set(context, value)] sets the state associated with [context] (for the
   * current user) to [value].
   * It is a convenience function that can be used instead of [change] when
   * setting new state without the need to know the previous one.
   */
  set(context : UserContext.t('state), v : 'state) =
    change(_ -> v, context)

  /**
   * [set_default(default, context)] Set the new default value for the user context.
   *
   * @param  default the new default value
   * @param context UserContext
   *
   * @return void
   */
  @server
  set_default(default : 'state, context :  UserContext.t('state)) : void =
    ignore(call(context, {set_default=default}))

}}
