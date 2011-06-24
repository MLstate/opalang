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

import stdlib.core.{rpc.core, map, db, web.core}


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
/**
 * The type of UserContext parametrized by the type of value stored
 */
@abstract
type UserContext.t('state) = Cell.cell(UserContext.messages('state), option( ->void))


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
  make(default : 'state) =
    init = (UserContextMap.empty: stringmap('state),default)
    aux((state,default), msg) =
      get_info(client, state) =
        Option.default(default, UserContextMap.get(client, state))
      client = match thread_context().key with
        | {nothing} | {server=_server} -> do Log.warning("UserContext","The thread context is not client") ""
        | {~client} -> client.client
      match msg with
      | {~exec} ->
        {return=some(exec(get_info(client, state)))
            instruction={unchanged}}
      | {~set} ->
        new_info = set(get_info(client, state))
        match new_info
        | ~{some} ->
          {return=none
           instruction={set = (UserContextMap.add(client, some, state),default)}}
        | {none} ->
          {return=none
           instruction={set = (UserContextMap.remove(client, state),default)}}
        end
      | {remove} ->
        {return=none
         instruction={set = (UserContextMap.remove(client, state),default)}}
      | {~set_default} ->
        {return=none
         instruction={set=(state,set_default)}}
    Cell.make(init, aux): UserContext.t('state)

  /**
   * [make(default, path)] Create an empty context from a default value and a path
   * This UserContext will be stored in the database.
   *
   * @param default the default value of the UserContext
   * @param path the database path for storing the UserContext
   *
   * @return A UserContext
   */
  @server
  make_stored(default: 'state, path: ref_path(stringmap('state))) =
    aux(default, msg)=
      get_info(client) =
        UserContextMap.get(client, Db.read(path)) ? default
      client = match thread_context().key with
        | {nothing} | {server=_server} -> ""
        | {~client} -> client.client
      match msg with
      | {~exec} ->
        {return=some(exec(get_info(client)))
            instruction={unchanged}}
      | {~set} ->
        do match set(get_info(client))
        | {some=info} ->
          Db.write(path, UserContextMap.add(client, info, Db.read(path)))
        | {none} ->
          Db.write(path, UserContextMap.remove(client, Db.read(path)))
        end
        {return=none instruction={unchanged}}
      | {remove} ->
        do Db.write(path, UserContextMap.remove(client, Db.read(path)))
        {return=none instruction={unchanged}}
      | {~set_default} -> {return=none instruction={set=set_default}}
    Cell.make(default, aux):  UserContext.t('state)

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
    ignore(Cell.call(context, {set=(x -> some(change(x)))}))

  /**
   * [change_or_destroy(f, context)] Like [change(f, context)] but [f]
   * returns a [option('state)]. If returned value equals to [none]
   * state is removed.
   */
  @server
  change_or_destroy(change : ('state -> option('state)), context : UserContext.t('state)) =
    ignore(Cell.call(context, {set=change}))

  /**
   * [remove(context)] Remove the state associated with a user from a UserContext (the default value will be used instead)
   *
   * @param context a UserContext
   *
   * @return void
   */
  @server
  remove(context : UserContext.t('state)) : void =
    ignore(Cell.call(context, {remove}: UserContext.messages('state)))

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
    k(cont)= @with_thread_context(ThreadContext.get({from=cont}),
      f(state:'state) : ( -> void) =
        return = action(state)
        -> Continuation.return(cont, return)
      (Cell.call(context, {exec=f}) |> Option.get(_))()
    )
    @callcc(k)


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
    ignore(Cell.call(context, {set_default=default}))

}}
