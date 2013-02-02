/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

import stdlib.widgets.loginbox

/**
 * Login component : Provide a generic user login mecanism and keep a
 * state relied to a login status.
 *
 * @author Hugo Heuzard
 * @author Quentin Bourgerie
 * @destination public
 * @stability stable
 */

/**
 * {1 About this module}
 * The login component is used for create a strong login system. This
 * system is based onto cookie (via UserContext) and then it be able
 * to keep a durable state.
 *
 * {1 Type variables meaning}
 * Meaning of type variable using in this login component.
 * - ['login_data] : corresponding to data used for authenticate a
 *   user. Example : [(string, string)] for a (name, password) couple.
 * - ['state] : corresponding to the durable state keeped on server
 *   side.
 * - ['credential] : corresponding to a ['state] mapping can be
 *   manipulate by the client.
 *
 * {1 How to start}
 * For all user create one login component, using [CLogin.make]. You can use
 * the [default_config] for a simple initialisation. Then when you
 * have a page that you want protect with a login, use [CLogin.html]
 * for generate a view of login component.
 * If you want change state of user login you can use
 * [CLogin.do_change].
 */

/**
 * {1 Type defined in this module}
 */
/**
 * Type of a function that be able to create a "loginbox" from a
 * ['credential] value.
 * @param dochange A callback that should be call when user try to
 * logged.
 * @param credential A value that should allows to decide what xhtml
 * should be generated.
 */
type CLogin.loginbox('login_data, 'credential) = ('login_data -> void), 'credential -> xhtml

/**
 * Type of configuration of a login component.
 * @see {1 Type variables meaning}
 */
type CLogin.config('login_data, 'state, 'credential) = {

  /** A function that decide if the user should be
      logged. [some(state)] means user is logged and the returned
      state was stored on server side. [none] means user becomes
      unlogged and data stored by login components was removed.
      @note Morally this function should be only on server side. */
  authenticate : 'login_data, 'state -> option('state)

  /** A function used for map the login ['state] to a ['credential]
      state that be passed to client side.
      @note Morally this function should be only on server side. */
  get_credential : 'state -> 'credential

  /** A function that be able to create a "loginbox". */
  loginbox : CLogin.loginbox('login_data, 'credential)

  /** A callback that be call when the state change. */
  on_change : ('login_data -> void), 'credential -> void

  /** */
  prelude : option(-> option('login_data))

}

/**
 * Type of a login component.
 * @see {1 Type variables meaning}
 */
@abstract type CLogin.t('login_data, 'state, 'credential) = {
  config : CLogin.config('login_data, 'state,'credential)
  state  : UserContext.t('state)
  defaultc : 'credential //default credential
}

/**
 * The main module.
 */
CLogin = {{

  /**
   * {1 Creating login component}
   */
  /**
   * Create a login component from a default [state] and a [config].
   * The default [state] it's used when user is not logged.
   */
  make(state, config : CLogin.config) : CLogin.t = {
      ~config
      state = UserContext.make(state)
      defaultc = config.get_credential(state)
    }

  /**
   * Generate the html loginbox. i.e generate an user view of login
   * component.
   */
  html(login : CLogin.t) : xhtml =
    do match login.config.prelude
       | {none} -> void
       | {some=prelude} ->
         match prelude()
         | {none} -> void
         | {some = ld} -> do_change(login, ld)
    login.config.loginbox(do_change(login, _), get_credential(login))

  /**
   * {1 Some configurations}
   */

  @private default_config_builder(id, authenticate, https_host) : CLogin.config(option((string, string)), option('a), option('a)) =
    logout(dochange) = <a onclick={_ -> do (dochange(none):void)
                              WLoginbox.set_logged_out(id, <></>)}>Logout</a>
    get_credential(x) = x
    loginbox(dochange, credential) =
      box_config = { WLoginbox.default_config with ~https_host } : WLoginbox.config
      make_html = WLoginbox.html(box_config, id, (n, p -> dochange(some((n,p)))), _)
      match credential
      | {none} -> make_html(none)
      | {some=_credential} -> make_html(some(logout(dochange)))
    on_change(dochange, credential) =
      match credential
      | {some=_} -> WLoginbox.set_logged_in(id, logout(dochange))
      | _ -> WLoginbox.set_logged_out(id, <>Invalid password</>)
    prelude = none
    { ~authenticate ~get_credential ~loginbox ~on_change ~prelude }

  /**
   * The default configuration using widget login box (@see
   * WLoginbox). Data used for logged ([`login_data]) is the [(name,
   * password)] couple. The state is an option [none] means not logged
   * and some means logged with a custom encapsulated state. [`state]
   * is strictely equals to [`credential] for this version
   * ([get_credential = identity]).
   *
   * If you use this [default_config] you should certainly overwrite
   * [loginbox] and [on_change] fields. Indeed that configuration just
   * switch beetween two views :
   * - when not logged : 2 inputs (name, password) + 1 log button
   * - when logged : 1 unlog button
   */
  default_config(id, authenticate) : CLogin.config(option((string, string)), option('a), option('a)) =
    default_config_builder(id, authenticate, none)

  /**
   * The default configuration using widget login box (@see
   * WLoginbox). Data used for logged ([`login_data]) is the [(name,
   * password)] couple. The state is an option [none] means not logged
   * and some means logged with a custom encapsulated state. [`state]
   * is strictely equals to [`credential] for this version
   * ([get_credential = identity]).
   * In this version the POST is made through HTTPS
   *
   * If you use this [default_config] you should certainly overwrite
   * [loginbox] and [on_change] fields. Indeed that configuration just
   * switch beetween two views :
   * - when not logged : 2 inputs (name, password) + 1 log button
   * - when logged : 1 unlog button
   */
  default_config_ssl_post(id, authenticate, https_addr) : CLogin.config(option((string, string)), option('a), option('a)) =
    default_config_builder(id, authenticate, some(https_addr))

  /**
   * {1 Actions on login component}
   */
  /**
   * Do a login change. Change the state of login component, usefull
   * for generate by example a logout button, etc.
   */
  do_change(login : CLogin.t('a, 'b ,'c), login_data:'a) =
    change(state) =
      r = login.config.authenticate(login_data, state)
      c = Option.switch(login.config.get_credential, login.defaultc, r)
      do Scheduler.push(-> login.config.on_change(do_change(login, _), c))
      r
    UserContext.change_or_destroy(change, login.state)

  /**
   * Get the credential value from the login component.
   */
  @server
  get_credential(login:CLogin.t('a,'b,'c)):'c =
    UserContext.execute(login.config.get_credential, login.state)

  set_state(login:CLogin.t, state) =
    UserContext.change((_x -> state), login.state)

  get_state(login:CLogin.t) =
    UserContext.execute(identity, login.state)
  /**
   * Create a new component that share everything with the old one
   * unless his xhtml view.
   */
  change_box(login:CLogin.t, ~{loginbox on_change}) =
    {login with config = { login.config with ~loginbox ~on_change }} : CLogin.t

}}
