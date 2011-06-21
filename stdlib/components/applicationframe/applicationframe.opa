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

import stdlib.web.client
import stdlib.widgets.{core,loginbox}

/**
 * A generic application frame with login management and and optional search
 * box.
 *
 * A session storing logged in users is created server-side, and a
 * client-side one is created for each connected user.
 *
 * @author Guillem Rieu
 * @author Hugo Venturini
 *
 */

// TODO: documentation, refactoring and cleaning

/**
 * The type of an cappframe configuration. This record enables you to customize
 * the behaviour and some properties of the application.
 */
type CApplicationFrame.config('a, 'b) = {
  /**
   * Styling settings
   **/
  header_style: WStyler.styler /** The style of the login frame */
  login_style: WStyler.styler /** The style of the login box */

  /**
   * Authentication checking function returning {none} if user data and
   * secret match, {some=reason} if they don't.
   **/
  authenticate: string, string -> option(string)

  /**
   * Functions building the public and private pages. They both take the page
   * title as first argument, and the private one is additionaly given info
   * about the logged in user (the username, in a basic case.
   **/
  public_page: string -> xhtml
  public_head: string -> xhtml
  private_page: string, 'a -> xhtml

  /**
   * Message to display in place of the login form while logged in. The first
   * argument is user information, and the second is the function to call in
   * order to logout.
   **/
  login_message: (-> void), 'a -> xhtml

  /**
   * A XHTML chunk to add in the top-left bar. It can be used to add a search
   * box , for example.
   **/
  topbar: xhtml

  /**
   * The following settings enable you to bind callback functions to some
   * login-related events.
   **/

  /** Called client-side when login is successful.
   * Returned value is stored in the client-side session. */
  on_login_client: 'a, xhtml -> 'b
  /** Called client-side when login has failed */
  on_failure_client: 'a, string -> void
  /** Called client-side when logging out */
  on_logout_client: 'a, 'b -> void
  /** Called client-side when changing page while logged in */
  on_change_page_client: 'a -> 'b

  /** Called server-side when login is successful */
  on_login_server: string -> 'a
  /** Called server-side when login has failed */
  on_failure_server: string, string -> ('a, string)
  /** Called server-side when logging out */
  on_logout_server: string -> 'a
  /** Called server-side when changing page while logged in */
  on_change_page_server: 'a -> 'a

  /** Search box function call */
  search_callback : option(string -> void)

  /** Returns a string representation of a user context */
  str_of_user: 'a -> string
}

/** User identifier. For now it's a cookie. */
type CApplicationFrame.user_id = user_id

/** Session messages handled by the server (and sent by clients) */
type CApplicationFrame.server_message('a) =
    { try_login: (string, CApplicationFrame.user_id, channel(CApplicationFrame.client_message('a)), (string, string))}
  / { try_logout : CApplicationFrame.user_id}
  / { change_page : (CApplicationFrame.user_id, channel(CApplicationFrame.client_message('a))) }

/** Session messages handled by the client (and sent by the server) */
type CApplicationFrame.client_message('a) =
    { login_success: ('a, xhtml) }
  / { login_failure: ('a, string) }
  / { logout_success: 'a }


/**
 * The [CApplicationFrame] module itself.
 **/
CApplicationFrame =
{{

  /**
   * {1 High-level interface}
   */

  /**
   * A default configuration of the login frame.
   *
   * By default, no action is taken on login-related events, and all login
   * attempts fail. Thus you can use these settings as a starting point, but
   * should at least define a meaningful [authenticate] function for a useful
   * behaviour. For example:
   *
   * my_login_config = { CApplicationFrame.default_config with
   *   authenticate = user, password ->
   *     if user == "admin" && password == "MyPassword" then none
   *     else some("Sorry, you're not admin.")
   * }
   *
   * or, more interesting, with a DB storing of users:
   *
   * check_user_db(usr, pwd) =
   *   match ?/users[usr] with
   *     | {none} -> some("Unkown user")
   *     | {~some} ->
   *       if some.password == pwd then none
   *       else some("Wrong password")
   *
   * my_db_login_config = { CApplicationFrame.default_config with
   *   authenticate = check_user_db
   * }
   **/
  default_config : CApplicationFrame.config(string, void) = {

    header_style = WStyler.make_class([prefixed_header_id])
    login_style = WStyler.make_class([ prefixed_login_box_id])

    authenticate = _:string, _:string -> some("No authentication policy implemented")

    public_page = _:string -> <>Please log in!</>
    public_head = _:string -> <></>
    private_page = _:string, _:string -> <>You are logged in!</>

    login_message(on_logout: (-> void), username: string): xhtml =
      <>
        <strong>{username}</strong>
        [<a onclick={_ -> on_logout()}>Logout</a>]
      </>

    topbar : xhtml =
      <span id={prefixed_status_id}>
      </span>

    on_login_client = _, private_page ->
        Dom.transform([#{"{prefix_id}_status"} <- <></>, #capplicationframe_content <- private_page])
    on_failure_client = _, reason ->
         Dom.transform([#{"{prefix_id}_status"} <- <span style={css {color: red;}}>{reason}</span>])
    on_logout_client = _, _ -> Client.goto("/")
    on_change_page_client = _ -> void

    on_login_server = identity
    on_failure_server = f1, f2 -> (f1, f2)
    on_logout_server = identity
    on_change_page_server = identity

    search_callback = {none}

    str_of_user = identity: string -> string
  }

  /**
   * Initialization function to be called once at the server launch.
   **/
  @server
  init(config: CApplicationFrame.config('a, 'b))
  : (UserContext.t(option((channel(CApplicationFrame.client_message('a)), 'a))), channel(CApplicationFrame.server_message('a))) =
    authentications = UserContext.make(none: option((channel(CApplicationFrame.client_message('a)), 'a)))
    server_session =
      receive_server_message =
        CApplicationFrame.server_message_handler(config, authentications, _, _)
      Session.make_dynamic(void, receive_server_message)
    (authentications, server_session)

  /**
   * The main page creation function.
   **/
  make((authentications: UserContext.t(option((channel(CApplicationFrame.client_message('a)), 'a))),
      server_session: channel(CApplicationFrame.server_message('a))),
      config: CApplicationFrame.config('a, 'b), path: string) =

    common_page(page_fun, httpr: HttpRequest.request, usr_opt: option((channel(CApplicationFrame.client_message('a)), 'a)))
    : resource =
      usr_id = HttpRequest.Generic.get_user(httpr) ? error("cookie needed to login")
      Resource.full_page(path, CApplicationFrame.login_frame(config, server_session, path, usr_id, usr_opt, page_fun(path)), config.public_head(path), {success}, [])

    public_page(id, auth) = common_page(config.public_page, id, auth)
    private_page(id, authentified) = common_page(config.private_page(_, Option.get(authentified).f2), id, authentified)

    Server.protect(authentications,
      (id, authentified -> match authentified with
        | {none} -> some(public_page(id, authentified))
        | {some=_} -> none),
      (id, authentified -> private_page(id, authentified)))

  /* Receive and process messages sent by the client (server-side) */
  server_message_handler(config : CApplicationFrame.config('a, 'b),
                         authentications : UserContext.t(option((channel(CApplicationFrame.client_message('a)), 'a))),
                         _,
                         msg : CApplicationFrame.server_message('a)) =
    match msg with
    | {try_login=(path, _user_ident, user_channel,
        (user_data, user_secret))} ->
      match config.authenticate(user_data, user_secret) with
        | {none} ->
          ret_value = config.on_login_server(user_data)
          do UserContext.change((_ -> some((user_channel, ret_value))), authentications)
          page = config.private_page(path, ret_value)
          do Session.send(user_channel, {login_success=(ret_value, page)})
          {unchanged}
        | {some=reason} ->
          ret_value = config.on_failure_server(user_data, reason)
          do Session.send(user_channel, {login_failure=ret_value})
          {unchanged}
      end
    | {try_logout=_user_ident} ->
      do_logout(context_opt) =
        match context_opt with
          | {none} -> void
          | {some=(user_channel, user_data)} -> ret_value = config.on_logout_server(user_data)
                                                Session.send(user_channel, {logout_success=ret_value})
      do UserContext.execute(do_logout, authentications)
      do UserContext.remove(authentications)
      {unchanged}
    | {change_page=(_user_ident, new_channel)} ->
      do UserContext.change((usr_opt ->
          Option.map((_, usr_value) ->
            ret_value = config.on_change_page_server(usr_value)
            (new_channel, ret_value), usr_opt)),
        authentications)
      {unchanged}

  login_frame(config: CApplicationFrame.config('a, 'b),
              server_session: channel(CApplicationFrame.server_message('a)),
              path: string,
              usr_id: user_id,
              usr_opt: option((channel(CApplicationFrame.client_message('a)), 'a)),
              content: xhtml)
  : xhtml =
    <div id="{prefix_id}_main" onready={_ ->
          // FIXME: prefix login box and content container ID
          Dom.transform([#login_box_container <- login_box(config, server_session,
              path, usr_id, usr_opt)])
        }>
      {<div id="{prefix_id}_header">
        <a id="{prefix_id}_logo"></a>
        <div id="login_box_container">
        </div>
        <div id="{prefix_id}_topbar"
            style={css {position: absolute; float: left; display:inline;
                width: 100%;}}>
         {config.topbar}
        </div>
      </div>
        |> WStyler.set(config.header_style, _)}
      <div id="capplicationframe_content">{content}</div>
      <div id="{prefix_id}_toolbar">
        <div id="{prefix_id}_status"></div>
      </div>
    </div>

  /**
   * {1 Private functions aimed at internal use}
   *
   * Do not use them outside of the module.
   */

  @private prefix_id = "capplicationframe"
  @private prefixed_status_id = prefix_id ^ "_status"
  @private prefixed_header_id = prefix_id ^ "_header"
  @private prefixed_login_box_id = prefix_id ^ "_login_box"

  /**
   * Receive and process messages sent by the server (client side)
   */
  @private
  client_message_handler(config: CApplicationFrame.config('a, 'b),
                         logout_action,
                         client_context_opt,
                         msg: CApplicationFrame.client_message('a)) =
    match msg with
    | {login_success=(user_data, private_page)} ->
      do config.login_message(logout_action, user_data)
        |> WLoginbox.set_logged_in(prefixed_login_box_id, _)
      {set=some(config.on_login_client(user_data, private_page))}
    | {login_failure=(user_data, reason)} ->
      do config.on_failure_client(user_data, reason)
      {set=none}
    | {logout_success=user_data} ->
      do WLoginbox.set_logged_out(prefixed_login_box_id, <></>)
      do Option.switch(client_context -> config.on_logout_client(user_data,
          client_context), void, client_context_opt)
      {set=none}

  @private
  protect_interface( authentications: UserContext.t(option((channel(CApplicationFrame.client_message('a)), 'a))),
                     usr_id: user_id,
                     public_callback: user_id -> void,
                     private_callback: (user_id, string -> void)) =
    protect_exec(user_opt) =
        match user_opt with
        | {none} -> public_callback(usr_id)
        | {some=(_, usr_data)} -> private_callback(usr_id, usr_data)
    UserContext.execute(protect_exec, authentications)

  @private @client
  client_session(config: CApplicationFrame.config('a, 'b),
                 server_ssn,
                 usr_id, init_opt)
  : channel(CApplicationFrame.client_message('a)) =
    receive_client_message =
      client_message_handler(config, logout_action(server_ssn, usr_id), _, _)
    ssn = session(init_opt, receive_client_message)
    ssn

  @private
  login_action(server_session: channel(CApplicationFrame.server_message('a)),
               client_session: channel(CApplicationFrame.client_message('a)),
               path: string, usr_id: user_id, usr: string, pwd: string) =
    Session.send(server_session, {try_login=(path, usr_id, client_session, (usr, pwd))})

  @private
  logout_action(server_session: channel(CApplicationFrame.server_message('a)), usr_id: user_id)() =
    Session.send(server_session, {try_logout=usr_id})

  @private
  login_box(config: CApplicationFrame.config('a, 'b),
            server_ssn: channel(CApplicationFrame.server_message('a)),
            path: string,
            usr_id: user_id,
            usr_opt: option((channel(CApplicationFrame.client_message('a)), 'a)))
  : xhtml =
    loginbox_config = {WLoginbox.default_config with
      style = config.login_style
    }
    init_opt = Option.map((_, usr) ->
                            config.on_change_page_client(usr), usr_opt)
    client_ssn = client_session(config, server_ssn, usr_id, init_opt)
    message_opt = Option.map((_, usr) ->
        do Session.send(server_ssn, {change_page=(usr_id, client_ssn)})
        config.login_message(logout_action(server_ssn, usr_id), usr), usr_opt)
    WLoginbox.html(loginbox_config, prefixed_login_box_id,
        login_action(server_ssn, client_ssn, path, usr_id, _, _), message_opt)
}}
