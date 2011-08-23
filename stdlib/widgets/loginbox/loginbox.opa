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

/**
 * A configuarble widget displaying a login prompt or information about the logged user.
 *
 * @author Guillem Rieu, 2010
 * @author Frederic Ye, 2011
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.3
 */

import stdlib.widgets.core
import stdlib.widgets.button

type WLoginbox.stylers = {
  login_box : WStyler.styler
  logged_box : WStyler.styler
  unlogged_box : WStyler.styler
}

type WLoginbox.config = {
  login_text : string
  login_label : option(string)
  password_label : option(string)
  login_placeholder : option(string)
  password_placeholder : option(string)
  password_help : option(xhtml)
  fading_time : int // in ms
  stylers : WLoginbox.stylers
}

/** Display a login or the logged user if there is one. */
WLoginbox =

  get_username_id(id: string)    = id ^ "_username"
  get_password_id(id: string)    = id ^ "_password"
  get_logged_id(id: string)      = id ^ "_logged"
  get_unlogged_id(id: string)      = id ^ "_unlogged"
  get_not_logged_id(id: string)  = id ^ "_not_logged"
  get_logged_user_id(id: string) = id ^ "_logged_user"

{{

  default_config = {
    login_text = "Login"
    login_label = none
    password_label = none
    login_placeholder = some("username")
    password_placeholder = some("password")
    password_help = none
    fading_time = 0
    stylers = {
      login_box = WStyler.empty
      logged_box = WStyler.empty
      unlogged_box = WStyler.empty
    }
  } : WLoginbox.config

  @private @client
  place_holder(_id,_init,_is_password)(_)= void
//     if false //not(Dom.Support.placeholder())
//     then
//       if is_password
//       then
//         fid = "{id}_placeholder"
//         real() : void = do Dom.hide(#{fid}) Dom.show(#{id})
//         fake() : void = do Dom.show(#{fid}) Dom.hide(#{id})
//         blur() : void = if Dom.get_value(#{id}) == "" then fake() else real()
//         finput = Dom.of_xhtml(<input id="{fid}" type="text" value="{init}" style="display:none" onfocus={_ -> _ = real() Dom.give_focus(#{id})} />)
//         _ = Dom.put_after(#{id},finput)
//         _ = Dom.bind(#{id},{blur},( _ -> blur()))
//         _ = Dom.bind(#{id},{ready},( _ -> blur()))
//         Scheduler.sleep(300,blur)
//       else
//         f() = if Dom.get_value(#{id}) == ""
//                then Dom.set_value(#{id}, init)
//                else void
//         g() = if Dom.get_value(#{id}) == init
//                then Dom.set_value(#{id}, "")
//                else void
//         _ = Dom.bind(#{id},{blur},(_ ->f() ))
//         _ = Dom.bind(#{id},{focus},(_ -> g()))
//         Scheduler.sleep(300,f)
//     else void

    /**
     * Set the username and password fields to [username_init] and
     * [password_init] values as an hint to the user. The fields are cleared when
     * focused, and the password field, initially set to 'text' type (to make the
     * hint visible), is turned to a real 'password' type field.
     */
    @client @private
    init_login(id: string, username_init: string, password_init: string): void =
      username_id = get_username_id(id)
      password_id = get_password_id(id)
      do Dom.set_value(#{username_id}, username_init)
      do Dom.set_value(#{password_id}, password_init)
      void

    html_default(id: string, login_action: (string, string -> void),
        //logout_action: (string -> void),
        usr_opt: option(xhtml)): xhtml =
      html(default_config, id, login_action, usr_opt)

    @private @client
    on_login(id, login_action)(_) =
      username_id = get_username_id(id)
      password_id = get_password_id(id)
      usr = Dom.get_value(#{username_id})
      pwd = Dom.get_value(#{password_id})
      login_action(usr, pwd)

    @private dummy_page = Resource.raw_text("")
    @publish
    fake_url_1 = DynamicResource.publish(dummy_page, {consumption={unlimited}; expiration={none}; visibility={shared}})
    @publish
    fake_url_2 = DynamicResource.publish(dummy_page, {consumption={unlimited}; expiration={none}; visibility={shared}})

    html(config: WLoginbox.config, id: string,
        login_action: (string, string -> void),
        usr_opt: option(xhtml))
        : xhtml =
      form_id = "{id}__form"
      iframe_id = "{id}__iframe"
      _button_id = "{id}__button" //TODO use widgets button when fixed
      username_id = get_username_id(id)
      password_id = get_password_id(id)
      logged_id   = get_logged_id(id)
      unlogged_id   = get_unlogged_id(id)
      (login_css, _ /* logout_css */ ) = // Display login or logout box?
        Option.switch(_ -> (css {display: none;}, []),
            ([], css{display: none;}), usr_opt)
      login_form(init_username: string, init_password: string) =
      /* ugly hack : needs iframe to be the fake target of the submit form  */
        <iframe src="{fake_url_1}" id="{iframe_id}" name="{iframe_id}" style="display:none;with:0px;height:0px;"></iframe>
        <form  target="{iframe_id}" method="post"  action="{fake_url_2}" name="{form_id}" id="{form_id}" autocomplete="on" onsubmit={on_login(id, login_action)} >
        <span id={get_not_logged_id(id)} style={login_css}>
          {match config.login_label
           {some=s} -> <label for={username_id}>{s}</label>
           {none} -> <></>}
          <input id={username_id} type="text" autocomplete="on" name="{username_id}" placeholder="{init_username}" onready={place_holder(username_id,init_username,false)} />
          {match config.password_label
           {some=s} -> <label for={password_id}>{s}</label>
           {none} -> <></>}
          <input id={password_id} type="password" autocomplete="on" name="{password_id}" placeholder="{init_password}" onready={place_holder(password_id,init_password,true)} />
          {match config.password_help {some=ph} -> ph {none} -> <></>}
          <input type="submit" value="{config.login_text}" />
        </span>
        </form>
        ;
      lg_ph = config.login_placeholder?""
      ps_ph = config.password_placeholder?""
      // Login XHTML chunk itself
      <div id={id}>
        {login_form(lg_ph, ps_ph)}
        {<span id={logged_id}>{usr_opt ? <></>}</span> |> WStyler.add(config.stylers.logged_box, _)}
        {<span id={unlogged_id}/> |> WStyler.add(config.stylers.unlogged_box, _)}
      </div> |> WStyler.add(config.stylers.login_box, _)

    // FIXME: we should use config (pass it in parameter), but we need for that to modify CLogin also...
    @client
    set_logged_in(id: string, message: xhtml): void =
      logged_id = get_logged_id(id)
      d = Dom.unsplit([Dom.select_id(get_unlogged_id(id)), Dom.select_id(get_not_logged_id(id))])
      //_ = Dom.transition(d, Dom.Effect.with_duration({millisec = config.fading_time}, Dom.Effect.hide()))
      _ = Dom.hide(d)
      do Dom.transform([#{logged_id} <- message])
      //_ = Dom.transition(#{logged_id}, Dom.Effect.with_duration({millisec = config.fading_time}, Dom.Effect.show()))
      _ = Dom.show(#{logged_id})
      do init_login(id, default_config.login_placeholder?"", default_config.password_placeholder?"")
      void

    // FIXME: we should use config (pass it in parameter), but we need for that to modify CLogin also...
    @client
    set_logged_out(id: string, message:xhtml): void =
      logged_id = get_logged_id(id)
      unlogged_id = get_unlogged_id(id)
      //_ = Dom.transition(#{logged_id}, Dom.Effect.with_duration({millisec = config.fading_time}, Dom.Effect.hide()))
      _ = Dom.hide(#{logged_id})
      do Dom.transform([#{unlogged_id} <- message])
      d = Dom.unsplit([Dom.select_id(unlogged_id), Dom.select_id(get_not_logged_id(id))])
      //_ = Dom.transition(d, Dom.Effect.with_duration({millisec = config.fading_time}, Dom.Effect.show()))
      _ = Dom.show(d)
      _ = Dom.remove(#{get_logged_user_id(id)})
      void

}}
