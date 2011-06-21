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
 * A widget displaying a login prompt or information about the logged user.
 */

import stdlib.widgets.core
import stdlib.widgets.button

type WLoginbox.config('a) = {
  style : WStyler.styler
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

  @private
  fading_time = 0 //ms

  @private @client
  place_holder(id,init,is_password)(_)=
    if false //not(Dom.Support.placeholder())
    then
      if is_password
      then
        fid = "{id}_placeholder"
        real() : void = do Dom.hide(#{fid}) Dom.show(#{id})
        fake() : void = do Dom.show(#{fid}) Dom.hide(#{id})
        blur() : void = if Dom.get_value(#{id}) == "" then fake() else real()
        finput = Dom.of_xhtml(<input id="{fid}" type="text" value="{init}" style="display:none" onfocus={_ -> _ = real() Dom.give_focus(#{id})} />)
        _ = Dom.put_after(#{id},finput)
        _ = Dom.bind(#{id},{blur},( _ -> blur()))
        _ = Dom.bind(#{id},{ready},( _ -> blur()))
        Scheduler.sleep(300,blur)
      else
        f() = if Dom.get_value(#{id}) == ""
               then Dom.set_value(#{id}, init)
               else void
        g() = if Dom.get_value(#{id}) == init
               then Dom.set_value(#{id}, "")
               else void
        _ = Dom.bind(#{id},{blur},(_ ->f() ))
        _ = Dom.bind(#{id},{focus},(_ -> g()))
        Scheduler.sleep(300,f)
    else void

    default_config = {
      style = WStyler.empty
    }

    /* Set the username and password fields to [username_init] and
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
          <input id={username_id} type="text" autocomplete="on" name="{username_id}" placeholder="{init_username}" onready={place_holder(username_id,init_username,false)} />
          <input id={password_id} type="password" autocomplete="on" name="{password_id}" placeholder="{init_password}" onready={place_holder(password_id,init_password,true)} />
          <input type="submit" value="Login" />
        </span>
        </form>
        ;
      // Login XHTML chunk itself
      <div id={id}>
        {login_form("username", "password")}
        <span id={logged_id}>
          {usr_opt ? <></>}
        </span>
        <span id={unlogged_id}>
        </span>
      </div>
        |> WStyler.add(config.style, _)

    @client
    set_logged_in(id: string, message: xhtml): void =
      logged_id = get_logged_id(id)
      d = Dom.unsplit([Dom.select_id(get_unlogged_id(id)), Dom.select_id(get_not_logged_id(id))])
      _ = Dom.transition(d, Dom.Effect.with_duration({millisec = fading_time}, Dom.Effect.hide()))
      do Dom.transform([#{logged_id} <- message])
      _ = Dom.transition(#{logged_id}, Dom.Effect.with_duration({millisec = fading_time}, Dom.Effect.show()))
      do init_login(id, "username", "password")
      void

    @client
    set_logged_out(id: string, message:xhtml): void =
      logged_id = get_logged_id(id)
      unlogged_id = get_unlogged_id(id)
      _ = Dom.transition(#{logged_id}, Dom.Effect.with_duration({millisec = fading_time}, Dom.Effect.hide()))
      do Dom.transform([#{unlogged_id} <- message])
      d = Dom.unsplit([Dom.select_id(unlogged_id), Dom.select_id(get_not_logged_id(id))])
      _ = Dom.transition(d, Dom.Effect.with_duration({millisec = fading_time}, Dom.Effect.show()))
      _ = Dom.remove(#{get_logged_user_id(id)})
      void

}}
