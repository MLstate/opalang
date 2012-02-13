/*
    Copyright © 2011 MLstate

    This file is part of OPA.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

/**
 * A widget displaying popups
 *
 * @author Hugo Heuzard, Guillem Rieu, 2011
 * @author Adam Koprowski, 2011 (fade-in/out animations, better customization, ...)
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */

import stdlib.widgets.core
import stdlib.widgets.button

type WNotification.config_box = {
     style_top: WStyler.styler
     style_content:WStyler.styler
     style_buttons:WStyler.styler
     style_main:WStyler.styler
     sizex: int
     sizey: int
     }


type WNotification.config = {
  style_box : WNotification.config_box
  style_back : WStyler.styler
}

type WNotification.box_id = (string, string)

type WNotification.button = {
  label : string
  action : WNotification.box_id -> void
}

type WNotification.box = {
  /**
   * Whether the notification dialog blocks actions on the page or not:
   * - if 'modal', a user action is required on the dialog box itself.
   * - if 'loose', the dialog disappears when clicking outside.
   * - if 'free', actions on the page are still possible.
   */
  mode: { modal } / { loose } / { free }

  /** Fade in/out speed when showing/hiding the box (no fade effect for {none}) */
  fade: {none} / {slow} / {default} / {fast}

  /** Delay in ms to keep the notification open */
  delay: option(int)

  /** Title of dialog box **/
  title : xhtml

  /** Content of dialog box **/
  content : xhtml

  /** Buttons of dialog box **/
  buttons : { standard : list(WNotification.button) } / { customized : WNotification.box_id -> xhtml } / { no_buttons }
}


WNotification =

  get_notification_id(id) = id ^ "_notification"

  get_notification_box_id(id) = id ^ "_notificationbox"

  get_notification_modal_id(id) = id ^ "_notificationmodal"

  get_notification_background_id(id) = id ^ "_background"

  animate(immediate, fade, speed) : dom -> void =
    do_fade(speed) = id ->
      effect = Dom.Effect.with_duration(speed, fade())
      Dom.transition(id, effect) |> ignore
    match speed with
    | {none} -> immediate
    | {slow} -> do_fade({slow})
    | {default} -> do_fade({default})
    | {fast} -> do_fade({fast})

  add_notification(fade, cid, id, modal, new_html) =
    _ = Log.debug("add notif",new_html)
    show = animate(Dom.show, Dom.Effect.fade_in, fade)
    if modal
    then
      do Dom.transform([#{get_notification_modal_id(cid)} +<- new_html])
      do show(#{get_notification_box_id(id)})
      do show(#{get_notification_background_id(cid)})
      do show(#{get_notification_modal_id(cid)})
      void
    else
      do Dom.transform([#{get_notification_id(cid)} +<- new_html])
      do show(#{get_notification_box_id(id)})
      do show(#{get_notification_id(cid)})
      void

  rem_notification(fade, cid, id, modal) =
    _ = Log.debug("rem notif",id)
    hide = animate(Dom.hide, Dom.Effect.fade_out, fade)
    do hide(#{get_notification_box_id(id)})
    do Dom.remove(#{get_notification_box_id(id)})
    if modal
    then do
      if Dom.is_empty(Dom.select_children(#{get_notification_modal_id(cid)}))
      then (do hide(#{get_notification_modal_id(cid)})
            do hide(#{get_notification_background_id(cid)})
            Dom.unbind_event(#{get_notification_background_id(cid)},{click}))
      void
    else
      do
      if Dom.is_empty(Dom.select_children(#{get_notification_id(cid)}))
      then hide(#{get_notification_id(cid)})
      void

  notification_html(id: string, new_id:string, modal:bool, notification: WNotification.box, conf): xhtml =
    ids = (id, new_id)
    rem = ( -> rem_notification(notification.fade, id, new_id, modal))
    // TODO: add drag'n'drop when available
    prefix = "{id}_notification_{new_id}"
    // action_close = [("Close", rem)]
    (kind_html, content_html, actions) = (notification.title, notification.content, notification.buttons)
      // | {error} -> (<>Error</>, notification.value, action_close)
      // | {warning} -> (<>Warning</>, notification.value, action_close)
      // | {info} -> (<>Info</>, notification.value, action_close)
      // | {~custom} -> custom(notification.value))

    make_button(~{label action}) : xhtml =
      uniq_id = Dom.fresh_id()
      WSimpleButton.html(uniq_id, (_ -> action(ids)), label)
      |> WCore.make_inline_default(uniq_id, _)

    index = match notification.mode with
      | {modal} -> 1001
      | {loose} -> _ = Dom.bind(#{get_notification_background_id(id)},{click},(_ -> rem())) 1001
      | {free} -> 999

    top =
        // <span style="text-align: right; position: relative; float: right;">
        //   <a onclick={rem}>X</a>
        // </span>
        // <span style="clear:both"></span>
      <div id="{prefix}_notification_bar">
        <span>{kind_html}</span>
      </div> |>  WStyler.add(conf.style_top,_)
    content =
      <div>
        <div id="{prefix}_notification_content">
          {content_html}
        </div>
      </div> |> WStyler.add(conf.style_content,_)
    buttons =
      match actions with
      | {standard=buttons} ->
          <div id="{prefix}_notification_actions">
            {List.map((action -> make_button(action)), buttons)}
          </div> |> WStyler.add(conf.style_buttons,_)
      | {customized=f} -> f(ids)
      | {no_buttons} -> <></>;

    <div id="{get_notification_box_id(new_id)}"
         style="z-index:{index};display:none;
                height: {conf.sizey}px;
                width: {conf.sizex}px; position: fixed; top: 50%; left: 50%;
                margin-left: -{conf.sizex/2}px;
                margin-top: -{conf.sizey/2}px;
                padding-bottom: 20px;">
       {top}
       {content}
       {buttons}
    </div>
    |> WStyler.add(conf.style_main,_)


{{

  default_config_box : WNotification.config_box = {
    sizex = 300
    sizey = 160
    style_top = WStyler.make_style( css {
      width: 100%;
      height: 30px;
      border-bottom: 1px solid black;
    })
    style_content = WStyler.make_style( css {
      height: 100px;
      overflow: auto;
    })
    style_buttons = WStyler.make_style( css {
      height: 30px;
      text-align: right;
      width: 100%;
      border-top: 1px solid black;
    })
    style_main = WStyler.make_style( css {
      background: gray;
      border: 1px solid black;
    })
  }

  default_config : WNotification.config = {
    style_box = default_config_box
    style_back = WStyler.make_style(
      (css {
        display:none;
        position: fixed;
        left: 0;
        top: 0;
        width: 100%;
        height: 100%;
        z-index: 1000;
        background: black;
        opacity: .6
      }))
    }


  /* Add this xhtml to your page before using notify function */
  html(config : WNotification.config, id : string) : xhtml =
    back = <div id="{get_notification_background_id(id)}" style=""></div> |> WStyler.add(config.style_back,_)
    <>
      {back}
      <div id="{get_notification_id(id)}" style="display:none"/>
      <div id="{get_notification_modal_id(id)}" style="display:none"/>
    </>


  /* default warning notification box with custom content */
  default_warning(xhtml : xhtml) :  WNotification.box =
    fade = {default}
    {
      title = <>Warning</>
      content = xhtml
      buttons = {standard = [{label="Ok" action=destroy(fade, _)}]}
      mode = {loose}
      delay = none
      ~fade
    }

  /* default error notification box with custom content */
  default_error(xhtml : xhtml) : WNotification.box =
    fade = {default}
    {
      title = <>Error</>
      content = xhtml
      buttons = {standard = [{label="Ok" action=destroy(fade, _)}]}
      mode = {modal}
      delay = none
      ~fade
    }

  /* default info notificaiton box with custom content */
  default_info(xhtml : xhtml) : WNotification.box =
    fade = {default}
    {
      title = <>Info</>
      content = xhtml
      buttons =  {standard = [{label="Ok" action=destroy(fade, _)}]}
      mode = {free}
      delay = none
      ~fade
    }

  default_confirm(xhtml : xhtml, result : (bool -> void)) : WNotification.box =
    fade = {default}
    yes_no(label, v) = {~label action=(box_id -> do result(v) destroy(fade, box_id))}
    yes = yes_no("Yes", true)
    no = yes_no("No", false)
    {
      title = <>Confirm</>
      content = xhtml
      buttons = {standard = [yes, no]}
      mode = {modal}
      delay = none
      ~fade
    }

  // default_prompt(xhtml : xhtml, result : (string -> void)) : WNotification.box =
  // yes = ("Yes",(_ -> result(true)))
  // no = ("No", (_ -> result(false)))
  // {
  //   kind = content : xhtml -> (<>Confirm</>, content, [yes,no]) }
  //   mode = {modal}
  //   delay = none
  //   value = xhtml
  // }

  /* display a notification box
   * notify(config,id,newid,notification)
   * where :
   * - 'id' is the id used in the 'html' function
   * - 'newid' is a new id to identify the popup
   */
  notify(config : WNotification.config,(id : string, id_box : string) : WNotification.box_id, notification: WNotification.box) : void =
      modal = match notification.mode with
                | {modal} -> {true}
                | {loose} -> {true}
                | {free} -> {false}
      html = notification_html(id, id_box, modal, notification, config.style_box)
      do add_notification(notification.fade, id, id_box, modal, html)
      match notification.delay with
      | {none} -> void
      | {~some} -> sleep(some, (-> rem_notification(notification.fade, id, id_box, modal) ))

  /* remove a notification box
   * destroy(id,newid)
   * where :
   * - 'id' is the id used in the 'html' function
   * - 'newid' is a id identifying the popup
   */
  destroy(fade, (id : string, new_id : string) : WNotification.box_id) : void =
    do rem_notification(fade, id, new_id, true)
    do rem_notification(fade, id, new_id, false)
    void

  box_id(id : string) : WNotification.box_id = (id, Dom.fresh_id())

}}


