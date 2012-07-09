/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Elvis Clickables
 *
 * @category UI
 * @author David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 Simple clickable}
 *
 * Wrap an elvis as something clickable.
 */

/**
 * {1 Theming}
 *
 * The clickable always has classes "mlstate" "elvis" "clickable"
 * During a click, it also has class "down", otherwise it has class "up"
 * When enabled, it has class "enabled", otherwise it has class "disabled"
 */

type EClickable.options = {
  content: Elvis.elvis(Elvis.masked, Elvis.masked)
  is_enabled: bool
}

type EClickable.sources = {
  chosen: Event.source(void)
  //unfocused:Event.source(void)
  dbclick: Event.source(void)
}
@abstract type EClickable.implementation = {
  dom: dom
  state_enabled: Client.reference(bool)
}

type EClickable.elvis = Elvis.elvis(EClickable.sources, EClickable.implementation)

EClickable =
{{
/**
 * {1 Constructors}
 */
   simple(content:xhtml): EClickable.elvis =
      make({is_enabled = true
            content = Elvis.pack(ELabel.simple(content))})

   make(options:EClickable.options): EClickable.elvis =
   (
      id         = "eclickable_{Random.string(32)}"
      dom        = Dom.select_id(id)
      chosen_net    = Network.empty()
      dbclick_net   = Network.empty()
      //unfocused_net = Network.empty()
      state_enabled = Client_reference.create(options.is_enabled)
      display(theme) =
      (
         mouse_changed(up) =
         (
           if Client_reference.get(state_enabled) then
             dom = Dom.resolve(dom)
             if up then
               do Dom.remove_class(dom, class_down)
               do Dom.add_class(dom,    class_up)
               void
             else
               do Dom.remove_class(dom, class_up)
               do Dom.add_class(dom,    class_down)
               void
         )
         theme_classes = Elvis.Theme.get_classes(theme)
         xhtml = <div id={id}
              class="{theme_classes} elvis mlstate clickable {class_up} {if Client_reference.get(state_enabled) then class_enabled else class_disabled}"
              onclick={_ -> if Client_reference.get(state_enabled) then Network.broadcast({}, chosen_net) else void}
              ondblclick={_ -> if Client_reference.get(state_enabled) then Network.broadcast({}, dbclick_net) else void}
              //onfocusout={_ -> Network.broadcast({}, unfocused_net)}
              onmouseup={_ -> mouse_changed({true})}
              onmousedown={_ -> mouse_changed({false})}>
              <div class="{theme_classes} elvis mlstate clickable_content">{
              Elvis.for_display_in_theme(options.content, theme)
              }</div></div>
         dom = Dom.of_xhtml(xhtml)
         ~{xhtml dom}
      )
      implem = ({
         ~dom
         ~state_enabled
      })
      sources = {
         chosen    = (chosen_net)
         dbclick   = (dbclick_net)
         //unfocused = (unfocused_net)
      }
      Elvis.make(sources, implem, display)
   )

   default_options =
   {
      content = ELabel.simple(<>Nothing to see</>)
      is_enabled = {true}
   }
/**
 * {1 Accessors}
 */
  set_enabled(clickable: EClickable.elvis, enabled: bool): void =
  (
     implem = (Elvis.implem(clickable))
     dom = Dom.resolve(implem.dom)
     state_enabled = implem.state_enabled
     do if enabled != Client_reference.get(state_enabled) then
       if enabled then
          do Dom.remove_class(dom, class_disabled)
          do Dom.add_class(dom,    class_enabled)
          void
       else
          do Dom.remove_class(dom, class_enabled)
          do Dom.add_class(dom,    class_disabled)
          void
     Client_reference.set(state_enabled, enabled)
  )

  @private class_enabled = "enabled"
  @private class_disabled= "disabled"
  @private class_up      = "up"
  @private class_down    = "down"
}}

