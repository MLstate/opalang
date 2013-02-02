/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Elvis Buttons
 *
 * @category UI
 * @author David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 Simple clickable}
 *
 * Native button.
 */

/**
 * {1 Theming}
 *
 * The button always has classes "mlstate" "elvis" "button"
 * During a click, it also has class "down", otherwise it has class "up"
 * When enabled, it has class "enabled", otherwise it has class "disabled"
 */

type EButton.options = {
  content:    xhtml
  is_enabled: bool
}

type EButton.sources = {
  chosen:  Event.source(void)
}
@abstract type EButton.implementation = {
  dom: dom
  state_enabled: Client.reference(bool)
}

type EButton.elvis = Elvis.elvis(EButton.sources, EButton.implementation)

EButton =
{{
/**
 * {1 Constructors}
 */
   simple(content:xhtml): EButton.elvis =
      make({is_enabled = true
            content = content})

   make(options:EButton.options): EButton.elvis =
   (
      id         = "ebutton_{Random.string(32)}"
      dom        = Dom.select_id(id)
      chosen_net    = Network.empty()
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
         xhtml = <button id={id}
              class="{theme_classes} elvis mlstate button {class_up} {if Client_reference.get(state_enabled) then class_enabled else class_disabled}"
              onclick={_ -> if Client_reference.get(state_enabled) then Network.broadcast({}, chosen_net) else void}
              onmouseup={_ -> mouse_changed({true})}
              onmousedown={_ -> mouse_changed({false})}
              >{
              options.content
              }</button>
         dom = Dom.of_xhtml(xhtml)
         ~{xhtml dom}
      )
      implem = ({
         ~dom
         ~state_enabled
      })
      sources = {
         chosen    = (chosen_net)
      }
      Elvis.make(sources, implem, display)
   )

/**
 * {1 Accessors}
 */
  set_enabled(button: EButton.elvis, enabled: bool): void =
  (
     implem = (Elvis.implem(button))
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
