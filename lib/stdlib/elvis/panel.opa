/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * Elvis Panels
 *
 * @category UI
 * @author David Rajchenbach-Teller, 2011
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 */

/**
 * {1 Panels}
 *
 * Panels have multiple uses:
 * - they serve as containers for several elvises
 * - they serve as cut-off points for changing theme, e.g. the theme outside a panel can be different from the theme inside the panel
 * - they can be given classes that can be referenced from CSS, e.g. to allow sizing
 * - children elvises are automatically given classes, e.g. to allow positioning
 */

/**
 * {1 Theming}
 *
 * The panel always has classes "mlstate", "elvis", "panel", plus any classes implied by the theme or by the options.
 * The children of the panel have the same theme as the panel itself, which is either the theme provided as option, or otherwise the parent theme.
 * Each child is embedded in a container numbered [panel_child_0], [panel_child_1], etc.
 */

type EPanel.options =
{
   classes:  list(string)
   children: list(Elvis.elvis(Elvis.masked, Elvis.masked))
   theme:    option(Elvis.theme)
   is_visible:  bool
}

type EPanel.sources =
{
}

@abstract type EPanel.implementation = {
  dom: dom
}

type EPanel.elvis = Elvis.elvis(EPanel.sources, EPanel.implementation)

EPanel =
{{
/**
 * {1 Constructors}
 */
   make(options: EPanel.options): EPanel.elvis =
   (
      id = "epanel_{Random.string(32)}"
      dom = Dom.select_id(id)
      content(parent_theme) =
         theme        = options.theme?parent_theme
         theme_classes= Elvis.Theme.get_classes(theme)
         more_classes = List.to_string_using("", "", " ", options.classes)
         xhtml =
              <div id={id} class="{theme_classes} mlstate elvis panel {more_classes}" style={if not(options.is_visible) then [{display = {css_none}}] else []}>
              {
                  List.map((child -> <div class="panel_child">{Elvis.for_display_in_theme(child, theme)}</div>), options.children)
              }
              </div>
         dom = Dom.of_xhtml(xhtml)
         {~dom ~xhtml}
      Elvis.make({}, ({~dom}), content)
   )

   default_options =
   {
      classes = []
      children= []
      theme   = {none}
      is_visible = {true}
   }

/**
 * {1 Effects}
 */
  set_visible(panel: EPanel.elvis, visible: bool): void =
  (
    dom = (Elvis.implem(panel)).dom
    if visible then Dom.show(dom) else Dom.hide(dom)
  )

  transition(panel: EPanel.elvis, transition: Dom.animation): void =
  (
    dom = (Elvis.implem(panel)).dom
    _ = Dom.transition(dom, transition)
    void
  )

  scroll_into_view(panel: EPanel.elvis): void =
  (
    dom = (Elvis.implem(panel)).dom
    do Dom.scroll_into_view(dom)
    void
  )
}}
