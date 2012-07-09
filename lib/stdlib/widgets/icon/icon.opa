/*
    Copyright © 2011, 2012 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

/**
 * A configurable icon
 *
 * @author Hugo Venturini, 2011
 * @category WIDGET
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
 */


import stdlib.widgets.core


type WIcon.config = {
  togglable: bool // Whether the icon has a toggled state or not
  show_label: bool // Whether to show or not the label next to the image

  default_style: WStyler.styler
  toggled_style: WStyler.styler
}

type WIcon.image =
    { noicon }
  / { url: string } // Independent icon
  / { url: string pos: (int, int) size: (int, int) } // Icon in a set
  / { predefined : WIcon.predefined } // Predefined icon

type WIcon.predefined =
    { text_align_left }
  / { text_align_right }
  / { text_justify }
  / { text_center }
  / { text_underline }
  / { text_color }
  / { eraser }
  / { xhtml_doc }
  / { undo }
  / { redo }
  / { background_color }
  / { save }
  / { print }
  / { help }

type WIcon.icon_grid = { resource : resource
                         parameters : DynamicResource.parameters }

WIcon =
{{
  default_config: WIcon.config = {
    togglable = false
    show_label = false

    default_style = WStyler.empty
    toggled_style = WStyler.empty
  }

  default_togglable_config: WIcon.config = { default_config with
    togglable = true
    toggled_style = WStyler.make_style(css {border: 1px solid;})
  }

  @private predefined_image: WIcon.predefined -> { url: string; pos: (int, int); size: (int, int) } =
    predefined_icon_grid = @static_resource("lib/stdlib/widgets/icon/icon_grid-16x16.gif")
    predefined_icon_grid_parameters = { consumption={unlimited}; expiration={none}; visibility={current_context} }
    predefined_url = DynamicResource.publish("", predefined_icon_grid, predefined_icon_grid_parameters)
    w = 16; h = 16;
    (what : WIcon.predefined) ->
      (x, y) = match what with
        | { text_align_left } -> (1,1)
        | { text_align_right } -> (2,1)
        | { text_justify } -> (1,2)
        | { text_center } -> (2,2)
        | { text_underline } -> (1,3)
        | { text_color } -> (4,4)
        | { eraser } -> (5,5)
        | { xhtml_doc } -> (1,8)
        | { undo } -> (3,5)
        | { redo } -> (3,6)
        | { background_color } -> (4,3)
        | { save } -> (2,10)
        | { print } -> (2,9)
        | { help } -> (10, 10)
        end
      {url=predefined_url; size=(w, h); pos=(x * w, y * h)}

  @private mk_styler({~url : string; pos = (x : int, y : int); size = (w : int, h : int) }) =
    WStyler.make_style([
      { display = { block }}, { width = { px = w }}, { height = { px = h }},
      Css_build.background([
        Css_build.background_no_repeat,
        Css_build.background_image(Url.make(url)),
        Css_build.background_position({size={px=-x}}, {size={px=-y}})])
      ])

  html(config: WIcon.config, onclick_handle: -> void, id: string, label: string, image: WIcon.image): xhtml =
    toggle_handle: (-> void) =
      if config.togglable then
        -> Dom.transform([#{id} <- html({config with
            default_style=config.toggled_style
            toggled_style=config.default_style},
          onclick_handle, id, label, image)])
      else
        -> void

    stl = match image with
      | {noicon} | {url = _} -> WStyler.empty // case url: no style, but <img> tag (otherwise no height and width)
      | {url=_ pos=_ size=_} as img -> mk_styler(img)
      | ~{predefined} -> mk_styler(predefined_image(predefined))

    style = WStyler.merge([config.default_style, stl])

    html = match image with
      | ~{url} -> <img src={url} alt={label} onclick={_ -> _ = toggle_handle(); onclick_handle()}/>
      | _ ->
        <a id={"{id}_link"} title={label} onclick={_ -> _ = toggle_handle(); onclick_handle()}>
          {if config.show_label then label else ""}
        </a>;
    html  |> WStyler.set(style, _)

}}
