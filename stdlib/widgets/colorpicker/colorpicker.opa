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
 * A configurable colorpicker widget.
 *
 * @category WIDGET
 * @author Hugo Heuzard, 2011
 */

import stdlib.widgets.{core,slider}

/**
 * {1 Types defined in this module}
 */

type WColorpicker.config = {
  on_select: color -> void
  size: (int,int)
  mode: WColorpicker.mode
  display: WColorpicker.display
  style:  WColorpicker.style
  initial: Color.color
}

type WColorpicker.style = {
  cursor : WStyler.styler
  thumb : WStyler.styler
  thumb_over : WStyler.styler
  thumb_dragged : WStyler.styler
  gauge : WStyler.styler
  preview : WStyler.styler
}

type WColorpicker.display =
  {full}
/ {preview}

type WColorpicker.mode =
  // {value}
// / {hue}
{saturation}

WColorpicker = {{
  @private
  map_saturation_overlay = @static_resource("stdlib/widgets/colorpicker/resource/map-saturation-overlay.png")
  @private
  map_saturation =         @static_resource("stdlib/widgets/colorpicker/resource/map-saturation.png")
  @private
  bar_saturation =         @static_resource("stdlib/widgets/colorpicker/resource/bar-saturation.png")
  @private
  parameters = { consumption={unlimited}; expiration={none}; visibility={shared} }
  @private @publish
  url_map_saturation_overlay = DynamicResource.publish(map_saturation_overlay,parameters)
  @private @publish
  url_map_saturation = DynamicResource.publish(map_saturation,parameters)
  @private @publish
  url_bar_saturation = DynamicResource.publish(bar_saturation,parameters)

  /**
   * {1 Configuration}
   */
  @private
  default_thumb_css = css {
    height:14px;
    width:5px;
    border:2px double black;
    top:-2px;
    position:absolute;
  }
  @private
  default_thumb_over_css = css {
    height:14px;
    width:5px;
    border:2px double #ccc;
    top:-2px;
    position:absolute;
  }
  @private
  default_thumb_dragged_css = css {
    height:14px;
    width:5px;
    border:2px double #ccc;
    top:-2px;
    position:absolute;
  }
  @private
  default_gauge_css = css {
    position: relative;
    width:256px;
    height:16px;
    border:1px double #bbb;
  }

  default_config_style = {
    cursor= WStyler.make_style(css { height:10px; width:10px; border:2px double black;})
    thumb =  WStyler.make_style(default_thumb_css)
    thumb_over = WStyler.make_style(default_thumb_over_css)
    thumb_dragged = WStyler.make_style(default_thumb_dragged_css)
    gauge = WStyler.make_style(default_gauge_css)
    preview = WStyler.make_style(css { height:50px; width:50px; border:1px double black;})
  }

  default_config : WColorpicker.config  = {
    size = (256,256)
    mode = {saturation}
    on_select = c -> Log.info("WColorpicker", Color.color_to_string(c))
    display = {preview}
    style = default_config_style
    initial = Color.blue
  }

 /**
  * {1 High-level interface}
  */
  @private @both
  on_change(id,config)(s) =
    on_change_client(id,config,s)

  @private @client
  on_change_client(id,config,s) =
    overlay_id="{id}_overlay"
    range_id2="{id}_range2"
    bg_id="{id}_bg"
    s = Int.to_float(s) / 100.
    _ = Dom.set_style_property_unsafe(#{overlay_id},"opacity","{s}")
    _ = Dom.set_value(#{range_id2}, "{s}")
    _ = set_color_hsv(id, config, {Color.to_hsv(Color.of_string(Dom.get_text(#{bg_id})) |> Option.get) with s=(1.-s)})
    void

  html(id : string, config : WColorpicker.config) : xhtml =
    map_id="{id}_map"
    overlay_id="{id}_overlay"
    preview_id="{id}_preview"
    range_id="{id}_range"
    range_id2="{id}_range2"
    cursor_id = "{id}_cursor"
    content_id= "{id}_content"
    bg_id="{id}_bg"
    range_config = { WSlider.default_config with
                       on_change = on_change(id,config)
                       on_release = on_change(id,config)
                       range = (1,100)
                       step = 1
                       style = {
                         thumb=config.style.thumb
                         thumb_over=config.style.thumb_over
                         thumb_dragged=config.style.thumb_dragged
                         gauge=WStyler.merge([config.style.gauge,WStyler.make_style(css {width:{config.size.f1}px})])}}
    range = WSlider.html(range_config, range_id)
    cursor = <div onready={_ -> set_color(id,config,config.initial,false)} id="{cursor_id}" style="z-index:4;position:relative;"></div> |> WStyler.add(config.style.cursor,_)
    action(ev) =
      d = Dom.Dimension.sub(ev.mouse_position_on_page,Dom.get_offset(#{map_id}))
      x_px = if d.x_px < 0 then 0 else if d.x_px > 255 then 255 else d.x_px
      y_px = if d.y_px < 0 then 0 else if d.y_px > 255 then 255 else d.y_px
      d = ~{x_px y_px}
      s = (1.-Float.of_string(Dom.get_value(#{range_id2})))
      v = Int.to_float(config.size.f2 - (d.y_px)) / Int.to_float(config.size.f2)
      h = Int.to_float((d.x_px)) / Int.to_float(config.size.f1)
      set_color_hsv(id,config,~{h s v})
    preview = match config.display with
      | {full} -> <div id="{preview_id}"></div> |> WStyler.add(config.style.preview,_)
      | {preview} ->  <div onclick={_ -> Dom.toggle(#{content_id})} id="{preview_id}"></div> |> WStyler.add(config.style.preview,_)
    <div id={id}>
      {preview}
      <div id={content_id} style={[match config.display with {preview} -> {display={css_none}} _ -> {display={block}} ]} >
        <div id={map_id} style="z_index:5;width:{config.size.f1}px;height:{config.size.f2}px"
           onclick={_-> void}
           onmousedown={e ->_ = action(e) _ = Dom.bind(#{map_id},{mousemove},action)  void }
           onmouseup={_ -> _ = Dom.unbind_event(#{map_id},{mousemove});  _ = config.on_select(get_color(id , config )); void } >
          <img src="{match config.mode with | {saturation} -> url_map_saturation}"
               style="width:{config.size.f1}px;height:{config.size.f2}px;z-index:1;display:block;margin:0px;padding:0px" />
          <img onmousedown={_ -> void} onclick={_-> void} onmousemove={_ -> void} id="{overlay_id}" src="{match config.mode with | {saturation} -> url_map_saturation_overlay}"
               style="width:{config.size.f1}px;height:{config.size.f2}px;z_index:2;display:block;margin:Opx;padding:0px;margin-top:-{config.size.f2}px;opacity:0.0;" />
          {cursor}
        </div>
        <input id="{range_id2}" type="text" style="display:none;" value="0" />
        <div id="{bg_id}" type="text" style="display:none;">{Color.color_to_string(config.initial)}</div>
        {range}
      </div>
    </div>

  get_color(id : string, _config : WColorpicker.config) : Color.color =
    bg_id="{id}_bg"
    Color.of_string(Dom.get_text(#{bg_id})) |> Option.get

  @private
  set_color_hsv(id : string, config : WColorpicker.config, {~h ~s ~v} : Color.color_hsv ) : void =
    preview_id="{id}_preview"
    range_id="{id}_range"
    cursor_id = "{id}_cursor"
    bg_id="{id}_bg"

    c = Color.of_hsv(~{h s v})
    c2 = Color.of_hsv(~{h s=1. v=1.})
    tmp = "{range_id}_gauge"
    _ = Dom.set_style(#{tmp}, [{background = Css_build.sum_background(Css_build.background_color(c2), Css_build.background_image(Url.make(url_bar_saturation)))}])
    _ = Dom.set_style(#{preview_id}, [{background = Css_build.background_color(c)}])
    (cursor_w,cursor_h) = (Dom.get_width(#{cursor_id}),Dom.get_width(#{cursor_id}))
    mtop=Int.of_float((-v)*Int.to_float(config.size.f2)-Int.to_float(cursor_h/2))
    mleft=Int.of_float(h*Int.to_float(config.size.f1) - Int.to_float(cursor_w/2))
    _ = Dom.set_style(#{cursor_id}, [{top={px=mtop }},{left={px=mleft}}])
    _ = Dom.set_text(#{bg_id},Color.color_to_string(c))
    void

  set_color(id : string, config : WColorpicker.config, color : Color.color, notify : bool) : void =
    hsv = Color.to_hsv(color)
    // todo set the slider value
    _ = set_color_hsv(id,config,hsv)
    if notify
    then _ = config.on_select(get_color(id , config )) void
    else void

}}
