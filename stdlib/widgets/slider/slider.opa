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
 * A configurable slider widget.
 *
 * @category WIDGET
 * @author Adrien Jonquet, 2011
 */

import stdlib.widgets.core

/**
 * {1 About this module}
 *
 * This widget aims at letting the user choose a value by moving a thumb
 * of a gauge bar.
 *
 */

/**
 * {1 Types defined in this module}
 */

type WSlider.style = {
  thumb : WStyler.styler
  thumb_dragged : WStyler.styler
  thumb_over : WStyler.styler
  gauge  : WStyler.styler
}

type WSlider.config = {
  style : WSlider.style /** Global CSS style of the widget */
  range : (int, int)    /** Range of values for the slider */
  step : int
  init : int
  on_change : (int -> void)
  on_release : (int -> void)
}

WSlider =

{{

/**
 * {1 Configuration}
 */

  default_style_config : WSlider.style = {
     thumb =  WStyler.make_style(default_thumb_css)
     thumb_over = WStyler.make_style(default_thumb_over_css)
     thumb_dragged = WStyler.make_style(default_thumb_dragged_css)
     gauge = WStyler.make_style(default_gauge_css)
   }

   default_config : WSlider.config = {
     style = default_style_config
     range = (0, 200)
     step = 10
     init = 0
     on_release = (_ -> void)
     on_change =  (_ -> void)
   }

/**
 * {1 High-level interface}
 */

  /**
   * Display a slider
   *
   * @param config the widget configuration
   * @param id the main ID identifier of the widget
   */



  @private @client
  onready(id,config)=
    onready_aux(id,config,_)

  @private @client
  onready_aux(id,config,_)=
    gauge_id = gen_id(id, "gauge")
    Dom.transform([#{gauge_id} <- thumb(id,config)])

  @private @client
  dummy(_) = void

  html(config:WSlider.config, id:string) : xhtml =
    thumb_left_id = gen_id(id, "thumb_pos")
    gauge_id = gen_id(id, "gauge")
    gauge = <div id=#{gauge_id} onready={onready(id,config)} ></div> |> WStyler.set(config.style.gauge, _)
    <div id=#{gen_id(id, "")}>
      <input id=#{"{thumb_left_id}"} type="hidden" value="0"/>
      {gauge}
    </div>

/**
 * {2 Imperative interface}
 */


  @private
  round(x: int, step : int) : int =
   base = x / step * step
   if mod(x,step) > step/2
   then base + step
   else base

  @private @client
  range_to_pos(id : string, config : WSlider.config, i : int) : int =
   w = Dom.get_width(#{gen_id(id, "gauge")})
   max = config.range.f2 - config.range.f1
   i = if i < config.range.f1
       then 0
       else if i > config.range.f2
       then max
       else i - config.range.f1
   i = round(i,config.step)
   i = Int.of_float((Int.to_float(w) / Int.to_float(max)) * Int.to_float(i))
   i



  @private @client
  pos_to_range(id : string, config :WSlider.config) : int =
   w = Dom.get_width(#{gen_id(id, "gauge")})
   max = config.range.f2 - config.range.f1
   step =  Int.to_float(w) / Int.to_float(max)
   pos = get_left_offset(id, config)
   w = Int.to_float(w)
   max = Int.to_float(max)
   pos = Int.to_float(pos)
   iii = Int.of_float(max / w * (pos + step))
   config.range.f1 + round(iii,config.step)

  @private @client
  set_left_offset(id:string, _config:WSlider.config,v : int) =
    thumb_id=gen_id(id, "thumb")
    thumb_left_id = gen_id(id, "thumb_pos")
    v = v - (Dom.get_width(#{thumb_id}) / 2)
    _ = Dom.set_style_property_unsafe(#{thumb_id}, "left", "{v}px")
    do Dom.set_value(#{thumb_left_id},"{v}")
    void

  @private @client
  set_left_offset_trans(id:string, _config:WSlider.config,v : int) =
    thumb_id=gen_id(id, "thumb")
    thumb_left_id = gen_id(id, "thumb_pos")
    v = v - (Dom.get_width(#{thumb_id}) / 2)
    do Dom.set_value(#{thumb_left_id},"{v}")
    a = Dom.Effect.change_to([{left={px=v}}])
    a = Dom.Effect.with_duration({fast},a)
    a = Dom.Effect.with_easing({linear}, a)
    _ = Dom.transition(#{thumb_id},a)
    void
  @private @client
  get_left_offset(id:string, _config:WSlider.config) : int =
    thumb_id=gen_id(id, "thumb")
    thumb_left_id = gen_id(id, "thumb_pos")
    v = Int.of_string(Dom.get_value(#{thumb_left_id}))
    v + (Dom.get_width(#{thumb_id}) / 2)

  @private @client
  get_gauge_width(id) =
    Dom.get_width(#{gen_id(id, "gauge")})

  @client
  set_value(config:WSlider.config, id:string, v : int) =
    pos = range_to_pos(id, config, v)
    set_left_offset_trans(id, config, pos)

  @client
  get_value(config:WSlider.config, id:string) : int =
    pos_to_range(id : string, config :WSlider.config)


  @private @client
  move_thumb(e, config :WSlider.config,id) =
    gauge = #{gen_id(id, "gauge")}
    curpos = Dom.Dimension.sub(e.mouse_position_on_page,Dom.get_offset(gauge))
    max = Dom.get_width(gauge)
    x = curpos.x_px
    x = if x < 0 then 0
        else if x > max then max
        else x
    old = get_value(config, id)
    do set_left_offset(id, config, x)
    new = get_value(config, id)
    if not(old == new)
      then config.on_change(new)
      else void


  @private @client
  thumb(id : string, config :WSlider.config) =
    thumb_id = gen_id(id, "thumb")
    gauge_id = gen_id(id, "gauge")
    _ = Dom.bind(#{gauge_id},{mousedown},(e ->
        eh1 = Dom.bind(Dom.select_all(), {mousemove},(e -> move_thumb(e, config, id)))
        rec val eh2 = Dom.bind(Dom.select_all(), {mouseup},  (_ ->
            do Dom.unbind(Dom.select_all(),eh1)
            do Dom.unbind(Dom.select_all(),eh2)
            i = get_value(config, id)
            do set_value(config,id, i)
            do config.on_release(i)
            WStyler.set_dom(config.style.thumb, thumb_id)))
        move_thumb(e, config, id)))
    <div id=#{thumb_id} onmousedown={_ -> WStyler.set_dom(config.style.thumb_dragged,thumb_id)}
                        onmouseover={_ -> WStyler.set_dom(config.style.thumb_over, thumb_id)}
                        onmouseout={_ -> WStyler.set_dom(config.style.thumb, thumb_id)}
                        onready={_ -> set_value(config, id, config.init)}>
    </div>
      |> WStyler.add(config.style.thumb, _)


  /**
   * {1 Private functions}
   */
  @private gen_id(id:string, suffixe:string) = "{id}_{suffixe}"

  @private
  default_thumb_css = css {
    background:#eeeeee;
    height:16px;
    width:16px;
    border:1px double #ccc;
    top:-6px;
    position:absolute;
  }

  @private
  default_thumb_over_css = css {
    background:#e3e3e3;
    height:16px;
    width:16px;
    border:1px double #ccc;
    top:-6px;
    position:absolute;
  }

  @private
  default_thumb_dragged_css = css {
    background:#d1d1d1;
    height:16px;
    width:16px;
    border:1px double #ccc;
    top:-6px;
    position:absolute;
  }

  @private
  default_gauge_css = css {
    position: relative;
    width:200px;
    height:6px;
    background:#cccccc;
    border:1px double #bbb;
  }

}}
