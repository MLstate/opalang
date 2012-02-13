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
 * @author Adam Koprowski
 **/
import stdlib.web.canvas


type WBarChart.config = {
  bg_style : Canvas.style
  bar_fill_style : Canvas.style
  margins : WChart.percentage
  bar_spacing : WChart.percentage
}

WBarChart = {{

  default_config : WBarChart.config =
    { bg_style = {color = Color.black}
    ; bar_fill_style = {color = Color.black}
    ; margins = 0.1
    ; bar_spacing = 0.1
    }

//  @private
  draw(config, data, canvas, w, h) =
    if List.is_empty(data.data)
    then {success}
    else
    hf = Float.of_int(h)
    wf = Float.of_int(w)
    n = WChartData.length(data)
    x_weight(i) = Float.of_int(i) + (Float.of_int(i + 1) * config.bar_spacing)
    fx = WChartData.generate_scaler((0., x_weight(n)), (0., wf))
    fy = WChartData.generate_scaler(WChartData.get_min_max(data),
      (0., hf - hf * (2. * config.margins)))
    generate_bar(i, v) =
      x1 = fx(x_weight(i))
      x2 = fx(x_weight(i + 1) - config.bar_spacing)
      ys = fy(v)
      y2 = hf * (1. - config.margins)
      px = Int.of_float
      Canvas.fill_rect(canvas, px(x1), px(y2 - ys), px(x2 - x1), px(y2))
    do Canvas.set_fill_style(canvas, config.bar_fill_style)
    do List.iteri(generate_bar, data.data)
    {success}

}}
