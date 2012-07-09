/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
