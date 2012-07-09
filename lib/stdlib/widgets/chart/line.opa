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


type WLineChart.config = {
  bg_style : Canvas.style
  line_stroke_style : Canvas.style
  margin_px : int
}

WLineChart = {{

  default_config : WLineChart.config =
    { line_stroke_style = {color = Color.black}
    ; bg_style = {color = Color.black}
    ; margin_px = 3
    }

//  @private
  draw(config, data, canvas, w, h) =
    if List.is_empty(data.data)
    then {success}
    else
    fl = Float.of_int
    px = Int.of_float
    margin = fl(config.margin_px)
    n = WChartData.length(data)
    fx = WChartData.generate_scaler((0., fl(n-1)), (margin, fl(w) - margin))
    fy = WChartData.generate_scaler(WChartData.get_min_max(data),
      (margin, fl(h) - margin))
    posx(i) = fl(i) |> fx |> px
    posy(v) = h - (fy(v) |> px)
    draw(i, v) = Canvas.line_to(canvas, posx(i + 1), posy(v))
    do Canvas.set_stroke_style(canvas, config.line_stroke_style)
    do Canvas.begin_path(canvas)
    do match data.data with
    | [] -> void
    | [v | vs] ->
        do Canvas.move_to(canvas, posx(0), posy(v))
        do List.iteri(draw, vs)
        void
    do Canvas.stroke(canvas)
    {success}

}}
