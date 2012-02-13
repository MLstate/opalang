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
