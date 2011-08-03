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
 * @author Adam Koprowski
 **/


type WLineChart.config = {
  margin_px : int
}

WLineChart = {{

  default_config : WLineChart.config =
    { margin_px = 3 }

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
