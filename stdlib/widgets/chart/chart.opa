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


type WChart.percentage = float

type WChart.config = {
  width : Css.size
  height : Css.size
}

@abstract
type WChart.chart =
  { config : WChart.config
  ; id : string
  }

type WChart.failure = {canvas_not_found}
                    / {canvas_2d_context_not_found}

type WChart.result = outcome(void, WChart.failure)

WChart = {{

  default_config : WChart.config =
    { width = {px = 300}
    ; height = {px = 100}
    }

  @private
  generate(config, data, canvas_id, draw_on) =
    canvas_dom = #{canvas_id}
    match Canvas.get(canvas_dom) with
    | {none} -> {failure = {canvas_not_found}}
    | {some=canvas} ->
      match Canvas.get_context_2d(canvas) with
      | {none} -> {failure = {canvas_2d_context_not_found}}
      | {some=ctx} ->
          w = Dom.get_width(canvas_dom)
          h = Dom.get_height(canvas_dom)
          do Canvas.clear_rect(ctx, 0, 0, w, h)
          draw_on(config, data, ctx, w, h)

  create(config : WChart.config, id : string) : (xhtml, WChart.chart) =
    xhtml = Canvas.create(id, config.width, config.height)
    chart = ~{config id}
    (xhtml, chart)

  draw_bar_chart(chart : WChart.chart, config : WBarChart.config,
    data : WChartData.instance) : WChart.result =
    generate(config, data, chart.id, WBarChart.draw)

  draw_line_chart(chart : WChart.chart, config : WLineChart.config,
    data : WChartData.instance) : WChart.result =
    generate(config, data, chart.id, WLineChart.draw)

}}
