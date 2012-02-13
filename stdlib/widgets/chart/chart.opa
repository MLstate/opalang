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


type WChart.percentage = float

type WChart.config = {
  width : Css.size
  height : Css.size
}

@abstract
type WChart.chart = {
  config : WChart.config
  id : string
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
          do Canvas.set_fill_style(ctx, config.bg_style)
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
