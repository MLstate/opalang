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
