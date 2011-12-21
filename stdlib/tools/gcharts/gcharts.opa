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
 * Opa interface to gcharts js api
 *
 * @author Nicolad Glondu, 2011
 * @category TOOL
 * @destination PUBLIC
 * @stability EXPERIMENTAL
 * @version 0.1
**/

import-plugin gcharts
package stdlib.tools.gcharts

/**
 * {1 About this module}
 *
 * Undocumented experimental plugin
 * Refer to Google's API reference for more information
 * about the options of a chart type
 */

type json = RPC.Json.json

/* Available types of charts */

type GCharts.chart_type =
    { area_chart }
  / { bar_chart }
  / { column_chart }
  / { combo_chart }
  / { gauge_chart }
  / { geo_chart }
  / { line_chart }
  / { intensity_map } /**  Note: Keys MUST ALL be uppercase ISO3166 codes. One incorrect key breaks the whole graph */
  / { pie_chart }
  // / { table } /** Does not work */
  / { tree_map } /** Note: The container MUST have a height, a width and be displayed as a 'block". Else the graph won't be visible */

/* Types for data construction */

type GCharts.DataTable.col_type =
    { boolean }
  / { number }
  / { string }

type GCharts.DataTable.col = {
  col_id    : string
  col_type  : GCharts.DataTable.col_type
  col_label : string
}

type GCharts.DataTable.cell_data =
    { Int    : int }
  / { Float  : float }
  / { String : string }
  / { Bool   : bool }

type GCharts.DataTable.cell =
    { cell_val   : GCharts.DataTable.cell_data }
  / { serialized : string
      cell_val   : GCharts.DataTable.cell_data }

type GCharts.DataTable.row = list(GCharts.DataTable.cell)

type GCharts.DataTable.t = {
  cols : list(GCharts.DataTable.col)
  rows : list(GCharts.DataTable.row)
}

type GCharts.region =
    { World } // Whole map - default value
  / { ISO3166 : string } // ISO-3166 country code
  / { South_America } // 005
  / { Central_America } // 013
  / { North_America } // 021
  / { All_of_Africa } // 002
  / { Central_Africa } // 017
  / { Northern_Africa } // 015
  / { Southern_Africa } // 018
  / { Eastern_Asia } // 030
  / { Southern_Asia } // 034
  / { Asia_Pacific_region } // 035
  / { Central_Asia } // 143
  / { Middle_East } // 145
  / { Europe } // 150
  / { Northern_Asia } // 151
  / { Northern_Europe } // 154
  / { Western_Europe } // 155
  / { Southern_Europe } // 039

type GCharts.series_type = {line} / {bars} / {area}

type GCharts.series_option =
    { series_type : GCharts.series_type }
  / { color : color }
  / { right_axis : bool } // targetAxisIndex = 1
  / { target_axis_index : int }
  / { point_size : int }
  / { line_width : int }
  / { area_opacity : float } // in [0.0 : 1.0]
  / { visible_in_legend : bool }

type GCharts.text_style = {
  color : option(color)
  font_name : option(string)
  font_size : option(int)
}

type GCharts.axis_option =
    { baseline : int }
  / { baseline_color : color }
  / { reverse_direction : bool }
  / { format : string }
  / { gridline_color : color }
  / { log_scale : bool }
  / { text_style : GCharts.text_style }
  / { text_position : {out}/{in}/{none} }
  / { title : string }
  / { title_text_style : GCharts.text_style }
  / { max_value : int }
  / { min_value : int }

/**
 * Various options for charts
 *
 * Note some options may not work for all charts
 * You should test it or check Google Charts's API reference
 * if you have a doubt
 */
type GCharts.option =
    { title : string }

  / { width : int }
  / { height : int }
  / { bgcolor : color } // No transparency
  / { stroke_color : color }
  / { stroke_width : int }
  / { colors : list(color) }

  / { min_color : color }
  / { mid_color : color }
  / { max_color : color }
  / { header_height : int }
  / { font_name : string }
  / { font_size : int }
  / { font_color : color }
  / { show_scale : bool }
  / { curve_type : {none}/{function} }

  // Gauge options
  / { min : int }
  / { max : int }
  / { green_color : color }
  / { green_from : int }
  / { green_to : int }
  / { yellow_color : color }
  / { yellow_to : int }
  / { yellow_from : int }
  / { red_color : color }
  / { red_from : int }
  / { red_to : int }
  / { minor_ticks : int }
  / { major_ticks : list(string) }
  / { pie_slice_text_style : GCharts.text_style }

  / { h_axis : list(GCharts.axis_option) }
  / { v_axis : list(GCharts.axis_option) }
  / { v_axes : list((int, list(GCharts.axis_option))) }

  / { default_series_type : GCharts.series_type }
  / { series : list((int, list(GCharts.series_option))) } // (Affected series, options)

  / { legend : {left}/{right}/{top}/{bottom}/{in}/{none} }
  / { region : GCharts.region }
  / { is_stacked : bool }

@private GCJson = {{

  DataTable = {{

    col_type(ct:GCharts.DataTable.col_type):json =
      match ct with
      | {boolean} -> {String="boolean"}
      | {number}  -> {String="number"}
      | {string}  -> {String="string"}

    col(c:GCharts.DataTable.col):json =
      {Record=[
         ("id", {String=c.col_id}:json),
         ("label", {String=c.col_label}),
         ("type", col_type(c.col_type)),
       ]}

    cell_data(cd:GCharts.DataTable.cell_data):json =
      @opensums(cd)

    cell(c:GCharts.DataTable.cell):json =
      match c with
      | ~{cell_val} -> {Record=[("v", cell_data(cell_val))]}
      | ~{cell_val serialized} ->
          {Record=[("v", cell_data(cell_val)),
                   ("f", {String=serialized}:json)]}

    row(r:GCharts.DataTable.row):json =
      List.map(cell, r) |> l -> {List=l}

    data(data:GCharts.DataTable.t):json =
      cols = List.map(col, data.cols) |> l -> {List=l}:json
      rows = List.map(
        r -> {Record=[("c",row(r))]},
        data.rows) |> l -> {List=l}:json
      {Record=[("cols", cols), ("rows", rows)]}

  }}

  aux_color(c:color):json =
    hex(n) =
      if n == 0 then "00"
      else if n < 16 then "0{Int.to_hex(n)}"
      else Int.to_hex(n)
    {String="#{hex(c.r)}{hex(c.g)}{hex(c.b)}"}

  aux_series_type(st:GCharts.series_type):json =
    match st with
    | {line} -> {String="line"}
    | {bars} -> {String="bars"}
    | {area} -> {String="area"}

  aux_text_style(ts:GCharts.text_style):json =
    {Record=[
      ("color", aux_color((ts.color?Color.black))),
      ("fontName", {String=(ts.font_name?"<global-font-name>")}),
      ("fontSize",
       match ts.font_size with
       | {some=s} -> {Int=s}
       | {none} -> {String="<global-font-size>"})]}

  aux_axis_option(opt:GCharts.axis_option):(string, json) =
    match opt with
    | ~{baseline} -> ("baseline", {Int=baseline})
    | ~{baseline_color} -> ("baselineColor", aux_color(baseline_color))
    | ~{reverse_direction} -> ("direction", if reverse_direction then {Int=-1} else {Int=1})
    | ~{format} -> ("format", {String=format})
    | ~{gridline_color} -> ("gridlineColor", aux_color(gridline_color))
    | ~{log_scale} -> ("logScale", {Bool=log_scale})
    | ~{text_style} -> ("textStyle", aux_text_style(text_style))
    | ~{text_position} ->
      ("textPosition", match text_position with
        | {out} -> {String="out"}
        | {in} -> {String="in"}
        | {none} -> {String="none"})
    | ~{title} -> ("title", {String=title})
    | ~{title_text_style} -> ("titleTextStyle", aux_text_style(title_text_style))
    | ~{max_value} -> ("maxValue", {Int=max_value})
    | ~{min_value} -> ("minValue", {Int=min_value})

  aux_series_option(opt:GCharts.series_option):(string, json) =
    match opt with
    | ~{series_type} -> ("type", aux_series_type(series_type))
    | ~{color} -> ("color", aux_color(color))
    | ~{right_axis} -> ("targetAxisIndex",
                        (if right_axis then {Int=1} else {Int=0}))
    | ~{target_axis_index} -> ("targetAxisIndex", {Int=target_axis_index})
    | ~{point_size} -> ("pointSize", {Int=point_size})
    | ~{line_width} -> ("lineSize", {Int=line_width})
    | ~{area_opacity} -> ("areaOpacity", {Float=area_opacity})
    | ~{visible_in_legend} -> ("visibleInLegend", {Bool=visible_in_legend})

  chart_options(opts:list(GCharts.option)):string =
    aux(o:GCharts.option):(string,json) =
      match o with
      | ~{title} -> ("title", {String=title})

      | ~{width} -> ("width", {Int=width})
      | ~{height} -> ("height", {Int=height})
      | ~{bgcolor} -> ("backgroundColor", aux_color(bgcolor))
      | ~{stroke_color} -> ("backgroundColor.stroke", aux_color(stroke_color))
      | ~{stroke_width} -> ("backgroundColor.strokeWidth", {Int=stroke_width})
      | ~{colors} -> ("colors", {List=List.map(aux_color, colors)})

      | ~{min_color} -> ("minColor", aux_color(min_color))
      | ~{mid_color} -> ("midColor", aux_color(mid_color))
      | ~{max_color} -> ("maxColor", aux_color(max_color))
      | ~{header_height} -> ("headerHeight", {Int=header_height})
      | ~{font_name} -> ("fontName", {String=font_name})
      | ~{font_size} -> ("fontSize", {Int=font_size})
      | ~{font_color} -> ("fontColor", aux_color(font_color))
      | ~{show_scale} -> ("showScale", {Bool=show_scale})
      | ~{curve_type} ->
        ("curveType", match curve_type with
          | {none} -> {String="none"}
          | {function} -> {String="function"})

      | ~{min} -> ("min", {Int=min})
      | ~{max} -> ("max", {Int=max})
      | ~{green_color} -> ("greenColor", aux_color(green_color))
      | ~{green_from} -> ("greenFrom", {Int=green_from})
      | ~{green_to} -> ("greenTo", {Int=green_to})
      | ~{yellow_color} -> ("yellowColor", aux_color(yellow_color))
      | ~{yellow_to} -> ("yellowTo", {Int=yellow_to})
      | ~{yellow_from} -> ("yellowFrom", {Int=yellow_from})
      | ~{red_color} -> ("redColor", aux_color(red_color))
      | ~{red_from} -> ("redFrom", {Int=red_from})
      | ~{red_to} -> ("redTo", {Int=red_to})
      | ~{minor_ticks} -> ("minorticks", {Int=minor_ticks})
      | ~{major_ticks} ->
        ("majorTicks", {List=List.map(s->{String=s}, major_ticks)})
      | {pie_slice_text_style=t} -> ("pieSliceTextStyle", aux_text_style(t))

      | ~{h_axis} -> ("hAxis", {Record=List.map(aux_axis_option, h_axis)})
      | ~{v_axis} -> ("vAxis", {Record=List.map(aux_axis_option, v_axis)})

      | ~{v_axes} -> ("vAxes", {Record=List.map(
          (num, opts) ->
            opts = {Record=List.map(aux_axis_option, opts)}
            (Int.to_string(num), opts),
          v_axes
        )})

      | ~{default_series_type} -> ("seriesType", aux_series_type(default_series_type))
      | ~{series} -> ("series", {Record=List.map(
          (num, opts) ->
            opts = {Record=List.map(aux_series_option, opts)}
            (Int.to_string(num), opts),
          series
        )})

      | ~{is_stacked} -> ("isStacked", {Bool=is_stacked})
      | ~{legend} ->
        ("legend", match legend with
          | {left}   -> ({String="left"})
          | {right}  -> ({String="right"})
          | {top}    -> ({String="top"})
          | {bottom} -> ({String="bottom"})
          | {in}     -> ({String="in"})
          | {none}   -> ({String="none"}))
      | ~{region} ->
        ("region", match region with
          | {World} -> ({String="World"})
          | {~ISO3166} -> ({String=String.uppercase(ISO3166)})
          | {South_America} -> ({String="005"})
          | {Central_America} -> ({String="013"})
          | {North_America} -> ({String="021"})
          | {All_of_Africa} -> ({String="002"})
          | {Central_Africa} -> ({String="017"})
          | {Northern_Africa} -> ({String="015"})
          | {Southern_Africa} -> ({String="018"})
          | {Eastern_Asia} -> ({String="030"})
          | {Southern_Asia} -> ({String="034"})
          | {Asia_Pacific_region} -> ({String="035"})
          | {Central_Asia} -> ({String="143"})
          | {Middle_East} -> ({String="145"})
          | {Europe} -> ({String="150"})
          | {Northern_Asia} -> ({String="151"})
          | {Northern_Europe} -> ({String="154"})
          | {Western_Europe} -> ({String="155"})
          | {Southern_Europe} -> ({String="039"}))
   {Record=List.map(aux, opts)} |> Json.to_string

}}

GCharts = {{

  script =
    <script src="https://www.google.com/jsapi" type="text/javascript" xmlns="http://www.w3.org/1999/xhtml"></script>

  DataTable = {{

    to_json_string(data:GCharts.DataTable.t):string =
      GCJson.DataTable.data(data)
      |> Json.to_string

    /**
     * Create a DataTable object from its labels and a (key, value) list
     */
    make_simple(labels:(string,string), data:list((string, int))) =
      cols = [
        {col_id=Dom.fresh_id() col_type={string} col_label=labels.f1},
        {col_id=Dom.fresh_id() col_type={number} col_label=labels.f2}
      ]
      rows = List.map(
        (n,v) -> [{cell_val={String=n}}, {cell_val={Int=v}}],
        data)
      ~{cols rows}

    /**
     * Merge two data tables by label
     * Will return a data table with common fields asusming
     * that the key is the first field
     * Warning : Will be slow on large data sets (n*m complexity)
     */
    merge_simple(dt1:GCharts.DataTable.t, dt2:GCharts.DataTable.t) =
      cols = List.append(dt1.cols, List.tail(dt2.cols))
      rows = List.filter_map(
        (line1 ->
           if line1 == [] then none
           else
             key = List.head(line1)
             match List.find(
               e -> if e == [] then false else List.head(e) == key,
               dt2.rows) with
             | {none} -> none
             | {some=line2} -> some( List.append(line1, List.tail(line2)) )
        ), dt1.rows)
      ~{cols rows}

    @private add_at_end(elt:'a, list:list('a)) : list('a) =
      List.rev([elt|List.rev(list)])

    /**
     * Add int columns
     * Adds integer data to a datatable
     * Assumes that the additional data is correctly ordered
     * If the size of the additional data is different than the size of
     * existing data, the smallest size is kept (exceding data is lost)
     */
    add_int_column(dt:GCharts.DataTable.t, label:string, data:list(int)) =
      cols = add_at_end({col_id=Dom.fresh_id() col_type={number} col_label=label}, dt.cols)
      rows = List.map2(
        row, v -> add_at_end({cell_val={Int=v}}, row),
        dt.rows, data)
      ~{cols rows}

    /**
     * Will NOT work with empty dataset (col types are computed with first line)
     */
    from_array(labels:list(string), data:list(list(GCharts.DataTable.cell_data))) =
      cols = List.map2(
        col_label, data ->
          col_id = Dom.fresh_id()
          col_type = match data with
            | {Bool=_} -> {boolean}
            | {String=_} -> {string}
            | {Int=_} | {Float=_} -> {number}
          ~{col_id col_type col_label},
        labels, List.head(data))
      rows = List.map(l -> List.map(d -> {cell_val=d}, l), data)
      ~{cols rows}

  }}

  draw(t:GCharts.chart_type, id:string, data:GCharts.DataTable.t, options:list(GCharts.option)) =
    bsl_fun = match t with
      | {area_chart} -> %% gcharts.draw_area_chart %%
      | {bar_chart} -> %% gcharts.draw_bar_chart %%
      | {column_chart} -> %% gcharts.draw_column_chart %%
      | {combo_chart} -> %% gcharts.draw_column_chart %%
      | {gauge_chart} -> %% gcharts.draw_gauge_chart %%
      | {geo_chart} -> %% gcharts.draw_geo_chart %%
      | {line_chart} -> %% gcharts.draw_line_chart %%
      | {intensity_map} -> %% gcharts.draw_intensity_map %%
      | {pie_chart} -> %% gcharts.draw_pie_chart %%
      // | {table} -> %% gcharts.draw_table %%
      | {tree_map} -> %% gcharts.draw_tree_map %%
    json_data = DataTable.to_json_string(data)
    json_options = GCJson.chart_options(options)
    // do Dom.set_value(#debug_data, json_data)
    // do Dom.set_value(#debug_options, json_options)
    bsl_fun(id, json_data, json_options)

}}
