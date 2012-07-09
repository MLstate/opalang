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


import stdlib.{core}

type WChartData.series = list(float)

type WChartData.instance = {
  data : WChartData.series
  min : option(float)
  max : option(float)
}

type WChartData.range = (float, float)

// TODO This could possibly be more generic than just for WChart widgets (i.e. generic data-series manipulations)

WChartData = {{

  create(data : WChartData.series) =
    { ~data min=none max=none }

  of_int_list(data : list(int)) : WChartData.series =
    List.map(Float.of_int(_), data)

  map(f : float -> float, data : WChartData.series) : WChartData.series =
    List.map(f, data)

  length(instance : WChartData.instance) =
    List.length(instance.data)

  get_min_max(instance : WChartData.instance) =
    (min,max) = List.min_max(instance.data)
    (instance.min ? Float.min(0., min), instance.max ? max)

  generate_scaler(from : WChartData.range, to : WChartData.range) : float -> float =
    (from_min, from_max) = from
    (to_min, to_max) = to
    range_from = from_max - from_min
    range_to = to_max - to_min
    mult = range_to / range_from
    final_min = Float.min(to_min, to_max)
    final_max = Float.max(to_min, to_max)
    val ->
      res = ((val - from_min) * mult) + to_min
      res |> Float.min(_, final_max) |> Float.max(_, final_min)

}}
