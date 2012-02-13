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
