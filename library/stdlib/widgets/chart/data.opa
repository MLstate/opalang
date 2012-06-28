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
