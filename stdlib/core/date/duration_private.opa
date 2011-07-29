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

import stdlib.core.parser

/**
 * @author Adam Koprowski
 */

@private Duration_private = {{

  ToString = {{

    static_printer(s : string) : Duration.printer =
      _d, _v -> s

    empty_printer =
      static_printer("")

    choose_directive = parser
      | [x] -> {success = 1}
      | [s] -> {success = Duration.ms_in_s}
      | [m] -> {success = Duration.ms_in_min}
      | [h] -> {success = Duration.ms_in_h}
      | [D] -> {success = Duration.ms_in_day}
      | [W] -> {success = Duration.ms_in_week}
      | [M] -> {success = Duration.ms_in_month}
      | [Y] -> {success = Duration.ms_in_year}
      | c=(.) -> {failure = "Unknown directive: {Text.to_string(c)}"}

    directive = parser
      | [%] v={choose_directive} -> v

    apply_directive(units : int, d : Duration.duration) : (Duration.duration, int) =
      dv = d
      fwd = dv >= 0
      update = if fwd then Duration.subtract else Duration.add
      v = Int.abs(dv) / units
      new_d = update(d, (v * units))
      (new_d, v)

    show_value(value_defined, width) =
      if value_defined then
        printer(_d, v) =
          str = "{v}"
          i = width - String.length(str)
          if i > 0 then
            String.repeat(i, "0") ^ str
          else
            str
        {success=printer}
      else
        {failure="Attempt to use [#] value in an undefined context"}

    parse_segment(value_defined) = parser
      | "\\%" ->
          {success = static_printer("%")}
      | "\\#" ->
          {success = static_printer("#")}
      | "\\[" ->
          {success = static_printer("[")}
      | "\\]" ->
          {success = static_printer("]")}
      | "\\:" ->
          {success = static_printer(":")}
      | "#####" -> show_value(value_defined, 5)
      | "####" -> show_value(value_defined, 4)
      | "###" -> show_value(value_defined, 3)
      | "##" -> show_value(value_defined, 2)
      | "#" -> show_value(value_defined, 1)
      | "[" mdir={directive} "]" ->
          (match mdir with
           | {success=dir} ->
             printer(d, _v) =
               (_, v) = apply_directive(dir, d)
               "{v}"
             {success=printer}
           | ~{failure} -> ~{failure}
          )
      | "[%" check=([<] -> Duration.is_negative | [0] -> Duration.is_instantenous | [>] -> Duration.is_positive) [:] mf={format_parser(value_defined)} "]" ->
          (match mf with
           | {success=f} ->
              printer(d, v) =
                if check(d) then
                  f(d, v)
                else
                  ""
              {success=printer}
           | ~{failure} -> ~{failure}
          )
      | "[#" cmp=("<>" -> Int.`!=` | [<] -> Int.`<` | [=] -> Int.`==` | [>] -> Int.`>`) n={Rule.natural} [:] mf={format_parser(value_defined)}
         felse=([:] ef={format_parser(value_defined)} -> ef)? "]" ->
           success(f, f_else) =
             printer(d, v) =
               if cmp(v, n) then
                 f(d, v)
               else
                 f_else(d, v)
             {success=printer}
          (match mf with
           | ~{failure} -> ~{failure}
           | {success=f} ->
               match felse with
               | {none} -> success(f, empty_printer)
               | {some={success=f_else}} -> success(f, f_else)
               | {some=~{failure}} -> ~{failure}
          )
      | "[" mdir={directive} [:] mf={format_parser(true)} "]" ->
          (match (mdir, mf) with
           | ({success=dir}, {success=f}) ->
               printer(d, _v) =
                 (nd, nv) = apply_directive(dir, d)
                 f(nd, nv)
               {success=printer}
           | _ -> {failure = "Wrong use of [%X:...] mode"} // FIXME, more specific error message
          )
      | c=("%" | "[") .* ->
          {failure = "Invalid use of special character: {Text.to_string(c)}"}
      | c=(!("]" | ":") .) ->
          {success = static_printer(Text.to_string(c))}

    combine_results(r1, r2) =
      match (r1, r2) : (outcome, outcome) with
      | ({success = s1}, {success = s2}) -> {success = d, v -> s1(d, v) ^ s2(d, v)}
      | ({failure = e1}, {failure = e2}) -> {failure = e1 ^ "\n" ^ e2}
      | ({failure = _}, _) -> r1
      | (_, {failure = _}) -> r2

    format_parser(value_defined) =
      parser l={parse_segment(value_defined)}* -> List.fold_backwards(combine_results, l, {success=empty_printer})

    top_format_parser =
      parser f={format_parser(false)} suffix=(.*) ->
        if Text.length(suffix) == 0 then
          f
        else
          {failure = "Failed to parse format suffix: {Text.to_string(suffix)}"}

    generate_printer(format) =
      Parser.parse(top_format_parser, format)

  }}

  OfString = {{

    unit_parser = parser
      | [s] -> Duration.s
      | [m] -> Duration.min
      | [h] -> Duration.h
      | [D] -> Duration.days
      | [M] -> Duration.months
      | [Y] -> Duration.years

    segment_parser =
      parser prefix=("+" -> (n -> n) | "-" -> (n -> -n) | "" -> (n -> n)) n={Rule.natural} unit={unit_parser} ->
        unit(prefix(n))

    duration_parser =
      parser l={segment_parser}* -> List.fold_backwards(Duration.add, l, Duration.empty)

  }}

}}
