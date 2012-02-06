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

@private Date_private = {{

  /* The date corresponding to the compilation date */
  compilation_date : Date.date =
    @compiletime("compilation_date")

  ll_import(t : time_t) : Date.date =
    import = %%BslTime.import_t%% : time_t -> int
    import(t)

  ll_export(d : Date.date) : time_t =
    export = %%BslTime.export_t%% : int -> time_t
    export(d)

  date_in(bsl) =
    date -> bsl(ll_export(date))

  date_out(bsl) =
    ll_import(bsl())

  // -------------------------------------------------------

  time_now() : Date.date =
    date_out(%%BslTime.now%% : -> time_t)

  time_process_utime() : Date.date =
    date_out(%%BslTime.process_utime%% : -> time_t)

  time_process_stime() : Date.date =
    date_out(%%BslTime.process_stime%% : -> time_t)

  time_process_cutime() : Date.date =
    date_out(%%BslTime.process_cutime%% : -> time_t)

  time_process_cstime() : Date.date =
    date_out(%%BslTime.process_cstime%% : -> time_t)

  // -------------------------------------------------------

  time_gmt_msec : Date.date -> int =
    date_in(%%BslTime.gmt_msec%% : time_t -> int)

  time_gmt_sec : Date.date -> int =
    date_in(%%BslTime.gmt_sec%% : time_t -> int)      // Seconds 0..60

  time_gmt_min : Date.date -> int =
    date_in(%%BslTime.gmt_min%% : time_t -> int)      // Minutes 0..59

  time_gmt_hour : Date.date -> int =
    date_in(%%BslTime.gmt_hour%% : time_t -> int)     // Hours 0..23

  time_gmt_mday : Date.date -> int =
    date_in(%%BslTime.gmt_mday%% : time_t -> int )    // Day of month 1..31

  time_gmt_mon : Date.date -> int =
    date_in(%%BslTime.gmt_mon%% : time_t -> int)      // Month of year 0..11

  time_gmt_year : Date.date -> int =
    date_in(%%BslTime.gmt_year%% : time_t -> int)     // Year

  time_gmt_wday : Date.date -> int =
    date_in(%%BslTime.gmt_wday%% : time_t -> int)     // Day of week (Sunday is 0)

  time_gmt_yday : Date.date -> int =
    date_in(%%BslTime.gmt_yday%% : time_t -> int)     // Day of year 0..365

  time_gmt_isdst : Date.date -> bool =
    date_in(%%BslTime.gmt_isdst%% : time_t -> bool)   // Daylight time savings in effect

  // -------------------------------------------------------

  time_local_msec : Date.date -> int =
    date_in(%%BslTime.local_msec%% : time_t -> int)

  time_local_sec : Date.date -> int =
    date_in(%%BslTime.local_sec%% : time_t -> int)    // Seconds 0..59

  time_local_min : Date.date -> int =
    date_in(%%BslTime.local_min%% : time_t -> int)    // Minutes 0..59

  time_local_hour : Date.date -> int =
    date_in(%%BslTime.local_hour%% : time_t -> int)   // Hours 0..23

  time_local_timezone_offset : -> int =
    %%BslTime.local_timezone_offset%% : -> int

  time_local_mday : Date.date -> int =
    date_in(%%BslTime.local_mday%% : time_t -> int)   // Day of month 1..31

  time_local_mon : Date.date -> int =
    date_in(%%BslTime.local_mon%% : time_t -> int)    // Month of year 0..11

  time_local_year : Date.date -> int =
    date_in(%%BslTime.local_year%% : time_t -> int)   // Year

  time_local_wday : Date.date -> int =
    date_in(%%BslTime.local_wday%% : time_t -> int)   // Day of week (Sunday is 0)

  time_local_yday : Date.date -> int =
    date_in(%%BslTime.local_yday%% : time_t -> int)   // Day of year 0..365

  time_isdst : Date.date -> bool =
    date_in(%%BslTime.local_isdst%% : time_t -> bool) // Daylight time savings in effect

  // -------------------------------------------------------

  /**
   * {1 Parsing & pretty-printing dates}
  **/
  ToString = {{

    fill(s, width, c) =
      n = width - String.length(s)
      if n > 0 then
        String.repeat(n, c) ^ s
      else
        s

    pad(value, pad_info, default_padding, width) =
      p = if pad_info == {no_padding_info} then default_padding else pad_info
      s = Int.to_string(value)
      match p with
      | {do_not_pad} -> s
      | {pad_with_zeros} -> fill(s, width, "0")
      | {pad_with_spaces} -> fill(s, width, " ")
      | {no_padding_info} -> error("Date.ToString.pad")

    abbreviations =
      [ ("c", "%a %b %_d %Y %H:%M:%S")
      , ("e", "%_d")
      , ("D", "%m/%d/%y")
      , ("F", "%Y-%m-%d")
      , ("h", "%b")
      , ("R", "%H:%M")
      , ("T", "%H:%M:%S")
      ]

    directives =
      [ ("%", false, ((_, _) -> "%"))
      , ("a", false, ((_, d) -> String.sub(0, 3, Date.Weekday.to_string(Date.get_weekday(d)))))
      , ("A", false, ((_, d) -> Date.Weekday.to_string(Date.get_weekday(d))))
      , ("b", false, ((_, d) -> String.sub(0, 3, Date.Month.to_string(Date.get_month(d)))))
      , ("B", false, ((_, d) -> Date.Month.to_string(Date.get_month(d))))
      , ("C", true,  ((p, d) -> pad(Date.get_year(d) / 100, p, {pad_with_spaces}, 2)))
      , ("d", true,  ((p, d) -> pad(Date.get_day(d), p, {pad_with_zeros}, 2)))
      , ("E", false, ((_, d) -> match Date.get_day(d) with | 1 -> "1st" | 2 -> "2nd" | 3 -> "3rd" | x -> "{x}th"))
      , ("H", true,  ((p, d) -> pad(Date.get_hour(d), p, {pad_with_zeros}, 2)))
      , ("I", true,  ((p, d) -> pad(Date.Hour.convert_24h_to_12h(Date.get_hour(d)), p, {pad_with_zeros}, 2)))
      , ("k", true,  ((p, d) -> pad(Date.get_hour(d), p, {pad_with_spaces}, 2)))
      , ("l", true,  ((p, d) -> pad(Date.Hour.convert_24h_to_12h(Date.get_hour(d)), p, {pad_with_spaces}, 2)))
      , ("m", true,  ((p, d) -> pad(Date.Month.to_int(Date.get_month(d)) + 1, p, {pad_with_zeros}, 2)))
      , ("M", true,  ((p, d) -> pad(Date.get_min(d), p, {pad_with_zeros}, 2)))
      , ("n", false, ((_, _) -> "\n"))
      , ("p", false, ((_, d) -> if Date.Hour.is_am(Date.get_hour(d)) then "AM" else "PM"))
      , ("P", false, ((_, d) -> if Date.Hour.is_am(Date.get_hour(d)) then "am" else "pm"))
      , ("S", true,  ((p, d) -> pad(Date.get_sec(d), p, {pad_with_zeros}, 2)))
      , ("t", false, ((_, _) -> "\t"))
      , ("u", false, ((_, d) -> wd = Date.Weekday.to_int(Date.get_weekday(d)); Int.to_string(if wd == 0 then 7 else wd)))
      , ("w", false, ((_, d) -> Int.to_string(Date.Weekday.to_int(Date.get_weekday(d)))))
      , ("x", true,  ((p, d) -> pad(Date.get_msec(d), p, {pad_with_zeros}, 3)))
      , ("y", false, ((_, d) -> pad(mod(Date.get_year(d), 100), {pad_with_zeros}, {pad_with_zeros}, 2)))
      , ("Y", true,  ((p, d) -> pad(Date.get_year(d), p, {pad_with_spaces}, 4)))
      , ("z", true, ((_, _) -> Date.get_local_timezone()))
      ]

    padding_flag_parser = parser
      | "-" -> {do_not_pad}
      | "_" -> {pad_with_spaces}
      | "0" -> {pad_with_zeros}
      | Rule.succeed -> {no_padding_info}

    parse_directive_with((d, accepts_padding, f)) =
      directive_char = Parser.of_string(d)
      parser pad_info=padding_flag_parser directive_char ->
        if accepts_padding || pad_info == {no_padding_info} then
          {success = date -> f((pad_info, date))}
        else
          {failure = "Directive %{d} does not accept a padding flag"}

    parse_directive(dirs : list) =
      match dirs with
      | [] ->
          (parser c=(.) .* -> { failure = "Unknown directive '%{c}'" })
      | [x | xs] ->
          parser
          | v={parse_directive_with(x)} -> v
          | v={parse_directive(xs)} -> v

    parse_segment = parser
       // for every '%' character we try to interpret it with our list of directives
      | [%] r={parse_directive(directives)} -> r
       // we leave the segments of non-percent characters uninterpreted
      | r=(![%] .)+ -> {success = _ -> Text.to_string(Text.ltconcat(r))}

    combine_results(f, r1, r2) =
      match (r1, r2) : (outcome, outcome) with
      | ({success=s1}, {success=s2}) -> {success = f(s1, s2)}
      | ({failure=e1}, {failure=e2}) -> {failure = e1 ^ "\n" ^ e2}
      | ({failure=_}, _) -> r1
      | (_, {failure=_}) -> r2

    combine_results_printer(r1, r2) =
      f(s1, s2) = date -> s1(date) ^ s2(date)
      combine_results(f, r1, r2)

    combine_results_abbrev(r1, r2) =
      f(s1, s2) = s1 ^ s2
      combine_results(f, r1, r2)

    format_parser =
      parser l={parse_segment}* -> List.fold_backwards(combine_results_printer, l, {success=_ -> ""})

    unfold_abbreviations_with(abbrevs : list) =
      match abbrevs with
      | [] ->
           // no matching abbreviation - parsing fails
          (parser Rule.fail -> error("Date.ToString.unfold_abbreviations_with"))
      | [(x_short, x_long) | xs] ->
          directive_char = Parser.of_string(x_short)
          parser
          | pad=padding_flag_parser directive_char ->
             if pad == {no_padding_info} then
               {success = x_long}
             else
               {failure = "Abbreviation '{x_short}' does not accept padding prefix"}
          | r={unfold_abbreviations_with(xs)} -> r

    unfold_abbreviation = parser
      | "%%" -> {success = "%%"}
       // try to unfold abbreviations
      | "%" r={unfold_abbreviations_with(abbreviations)} -> r
       // and leave the rest untouched
      | c=(.) -> {success = Text.to_string(c)}

    unfold_abbreviations =
      parser l={unfold_abbreviation}* -> List.fold_backwards(combine_results_abbrev, l, {success=""})

    generate_printer(format) : outcome(Date.date -> string, string) =
      match Parser.parse(unfold_abbreviations, format) with
      | {success=format_no_abbrevs} -> Parser.parse(format_parser, format_no_abbrevs)
      | ~{failure} -> ~{failure}

  }}

  // -------------------------------------------------------

  /**
   * {1 Parsing dates}
  **/

  OfString = {{

    parse_month(short, m) =
      if m == 12 then
        parser
        | Rule.fail -> error("Date.OfString.parse_month")
      else
        month = Date.Month.of_int(m)
        month_str = Date.Month.to_string(month)
        month_str = if short then String.sub(0, 3, month_str) else month_str
        month_parser = Parser.of_string(month_str)
        parser
        | month_parser -> d -> ({d with month = month} : Date.human_readable)
        | r={parse_month(short, m+1)} -> r

    parse_wday(short, w) =
      if w == 7 then
        parser
        | Rule.fail -> error("Date.OfString.parse_wday")
      else
        wday = Date.Weekday.of_int(w)
        wday_str = Date.Weekday.to_string(wday)
        wday_str = if short then String.sub(0, 3, wday_str) else wday_str
        wday_parser = Parser.of_string(wday_str)
        parser
        | wday_parser -> d -> ({d with wday = wday} : Date.human_readable)
        | r={parse_wday(short, w+1)} -> r

    id(d) = d

    parse_num(f) =
      parser Rule.ws v=Rule.natural -> f(v)

    update_am(d : Date.human_readable) = {d with h = if d.h == 12 then  0 else d.h}
    update_pm(d : Date.human_readable) = {d with h = if d.h == 12 then 12 else d.h + 12}

    @both_implem directives : list((string,Parser.general_parser(Date.human_readable -> Date.human_readable))) =
      [ ("%", parser "%" -> id)
      , ("a", parse_wday(true, 0))
      , ("A", parse_wday(false, 0))
      , ("b", parse_month(true, 0))
      , ("B", parse_month(false, 0))
//      , ('C', *not supported for parsing*
      , ("d", parse_num(v -> d -> {d with day=v}))
      , ("H", parse_num(v -> d -> {d with h = v}))
      , ("I", parse_num(v -> d -> {d with h = v}))
      , ("k", parse_num(v -> d -> {d with h = v}))
      , ("l", parse_num(v -> d -> {d with h = v}))
      , ("m", parse_num(v -> d -> {d with month = Date.Month.of_int(v-1)}))
      , ("M", parse_num(v -> d -> {d with min = v}))
      , ("n", parser "\n" -> id)
      , ("p", parser "AM" -> update_am | "PM" -> update_pm)
      , ("P", parser "am" -> update_am | "pm" -> update_pm)
      , ("S", parse_num(v -> d -> {d with s = v}))
      , ("t", parser "\t" -> id)
//      , ('u', *not supported for parsing*
//      , ('w', *not supported for parsing*
      , ("x", parse_num(v -> d -> {d with ms = v}))
      , ("y", parse_num(v -> d -> {d with year=if v < 70 then 2000 + v else 1900 + v}))
      , ("Y", parse_num(v -> d -> {d with year=v}))
//      , ("z", *not supported for parsing*
      ]

    parse_directive_with((d, p)) =
      directive_char = Parser.of_string(d)
      parser directive_char -> p

    parse_directive(dirs : list) =
      match dirs with
      | [] ->
          (parser c=(.) .* -> {failure = "Unknown directive '%{c}'"})
      | [x | xs] ->
          parser
            | v={parse_directive_with(x)} -> {success = v}
            | v={parse_directive(xs)} -> v

    parse_token = parser
       // we process directives
      | "%" ToString.padding_flag_parser r={parse_directive(directives)} -> r
       // when we see some spaces (>0) in the format we need to consume some spaces (>0) in the input
      | " "+ -> {success = parser " "+ -> id}
       // for any other character we expect it verbatim in the input
      | c=(.) ->
          cp = Parser.of_string(Text.to_string(c))
          {success = parser cp -> id}

    combine_results(r1, r2) =
      match (r1, r2) : (outcome, outcome) with
      | ({success=s1}, {success=s2}) -> {success = parser v1=s1 v2=s2 -> /*v2 @ v1*/ (x -> v2(v1(x)))}
      | ({failure=e1}, {failure=e2}) -> {failure=e1 ^ "\n" ^ e2}
      | ({failure=_}, _) -> r1
      | (_, {failure=_}) -> r2

    date_parser : Parser.general_parser(outcome) = parser
      | ts=parse_token* -> List.fold_backwards(combine_results, ts, {success = parser Rule.succeed -> id})

    generate_scanner(format) =
      match Parser.parse(ToString.unfold_abbreviations, format) with
      | {success=format_no_abbrevs} ->
          Parser.parse(date_parser, format_no_abbrevs)
      | ~{failure} -> ~{failure}

    of_string(date_parser, input, init_date) =
      match Parser.try_parse(date_parser, input) with
      | {none} -> {none}
      | ~{some = f} ->
        hrd = f(Date.to_human_readable(init_date))
        some(Date.of_human_readable(hrd))

  }}

}}
