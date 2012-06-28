/*
    Copyright © 2011 MLstate

    This file is part of OPA.

    OPA is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    OPA is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with OPA. If not, see <http://www.gnu.org/licenses/>.
*/

type Ansi.color =
   {black}
 / {red}
 / {green}
 / {yellow}
 / {blue}
 / {magenta}
 / {cyan}
 / {white}

Ansi = {{

  color(c) =
    match c with
    | {black} -> 0
    | {red} -> 1
    | {green} -> 2
    | {yellow} -> 3
    | {blue} -> 4
    | {magenta} -> 5
    | {cyan} -> 6
    | {white} -> 7

  foreground(c) = 30 + color(c)
  background(c) = 40 + color(c)

  open_color_code(c) = "\027[{foreground(c)}m"
  close_color_code = "\027[0m"

  /** all call to produce color come from here,
      so this global property is here to enforce that
      no any production are in color */
  @private __ignore_color = ServerReference.create(false)
  set_ignore_color(b) = ServerReference.set(__ignore_color,b)
  get_ignore_color() = ServerReference.get(__ignore_color)

  print =
    //if %%BslSyslog.os_type%%() != "Unix"
    //then
    //  (_:Ansi.color, s -> s)
    //else
      (c:Ansi.color, s ->
        if get_ignore_color()
        then s
        else
          match c with
          | {black} -> s
          | c -> "{open_color_code(c)}{s}{close_color_code}")

  string_of_color(c) =
    match c with
    | {black} -> "black"
    | {red} -> "red"
    | {green} -> "green"
    | {yellow} -> "yellow"
    | {blue} -> "blue"
    | {magenta} -> "magenta"
    | {cyan} -> "cyan"
    | {white} -> "white"

  color_of_string(s) =
    match s with
    | "black" -> {some={black}}
    | "red" -> {some={red}}
    | "green" -> {some={green}}
    | "yellow" -> {some={yellow}}
    | "blue" -> {some={blue}}
    | "magenta" -> {some={magenta}}
    | "cyan" -> {some={cyan}}
    | "white" -> {some={white}}
    | _ -> {none}

  symbols = [
    "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"
  ]

  uncolor(i) =
    match i with
    | 0 -> {black}
    | 1 -> {red}
    | 2 -> {green}
    | 3 -> {yellow}
    | 4 -> {blue}
    | 5 -> {magenta}
    | 6 -> {cyan}
    | 7 -> {white}
    | _ -> do @assert(false) {black}

}}
