/*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
