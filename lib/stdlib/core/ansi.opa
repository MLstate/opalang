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
 / {xterm:int}
 / {`default`}

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
    | {~xterm} -> 1000+xterm
    | {`default`} -> 9

  foreground(c) =
    match c with
    | {xterm=(x:int)} ->
      if x >= 0 && x <= 255
      then "38;5;{x}"
      else "39" // default colour
    | _ -> "{30 + color(c)}"
  background(c) =
    match c with
    | {xterm=(x:int)} ->
      if x >= 0 && x <= 255
      then "48;5;{x}"
      else "39" // default colour
    | _ -> "{40 + color(c)}"

  foreground_code(c) = "\027[{foreground(c)}m"
  background_code(c) = "\027[{background(c)}m"
  reset_code = "\027[0m"
  bold_code = "\027[1m"
  faint_code = "\027[2m" // Not widely supported
  italic_code = "\027[3m" // Not widely supported, sometime treated as inverse
  underline_code = "\027[4m"
  bold_off_code = "\027[21m" // Hardly ever supported
  normal_code = "\027[22m" // Neither bold nor faint
  underline_off_code = "\027[24m"
  blink_slow_code = "\027[5m"
  blink_rapid_code = "\027[6m" // Not widely supported
  blink_off_code = "\027[25m"
  inverse_code = "\027[7m"
  uninverse_code = "\027[27m"
  conceal_code = "\027[8m" // Not widely supported
  reveal_code = "\027[28m"
  crossed_out_code = "\027[9m" // Not widely supported
  not_crossed_out_code = "\027[29m" // Not widely supported
  framed_code = "\027[51m"
  encircled_code = "\027[52m"
  overlined_code = "\027[53m"
  not_framed_or_encircled_code = "\027[54m"
  not_overlined_code = "\027[55m"
  font_code(n) =
    if n >= 0 && n <= 9
    then "\027[1{n}m"
    else "\027[10m"

  erase_data_code(code) =
    match code with
    | {before} -> "\027[0J"
    | {after} -> "\027[1J"
    | {all} -> "\027[2J"

  erase_line_code(code) =
    match code with
    | {before} -> "\027[0K"
    | {after} -> "\027[1K"
    | {all} -> "\027[2K"

  cuu_code(n:int) = "\027[{n}A"
  cud_code(n:int) = "\027[{n}B"
  cuf_code(n:int) = "\027[{n}C"
  cub_code(n:int) = "\027[{n}D"
  cnl_code(n:int) = "\027[{n}E"
  cpl_code(n:int) = "\027[{n}F"
  cha_code(n:int) = "\027[{n}G"
  cup_code(n:int,m:int) = "\027[{n};{m}H"
  scp_code = "\027[s"
  rcp_code = "\027[u"
  hide_cursor_code = "\027[?25l"
  show_cursor_code = "\027[?25h"
  su_code(n:int) = "\027[{n}S"
  sd_code(n:int) = "\027[{n}T"

  @private args(pos0,s,len) =
    rec aux(pos,args) =
      add(n) = match args with [] -> [n] [x|rest] -> [x*10+n|rest]
      if pos < len
      then
        match String.get(pos, s) with
        | "0" -> aux(pos+1,add(0))
        | "1" -> aux(pos+1,add(1))
        | "2" -> aux(pos+1,add(2))
        | "3" -> aux(pos+1,add(3))
        | "4" -> aux(pos+1,add(4))
        | "5" -> aux(pos+1,add(5))
        | "6" -> aux(pos+1,add(6))
        | "7" -> aux(pos+1,add(7))
        | "8" -> aux(pos+1,add(8))
        | "9" -> aux(pos+1,add(9))
        | "," -> aux(pos+1,[0|args])
        | ")" -> (pos+1,args)
        | _ -> (pos0,[])
        end
      else (pos,args)
    if pos0 < len
    then
      match String.get(pos0, s) with
      | "(" -> aux(pos0+1,[])
      | _ -> (pos0,[])
      end
    else (pos0,[])

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
          | c -> "{foreground_code(c)}{s}{reset_code}")

  xform(s) =
    len = String.length(s)
    rec aux(pos, t) =
      if pos >= len
      then Text.to_string(t)
      else
        match String.get(pos, s) with
        | "%" ->
          arg1(pos,fn) =
            match args(pos,s,len) with
            | (pos,[n]) -> aux(pos, Text.insert_right(t,fn(n)))
            | (_,_) -> aux(pos+1, Text.insert_right(t,"%"))
            end
          if pos+1 < len
          then
            match String.get(pos+1,s) with
            | "0" -> aux(pos+2, Text.insert_right(t,reset_code))
            | "l" -> aux(pos+2, Text.insert_right(t,foreground_code({black})))
            | "r" -> aux(pos+2, Text.insert_right(t,foreground_code({red})))
            | "g" -> aux(pos+2, Text.insert_right(t,foreground_code({green})))
            | "y" -> aux(pos+2, Text.insert_right(t,foreground_code({yellow})))
            | "b" -> aux(pos+2, Text.insert_right(t,foreground_code({blue})))
            | "m" -> aux(pos+2, Text.insert_right(t,foreground_code({magenta})))
            | "c" -> aux(pos+2, Text.insert_right(t,foreground_code({cyan})))
            | "w" -> aux(pos+2, Text.insert_right(t,foreground_code({white})))
            | "t" -> arg1(pos+2, (xterm -> foreground_code({~xterm})))
            | "d" -> aux(pos+2, Text.insert_right(t,foreground_code({`default`})))
            | "L" -> aux(pos+2, Text.insert_right(t,background_code({black})))
            | "R" -> aux(pos+2, Text.insert_right(t,background_code({red})))
            | "G" -> aux(pos+2, Text.insert_right(t,background_code({green})))
            | "Y" -> aux(pos+2, Text.insert_right(t,background_code({yellow})))
            | "B" -> aux(pos+2, Text.insert_right(t,background_code({blue})))
            | "M" -> aux(pos+2, Text.insert_right(t,background_code({magenta})))
            | "C" -> aux(pos+2, Text.insert_right(t,background_code({cyan})))
            | "W" -> aux(pos+2, Text.insert_right(t,background_code({white})))
            | "T" -> arg1(pos+2, (xterm -> background_code({~xterm})))
            | "D" -> aux(pos+2, Text.insert_right(t,background_code({`default`})))
            | "O" -> aux(pos+2, Text.insert_right(t,bold_code))
            | "f" -> aux(pos+2, Text.insert_right(t,faint_code))
            | "i" -> aux(pos+2, Text.insert_right(t,italic_code))
            | "_" -> aux(pos+2, Text.insert_right(t,underline_code))
            | "o" -> aux(pos+2, Text.insert_right(t,bold_off_code))
            | "." -> aux(pos+2, Text.insert_right(t,underline_off_code))
            | "n" -> aux(pos+2, Text.insert_right(t,normal_code))
            | "k" -> aux(pos+2, Text.insert_right(t,blink_slow_code))
            | "K" -> aux(pos+2, Text.insert_right(t,blink_rapid_code))
            | "Q" -> aux(pos+2, Text.insert_right(t,blink_off_code))
            | "I" -> aux(pos+2, Text.insert_right(t,inverse_code))
            | "J" -> aux(pos+2, Text.insert_right(t,uninverse_code))
            | "-" -> aux(pos+2, Text.insert_right(t,conceal_code))
            | "+" -> aux(pos+2, Text.insert_right(t,reveal_code))
            | "X" -> aux(pos+2, Text.insert_right(t,crossed_out_code))
            | "x" -> aux(pos+2, Text.insert_right(t,not_crossed_out_code))
            | "[" -> aux(pos+2, Text.insert_right(t,framed_code))
            | "@" -> aux(pos+2, Text.insert_right(t,encircled_code))
            | "¬" -> aux(pos+2, Text.insert_right(t,overlined_code))
            | "]" -> aux(pos+2, Text.insert_right(t,not_framed_or_encircled_code))
            | "~" -> aux(pos+2, Text.insert_right(t,not_overlined_code))
            | "F" ->
              if pos+2 < len
              then
                match String.get(pos+2,s) with
                | "1" -> aux(pos+3,Text.insert_right(t,font_code(1)))
                | "2" -> aux(pos+3,Text.insert_right(t,font_code(2)))
                | "3" -> aux(pos+3,Text.insert_right(t,font_code(3)))
                | "4" -> aux(pos+3,Text.insert_right(t,font_code(4)))
                | "5" -> aux(pos+3,Text.insert_right(t,font_code(5)))
                | "6" -> aux(pos+3,Text.insert_right(t,font_code(6)))
                | "7" -> aux(pos+3,Text.insert_right(t,font_code(7)))
                | "8" -> aux(pos+3,Text.insert_right(t,font_code(8)))
                | "9" -> aux(pos+3,Text.insert_right(t,font_code(9)))
                | _ -> aux(pos+3,Text.insert_right(t,font_code(0)))
                end
              else aux(pos+1,Text.insert_right(t,"%"))
            | "E" ->
              if pos+2 < len
              then
                match String.get(pos+2,s) with
                | "0" -> aux(pos+3,Text.insert_right(t,erase_data_code({after})))
                | "1" -> aux(pos+3,Text.insert_right(t,erase_data_code({before})))
                | _ -> aux(pos+3,Text.insert_right(t,erase_data_code({all})))
                end
              else aux(pos+1,Text.insert_right(t,"%"))
            | "e" ->
              if pos+2 < len
              then
                match String.get(pos+2,s) with
                | "0" -> aux(pos+3,Text.insert_right(t,erase_line_code({after})))
                | "1" -> aux(pos+3,Text.insert_right(t,erase_line_code({before})))
                | _ -> aux(pos+3,Text.insert_right(t,erase_line_code({all})))
                end
              else aux(pos+1,Text.insert_right(t,"%"))
            | "#" ->
              if pos+2 < len
              then
                match String.get(pos+2,s) with
                | "U" -> arg1(pos+3,cuu_code)
                | "D" -> arg1(pos+3,cud_code)
                | "F" -> arg1(pos+3,cuf_code)
                | "B" -> arg1(pos+3,cub_code)
                | "N" -> arg1(pos+3,cnl_code)
                | "P" -> arg1(pos+3,cpl_code)
                | "H" -> arg1(pos+3,cha_code)
                | _ -> aux(pos+1,Text.insert_right(t,"%"))
                end
              else aux(pos+1,Text.insert_right(t,"%"))
            | "P" ->
              match args(pos+2,s,len) with
              | (pos,[m,n]) -> aux(pos, Text.insert_right(t,cup_code(n,m)))
              | (_,_) -> aux(pos+1, Text.insert_right(t,"%"))
              end
            | "^" -> arg1(pos+2, su_code)
            | "v" -> arg1(pos+2, sd_code)
            | "<" -> aux(pos+2, Text.insert_right(t,scp_code))
            | ">" -> aux(pos+2, Text.insert_right(t,rcp_code))
            | "h" -> aux(pos+2, Text.insert_right(t,hide_cursor_code))
            | "H" -> aux(pos+2, Text.insert_right(t,show_cursor_code))
            | _ -> aux(pos+1,Text.insert_right(t,"%"))
          else
            Text.to_string(Text.insert_right(t,"%"))
        | s -> aux(pos+1,Text.insert_right(t,s))
        end
    aux(0, Text.cons(""))

  printc =
    (s ->
      if get_ignore_color()
      then s
      else xform(s))

  jlog(s) = Debug.jlog(printc(s))

  string_of_color(c:Ansi.color) : string =
    match c with
    | {black} -> "black"
    | {red} -> "red"
    | {green} -> "green"
    | {yellow} -> "yellow"
    | {blue} -> "blue"
    | {magenta} -> "magenta"
    | {cyan} -> "cyan"
    | {~xterm} -> "xterm{xterm}"
    | {white} -> "white"
    | {`default`} -> "default"

  color_of_string(s) : option(Ansi.color) =
    match s with
    | "black" -> {some={black}}
    | "red" -> {some={red}}
    | "green" -> {some={green}}
    | "yellow" -> {some={yellow}}
    | "blue" -> {some={blue}}
    | "magenta" -> {some={magenta}}
    | "cyan" -> {some={cyan}}
    | "white" -> {some={white}}
    | "default" -> {some={`default`}}
    | _ ->
      if String.has_prefix("xterm",s)
      then
        match Int.of_string_opt(String.sub(5,String.length(s)-5,s)) with
        | {some=xterm} -> if xterm < 256 then {some={~xterm}} else {none}
        | {none} -> {none}
        end
      else {none}

  symbols = [
    "black", "red", "green", "yellow", "blue", "magenta", "cyan", "white", "xterm", "default"
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
    | 9 -> {`default`}
    | _ ->
      if i >= 1000 && i <= 1255
      then {xterm=i-1000}
      else do @assert(false) {black}

}}
