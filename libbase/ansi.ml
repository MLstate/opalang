(*
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
*)

type color =
    [ `black
    | `red
    | `green
    | `yellow
    | `blue
    | `magenta
    | `cyan
    | `white ]

let color = function
  | `black -> 0
  | `red -> 1
  | `green -> 2
  | `yellow -> 3
  | `blue -> 4
  | `magenta -> 5
  | `cyan -> 6
  | `white -> 7

let foreground c = 30 + color c
let background c = 40 + color c

let open_color_code c = Printf.sprintf "\027[%dm" (foreground c)
let close_color_code = "\027[0m"

(** all call to produce color come from here,
    so this global propertie is here to enforce that
    no any production are in color *)
let set_ignore_color, get_ignore_color =
  let __ignore_color = ref false in
  (fun b -> __ignore_color := b), (fun () -> !__ignore_color)

let print =
  if Sys.os_type = "Win32" then
    fun (_:color) s -> s
  else
    fun (c:color) s ->
      if get_ignore_color () then s else
        match c with
        | `black -> s
        | c -> Printf.sprintf "%s%s%s" (open_color_code c) s close_color_code

let string_of_color = function
  | `black -> "black"
  | `red -> "red"
  | `green -> "green"
  | `yellow -> "yellow"
  | `blue -> "blue"
  | `magenta -> "magenta"
  | `cyan -> "cyan"
  | `white -> "white"

let color_of_string = function
  | "black" -> Some `black
  | "red" -> Some `red
  | "green" -> Some `green
  | "yellow" -> Some `yellow
  | "blue" -> Some `blue
  | "magenta" -> Some `magenta
  | "cyan" -> Some `cyan
  | "white" -> Some `white
  | _ -> None

let symbols = [
  "black"; "red"; "green"; "yellow"; "blue"; "magenta"; "cyan"; "white"
]

let uncolor = function
  | 0 -> `black
  | 1 -> `red
  | 2 -> `green
  | 3 -> `yellow
  | 4 -> `blue
  | 5 -> `magenta
  | 6 -> `cyan
  | 7 -> `white
  | _ -> assert false
