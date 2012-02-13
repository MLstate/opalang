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
(*
    @author Adam Koprowski
**)

(* depends *)
module List = BaseList

let emph s = "[1m" ^ s ^ "[22m"
let strong s = "[4m" ^ emph s ^ "[24m"

let red s = "[31m" ^ s ^ "[39m"
let blue s = "[34m" ^ s ^ "[39m"
let green s = "[32m" ^ s ^ "[39m"

let emph_fmt fmt = "[1m" ^^ fmt ^^ "[22m"
let strong_fmt fmt = "[4m" ^^ emph_fmt fmt ^^ "[24m"
let blue_fmt s = "[34m" ^^ s ^^ "[39m"

let build_logger fn label format = fn (strong_fmt label ^^ " " ^^ format)

let walker () =
  let size = 12 in
  let seq = "o" in
  let make_frame i j = Printf.sprintf "|%s%c%s|" (String.make i ' ') (String.get seq j) (String.make (size - i - 1) ' ') in
  let make_entry i = List.init (String.length seq) (make_frame (if i >= size then 2*size - i - 2 else i)) in
  let al = List.concat (List.init (size + size - 2) make_entry) in
  let a = Array.map (fun s -> [| s |]) (Array.of_list al) in
  ConsoleAnim.init ~och:stdout ~auto_walking:false a a
