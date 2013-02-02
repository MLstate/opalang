(*
    Copyright © 2011 MLstate

    This file is part of Opa.

    Opa is free software: you can redistribute it and/or modify it under the
    terms of the GNU Affero General Public License, version 3, as published by
    the Free Software Foundation.

    Opa is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
    FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for
    more details.

    You should have received a copy of the GNU Affero General Public License
    along with Opa. If not, see <http://www.gnu.org/licenses/>.
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
