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

(** Parses a list of Opa files taken from stdin & prints some basic timing info *)

let measureTime f =
  let startT = Sys.time () in
  let res = f () in
  let endT = Sys.time () in
  res, endT -. startT

let opa_parse fn =
  let go () =
    try
      let _ = OpaParser.parse_file fn in
      true
  with
    e -> false
  in
  Printf.eprintf "Parsing %s... %!" fn;
  let res, t = measureTime go in
  Printf.eprintf "%s (%4.2fs.)%!\n" (if res then "OK" else "FAIL") t;
  res

let _ =
  let inc = stdin in
  let rec file_list () =
    try
      let fn = input_line inc in
      fn :: file_list ()
    with
      End_of_file -> []
  in
  let files = file_list () in
  let total = List.length files in
  let rec go n files () =
    match files with
    | [] -> 0
    | fn::fns ->
	Printf.eprintf "[%4d/%4d] " n total;
	let success = opa_parse fn in
	let res = go (n + 1) fns () in
        if success then
          res
        else
          res + 1
        in
  let fail, t = measureTime (go 1 files) in
  Printf.eprintf "Total time of parsing %d files [%d failures]: %4.2fsec.\n" total fail t
