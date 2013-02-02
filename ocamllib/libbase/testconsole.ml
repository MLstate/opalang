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
let _ =
  let reaction = function
    | Some (ConsoleParser.Directive s) ->
        print_endline "/* parsed directive */";
        print_endline s
    | Some (ConsoleParser.Code s) ->
        print_endline "/* parsed code */";
        print_endline s
    | None -> ()
  in
  let t = ConsoleParser.create () in
  try
    while true do
      reaction (ConsoleParser.accumulate t (input_line stdin))
    done
  with
  | End_of_file -> reaction (ConsoleParser.flush t)
