(*
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
*)
(*
    @author Adam Koprowski
**)

let _ =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "Usage: %s FILE.java\n" Sys.argv.(0)
  else
    let s = File.content Sys.argv.(1) in
    try 
      let pos, _ = Java.parse_java_compilationunit s in
      Printf.printf "OK [%d]\n" pos
    with
      _ -> Printf.eprintf "FAIL!"; exit 1
