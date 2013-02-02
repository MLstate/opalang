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
let opt_expr = ref false

let _ =
  Arg.parse
    (Arg.align
       ["--expr",Arg.Unit (fun () -> opt_expr := true),"print expressions as expressions (do not insert \"do\")"]
    )
    (fun _ -> ())
    (Printf.sprintf "%s: simple qml to opa preprocessing\nUsage: %s [options] <filein.opa >fileout.opa\nOptions:" Sys.argv.(0) Sys.argv.(0))

let _ =
  if !opt_expr then
    let expr = OpaParser.expr (File.channel_contents stdin) in
    OpaPrint.string#expr Format.std_formatter expr
  else
    let code = OpaParser.code (File.channel_contents stdin) in
    OpaPrint.string#code Format.std_formatter code
