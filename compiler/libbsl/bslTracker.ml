(*
    Copyright © 2011, 2012 MLstate

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

(* depends *)
module Format = Base.Format

module Printer =
struct
  let bymap fmt e =
    Format.fprintf fmt "/* printer: --print bymap */@\n@\n" ;
    BslLib.BSL.ByPassMap.pp fmt e.BslLib.bymap ;
    Format.fprintf fmt "@."
end

let define = PassHandler.define_printer
let bymap_id = define "bymap"

let printers extract _ =
  let make_bymap fct fmt env =
    fct fmt (extract env) in
  [
    bymap_id, make_bymap Printer.bymap ;
  ]
