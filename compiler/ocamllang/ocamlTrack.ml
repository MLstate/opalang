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
(* CF mli *)

(* depends *)
module Format = Base.Format

let size fmt code =
  let i = List.fold_left (fun acc e -> acc + OcamlUtils.Misc.size  e) 0 code in
  Format.fprintf fmt
    "%d declarations\n%d nodes" (List.length code) i

let size_id = PassHandler.define_printer "size"

let printers extract _options =
  let make fct fmt env = fct fmt (extract env) in
  [
    size_id, make size ;
  ]
