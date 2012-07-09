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

module Format = Base.Format

type 'a pprinter = 'a Format.pprinter

let pp = Format.fprintf

(*
  <!> Rather than trying to add some more option,
  OcamlPrint defines his own pp_parameters.
*)
let pp_parameters pp name fmt params =
  if (params<>[])
  then Format.fprintf fmt "%s(%a)" name (Format.pp_list ", " pp) params
  else Format.pp_print_string fmt name

let pp_field sep pp fmt f t =
  Format.pp_print_string fmt f;
  Format.fprintf fmt sep;
  pp fmt t

let pp_field_cp sep pp fmt (f, t) = pp_field sep pp fmt f t
