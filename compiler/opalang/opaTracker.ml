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
module Printer =
struct
  let code_uids fmt code =
    Format.fprintf fmt "/* printer: --print code */@\n@\n" ;
    OpaPrint.ident#code fmt code ;
    Format.fprintf fmt "@."

  let full_ident fmt code =
    Format.fprintf fmt "/* printer: --print full_ident */@\n@\n" ;
    OpaPrint.full_ident#code fmt code ;
    Format.fprintf fmt "@."

  let readable_ident fmt code =
    Format.fprintf fmt "/* printer: --print readable_ident */@\n@\n" ;
    OpaPrint.readable_ident#code fmt code ;
    Format.fprintf fmt "@."

  let code_nonuid fmt code =
    Format.fprintf fmt "/* printer: --print code */@\n@\n" ;
    OpaPrint.string#code fmt code ;
    Format.fprintf fmt "@."

  let size fmt code =
    Format.fprintf fmt
      "%d declarations@\n%d nodes@."
      (OpaWalk.Code.length code)
      (OpaWalk.Code.size code)
end

let define = PassHandler.define_printer
let code_id = define "code"
let full_ident_id = define "full_ident"
let readable_ident_id = define "readable_ident"
let size_id = define "size"
(* let declaration_id = define "declaration" *)
(* let annotation_id = define "annotation" *)
(* let tracked_id = define "tracked" *)

let printers_uids extract options =
  ignore(options);
  let make fct fmt env = fct fmt (extract env) in
  [
    code_id, make Printer.code_uids;
    full_ident_id, make Printer.full_ident;
    readable_ident_id, make Printer.readable_ident;
    size_id, make Printer.size;
  ]

let printers_nonuid extract options =
  ignore(options);
  let make fct fmt env = fct fmt (extract env) in
  [
    code_id, make Printer.code_nonuid;
    size_id, make Printer.size;
  ]

module Tracker =
struct

end

let trackers (* extract *) _ _ =
  (* let make fct fmt env = fct fmt (extract env) in *)
  [
    (* directive_id, make Tracker.directive ; *)
    (* val_id, make Tracker.val_ ; *)
  ]
