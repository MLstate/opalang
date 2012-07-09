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

(**
   Utils for main

   @author Mathieu Barbin
   @author Mehdi Bouaziz
   @author Cédric Soulas
*)

module E = OpaEnv

(**
   {6 Compiler options}
*)

module If =
struct

  (* alphabetic order *)

  let closure ~options _env = options.E.closure
  let constant_sharing ~options _env = options.E.constant_sharing
  let constant_sharing_client ~options _env = options.E.constant_sharing_client
  let cps ~options _env = options.E.cps
  let cps_client ~options _env = options.E.cps_client

  let explicit_instantiation ~options _env = options.E.explicit_instantiation

  let generate_interface ~options _env = options.E.generate_interface

  let no_closure ~options _env = not options.E.closure
  let no_discard_of_unused_stdlib ~options _env = options.E.no_discard_of_unused_stdlib
  let no_server ~options _env = Option.default false options.E.no_server

  let undot ~options _env = options.E.undot

  let server ~options env = not (no_server ~options env)
  let slicer_test ~options _env = options.E.slicer_test
  let stdlib ~options _env = options.E.stdlib
  let separated ~options:_ _env = not (ObjectFiles.global_compilation ())
  let compilation ~options:_ _env =
    match ObjectFiles.compilation_mode () with
    | `compilation -> true
    | `linking | `prelude | `init -> false

  let init ~options:_ _env =
    match ObjectFiles.compilation_mode () with
    | `init -> true
    | `linking | `prelude | `compilation -> false
  let full_separation ~options:_ _env =
    ObjectFiles.Arg.is_fully_separated ()

  let i18n_template ~options _env = E.i18n_template options

  let database engine ~options _env =
    ignore options;
    List.mem engine (Pass_DbEngineImportation.get_engines ())
end

module Switch =
struct
  let back_end ~options _env = options.E.back_end
end

let if_not f_cond ~options env = not (f_cond ~options env)
let if_and f_cond1 f_cond2 ~options env = (f_cond1 ~options env) && (f_cond2 ~options env)
