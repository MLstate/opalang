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
module A = Base.Arg

module Arg = struct
  let no_pattern_optim = ref false
  let no_alpha_renaming = ref false
  let bundle = ref None

  let private_options = [
    "--qmljs-no-pattern-optim",
    A.Unit (fun () -> no_pattern_optim := true ),
    " Disable all optimisations for patterns matching" ;

    "--qmljs-no-alpha-renaming",
    A.Unit (fun () -> no_alpha_renaming := true ),
    " disable alpharenaming for local bindings" ;
  ]

  let public_options = [
    "--bundle", A.String (fun b -> bundle := Some b),
    " Create a bundle of the application"
  ]

  let options = public_options @
    if BuildInfos.is_release then [] else private_options

end


let no_pattern_optim () = ! Arg.no_pattern_optim
let no_alpha_renaming () = ! Arg.no_alpha_renaming
let bundle () = ! Arg.bundle
