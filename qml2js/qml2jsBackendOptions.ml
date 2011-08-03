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
module A = Base.Arg

module Arg = struct
  let no_pattern_optim = ref false
  let no_alpha_renaming = ref false
  let options = [
    "--qmljs-no-pattern-optim",
    A.Unit (fun () -> no_pattern_optim := true ),
    " Disable all optimisations for patterns matching" ;

    "--qmljs-no-alpha-renaming",
    A.Unit (fun () -> no_alpha_renaming := true ),
    "disable alpharenaming for local bindings" ;
  ]
end


let no_pattern_optim () = ! Arg.no_pattern_optim
let no_alpha_renaming () = ! Arg.no_alpha_renaming
