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
(* CF mli *)

(* depends *)
module Arg = Base.Arg

(* -- *)

(* n *)
let no_cache = ref false
let no_optim = ref false

let options = [

  (* n *)

  "--qmlc-no-cache",
  Arg.Set no_cache,
  " Do not activate cache-field-access optimizations of qmlc"
  ;

  "--qmlc-no-optim",
  Arg.Set no_optim,
  " Do not activate all optimizations of qmlc"
  ;

]

let no_cache () = !no_cache
let no_optim () = !no_optim
