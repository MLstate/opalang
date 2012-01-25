(*
    Copyright © 2012 MLstate

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

type t = {
  no_absolute : bool;
}

let default = {
  no_absolute = false;
}

let x = ref default

let get () = !x

let options = [
  ("--no-absolute-plugin", Base.Arg.Unit (fun () -> x := {no_absolute = true})
     , "Plugin paths should not be considered as absolute path (package will not refers to plugin with an absolute path)")
]
