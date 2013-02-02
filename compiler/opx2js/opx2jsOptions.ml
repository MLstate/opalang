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

module Arg = Base.Arg
module Format = BaseFormat
module String = BaseString

type t = {
  build_dir : string;
  extra_path : string list;
  packages : string list;
}

let default_options = {
  packages = [];
  build_dir = Sys.getcwd ();
  extra_path = [];
}

let parsed = ref false

let r = ref default_options

let add_packages f =
  r := {!r with packages = f::!r.packages}

let add_extra_path f =
  r := {!r with extra_path = f::!r.extra_path}

let set_build_dir build_dir = r := {!r with build_dir}

let options = [
  "--build-dir", Arg.String set_build_dir,
  " Set the build directory";

  "-I", Arg.String add_extra_path,
  " Add the given directory to the list of directories searched";

] @ OManager.Arg.options

let options = Arg.sort ( Arg.align options )

let anon_fun arg = add_packages arg

let usage = "Usage: opa export [options] opa.packages.to.export...\n\nWhere options are:"

let get_options () =
  if not !parsed then (
    Arg.parse options anon_fun usage;
    parsed := true;
    ObjectFiles.set_relative_stdlib "stdlib.qmljs";
    ObjectFiles.set_extrapaths ~no_stdlib:false !r.extra_path;
  );
  !r

let print_help () =
  Arg.usage options usage
