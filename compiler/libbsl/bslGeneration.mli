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
   Wrapper around BslRegisterLib to make it easier to call from outside code.

   Most of this file used to be part of bslregister.ml. This was
   refactor to make it easier to call the plugin compiler from the
   rest of the code.
*)

type options = {
  (** A boolean to say if we want to generate the Plugin and the
      Loader file.  these files are used to link statically a plugin
      with an executable, so that there is no need to load dynamically
      a .bypass file for being able to load bypasses of a given plugin.

      Currently, the static mode is used only for the opabsl, and the
      resulting plugin is linked with opa.exe

      The loader file can be used for interpreter in ml only (opatop)
      to add new primitives.  *)
  static: bool;

  (** A boolean to say that generated files should be put in the
      build_dir directly, without storing them in a opp directory. *)
  no_opp: bool;

  (** Flag that says whether identifiers in the nodejs plugin should
      be exported into the global namespace or if they should be used
      as regular module exports (i.e. "exports.foo = foo;") *)
  modular_plugins: bool;

  build_dir: string;
  bsl_pref: string;
  auto_build: bool;
  check_style: bool;
  clean: bool;
  clean_would_only: bool;
  default_iformats: bool;
  extrapaths: string list;
  unsafe_js: bool;
  unsafe_opa: bool;
  bypass_plugins: BslDynlink.bypass_plugin_file list;
  files: string list;
  package_version: string;
  spec_process: string StringMap.t;
  ml_flags: string list;
  mlopt_flags: string list;
  js_validator: string option;
  js_validator_files: string list;
  js_validator_options: string list;
  pprocess: string option;
  js_classic_bypass_syntax: bool;
}

val default_opts : options

val process : options -> unit
