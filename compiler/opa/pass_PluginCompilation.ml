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

(** Generate plugin files from JS given to the opa compiler. Mostly
    copied from bslregister.ml.

    @author Arthur Azevedo de Amorim
*)

module BR = BslRegisterLib
module BI = BslInterface
module PH = PassHandler
module O = OpaEnv
module List = BaseList

let process env =
  let js_files = env.PH.options.O.client_plugin_files in
  let nodejs_files = env.PH.options.O.server_plugin_files in
  let all_files = js_files @ nodejs_files in
  if not (List.is_empty js_files && List.is_empty nodejs_files) then (
    (* TODO: ensure we are in qmljs, and not qmlflat *)
    let basename = None in
    let ml_plugin_filename =
      "bundled" ^ BslConvention.Suffix.plugin ^ ".ml" in
    let ml_runtime_filename =
      "bundled" ^ BslConvention.Suffix.mlruntime ^ ".ml" in
    let options = {
      BI.
      basename;
      bypass_plugins = [];
      check_style = false;
      js_files;
      nodejs_files;
      js_validator = None;
      ml_plugin_filename;
      ml_runtime_filename;
      modular_plugins = false;
      unsafe_js = false;
      unsafe_opa = false;
      js_classic_bypass_syntax = env.PH.options.O.js_classic_bypass_syntax;
    } in

    let session = BR.create ~options in
    let session = List.fold_left BR.preprocess_file session all_files in
    let fin = BR.finalize session in
    let plugin = BR.plugin fin in

    BslPluginTable.store plugin;
  );

  env
