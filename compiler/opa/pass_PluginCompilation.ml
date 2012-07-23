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

let handle_open_out file =
  try open_out_bin file
  with
  | Sys_error s ->
    OManager.error
      "@[<2>@{<bright>bslregister@}: cannot open_out @{<bright>%s@}:@\n%s@]"
      file s

let handle_close_out file oc =
  try close_out oc
  with
  | Sys_error s ->
    OManager.error
      "@[<2>@{<bright>bslregister@}: cannot close_out @{<bright>%s@}:@\n%s@]"
      file s

let output filename pp a =
  OManager.verbose "writing file @{<bright>%S@}..." filename ;
  let oc = handle_open_out filename in
  pp oc a ;
  handle_close_out filename oc ;
  ()

let process env =
  let plugin_name =
    Filename.basename (File.chop_extension env.PH.options.O.target) in
  let js_files = env.PH.options.O.client_plugin_files in
  let nodejs_files = env.PH.options.O.server_plugin_files in
  let modular_plugins = env.PH.options.O.modular_plugins in
  let nodejs_package = plugin_name ^
    BslConvention.Suffix.nodejspackage ^ ".js" in
  if List.is_empty js_files && List.is_empty nodejs_files then env else
  let package_version = env.PH.options.O.package_version in
  let opp_dir = File.from_pattern "%b.opp" env.PH.options.O.target in
  if not (File.check_create_path opp_dir) then
    OManager.error "cannot create plugin directory %s" opp_dir;
  let plugin_file =
    Filename.concat opp_dir
      (plugin_name ^ BslConvention.Suffix.plugin ^ ".ml") in
  let runtime_file =
    Filename.concat opp_dir
      (plugin_name ^ BslConvention.Suffix.mlruntime ^ ".ml") in
  let package_json_file = Filename.concat opp_dir "package.json" in
  let marshalplugin_file = Filename.concat opp_dir
    (BslConvention.Suffix.marshalplugin ^ "." ^
       BslConvention.Extension.bypass) in

  let options = {
    BI.
    (* Use minimal options for now *)
    basename = plugin_name;
    bypass_plugins = [];
    check_style = false;
    js_files;
    nodejs_files;
    js_validator = None;
    ml_plugin_filename = plugin_file;
    ml_runtime_filename = runtime_file;
    modular_plugins;
    unsafe_js = true;
    unsafe_opa = true;
  } in

  (* Ignore PP for now *)
  let session = BR.create ~options in

  let finalized = BR.finalize session in

  (* Generate files *)
  let package_desc = JsUtils.basic_package_json ~version:package_version
    opp_dir nodejs_package in
  begin
    match
      File.pp_output plugin_file Format.pp_print_string package_desc
    with
    | None -> ()
    | Some error -> OManager.error "Couldn't write package.json: %s\n" error
  end;
  output marshalplugin_file BR.out_ml_marshal_plugin finalized;
  output (Filename.concat opp_dir nodejs_package)
    BR.out_nodejs_package finalized;
  let out_package_json oc _ =
    let package_desc = JsUtils.basic_package_json ~version:package_version
      (Filename.basename opp_dir) nodejs_package in
    Printf.fprintf oc "%s" package_desc in
  output package_json_file out_package_json finalized;
  env
