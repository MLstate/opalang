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
type relevant_options =
    { back_end : OpaEnv.available_back_end
    ; js_back_end : string
    ; closure : bool
    ; cps : bool
    ; cps_client : bool
    ; cps_toplevel_concurrency : bool
    ; explicit_instantiation : bool
    ; value_restriction : [`disabled | `normal | `strict] }

module S =
struct
  type t = relevant_options
  let pass = "CheckOptionsConsistency"
  let pp f _ = Format.pp_print_string f "<dummy>"
end

module R = ObjectFiles.Make(S)

let project options =
  { back_end = options.OpaEnv.back_end
  ; js_back_end =
      (let module B = (val options.OpaEnv.js_back_end : Qml2jsOptions.JsBackend) in
       B.name)
  ; closure = options.OpaEnv.closure
  ; cps = options.OpaEnv.cps
  ; cps_client = options.OpaEnv.cps_client
  ; cps_toplevel_concurrency = options.OpaEnv.cps_toplevel_concurrency
  ; explicit_instantiation = options.OpaEnv.explicit_instantiation
  ; value_restriction = options.OpaEnv.value_restriction }

let with_or_without = function
  | true -> "with"
  | false -> "without"
let would = function
  | true -> "wouldn't"
  | false -> "would"



(* ************************************************************************** *)
(** {b Descr}: String representing the status of the {b --value-restriction}
    option invoked when a package is built. This function is used to report
    inconsistency errors when some packages are compiled with different values
    of the {b --value-restriction} option.
    {b Visibility}: Not exported outside this module.                         *)
(* ************************************************************************** *)
let value_restiction_option_status = function
  | `disabled -> "disabled"
  | `normal -> "normal"
  | `strict -> "strict"



let diff package options1 options2 (*current options*) =
  if options1.back_end <> options2.back_end then
    OManager.serror "The package %s was compiled for backend %s, the current package would be compiled for %s@."
      package
      (OpaEnv.string_of_available_back_end options1.back_end)
      (OpaEnv.string_of_available_back_end options2.back_end);
  if options1.js_back_end <> options2.js_back_end then
    OManager.serror "The package %s was compiled for the js backend %s, the current package would be compiled for %s@."
      package
      options1.js_back_end
      options2.js_back_end;
  if options1.closure <> options2.closure then
    OManager.serror "The package %s was compiled %s closure, the current package %s."
      package
      (with_or_without options1.closure) (would options1.closure);
  if options1.cps <> options2.cps then
    OManager.serror "The package %s was compiled %s cps, the current package %s."
      package
      (with_or_without options1.cps) (would options1.cps);
  if options1.cps_client <> options2.cps_client then
    OManager.serror "The package %s was compiled %s client cps, the current package %s."
      package
      (with_or_without options1.cps_client)  (would options1.cps_client);
  if options1.cps_toplevel_concurrency <> options2.cps_toplevel_concurrency then
    OManager.serror "The package %s was compiled %s toplevel concurrency for cps, the current package %s."
      package
      (with_or_without options1.cps_toplevel_concurrency)  (would options1.cps_toplevel_concurrency);
  if options1.explicit_instantiation <> options2.explicit_instantiation then
    OManager.serror "The package %s was compiled %s ei, the current package %s."
      package
      (with_or_without options1.explicit_instantiation)  (would options1.explicit_instantiation);
  (* Check that the value restriction option was identically set in all the
     compiled packages. *)
  if options1.value_restriction <> options2.value_restriction then
    OManager.serror
      ("The package %s was compiled with value restriction set to %s, " ^^
       "the current package with %s." ^^
       "@[<2>@{<bright>Hint@}:@\n" ^^
       "Set --value-restriction option to the same value for all packages.@]")
      package
      (value_restiction_option_status options1.value_restriction)
      (value_restiction_option_status options2.value_restriction)



let process_code ~options:options env =
  let my_options = project options in
  R.iter_with_name ~deep:true ~packages:true
    (fun (package,_) options ->
       if options <> my_options then
         diff package options my_options
    );
  R.save my_options;
  env
