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
module Format = BaseFormat
(* CF mli *)

module BPI = BslPluginInterface

(* CF BslPluginInterface *)
type implementation  = string
type skey            = string
type language        = string
type path            = string
type filename        = string
type pathname        = string
type module_name     = string
type uniq_id         = string
type contents        = string

(* essentially the same as a BslPluginInterface.plugin
   without any functional values for safe marshaling *)
type t = {
  basename                             : BPI.plugin_basename option ;
  self_module_name                     : module_name ;
  uniq_id                              : uniq_id ;
  conf                                 : BslConf.conf ;
  ml_runtime                           : module_name ;
  depends                              : BPI.plugin_basename list ;

  opa_code                             : ( filename * contents ) list ;
  js_pack                              : JsPackage.t ;
  nodejs_pack                          : JsPackage.t ;

  has_server_code                      : bool ;

  ocaml_env                            : BPI.ocaml_env ;
  javascript_env                       : BPI.javascript_env ;

  row_register_primitive               : BPI.register_primitive_arguments   list ;
  row_register_type                    : BPI.register_type_arguments        list ;
}

type session = {
  mutable s_basename                   : BPI.plugin_basename option ;
  mutable s_self_module_name           : module_name ;
  mutable s_uniq_id                    : uniq_id ;
  mutable s_conf                       : BslConf.conf ;
  mutable s_ml_runtime                 : module_name ;
  mutable s_depends                    : BPI.plugin_basename list ;

  mutable s_opa_code                   : ( filename * contents ) list ;
  mutable s_js_pack                    : JsPackage.t ;
  mutable s_nodejs_pack                : JsPackage.t ;

  mutable s_has_server_code            : bool ;

  mutable s_ocaml_env                  : BPI.ocaml_env option ;
  mutable s_javascript_env             : BPI.javascript_env option ;

  (* we can use a stack if we provide a Stack.fold function *)

  s_register_primitive_arguments       : BPI.register_primitive_arguments   Queue.t ;
  s_register_type_arguments            : BPI.register_type_arguments        Queue.t ;
}

let create () = {
  s_basename = None ;
  s_self_module_name = "" ;
  s_uniq_id = "" ;
  s_conf = BslConf.default_conf ;
  s_ml_runtime = "" ;
  s_depends = [] ;

  s_opa_code = [] ;
  s_js_pack = JsPackage.default ~name:"BMP" ;
  s_nodejs_pack = JsPackage.default ~name:"BMP" ;

  s_has_server_code = false ;

  s_ocaml_env = None ;
  s_javascript_env = None ;

  s_register_primitive_arguments = Queue.create () ;
  s_register_type_arguments = Queue.create () ;
}

let list_of_queue q =
  List.rev ( Queue.fold ( fun acc e -> e::acc ) [] q )

let finalize s = {
  basename                   = s.s_basename ;
  self_module_name           = s.s_self_module_name ;
  uniq_id                    = s.s_uniq_id ;
  conf                       = s.s_conf ;
  ml_runtime                 = s.s_ml_runtime ;
  depends                    = s.s_depends ;

  opa_code                   = s.s_opa_code ;
  js_pack                    = s.s_js_pack ;
  nodejs_pack                = s.s_nodejs_pack ;

  has_server_code            = s.s_has_server_code ;

  ocaml_env                  = Option.get s.s_ocaml_env ;
  javascript_env             = Option.get s.s_javascript_env ;

  row_register_primitive     = list_of_queue s.s_register_primitive_arguments ;
  row_register_type          = list_of_queue s.s_register_type_arguments ;
}

let unsafe_register_primitive s ~ks ~ty ~ips ?obj:_ () =
  let rp =
    { BPI.
      rp_ks      = ks ;
      rp_ty      = ty ;
      rp_ips     = ips ;
      rp_obj     = None ;
    } in
  Queue.add rp s.s_register_primitive_arguments

let unsafe_register_type s ~ks ~ty =
  let rt =
    { BPI.
      rt_ks = ks ;
      rt_ty = ty ;
    } in
  Queue.add rt s.s_register_type_arguments

let register_basename s b               = s.s_basename <- b
let register_module_name s m            = s.s_self_module_name <- m
let register_uniq_id s id               = s.s_uniq_id <- id
let register_conf s conf                = s.s_conf <- conf
let register_ml_runtime s n             = s.s_ml_runtime <- n
let register_depends s d                = s.s_depends <- d

let register_opa_code s c               = s.s_opa_code <- c
let register_js_pack s c                = s.s_js_pack <- c
let register_nodejs_pack s c            = s.s_nodejs_pack <- c

let register_has_server_code s c        = s.s_has_server_code <- c

let register_ocaml_env s env            = s.s_ocaml_env <- Some env
let register_javascript_env s env       = s.s_javascript_env <- Some env

let fail action filename message =
  OManager.printf "Primitives library plugin:@ Cannot %s file @{<bright>%S@}@\n" action filename;
  OManager.error "@[<2>@{<bright>Hint@}:@\n%s@]@\n" message

(* for simplicity, every time the compiler changes, the object files are invalid *)
let this_file_version = BuildInfos.opa_git_sha

(* I/O : beware, read the ocaml doc, Marshal should be used with binary
   channel for a Windows OS compatibility *)

let output oc_b t =
  try
    Printf.fprintf oc_b "%s\n" this_file_version ;
    Marshal.to_channel oc_b t [] ;
    flush oc_b
  with
  | Failure s
  | Sys_error s ->
      fail "output" "out_channel" s

let input ~filename ic_b =
  try
    let version = input_line ic_b in
    if version <> this_file_version then
      OManager.error (
        "The file %S@\nwas compiled with a different version of the compiler.@\n"^^
        "@[<2>@{<bright>Hint@}:@\nTry to recompile the plugin @{<bright>%s@}@\n@]"
      ) filename (Filename.basename (Filename.dirname filename)) ;
    Marshal.from_channel ic_b
  with
  | Sys_error s
  | Failure s ->
      fail "input" "in_channel" s

let output_file filename t =
  try
    let oc_b = open_out_bin filename in
    output oc_b t ;
    close_out oc_b
  with
  | Failure s
  | Sys_error s ->
      fail "output" filename s

let input_file filename =
  try
    let ic_b = open_in_bin filename in
    let t = input ~filename ic_b in
    close_in ic_b ;
    t
  with
  | Sys_error s
  | Failure s ->
      fail "input" filename s

let plugin t path =
  (* invalid plugin name if empty *)
  let plugin_name = t.basename in
  let uniq_id = t.uniq_id in
  let dynloader ( get_register : BslPluginInterface.multi_loading_safe_get_dynloader_interface ) =
    match get_register ~uniq_id ~plugin_name with
    | None -> ()
    | Some { BslPluginInterface.
               register_primitive = register_primitive ;
               register_type = register_type ;

           } -> (

        let iter_register_type rt =
          BPI.apply_register_type register_type rt
        in

        let iter_register_primitive rp =
          BPI.apply_register_primitive register_primitive rp
        in

        (* type before primitive using types *)
        List.iter iter_register_type      t.row_register_type ;
        List.iter iter_register_primitive t.row_register_primitive ;

        ()

      )
  in
  { BslPluginInterface.

    basename                = t.basename ;
    path                    = path ;
    self_module_name        = t.self_module_name ;
    uniq_id                 = t.uniq_id ;
    conf                    = t.conf ;
    ml_runtime              = t.ml_runtime ;
    depends                 = t.depends ;

    opa_code                = t.opa_code ;
    js_pack                 = t.js_pack ;
    nodejs_pack             = t.nodejs_pack ;

    has_server_code         = t.has_server_code ;

    ocaml_env               = t.ocaml_env ;
    javascript_env          = t.javascript_env ;

    dynloader               = dynloader ;
  }

let loadfile_private file =
  let path =
    if File.is_relative file then
      Filename.concat (Sys.getcwd ()) file
    else
      file
  in
  let path = Filename.dirname path in
  let t = input_file file in
  let plugin = plugin t (Some path) in
  BslPluginTable.store plugin

let loadfile = loadfile_private
