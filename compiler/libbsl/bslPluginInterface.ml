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
    Interface of generated loaders and plugins

    @author Mathieu Barbin
    @author Mehdi Bouaziz
*)

module Format = BaseFormat

(** {6 Type alias (just for lisibility) } *)
(** *)
type filename = string
type pathname = string
type contents = string
type ocaml_module_name = string (* OcamlUtils.module_name *)

(** the implementation name, expressed as concrete syntax of the target language *)
type implementation = string

(** the string version of the bsl key *)
type skey = string

(** the extension, like ["ml", "js", "c"] *)
type language = BslLanguage.t

(** a uniq id for identifying the plugin *)
type uniq_id = string

(**
   The name of the opp directory, without the 'opp' extension
*)
type plugin_basename = string

(** {6 Plugin interface} *)

(**
   Dealing with external primitives for opa.
*)

(**
   Marshalable environment for separation of plugin building
   These types are magically produced by [BslOcaml] and [BslJs]
*)

type javascript_env
type ocaml_env

(**
   The type of the function that the plugin use to register external primitives.
   It works with a side effect on some table in BslLib.

   Label are volontary short for minimizing the generated code of plugins.

   + [ks] the hieararchy path until this primitive. The final bslkey will be
   built by lowercase all the path, and separated items with an underscore.
   + [ty] the type of the primitive
   + [ips] all informations about the implementations in all target languages
   + [language] the extension of the language
   + [filename] the complete filname with dirname where is the file
   + [parsed_t] the row tags.
   + [implementation] the complete identifier for the implementation.
   ["OpabslMLRuntime.Foo.Bar.function"]
   + [obj] optional, a pointer to the function (in this case, the code is linked
   with the runtime, this is no more just a plugin, but a loader for the interpreter)
*)
type register_primitive =
  ks:skey list ->
  ty:BslTypes.t ->
  ips:(language * filename *  BslTags.parsed_t * implementation) list ->
  ?obj:Obj.t ->
  unit -> unit


type register_primitive_arguments = {
  rp_ks                                : skey list ;
  rp_ty                                : BslTypes.t ;
  rp_ips                               : ( language * filename * BslTags.parsed_t * implementation ) list ;
  rp_obj                               : Obj.t option ;
}


let apply_register_primitive ( register_primitive : register_primitive ) rp =
  let ks           = rp.rp_ks in
  let ty           = rp.rp_ty in
  let ips          = rp.rp_ips in
  let obj          = rp.rp_obj in
  register_primitive ~ks ~ty ~ips ?obj ()


let (~>) buf = FBuffer.printf buf

let pp_escaped fmt s = Format.fprintf fmt "%S" s
let pp_fc fmt (f, c) = Format.fprintf fmt "(%S,@\n %S)@\n" f c
let pp_ml_list p = Base.Format.pp_list " ; " p

let pp_js_conf fmt conf =
  if BslJsConf.is_default conf
  then
    Format.pp_print_string fmt "BslJsConf.default"
  else
    Format.fprintf fmt "(Marshal.from_string %S 0)"
      (Marshal.to_string (conf : BslJsConf.conf) [])

let pp_fc_conf fmt (f, c, conf) =
  Format.fprintf fmt "(%S,@\n %S,@\n %a)@\n"
    f
    c
    pp_js_conf conf

let pp_conf fmt conf =
  if BslConf.is_default conf
  then
    Format.pp_print_string fmt "BslConf.default_conf"
  else
    Format.fprintf fmt "(Marshal.from_string %S 0)"
      (Marshal.to_string (conf : BslConf.conf) [])

(**
   Meta Generation of code for [register_primitive].
*)
let meta_register_primitive buf ~ks ~ty ~ips ?obj () =
  let register = "register" in
  let b = buf in
  let b = ~>b "%s " register in
  let b = ~>b "~ks:[ %a ] " (pp_ml_list pp_escaped) ks in
  let b = ~>b "~ty:(%a) " BslTypes.pp_meta ty in
  let b =
    let pp_impl fmt impl =
      let lang, filename, parsed_t, implementation = impl in
      Format.fprintf fmt "(%a, %S, %a, %S)"
        BslLanguage.pp_meta lang filename BslTags.pp_meta parsed_t implementation
    in
    ~>b "~ips:[ %a ] " (pp_ml_list pp_impl) ips
  in
  let b =
    let pp_obj fmt obj = Format.fprintf fmt "~obj:(Obj.repr %s) " obj in
    ~>b "%a" (Option.pp pp_obj) obj
  in
  let b =
    ~>b "();@\n"
  in
  b


(**
   The type of the function that the plugin use to register external types.
   It works with a side effect on some table in BslLib.

   + [ks] the hieararchy path until this type defition
   + [ty] An External type, defined in the target language.
*)
type register_type =
  ks:skey list ->
  ty:BslTypes.t ->
  unit


type register_type_arguments = {
  rt_ks                           : skey list ;
  rt_ty                           : BslTypes.t ;
}


let apply_register_type ( register_type : register_type ) rt =
  let ks    = rt.rt_ks in
  let ty    = rt.rt_ty in
  register_type ~ks ~ty


(**
   Meta Generation of code for [register_type].
*)
let meta_register_type buf ~ks ~ty =
  let register = "register_type" in
  let b = buf in
  let b = ~>b "%s " register    in
  let b = ~>b "~ks:[ %a ] " (pp_ml_list pp_escaped) ks in
  let b = ~>b "~ty:(%a) " BslTypes.pp_meta ty in
  let b = ~>b ";@\n" in
  b

(**
   When the plugin ask for regestering things, it could get such a record,
   so that it can perform its registering.
*)
type dynloader_interface = {
  register_primitive   : register_primitive ;
  register_type        : register_type ;
}

(**
   Whenever a plugin want to register some function, we should actually check
   if it was not already registred. For that, there is an abtraction, handled
   via a function named [multi_loading_safe_get_dynloader_interface], which given
   an uniq id and the runtime name, will let the plugin have acces or not
   to register funciton.
   <!> This is the only chance for the plugin to register anything in
   the bsl table. The next time it will ask, it will get a [None]
*)
type multi_loading_safe_get_dynloader_interface =
  uniq_id:string ->
  plugin_name:plugin_basename ->
  dynloader_interface option

(**
   So, a dynloader in a plugin, is the function which is in charge
   to get a dynloader_interface, and using it.
*)
type dynloader = multi_loading_safe_get_dynloader_interface -> unit

(**
   Showing what is actually going on in the plugin ["examplePlugin.ml"] generated e.g. with {[bslregister -o example]}

   {[
    module Self : BslPluginInterface.PLUGIN =
    struct
      (* This is an example of minimal plugin *)
      let self_module_name = "ExamplePlugin"
      let uniq_id = "ExamplePlugin_17431-2010-08-08-(368c3b4-75fe11b-21cbb74)"
      let ml_runtime_module_name = "ExampleMLRuntime"
      let depends = [ "opabsl" ]
      let opa_code = [...]
      let js_code = [...]
      let dynloader ( get_register : BslPluginInterface.multi_loading_safe_get_dynloader_interface) : unit =
        match get_register ~uniq_id ~plugin_name:basename with
        | None ->
            (* I should have been already loaded *)
            ()
        | Some { register = register ; register_type = register_type } ->
            begin
              ... (* a lot of code to use register and register_type for registering primitives *)
            end

      let self =  {
        self_module_name ;
        uniq_id ;
        ml_runtime_module_name ;
        depends ;
        opa_code ;
        js_code ;
        dynloader ;
        ocaml_env ;
        javascript_env ;
      }

      let self_store () = BslPluginTable.store self

    end

    let _ = Self.self_store ()

   ]}

*)
(** *)

let meta_plugin__01 buf
    ~basename
    ~self_module_name
    ~uniq_id
    ~conf
    ~ml_runtime
    ~depends
    ~js_code
    ~nodejs_code
    ~opa_code
    ~ocaml_env
    ~javascript_env
    =
  let static_part =
"(* Auto generated plugin / loader, DO NOT EDIT BY HAND *)
(* Part of this code is static and comes from BslPluginInterface.ml *)
open BslPluginInterface
module Self : BslPluginInterface.PLUGIN =
struct
"
  in
  (* I someone find something |> more style, bravo :) - hard to reverse ()format arguemnts... *)

  let b = buf in
  let b = FBuffer.add b static_part in

  let b = ~> b    "let basename             = %S\n"  basename                             in
  let b = ~> b    "let self_module_name     = %S\n"  self_module_name                     in
  let b = ~> b    "let uniq_id              = %S\n"  uniq_id                              in
  let b = ~> b    "let conf                 = %a\n"  pp_conf conf                         in
  let b = ~> b    "let ml_runtime           = %S\n"  ml_runtime                           in
  let b = ~> b    "let depends  = [ %a ]\n"          (pp_ml_list pp_escaped)   depends    in
  let b = ~> b    "let js_code  = ( [ %a ] : (string * string * BslJsConf.conf) list )\n"
    (pp_ml_list pp_fc_conf)
    js_code
  in
  let b = ~> b    "let nodejs_code  = ( [ %a ] : (string * string * BslJsConf.conf) list )\n"
    (pp_ml_list pp_fc_conf)
    nodejs_code
  in
  let b = ~> b    "let opa_code = [ %a ]\n"          (pp_ml_list pp_fc)        opa_code   in
  let b = ~> b    "let ocaml_env = (Marshal.from_string %S 0)\n" (Marshal.to_string (ocaml_env : ocaml_env) []) in
  let b = ~> b    "let javascript_env = (Marshal.from_string %S 0)\n" (Marshal.to_string (javascript_env : javascript_env) []) in
  b

let meta_plugin__02 = "
  (* the generated code seems to enjoy following some weird guidelines *)
  module Q = QmlAst
  module B = BslTypes
  let (~$) = B.(~$)
  module BPI = BslPluginInterface
  module L = BslLanguage
  let dynloader ( get_register : BPI.multi_loading_safe_get_dynloader_interface ) : unit =
    match get_register ~uniq_id ~plugin_name:basename with
    | None ->
        (* I should have been already loaded *)
        ()
    | Some { BPI.register_primitive = register ; BPI.register_type = register_type } ->
        begin
          let mp = B.meta_pos in
          (* from there, the code is generated, cf in BslRegisterLib and in BslPluginInterface *)
"
let meta_plugin__03 = "
          ()
        end
  (* Back there, the code in staticly known, cf BslPluginInterface.meta_plugin__03 *)
  let self =  {
    basename ;
    self_module_name ;
    uniq_id ;
    conf ;
    ml_runtime ;
    depends ;
    opa_code ;
    js_code ;
    nodejs_code ;
    dynloader ;
    ocaml_env ;
    javascript_env ;
  }

  let self_store () = BslPluginTable.store self

end

let _ = Self.self_store ()
"

(**
   The type record use for manipulating the module as a first level value
   Each field correspond to a value of the interface [PLUGIN].
   The documentation is in the documentation of this interface.

   This is an unmodularization of the plugin, used to access dynamicly
   the value of a plugin Module.
*)
type plugin = {
  basename              : plugin_basename ;
  path                  : pathname ;
  self_module_name      : ocaml_module_name ;
  uniq_id               : uniq_id ;
  conf                  : BslConf.conf ;
  ml_runtime            : ocaml_module_name ;
  depends               : plugin_basename list ;
  opa_code              : (filename * contents) list ;
  js_code               : (filename * contents * BslJsConf.conf) list ;
  nodejs_code           : (filename * contents * BslJsConf.conf) list ;
  dynloader             : dynloader ;
  ocaml_env             : ocaml_env ;
  javascript_env        : javascript_env ;
}

let pp fmt plugin =
  Format.fprintf fmt "@[<2>%s.opp: {@\n" plugin.basename ;
  Format.fprintf fmt "plugin: %S@\n" plugin.self_module_name ;
  Format.fprintf fmt "id: %S@\n" plugin.uniq_id ;
  Format.fprintf fmt "conf: %a@\n" BslConf.pp plugin.conf ;
  Format.fprintf fmt "mlruntime: %S@\n" plugin.ml_runtime ;
  Format.fprintf fmt "depends: %a@\n" (Format.pp_list " ; " Format.pp_print_string) plugin.depends ;
  Format.fprintf fmt "@]}" ;

module type PLUGIN =
sig

  (** {6 Identification} *)

  (**
     The name asked for the plugin, without the extension opp.
  *)
  val basename : plugin_basename

  (**
     Where this plugin was loaded from
  *)
  val path : pathname

  (**
     The name of the Ocaml module corresponding to this plugin.
     This name is already capitalized.
     e.g. [OpabslPlugin]
  *)
  val self_module_name : ocaml_module_name

  (**
     Used to identify different conflicting version of the same Plugin
     This contains the version of the compiler, and the date of generation.
  *)
  val uniq_id : string

  (**
     Configuration
  *)
  val conf : BslConf.conf

  (**
     This is the name of the ML runtime corresponding to this plugin.
     When we generate code, we need to know where are the bypass.
  *)
  val ml_runtime : ocaml_module_name

  (**
     The module_name list from parents.
     This correspond to the list of all [basename] of plugins
     which was loaded when [bslregister] generated this plugin.
     (cf option --use-plugin of bslregister)
  *)
  val depends : plugin_basename list

  (** {6 Embeded code} *)

  (**
     This is an escaped string optained by preprocessing {b opa} files
     with [bslregister].
     The files are given file by file, indexed by the name of the file.
  *)
  val opa_code : (filename * contents) list

  (**
     This is an escaped string optained by preprocessing {b javascript} files
     with [bslregister].
     The files are given file by file, indexed by the name of the file.
  *)
  val js_code : (filename * contents * BslJsConf.conf) list

  (**
     as [js_code] but on node files
  *)
  val nodejs_code : (filename * contents * BslJsConf.conf) list

  (** {6 Registering primitives and types} *)

  val dynloader : dynloader

  (**
     The plugin defines a record with its contains
  *)
  val self : plugin

  (**
     Make the plugin store itself in the BslPluginTable.
     It works with a side-effect on the plugin table,
     after that, we have a structure for folding plugins,
     etc...
  *)
  val self_store : unit -> unit

(**
   A side effect at the end call the function [self_store]
*)

end
