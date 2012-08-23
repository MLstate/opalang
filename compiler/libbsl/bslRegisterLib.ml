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
(* CF mli *)

module File = File
module List = Base.List
module String = Base.String
module Format = Base.Format

exception SigInt

let set_signal_sigint () =
  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ -> raise SigInt))

let unset_signal_sigint () =
  assert false (* TODO if needed *)

(* The bsl used is the dummy version, bslregister does not generated projection code *)
module BSL = BslLib.BSL

(* alias *)
module BPI = BslPluginInterface
module BI  = BslInterface
module BMP = BslMarshalPlugin
module BRI = BSL.RegisterInterface

module BRParse = BslRegisterParser
module BRState = BslRegisterParserState
module BDir    = BslDirectives


(*
  Types alias and definition.
*)
type language             = BslLanguage.t
type filename             = string
type module_name          = string
type contents             = string
type uniq_id              = string
type line_number          = int
type skey                 = string
type implementation       = string


(* debug *)

let debug fmt =
  OManager.printf ("@[<2>@{<cyan>[bslregister]@}@ "^^fmt^^"@]@.")


(*
  Parsing files.
*)


(*
  From a previous primitive library, Ordered topologicly
*)
type depends = {
  d_plugins                    : BPI.plugin list ;

  d_bymap                      : BSL.ByPassMap.t ;
  d_ml_runtimes                : module_name list ;
  d_parents                    : BPI.uniq_id list ;

  d_js_code                    : ( filename * contents ) list ;
  d_nodejs_code                : ( filename * contents ) list ;
  d_opa_code                   : ( filename * contents ) list ;

}


(*
  Some of then are needed, because their name appears in some generated files
*)
type identification = {
  i_basename                    : BPI.plugin_basename option ;

  i_ml_plugin                   : module_name ;
  i_ml_runtime                  : module_name ;

  i_uniq_id                     : uniq_id ;
}

(*
  Calls to BRI register_primitive and register_type need to be grouped,
  once all languages have been processed.
  This type is used for collecting calls, and the effective calls to the
  BRI is done only @finalize.

  TODO: there is currently a hack about BslTags, the file BslLib expects
  BslTags.parsed_t, where is should be BslTags.t.
  Once BslLib is no more using a hack about this, we can change
  the type of field c_rpi_tags for BslTags.t
*)

type rp_impl = {
  c_rpi_filename               : filename ;
  c_rpi_tags                   : BslTags.parsed_t ;
  c_rpi_type                   : BslTypes.t ;
  c_rpi_implementation         : implementation ;
}

type collecting_rp_ips         = rp_impl BslLanguageMap.t

type rp_call = {
  c_rp_ks                      : skey list ;
  c_rp_ty                      : BslTypes.t ; (* purged from opa values *)
  c_rp_ips                     : collecting_rp_ips ;
  c_rp_obj                     : implementation option ;
}

type collecting_rp_calls       = rp_call BslKeyMap.t

type rt_call = {
  c_rt_ks                      : skey list ;
  c_rt_ty                      : BslTypes.t ;
}

type collecting_rt_calls       = rt_call BslKeyMap.t

(*
  The session
*)

type session = {

  (* 1) static part, built at creation time *)

  s_options                    : BI.options ;

  s_pprocess                   : string -> string -> string ;

  s_depends                    : depends ;
  s_identification             : identification ;

  (* 2) evoluation as long as we process files *)

  s_conf                       : BslConf.t ;
  s_js_confs                   : BslJsConf.t ;
  s_nodejs_conf                : BslJsConf.t ;

  s_marshal_plugin_s           : BMP.session ;

  s_ml_dynloader_loader        : FBuffer.t ;
  s_ml_dynloader_plugin        : FBuffer.t ;

  s_rev_js_parsed_files        : BslJs.decorated_file list ;
  s_rev_nodejs_parsed_files    : BslJs.decorated_file list ;
  s_rev_ml_parsed_files        : BslDirectives.bypasslang_decorated_file list ;

  s_rev_opa_decorated_files    : BslDirectives.opalang_decorated_file  list ;

  s_has_server_code            : bool ;

  (*
    3) collecting calls to the Register Interface.
  *)
  s_rp_calls                   : collecting_rp_calls ;
  s_rt_calls                   : collecting_rt_calls ;

  (* 4) A pre-finalization, which keep the type session for its representation *)
  (*
    opa files may contains include directives which need to be solved at finalization
    opa_code and opa_interface are optained at finalization time
  *)
  s_js_code                    : (filename * contents * BslJsConf.conf) list ;
  s_nodejs_code                : (filename * contents * BslJsConf.conf) list ;

  s_ml_runtime                 : FBuffer.t option;
  s_ml_runtime_mli             : FBuffer.t option;

  s_opa_code                   : (filename * contents) list ;
  s_opa_interface              : (filename * contents) list ;
}


(*
  Create
*)


let create_s_fbuffer () = FBuffer.create (8 * 1024)


(*
   Load some plugins
*)
let load_plugin plugins =
  let dynlink plugin =
    OManager.verbose "loading primitives from file @{<bright>%S@}" (BslDynlink.file plugin) ;
    BslDynlink.load_bypass_plugin plugin
  in
  List.iter dynlink plugins


(*
  Fold depends plugin
*)
let d_fold_plugin
    (rev_ml_runtime_list, rev_parent_list, rev_js_code_list, rev_nodejs_code_list, rev_opa_code_list) plugin =

  BSL.RegisterInterface.dynload_no_obj plugin.BPI.dynloader ;

  let rev_ml_runtime_list = plugin.BPI.ml_runtime              :: rev_ml_runtime_list in
  let rev_parent_list     = plugin.BPI.uniq_id                 :: rev_parent_list     in
  let rev_js_code_list    = List.fold_left (fun rev (f, c, _) -> (f, c)::rev) rev_js_code_list plugin.BPI.js_code in
  let rev_nodejs_code_list= List.fold_left (fun rev (f, c, _) -> (f, c)::rev) rev_nodejs_code_list plugin.BPI.nodejs_code in
  let rev_opa_code_list   = List.rev_append plugin.BPI.opa_code   rev_opa_code_list   in

  rev_ml_runtime_list, rev_parent_list, rev_js_code_list, rev_nodejs_code_list, rev_opa_code_list


let create_s_depends options =

  (* This has a side effect on BslPluginTable *)
  load_plugin options.BI.bypass_plugins ;

  let d_plugins = BslPluginTable.finalize () in

  let (ml, prt, js, nodejs, opa)  =
    List.fold_left d_fold_plugin ([], [], [], [], []) d_plugins
  in

  let d_ml_runtimes       = List.rev ml  in
  let d_parents           = List.rev prt in
  let d_js_code           = List.rev js  in
  let d_nodejs_code       = List.rev nodejs  in
  let d_opa_code          = List.rev opa in


  (* All dynload have been done in d_fold, we can build the depends bymap *)
  let d_bymap             = BSL.RegisterTable.build_bypass_map () in

  let depends = {
    d_plugins ;
    d_bymap ;

    d_ml_runtimes ;
    d_parents ;

    d_js_code ;
    d_nodejs_code ;
    d_opa_code ;
  }

  in

  depends


let create_uniq_id name =
  (* FIXME: thing about where does go the function now () *)
  let now = DebugTracer.now in

  let buildinfos = BuildInfos.version_id in
  let now = now () in
  let uniq_id = Printf.sprintf "%s_%s_%s" name buildinfos now in
  uniq_id


let create_s_identification options =
  let i_basename        = options.BI.basename in
  let i_ml_plugin       = OcamlUtils.Module.of_filename options.BI.ml_plugin_filename in
  let i_ml_runtime      = OcamlUtils.Module.of_filename options.BI.ml_runtime_filename in

  let i_uniq_id         = create_uniq_id i_ml_plugin in

  let identification = {
    i_basename ;
    i_ml_plugin ;
    i_ml_runtime ;
    i_uniq_id ;
  }

  in

  identification


(* some checks *)
let check_session session =
  let depends        = session.s_depends in
  let identification = session.s_identification in

  let error_unicity kind name =
      OManager.error (
        "Generation of @{<bright>%s@} module-clash@\n" ^^
        "The asked target module name @{<bright>%S@} " ^^
        "is already used by a previous loaded primitive library@\n" ^^
        "Please specify another target ( try option --help )@\n"
      ) kind name

  in
  (* unicity of plugin names *)
  let _ =
    let ml_plugin = identification.i_uniq_id in
    if List.mem ml_plugin depends.d_parents then
      error_unicity "MLplugin" ml_plugin
  in
  (* unicity of runtime names *)
  let _ =
    let ml_runtime = identification.i_ml_runtime in
    if List.mem ml_runtime depends.d_ml_runtimes then
      error_unicity "MLruntime" ml_runtime
  in

  session


let create ~options =
  let s_options                      = options in

  let s_pprocess                     = fun _ content -> content in
  let s_depends                      = create_s_depends          options in
  let s_identification               = create_s_identification   options in

  let s_conf                         = BslConf.default in
  let s_js_confs                     = BslJsConf.empty options.BI.js_files in
  let s_nodejs_conf                  = BslJsConf.empty options.BI.nodejs_files in

  let s_marshal_plugin_s             = BMP.create () in

  let s_ml_dynloader_loader          = create_s_fbuffer () in
  let s_ml_dynloader_plugin          = create_s_fbuffer () in

  let s_rev_js_parsed_files          = [] in
  let s_rev_nodejs_parsed_files      = [] in
  let s_rev_ml_parsed_files          = [] in

  let s_rev_opa_decorated_files      = [] in

  let s_has_server_code              = false in

  let s_rp_calls                     = BslKeyMap.empty in
  let s_rt_calls                     = BslKeyMap.empty in

  let s_js_code                      = [] in
  let s_nodejs_code                  = [] in

  let s_ml_runtime                   = None in
  let s_ml_runtime_mli               = None in

  let s_opa_code                     = [] in
  let s_opa_interface                = [] in

  let session = {
    s_options ;

    s_pprocess ;

    s_depends ;
    s_identification ;

    s_conf ;
    s_js_confs ;
    s_nodejs_conf ;

    s_marshal_plugin_s ;

    s_ml_dynloader_plugin ;
    s_ml_dynloader_loader ;

    s_rev_js_parsed_files ;
    s_rev_nodejs_parsed_files ;
    s_rev_ml_parsed_files ;

    s_rev_opa_decorated_files ;

    s_has_server_code ;

    s_rp_calls ;
    s_rt_calls ;

    s_js_code ;
    s_nodejs_code ;

    s_ml_runtime ;
    s_ml_runtime_mli ;

    s_opa_code ;
    s_opa_interface ;
  }

  in

  check_session session

let set_pprocess ~pprocess session =
  { session  with s_pprocess = pprocess }


(*
  Registering
*)

(*
  Note

  The goal there is that from the plugin, it should be possible to get exactly
  to the same point as there, with the same calls to the RegisterInterface now,
  later when somebody will dynload the plugin.

  A way of doing that, is to have the strict same interface for the 3 register_*
  The register of this module returns the session.

  CF
  + BslPluginInterface.register_*
*)

(*
  Collecting calls.
*)

let compute_skey ks = String.concat_map "_" BslKey.normalize_string ks

let warning_set = WarningClass.Set.create_from_list [
  WarningClass.bsl_register ;
]

let warning fmt = OManager.warning ~wclass:WarningClass.bsl_register fmt

let equal_ks ks ks' =
  let c = List.make_compare String.compare ks ks' in
  c = 0

let pp_ks fmt ks = Format.pp_list "." Format.pp_print_string fmt ks


(*
  Note for hackers:

  This function is quite verbose, because there are a lot of error cases to detect.
  if --check-style is set, warning of the class bsl.register will be set to --warn-error.

  For citation, plugins file are no more available, so it makes not any sence to print
  citation for types from plugins. For the types of the current session, the citation
  is available.
*)
let register_type session ~ks ~ty =
  let _ =
    #<If:BSL_REGISTER $equals "2">
      debug "register-type: ks:[%a] ty:(%a)@\n" (Format.pp_list " ; " (fun f -> Format.fprintf f "%S") ) ks BslTypes.pp ty
    #<End>
  in
  let s_rt_calls = session.s_rt_calls in
  let depends_typesmap = BSL.ByPassMap.typesmap session.s_depends.d_bymap in
  let skey = compute_skey ks in
  let key = BslKey.normalize skey in
  let pos = BslTypes.pos ty in
  (* rt_call will be an option of rt_call, to know if there is something to collect or not *)
  let rt_call =
    (* check coherence with --use-plugin *)
    match BslKeyMap.find_opt key depends_typesmap with
    | Some (_, plugin_ks, plugin_ty) ->
        if equal_ks ks plugin_ks
        then (
          (* checking the type *)
          if (BslTypes.compare ~normalize:true ty plugin_ty) <> 0
          then (
            OManager.printf "%a" FilePos.pp_citation pos ;
            OManager.printf "%a@\nThis type is already defined in a used plugin: @{<bright>%a@}@\n"
              FilePos.pp_pos (BslTypes.pos plugin_ty) BslTypes.pp plugin_ty ;
            OManager.error "##extern-type: @{<bright>conflicting type definitions@} for key '@{<brigth>%a@}'@\n" BslKey.pp key
          )
          else (
            warning "@\n%aA used plugin already defines this type.@ You should avoid this kind of practice.@\n##extern-type: @{<bright>type redefinition@} for key '@{<brigth>%a@}'@\n"
              FilePos.pp_citation pos BslKey.pp key ;
            None (* in this case, we do not redefine the type *)
          )
        )
        else (
          OManager.printf "%a" FilePos.pp_citation pos ;
          OManager.printf "One of the used plugin contains a declaration of@ an extern-type with the same key@ but with a different module-path.@\nThe module-path in your lib is @{<bright>%a@},@ in the plugin the module-path is @{<bright>%a@}.@\n"
            pp_ks ks pp_ks plugin_ks
          ;
          OManager.error "##extern-type: @{<bright>module-path mismatch@} for key '@{<brigth>%a@}'@\n" BslKey.pp key
        )

    | None -> (
        (* Check if the type is already defined in an other file of the current session *)
        (* It is almost the same erros, but more restrictive *)
        match BslKeyMap.find_opt key s_rt_calls with
        | Some ( { c_rt_ks = c_rt_ks ; c_rt_ty = c_rt_ty } as rt_call ) ->
            let pos = FilePos.merge_pos pos (BslTypes.pos c_rt_ty) in
            if equal_ks ks c_rt_ks then (
              (* checking the type *)
              if (BslTypes.compare ~normalize:true ty c_rt_ty) <> 0
              then (
                OManager.printf "%a" FilePos.pp_citation pos ;
                OManager.error "##extern-type: @{<bright>conflicting type definitions@} for key '@{<brigth>%a@}'@\n" BslKey.pp key
              );
              let c_rt_ty = BslTypes.reset_pos c_rt_ty pos in
              Some {
                rt_call with
                  c_rt_ty = c_rt_ty
              }
            )
            else (
              OManager.printf "%a" FilePos.pp_citation pos ;
              OManager.printf "There are 2 extern-type definitions with the same key@ but with a different module-path.@\nThe first module-path is @{<bright>%a@},@ and the second is @{<bright>%a@}.@\n"
                pp_ks ks pp_ks c_rt_ks
              ;
              OManager.error "##extern-type: @{<bright>module-path mismatch@} for key '@{<brigth>%a@}'@\n" BslKey.pp key
            )

        | None ->
            (* This is the first time this type is defined, collect it *)
            Some { c_rt_ks = ks ; c_rt_ty = ty }
      )

  in

  let session =
    match rt_call with
    | None -> session
    | Some rt_call ->
        let s_rt_calls = BslKeyMap.add key rt_call s_rt_calls in
        {
          session with
            s_rt_calls    = s_rt_calls
        }

  in

  session


let register_primitive session ~ks ~ty ~ips ?obj:_ () =
  let _ =
    #<If:BSL_REGISTER $equals "3">
      debug "register-primitive: ks:[%a] ty:(%a)@\n" (Format.pp_list "." Format.pp_print_string) ks BslTypes.pp ty
    #<End>
  in
  let s_rp_calls = session.s_rp_calls in
  let depends_bymap = session.s_depends.d_bymap in
  let skey = compute_skey ks in
  let key = BslKey.normalize skey in
  let pos = BslTypes.pos ty in
  (* rt_call will be an option of rt_call, to know if there is something to collect or not *)
  let rp_call =
    (* check coherence with --use-plugin *)
    match BSL.ByPassMap.find_opt depends_bymap key with
    | Some bypass ->
        OManager.printf "%a" FilePos.pp_citation pos ;
        OManager.printf "@[<2>A primitive with the same key is already defined@ in a used plugin@\n%a@]@\n"
          (Format.pp_list "@, " BSL.Implementation.pp) (BSL.ByPass.all_implementations bypass) ;
        OManager.error "##register: @{<bright>primitive re-definition@} for key '@{<brigth>%a@}'@\n" BslKey.pp key

    | None -> (
        match BslKeyMap.find_opt key s_rp_calls with
        | Some rp_call -> (
            let c_rp_ks   = rp_call.c_rp_ks in
            let c_rp_ty   = rp_call.c_rp_ty in
            let c_rp_ips  = rp_call.c_rp_ips in
            let c_rp_obj  = rp_call.c_rp_obj in
            let pos = FilePos.merge_pos pos (BslTypes.pos c_rp_ty) in

            if equal_ks ks c_rp_ks then (
              (* checking the type *)
              (* We do not take opa value declarations into account *)
              let purged_ty = BslTypes.purge_opavalue ty in
              if (BslTypes.compare ~normalize:true purged_ty c_rp_ty) <> 0
              then (
                OManager.printf "%a" FilePos.pp_citation pos ;
                OManager.error "##register: @{<bright>conflicting primitive definitions@} for key '@{<brigth>%a@}'@\n" BslKey.pp key
              );
              (* merge pos in ty. BslTypes.reset_pos, pos is already the merged pos *)
              (* TODO: merge type variables names, or keep the one from Ocaml *)
              let c_rp_ty = BslTypes.reset_pos c_rp_ty pos in

              let c_rp_ips = List.fold_left
                (fun c_rp_ips (lang, filename, parsed_t, type_, implementation) ->
                   (* redefinition in the same language is not allowed *)
                   match BslLanguageMap.find_opt lang c_rp_ips with
                   | Some _ -> (
                       (* When this primitive was registred the first time, its position was merged *)
                       OManager.printf "%a" FilePos.pp_citation pos ;
                       OManager.printf "This primitive is already defined in %a@\n" BslLanguage.pp lang;
                       OManager.error "##register: @{<bright>multiple primitive definitions@} for key '@{<brigth>%a@}'@\n" BslKey.pp key
                     )
                   | None ->
                       let rp_impl = {
                         c_rpi_filename = filename ;
                         c_rpi_tags = parsed_t ;
                         c_rpi_type = type_ ;
                         c_rpi_implementation = implementation ;
                       }
                       in
                       BslLanguageMap.add lang rp_impl c_rp_ips
                ) c_rp_ips ips
              in

              let c_rp_obj =
                match c_rp_obj with
                | Some _ -> c_rp_obj
                | None ->
                    List.find_map
                      (fun (ml, _, _, _, imp) -> if BslLanguage.is_ml ml then Some imp else None) ips
              in

              let rp_call = {
                rp_call with
                  c_rp_ty    = c_rp_ty ;
                  c_rp_ips   = c_rp_ips ;
                  c_rp_obj   = c_rp_obj ;
              }
              in
              Some rp_call
            )
            else (
              OManager.printf "%a" FilePos.pp_citation pos ;
              OManager.printf "There are 2 primitive definitions with the same key@ but with a different module-path.@\nThe first module-path is @{<bright>%a@},@ and the second is @{<bright>%a@}.@\n"
                pp_ks ks pp_ks c_rp_ks
              ;
              OManager.error "##register: @{<bright>module-path mismatch@} for key '@{<brigth>%a@}'@\n" BslKey.pp key
            )
          )

        | None ->
            (* This is the first time this primitive is defined, collect it *)
            let c_rp_obj = List.find_map
              (fun (ml, _, _, _, imp) -> if BslLanguage.is_ml ml then Some imp else None) ips
            in
            let c_rp_ips = List.fold_left
              (fun c_rp_ips (lang, filename, parsed_t, type_, implementation) ->
                 let rp_impl = {
                   c_rpi_filename = filename ;
                   c_rpi_tags = parsed_t ;
                   c_rpi_type = type_ ;
                   c_rpi_implementation = implementation ;
                 }
                 in
                 BslLanguageMap.add lang rp_impl c_rp_ips
              ) BslLanguageMap.empty ips
            in
            let rp_call = {
              c_rp_ks = ks ;
              c_rp_ty = BslTypes.purge_opavalue ty ;
              c_rp_ips = c_rp_ips ;
              c_rp_obj = c_rp_obj ;
            }
            in
            Some rp_call
      )

  in

  let session =
    match rp_call with
    | None -> session
    | Some rp_call ->
        let s_rp_calls = BslKeyMap.add key rp_call s_rp_calls in
        {
          session with
            s_rp_calls    = s_rp_calls
        }

  in

  session



(*
  The finalized_t
*)

type finalized_t = {
  f_options                    : BI.options ;

  f_marshal_plugin_t           : BMP.t ;
                                             (*   loader     |     plugin    *)
  f_plugin_up                  : FBuffer.t ; (*     X        |       X       *)
  f_ml_dynloader_loader        : FBuffer.t ; (*     X        |               *)
  f_ml_dynloader_plugin        : FBuffer.t ; (*              |       X       *)

  f_nodejs_package             : FBuffer.t ;

  f_js_keys                    : FBuffer.t ;

  f_ml_runtime                 : FBuffer.t option ;
  f_ml_runtime_mli             : FBuffer.t option ;

  f_js_code                    : (filename * contents * BslJsConf.conf) list ;
  f_nodejs_code                : (filename * contents * BslJsConf.conf) list ;
  f_opa_code                   : (filename * contents) list ;
  f_opa_interface              : (filename * contents) list ;

}


(* Finalize *)

(*
  Effective calls, at the end (finalization)
*)
let f_register_types session =
  let fold_session _ rt_call session =
    let ks = rt_call.c_rt_ks in
    let ty = rt_call.c_rt_ty in

    let bmp_s = session.s_marshal_plugin_s in
    let _side_effect =
      try
        BMP.unsafe_register_type bmp_s ~ks ~ty ;
        BRI.unsafe_register_type       ~ks ~ty ;
        ()
      with
      | BRI.RegisterError error -> (
          (* pos : the pos of ty has merged the pos in all languages *)
          let pos = BslTypes.pos ty in
          OManager.printf "%a" FilePos.pp_citation pos ;
          OManager.error "%a@\n" BRI.pp_error error
        )
    in

    (* complete the buffer of call to BP *)

    let s_ml_dynloader_loader = session.s_ml_dynloader_loader in
    let s_ml_dynloader_plugin = session.s_ml_dynloader_plugin in

    let s_ml_dynloader_loader =
      BPI.meta_register_type s_ml_dynloader_loader ~ks ~ty
    in

    let s_ml_dynloader_plugin =
      BPI.meta_register_type s_ml_dynloader_plugin ~ks ~ty
    in

    {
      session with
        s_ml_dynloader_loader = s_ml_dynloader_loader ;
        s_ml_dynloader_plugin = s_ml_dynloader_plugin ;
    }

  in

  let session = BslKeyMap.fold fold_session session.s_rt_calls session in

  session


let f_collecting_rp_ips c_rp_ips =
  let fold lang rp_impl acc =
    let filename              = rp_impl.c_rpi_filename in
    let tags                  = rp_impl.c_rpi_tags in
    let type_                 = rp_impl.c_rpi_type in
    let implementation        = rp_impl.c_rpi_implementation in
    let item =
      lang, filename, tags, type_, implementation
    in
    item :: acc
  in

  let ips = BslLanguageMap.fold fold c_rp_ips [] in

  ips


let f_register_primitives session =
  let fold_session _ rp_call session =
    let ks            = rp_call.c_rp_ks in
    let ty            = rp_call.c_rp_ty in
    let c_rp_ips      = rp_call.c_rp_ips in
    let obj           = rp_call.c_rp_obj in
    (* ips should now be transformed into a list (cf BPI) *)
    let ips           = f_collecting_rp_ips c_rp_ips in

    let bmp_s = session.s_marshal_plugin_s in
    let _side_effect =
      try
        BMP.unsafe_register_primitive bmp_s ~ks ~ty ~ips () ;
        BRI.unsafe_register_primitive       ~ks ~ty ~ips () ;
        ()
      with
      | BRI.RegisterError error -> (
          (* pos : the pos of ty has merged the pos in all languages *)
          let pos = BslTypes.pos ty in
          OManager.printf "%a" FilePos.pp_citation pos ;
          OManager.error "%a@\n" BRI.pp_error error
        )
    in

    (* complete the buffer of call to BP *)

    let s_ml_dynloader_loader = session.s_ml_dynloader_loader in
    let s_ml_dynloader_plugin = session.s_ml_dynloader_plugin in

    let s_ml_dynloader_loader =
      BPI.meta_register_primitive s_ml_dynloader_loader ~ks ~ty ~ips ?obj () ;
    in

    let s_ml_dynloader_plugin =
      BPI.meta_register_primitive s_ml_dynloader_plugin ~ks ~ty ~ips () ;
    in

    {
      session with
        s_ml_dynloader_loader = s_ml_dynloader_loader ;
        s_ml_dynloader_plugin = s_ml_dynloader_plugin ;
    }

  in

  let session = BslKeyMap.fold fold_session session.s_rp_calls session in

  session


let finalizing_register_calls session =
  ignore (BRI.register
            ~uniq_id:session.s_identification.i_uniq_id
            ~plugin_name:session.s_identification.i_basename);
  let session = f_register_types       session in
  let session = f_register_primitives  session in
  session


let f_plugin_up ~conf ~ocaml_env ~javascript_env s =

  let s_identification = s.s_identification in

  let basename         = s_identification.i_basename in
  let basename         = Option.default "" basename in
  let self_module_name = s_identification.i_ml_plugin in
  let uniq_id          = s_identification.i_uniq_id in

  let ml_runtime       = s_identification.i_ml_runtime in
  let depends          = s.s_depends.d_parents in
  let js_code          = s.s_js_code in
  let nodejs_code      = s.s_nodejs_code in
  let has_server_code  = s.s_has_server_code in
  let opa_code         = s.s_opa_code in

  let buf = FBuffer.create 1024 in
  let buf =
    BPI.meta_plugin__01
      buf
      ~basename
      ~self_module_name
      ~uniq_id
      ~conf
      ~ml_runtime
      ~depends
      ~js_code
      ~nodejs_code
      ~has_server_code
      ~opa_code
      ~ocaml_env
      ~javascript_env
  in

  buf


let finalizing_opa ~final_bymap s =
  let s_opa_code =
    List.rev_map (BslOpa.preprocess ~final_bymap) s.s_rev_opa_decorated_files in

  let s_opa_code =
    List.map (fun (f, b) -> (f, FBuffer.contents b)) s_opa_code in

  (*
    Syntax check, and Type checking.
  *)

  let s_opa_interface =
    let true_means_error, s_opa_interface =
      BslOpa.checking_fail ~final_bymap s_opa_code in

    if true_means_error && not s.s_options.BI.unsafe_opa then
      OManager.error "Process stopped because of errors in the Opa code@\n"
    ;
    s_opa_interface
  in

  let s_opa_interface =
    List.map (fun (f, b) -> (f, FBuffer.contents b)) s_opa_interface in

  { s
    with
      s_opa_code             = s_opa_code ;
      s_opa_interface        = s_opa_interface ;
  }


let make_imperative_dynloader_interface session =
  let session = ref session in
  let register_type ~ks ~ty =
    let s = register_type !session ~ks ~ty in
    session := s
  in
  let register_primitive ~ks ~ty ~ips ?obj () =
    let s = register_primitive !session ~ks ~ty ~ips ?obj () in
    session := s
  in

  let dynloader_interface = {
    BPI.register_type        = register_type ;
    BPI.register_primitive   = register_primitive ;
  }

  in session, dynloader_interface


let finalizing_ocaml session =

  let options = session.s_options in

  let plugins = session.s_depends.d_plugins in
  let ml_decorated_files = List.rev session.s_rev_ml_parsed_files in

  let session_ref, dynloader_interface =
    make_imperative_dynloader_interface session
  in

  let ocaml_env, s_ml_runtime, s_ml_runtime_mli =
    BslOcaml.preprocess ~options ~plugins ~dynloader_interface ml_decorated_files
  in

  ocaml_env,
  { !session_ref
    with
      s_ml_runtime ;
      s_ml_runtime_mli ;
  }


let finalizing_js_code_conf s_js_confs s_js_code =
  let confs = BslJsConf.export s_js_confs in
  let map (filename, content) =
    let index = filename in
    match StringMap.find_opt index confs with
    | Some conf -> filename, content, conf
    | None ->
        (*
          Internal error, the list of given js files was not correct
        *)
        assert false
  in
  List.map map s_js_code

let collect_exports bymap nodejs_code =
  (* HACK: In compiled NodeJs code, we have to refer to bypasses by
     their keys, and not by their compiler representation. This
     happens because when the bypass is an alias to an identifier we
     don't know if that identifier is plugin-local (in which case
     client modules cannot refer to it directly) or a global one (in
     which case it cannot be found in the plugin exports). Thus, we
     need to export all such bypasses to their compiled
     representations using the bsl key as the identifier, since at
     this point the compiler representation still makes sense *)
  let module BPM = BSL.ByPassMap in
  let module I = BSL.Implementation in
  let module CF = I.CompiledFunction in

  let exports = BPM.fold (fun key bypass exports ->
    let nodejs_impl =
      BSL.ByPass.compiled_implementation bypass
        ~lang:BslLanguage.nodejs in
    match nodejs_impl with
    | Some compiled ->
      let rhs = JsCons.Expr.native (CF.compiler_repr compiled) in
      let export =
        JsCons.Statement.assign
          (JsCons.Expr.dot
             (JsCons.Expr.native "exports")
             (BslKey.to_string key))
          rhs in
      export :: exports
    | None -> exports
  ) bymap []
  in
  let exports = Format.to_string JsPrint.pp_min#code exports in
  nodejs_code @ [("exports", exports, BslJsConf.default)]

let js_error_after_pp filename contents message =
  ignore (File.output "jserror.js" contents);
  OManager.error
    ("Couldn't parse file @{<brigth>%s@} after preprocessing\n"^^
        "Take a look on generated file @{<brigth>jserror.js@}\n%!" ^^
        "Error: %s")
    filename message

let export_to_global_namespace nodejs_code =
  (* When not exporting identifiers as modules, we need to export them
     globally *)
  List.map (fun (filename, contents, conf) ->
    let contents =
      try
        JsParse.String.code ~throw_exn:true contents
      with
        JsParse.Exception e ->
          js_error_after_pp filename contents
            (Format.to_string JsParse.pp e)
    in
    let contents = JsUtils.export_to_global_namespace contents in
    (filename, Format.to_string JsPrint.pp_min#code contents, conf)
  ) nodejs_code

let finalizing_js ~depends ~js_decorated_files ~js_confs ~lang update_session session =

  let options = session.s_options in

  let plugins = session.s_depends.d_plugins in

  let session_ref, dynloader_interface =
    make_imperative_dynloader_interface session
  in

  let javascript_env, s_js_code =
    BslJs.preprocess ~options ~plugins ~dynloader_interface ~depends ~lang js_decorated_files
  in

  let s_js_code = finalizing_js_code_conf js_confs s_js_code in

  javascript_env, update_session !session_ref s_js_code

let finalizing_nodejs_package nodejs_code =
  let fold buf (filename, contents, conf) =
    ignore conf;
    FBuffer.printf buf "// From: %s\n%s\n" filename contents
  in
  let buf = FBuffer.create 1024 in
  List.fold_left fold buf nodejs_code

let finalizing_js_keys ~final_bymap =
  let fold key bypass buf =
    match BSL.ByPass.compiled_implementation bypass ~lang:BslLanguage.js with
    | Some compiled ->
      let skey = BslKey.to_string key in
      let resolution = BSL.Implementation.CompiledFunction.compiler_repr compiled in
        (* HACK: As long as we access global variables using the
           "global" accessor, we need to bypass this check. As soon
           as we fix this, we have to change this back. *)
        FBuffer.printf buf "var _check_definition_of_%s = global.%s@\n" skey resolution
    | None -> buf
  in
  let buf = FBuffer.create 1024 in
  let buf = FBuffer.addln buf "// Checking keys resolution." in
  let buf = FBuffer.addln buf "// File used by the jschecker only" in
  BSL.ByPassMap.fold fold final_bymap buf


let finalize s =

  let _ = OManager.flush_errors () in

  (*
    first, we should preprocess ml and js files,
    performing some side effects on the session
    + loader and plugin via the session ref with imperative
    dynloader interface
    + register table via call to dynloader_interface
  *)

  let ocaml_env, s          = finalizing_ocaml s in
  let javascript_env, s     =
    let depends = s.s_depends.d_js_code in
    let js_decorated_files = List.rev s.s_rev_js_parsed_files in
    let js_confs = s.s_js_confs in
    let lang = BslLanguage.js in
    let update_session session js_code = { session with s_js_code = js_code } in
    finalizing_js ~depends ~js_decorated_files ~js_confs ~lang update_session s
  in

  let _nodejs_env, s        =
    let depends = s.s_depends.d_nodejs_code in
    let js_decorated_files = List.rev s.s_rev_nodejs_parsed_files in
    let js_confs = s.s_nodejs_conf in
    let lang = BslLanguage.nodejs in
    let update_session session js_code = { session with s_nodejs_code = js_code } in
    finalizing_js ~depends ~js_decorated_files ~js_confs ~lang update_session s
  in

  (*
    Now all the calls to the register interface are regrouped in
    the session, we can do the effective calls
  *)
  let s                     = finalizing_register_calls s in

  (*
    Now, we can build the final bymap, every bypass has been registred.
  *)
  let final_bymap           = BSL.RegisterTable.build_bypass_map () in

  (*
    Opa generation, and checking
  *)
  let s                     = finalizing_opa ~final_bymap s in

  let conf                  = BslConf.export s.s_conf  in

  let f_options             = s.s_options              in
  let f_plugin_up           = f_plugin_up ~conf ~ocaml_env ~javascript_env s in

  let f_ml_dynloader_loader = s.s_ml_dynloader_loader  in
  let f_ml_dynloader_plugin = s.s_ml_dynloader_plugin  in

  let f_js_code             = s.s_js_code in

  let nodejs_code           =
    if s.s_options.BI.modular_plugins then
      collect_exports final_bymap s.s_nodejs_code
    else
      export_to_global_namespace s.s_nodejs_code in
  let f_nodejs_package      = finalizing_nodejs_package nodejs_code in
  let f_js_keys             = finalizing_js_keys ~final_bymap in
  let f_nodejs_code         = s.s_nodejs_code          in

  let f_ml_runtime          = s.s_ml_runtime           in
  let f_ml_runtime_mli      = s.s_ml_runtime_mli       in

  let f_opa_code            = s.s_opa_code             in
  let f_opa_interface       = s.s_opa_interface        in

  let f_marshal_plugin_t =
    let identification      = s.s_identification       in
    let depends             = s.s_depends.d_parents    in
    let bmp_session         = s.s_marshal_plugin_s     in

    (* we register all rest fields but primitives and types which are done in-line *)
    BMP.register_basename                  bmp_session   identification.i_basename ;
    BMP.register_module_name               bmp_session   identification.i_ml_plugin ;
    BMP.register_uniq_id                   bmp_session   identification.i_uniq_id ;
    BMP.register_conf                      bmp_session   conf ;
    BMP.register_ml_runtime                bmp_session   identification.i_ml_runtime ;
    BMP.register_depends                   bmp_session   depends ;

    BMP.register_js_code                   bmp_session   f_js_code ;
    BMP.register_nodejs_code               bmp_session   f_nodejs_code ;
    BMP.register_opa_code                  bmp_session   f_opa_code ;

    BMP.register_has_server_code           bmp_session   s.s_has_server_code ;

    BMP.register_ocaml_env                 bmp_session   ocaml_env ;
    BMP.register_javascript_env            bmp_session   javascript_env ;

    BMP.finalize bmp_session

  in

  let finalized_t = {
    f_options ;

    f_marshal_plugin_t ;

    f_plugin_up ;
    f_ml_dynloader_loader ;
    f_ml_dynloader_plugin ;

    f_nodejs_package ;
    f_js_code ;
    f_nodejs_code ;
    f_js_keys ;

    f_ml_runtime ;
    f_ml_runtime_mli ;

    f_opa_code ;
    f_opa_interface ;
  }

  in

  let _ = OManager.flush_errors () in

  finalized_t

let plugin finalized_t =
  BMP.plugin finalized_t.f_marshal_plugin_t None

let command_not_found = 127

let js_validator finalized_t =
  (* TODO: make validation work for anonymous plugins *)
  let name = Option.get finalized_t.f_options.BI.basename in
  match finalized_t.f_options.BI.js_validator with
  | Some (builddir,(executable, extern_files),cmd_options) when finalized_t.f_js_code <> [] ->
    let pp_str_list = Format.pp_list " " Format.pp_print_string in
    let pp_extern_files_list = Format.pp_list " " (
      if executable = "java" then (fun fmt v -> Format.fprintf fmt "--externs %s" v) (* probably google compiler *)
      else Format.pp_print_string (* probably js command *)
    )
    in
    let pp_file_list = Format.pp_list " " (
      if executable = "java" then (fun  fmt v -> Format.fprintf fmt "--js %s" v)
      else Format.pp_print_string
    )
    in
    let command = Format.sprintf "%s %a %a %a --js_output_file output.js"
      executable
      pp_str_list cmd_options
      pp_extern_files_list extern_files
      pp_file_list (List.map (fun (f,_,_)-> Printf.sprintf "%s.opp/%s/%s_%s" name (Filename.dirname f) name (Filename.basename f)) finalized_t.f_js_code)
    in
    OManager.verbose "JS VALIDATION : %s\n" command;
    let cwd = Sys.getcwd () in
    if builddir <> "" then Sys.chdir builddir;
    let r = Sys.command command in
    Sys.chdir cwd;
    if r<>0 && not(finalized_t.f_options.BI.unsafe_js) then (
      if r = command_not_found
      then warning "%s not found. Cannot validate js part of the plugin. Please install it or deactivate validation (use --help)" executable
      else OManager.error   "code %d:%s: fail to validate js part of the plugin\n" r command
    ) else ()
  | _ -> ()
;;



(* Output *)

type 'a output = out_channel -> 'a -> unit

type iterator = {
  output : 'a. filename -> (out_channel -> 'a -> unit) -> 'a -> unit
}

type 'a output_iterator = iterator -> 'a -> unit


let out_code_factory fc_extract extract iterator finalized_t =
  let iter = iterator.output in
  let iter fc_box =
    let filename, contents = fc_extract fc_box in
    iter filename output_string contents
  in
  List.iter iter (extract finalized_t)

let out_code extract iterator finalized_t =
  let fc (filename, contents) = filename, contents in
  out_code_factory fc extract iterator finalized_t


let out_js_code i f =
  let fc (filename, contents, _) = filename, contents in
  let extract f = f.f_js_code in
  out_code_factory fc extract i f

let out_nodejs_code i f =
  let fc (filename, contents, _) = filename, contents in
  let extract f = f.f_nodejs_code in
  out_code_factory fc extract i f


let out_opa_code i f =
  let extract f = f.f_opa_code in
  out_code extract i f


let out_opa_interface i f =
  let extract f = f.f_opa_interface in
  out_code extract i f


let out_ml_marshal_plugin oc f =
  BMP.output oc f.f_marshal_plugin_t


let out_fbuffer extract oc f =
  try
    FBuffer.output oc (extract f)
  with Not_found -> ()

let get_opt o = match o with
  | None -> raise Not_found
  | Some x -> x

let out_nodejs_package oc =
  let extract f = f.f_nodejs_package in
  out_fbuffer extract oc

let out_js_keys oc =
  let extract f = f.f_js_keys in
  out_fbuffer extract oc

let out_ml_runtime oc =
  let extract f = get_opt f.f_ml_runtime in
  out_fbuffer extract oc


let out_ml_runtime_mli oc =
  let extract f = get_opt f.f_ml_runtime_mli in
  out_fbuffer extract oc

let need_makefile f =
  match f.f_ml_runtime_mli, f.f_ml_runtime with
  | None, None -> false
  | _ -> true

let out_ml_plugin_or_loader dynloader oc f =

  (*
     module Self : .... =
     struct
        let self_module_name = "..."
        ...
        let opa_code = "..."
        let js_code  = "..." *)
  FBuffer.output   oc          f.f_plugin_up ;

  (*
        let dynloader ( get_register : ... ) =
          match get_register ...
          | None -> ...
          | Some ... ->
             begin
  *)
  output_string    oc          BPI.meta_plugin__02 ;

  (* The specific part, depends if loader or plugin *)
  FBuffer.output   oc          dynloader ;

  (*
             end
        let self = {
           ...
        }

        let self_register () = ...
     end

     let _ = Self.self_register () *)
  output_string    oc          BPI.meta_plugin__03 ;

  ()


let out_ml_loader oc f =
  out_ml_plugin_or_loader f.f_ml_dynloader_loader oc f


let out_ml_plugin oc f =
  out_ml_plugin_or_loader f.f_ml_dynloader_plugin oc f


(*
  Parsing files
*)

let directives = ["##extern"; "##register"; "##import"; "##module"; "##endmodule" ]

let mayforget t =
  String.is_contained "##" t &&
    List.exists (fun e -> String.is_contained e t) directives

let directive_prefix t =
  let t = String.ltrim t in
  List.exists (fun e -> String.is_prefix e t) directives

let unsecure t =
  let t = String.ltrim t in List.exists (fun e -> String.is_prefix e t) ["##"]

let check_style _options pos line =
  let (!!) fmt =
    OManager.printf "%a" FilePos.pp_citation pos ;
    OManager.serror fmt
  in
  let _ =
    if directive_prefix line then
      !! "Using directive prefix is a bad practice@\n"
  in
  let _ =
    if mayforget line then
      !! "This line contains a non parsed directive@\n"
  in
  let _ =
    if unsecure line then
      !! "This line contains @{<bright>'##'@} but not any known directive@\nIt will probably raise an error by compiling the file@\n"
  in
  ()


let cache_file filename =
  let contents =
    try
      File.content filename
    with
    | e ->
        OManager.error
          "@[<4>Cannot open file %S :@\n%s@]@\n" filename (Printexc.to_string e)
  in
  FilePos.add_file filename contents

let parse_line_factory process_directive set_last_directive options parser_rule do_check_style =
  (fun line -> (
     try
       let n, parsed_line = parser_rule line in
       if n < String.length line then
         BRState.error "Syntax error"
       else (
         (* Simplification of the maintenance, parsing of tags is independant *)
         match parsed_line with
         | ( BDir.Source (pos, line) ) as parsed_line -> (* as: parametric type *)
             (
               if do_check_style then
                 check_style options pos line
             );
             parsed_line

         | BDir.Directive (pos, tags, directive) -> (
             set_last_directive directive ;
             process_directive pos tags directive
           )
       )
     with
     | (Trx_runtime.SyntaxError _) as e ->
         BRState.error "@[<2>Syntax error@\n%s@]@\n" (Printexc.to_string e)

     (* support for ide mode, OManager will raise a special exception *)
     | e -> raise e
   )
  )

(*
  A warning for too long lines with directives
*)
let warning_size = 160

let parse_file_factory pprocess process_directive set_last_directive options parser_rule filename =
  let do_check_style = options.BI.check_style in
  let parse_multiline = parse_line_factory process_directive set_last_directive options parser_rule do_check_style in
  let aux (parsed_lines, multiline) line line_number =
    BRState.init_line ~line_number ;
    let trim_line = String.trim line in
    match multiline with
    | None ->
        (* search if this is the beginning of a multiline directive *)
        if String.is_prefix "##" trim_line
        then
          if String.is_suffix "\\" trim_line
          then (
            (* replace the last char by a space for concatenation *)
            trim_line.[pred (String.length trim_line)] <- ' ' ;
            parsed_lines, Some trim_line
          )
          else (
            let length = String.length line in
            let () =
              if length > warning_size then
                BRState.warning (
                  "This line is too long.@\n"^^
                  "@[<2>@{<bright>Hint@}:@\n"^^
                  "You can use several lines, with a '\\' at the end of unfinished lines@]"
                )
              in
            let parsed_line = parse_multiline trim_line in
            parsed_line::parsed_lines, None
          )
        else (
          let parsed_line = parse_multiline line in
          parsed_line::parsed_lines, None
        )

    | Some accu ->
        (* this is maybe not the last multiline *)
        if String.is_suffix "\\" trim_line
        then (
          (* replace the last char by a space for concatenation *)
          trim_line.[pred (String.length trim_line)] <- ' ' ;
          let accu = accu ^ trim_line in
          parsed_lines, Some accu
        )
        else (
          (* this is the last multitline *)
          let line = accu ^ line in
          let parsed_line = parse_multiline line in
          parsed_line::parsed_lines, None
        )
   in
  cache_file filename ;
  BRState.init_file ~filename;
  let content = File.content filename in
  let content = pprocess filename content in
  let lines_foldi f acc content =
    let lines = String.slice '\n' content in
    let rec aux nr acc lines =
      match lines with
      | line::tail -> aux (nr + 1) (f acc line nr) tail
      | [] -> acc
    in
    let acc = aux 1 acc lines in
    acc
  in
  let parsed_lines, multiline = lines_foldi aux ([], None) content in
  if Option.is_some multiline
  then BRState.error "@[<2>Syntax error@\nunfinished multiline directive@]@\n"
  ;
  let parsed_lines = List.rev parsed_lines in

  let parsed_file = {
    BDir.filename          = filename ;
    BDir.decorated_source  = parsed_lines ;
  }

  in

  parsed_file

(* Add some tags for the "register" directive based on its type *)
let add_register_tags bslty tags =
  let tags =
    if tags.BslTags.cps_bypass then
      { tags with
        BslTags.no_projection =
          let set =
            Option.default StringSet.empty tags.BslTags.no_projection in
          Some (StringSet.add "cps" set)
      }
    else tags
  in
  let second_order = tags.BslTags.second_order in
  let second_order = second_order || BslTypes.is_second_order bslty in
  let tags =
    { tags with
      BslTags.second_order = second_order ;
    }
  in
  tags

(*
  Feature: in some cases, you may want to add automatically some tags when
  you see some properties in the types, or in the keys.
*)
let bypass_auto_tags directive tags =
  match directive with
  | BDir.Register (_, _, _, bslty) -> add_register_tags bslty tags
  | _ ->
      tags

let bypass_process_directive pos tags directive =
  match directive with
  | BDir.Property _ ->
      let ptags = BslTags.parse ~pos [] in
      BDir.Directive (pos, ptags, BDir.Property tags)
  | _ ->
      let tags = BslTags.parse ~pos tags in
      let tags = bypass_auto_tags directive tags in
      BDir.Directive (pos, tags, directive)


let parse_opa_file pprocess options f =
  (* used only in bypass files *)
  let set_last_directive _ = () in
  let process_directive pos tags directive =
    let tags = BslTags.parse ~pos tags in
    BDir.Directive (pos, tags, directive)
  in
  let parsed = parse_file_factory pprocess process_directive set_last_directive options
    BRParse.parse_bslregisterparser_opalang f in
  ( parsed : BslDirectives.opalang_decorated_file )

let parse_bypass_file pprocess options filename =
  let set_last_directive d = BRState.set_last_directive d in
  let process_directive = bypass_process_directive in
  let parsed = parse_file_factory pprocess process_directive set_last_directive options
    BRParse.parse_bslregisterparser_bypasslang filename in
  ( parsed : BslDirectives.bypasslang_decorated_file )

let parse_js_bypass_file_new pprocess filename =
  let contents = pprocess filename (File.content filename) in
  FilePos.add_file filename contents;
  match BslJsParse.parse_string ~filename contents with
  | `error e -> js_error_after_pp filename contents e
  | `success {BslJsParse. directives; code = contents} ->
    let add_tags ((pos, tags, directive) as p) =
      match directive with
      | BDir.Js.Register (_, _, bslty) ->
        let tags = add_register_tags bslty tags in
        (pos, tags, directive)
      | _ -> p
    in
    let directives = List.map add_tags directives in
    { BslJs. filename; contents; directives; }

let parse_js_bypass_file pprocess options filename =
  if options.BI.js_classic_bypass_syntax then
    BslJs.Classic (parse_bypass_file pprocess options filename)
  else
    BslJs.DocLike (parse_js_bypass_file_new pprocess filename)

(*
  Main preprocessor
*)

let check_ml_filename filename =
  let basename = File.chop_extension filename in
  if not (String.is_universal_ident basename)
  then
    OManager.error (
      "Unsupported filename @{<bright>%S@}@\n"^^
      "@[<2>@{<bright>Hint@}:@\n"^^
      "This is not a valid ml module name.@]@\n"
    )
      filename

let preprocess_file session filename =
  let basename = Filename.basename filename in
  let ext = File.extension basename in
  match ext with
  | "opa" ->
      let decorated_file = parse_opa_file session.s_pprocess session.s_options filename in
      let s_rev_opa_decorated_files =
        decorated_file :: session.s_rev_opa_decorated_files in

      let session = {
        session with
          s_rev_opa_decorated_files ;
      } in
      session

  | "ml" ->
      check_ml_filename basename ;
      let parsed_file = parse_bypass_file session.s_pprocess session.s_options filename in
      let s_rev_ml_parsed_files =
        parsed_file :: session.s_rev_ml_parsed_files in

      let session = {
        session with
          s_rev_ml_parsed_files ;
          s_has_server_code = true ;
      } in
      session


  (* FIXME: right now we can't choose a preprocessor to pass bsl
     files, so we ignore external preprocessors for now and use our
     own here for JS. We need the PP to read our own bsl files
     (e.g. server). We need to find a better way of specifying
     this. *)
  | "js" ->
    let pp_js filename contents =
      let ppenv =
        Pprocess.fill_with_sysenv Pprocess.empty_env in
      let ppopt = Pprocess.default_options ppenv in
      let contents = Pprocess.process ~name:filename
        Pplang.js_description ppopt contents in
      contents
    in
    let parsed_file = parse_js_bypass_file pp_js session.s_options filename in
    let s_rev_js_parsed_files =
      parsed_file :: session.s_rev_js_parsed_files in

    let session = {
      session with
        s_rev_js_parsed_files ;
    } in
    session

  | "nodejs" ->
      let pp_nodejs filename contents =
        let ppenv =
          let ppenv = Pprocess.fill_with_sysenv Pprocess.empty_env in
          let ppenv = Pprocess.add_env "OPABSL_NODE" "1" ppenv in
          Pprocess.add_env "OPA_CPS_CLIENT" "1" ppenv in
        let ppopt = Pprocess.default_options ppenv in
        let contents = Pprocess.process ~name:filename
          Pplang.js_description ppopt contents in
        contents
      in
      let parsed_file = parse_js_bypass_file pp_nodejs session.s_options filename in
      let s_rev_nodejs_parsed_files =
        parsed_file :: session.s_rev_nodejs_parsed_files in

      let session = {
        session with
          s_rev_nodejs_parsed_files ;
          s_has_server_code = true ;
      } in
      session

  | "jsconf" ->
      let s_js_confs = session.s_js_confs in
      let s_js_confs = BslJsConf.fold filename s_js_confs in

      let session = {
        session with
          s_js_confs ;
      } in
      session

  | "nodejsconf" ->
      let s_js_confs = session.s_js_confs in
      let s_js_confs = BslJsConf.fold filename s_js_confs in

      let session = {
        session with
          s_js_confs ;
      } in
      session

  | "conf" ->
      let s_conf = session.s_conf in
      let s_conf = BslConf.fold ~filename s_conf in

      let session = {
        session with
          s_conf ;
      } in
      session

  | _ ->
      OManager.error
        "@[<2>File %S has extension @{<bright>%S@}@\nThis is not a file type handled by @{<bright>bslregister@}@]@\n"
        filename ext
