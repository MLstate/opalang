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

(* depends *)
module Format = BaseFormat
module List = BaseList
module String = BaseString

(* shorthands *)
module B     = BslTypes
module D     = BslDirectives
module BI    = BslInterface
module BPI   = BslPluginInterface
module L     = BslLanguage

type ml_runtime = FBuffer.t
type ml_runtime_mli = FBuffer.t
type module_name = string
type skey = string
type pos = FilePos.pos
type opaname = string


(* debug *)

let debug fmt =
  OManager.printf ("@[<2>"^^fmt^^"@]@.")


(* Note about ty_spec_map, types scope:
   from a key, returns the opaname, and the path in ocaml module of the definition of the type.
*)


let fbuffer () = FBuffer.create (8 * 1024)

type ty_spec_map = (opaname * module_name list) BslKeyMap.t

type env = {
  (* this is just for error messages *)
  last_module         : FilePos.pos ;
  last_endmodule      : FilePos.pos ;

  ml_runtime          : ml_runtime ;
  ml_runtime_mli      : ml_runtime_mli ;

  ml_line             : int ;

  property_mli        : bool ;

  rev_path            : ( skey * module_name * pos ) list ;

  runtime_module_name : module_name ;

  ty_spec_map         : ty_spec_map ;

  warn_x_field        : unit ;
}

(*
  Special env for separation of plugin compilation
*)
module SeparatedEnv :
sig
  type t

  val init : ty_spec_map:ty_spec_map -> t
  val fold : t -> BslPluginInterface.plugin -> t

  val ty_spec_map : t -> ty_spec_map

  module SideEffect :
  sig
    val get_ocaml_env : unit -> BslPluginInterface.ocaml_env
    val add_ty_spec_map : BslKey.t -> (opaname * module_name list) -> unit
  end
end =
struct
  type t = {
    ty_spec_map : ty_spec_map ;
  }
  let init ~ty_spec_map = {
    ty_spec_map ;
  }
  let ty_spec_map t = t.ty_spec_map
  external wrap : t -> BslPluginInterface.ocaml_env = "%identity"
  external unwrap : BslPluginInterface.ocaml_env -> t = "%identity"

  let fold t plugin =
    let tp = unwrap plugin.BPI.ocaml_env in
    let ty_spec_map =
      let fold key ((_opaname, path1) as value) ty_spec_map =
        BslKeyMap.replace key
          (function
           | None -> value
           | Some ((_, path2) as value2) ->
               OManager.warning ~wclass:WarningClass.bsl_register  (
                 "@[<2>The type @{<bright>%a@} is defined in several imported plugins:@\n"^^
                   "path1: %a@\n"^^
                   "path2: %a@\n"^^
                   "This plugin will use the path2.@]"
               )
                 BslKey.pp key
                 (Format.pp_list "." Format.pp_print_string) path1
                 (Format.pp_list "." Format.pp_print_string) path2
               ;
               value2
          ) ty_spec_map
      in
      BslKeyMap.fold fold tp.ty_spec_map t.ty_spec_map
    in
    {
      ty_spec_map ;
    }

  module SideEffect =
  struct
    let ty_spec_map = BslKeyTable.create 16
    let add_ty_spec_map key value = BslKeyTable.replace ty_spec_map key value

    let get_ocaml_env () =
      let ty_spec_map = BslKeyTable.fold BslKeyMap.add ty_spec_map BslKeyMap.empty in
      let t = {
        ty_spec_map ;
      } in
      wrap t
  end
end

let nopos = FilePos.nopos "BslOcaml"

let empty =
  let empty = {
    last_module             = nopos ;
    last_endmodule          = nopos ;

    ml_runtime              = fbuffer () ;
    ml_runtime_mli          = fbuffer () ;

    ml_line                 = 1 ;

    property_mli            = false ;
    rev_path                = [] ;

    runtime_module_name     = "" ;

    ty_spec_map             = BslKeyMap.empty ;

    warn_x_field            = () ;
  } in
  ( empty : env )

let (!!) pos fmt =
  OManager.printf "%a" FilePos.pp_citation pos ;
  OManager.error fmt

let warning pos fmt =
  let wclass = WarningClass.bsl_register in
  OManager.warning ~wclass ("%a"^^fmt) FilePos.pp_citation pos

let ocaml_module = String.capitalize
let kw_end = "end"
let kw_module = "module"
let kw_struct = "struct"
let kw_sig = "sig"

(*
  Map a name of type, taking in consederation the current path,
  and the ty_spec_map of the env.
  @raise Not_found if the type reference does not correspond to
  any known type.
*)
let find name ty_spec_map rev_path =
  let skey = String.rev_concat_map "_" BslKey.normalize_string (name :: rev_path) in
  let key = BslKey.of_string skey in
  let result = BslKeyMap.find_opt key ty_spec_map in
  let _=
    #<If:BSL_REGISTER $equals "find">
      match result with
      | Some (opaname, _) ->
          debug "find:%S opa:%S@\n" skey opaname
      | None ->
          debug "find:%S None@\n" skey
    #<End>
  in
  result

let env_map_ty_reference env pos skey =
  let ty_spec_map = env.ty_spec_map in
  let rev_pwd_keys = List.map (fun (s, _, _) -> s) env.rev_path in
  let pwd_ocaml = List.rev_map (fun (_, m, _) -> m) env.rev_path in
  (fun name ->
     (* 1) find the type reference *)
     let rec aux = function
       | [] -> (
           match find name ty_spec_map [] with
           | Some found -> found
           | None -> (
               (* inefficient, but it is just for an error message (computed once) *)
               let keys = BslKeyMap.keys_as_string ty_spec_map in
               OManager.printf "%a" FilePos.pp_citation pos ;
               OManager.printf "Cannot resolve the reference to the type @{<bright>%s@}@\n" name ;
               let _ = OManager.printf "%a" (HintUtils.pp_suggestion keys) name in
               OManager.error "##register: @{<bright>failing type resolution@} for key '@{<brigth>%s@}'@\n" skey
             )
         )
       | ( _::tl ) as rev_path -> (
           match find name ty_spec_map rev_path with
           | Some found -> found
           | None -> aux tl
         )
     in
     match aux rev_pwd_keys with
     | opaname, ocaml_path ->
         let module_path = OcamlUtils.Module.module_path ~full:ocaml_path ~pwd:pwd_ocaml in
         opaname, String.concat_map "." (fun s -> s) module_path
  )


let env_map_ty_reference_for_select ~select env pos skey ty =
  let map_ref = env_map_ty_reference env pos skey in
  BslTypes.Walk.map
    (function
     | B.External (p, name, vs) ->
         let name' = select (map_ref name) in
         let _ =
           #<If:BSL_REGISTER $equals "map_ref">
             debug "map_ref(%S): %S ==> %S" skey name name'
             #<End>
         in
         B.External (p, name', vs)

     | t -> t) ty

let env_map_ty_reference_for_ocaml env =
  env_map_ty_reference_for_select ~select:snd env

let env_map_ty_reference_for_opa env pos skey ty =
  let maped_ty =
    env_map_ty_reference_for_select ~select:fst env pos skey ty
  in
  let _ =
    #<If:BSL_REGISTER $equals ("walk_"^skey)>
      BslKeyMap.iter (
        fun key (opa, ocaml) ->
          Printf.fprintf stderr "key:%S ==> opa:%S, ocaml:%S\n%!"
            (BslKey.to_string key) opa
            (String.concat_map "." (fun s -> s) ocaml)
      ) env.ty_spec_map ;
      debug "OPA:walking through %a returns %a@\n" BslTypes.pp ty BslTypes.pp maped_ty
    #<End>
  in
  maped_ty


(*
  Compute the module path in ocaml, from the current_state of the env and the key
*)
let env_ocaml_path env skey =
  let skey_rev_path = List.map (fun (_, s, _) -> s) env.rev_path in
  List.rev (skey :: skey_rev_path)

(*
  Compute the module path in skeys, from the current state of the env and the key
*)
let env_rp_ks env skey =
  let skey_rev_path = List.map (fun (s, _, _) -> s) env.rev_path in
  List.rev (skey :: skey_rev_path)


let env_rp_ty env bslty =
  (* TODO: map bslty according to the path *)
  ignore env;
  bslty

let env_rt_ks = env_rp_ks

let env_rt_ty_skey env tags skey =
  match tags.BslTags.opaname with
  | false ->
      let skey_rev_path = List.map (fun (s, _, _) -> s) env.rev_path in
      String.rev_concat_map "_" BslKey.normalize_string ( skey :: skey_rev_path )
  | true -> skey

let env_rp_implementation env implementation injected =
  if injected then implementation
  else
    let impl_rev_path = List.map (fun (_, m, _) -> m) env.rev_path in
    let impl_rev_path = implementation :: impl_rev_path in
    let runtime = env.runtime_module_name ^"." in
    let implementation = String.rev_concat_map ~left:runtime "." (fun s -> s) impl_rev_path in
    implementation


let env_add_module pos skey implementation env =
  let implementation = Option.default skey implementation in

  let name = ocaml_module skey in
  let implementation = ocaml_module implementation in

  let add_module = name, implementation, pos in
  let rev_path = add_module :: env.rev_path in
  let last_module = pos in

  let ml_runtime = env.ml_runtime in
  let ml_runtime =
    FBuffer.printf ml_runtime "%s %s = %s@\n" kw_module implementation kw_struct in

  let ml_line = succ env.ml_line in

  let ml_runtime_mli = env.ml_runtime_mli in
  let ml_runtime_mli =
    FBuffer.printf ml_runtime_mli "%s %s : %s@\n" kw_module implementation kw_sig in

  let env = {
    env with
      last_module ;
      ml_runtime ;
      ml_runtime_mli ;
      ml_line ;
      rev_path ;
  } in

  env


let env_add_endmodule pos env =
  let rev_path =
    match env.rev_path with
    | [] ->
        let pos = List.fold_left FilePos.merge_pos pos [ env.last_module ; env.last_endmodule ] in
        !! pos "This @{<bright>##endmodule@} does not match any @{<bright>##module@}@\n"

    | _::tl ->
        tl
  in
  let last_endmodule = pos in
  let ml_runtime = env.ml_runtime in
  let ml_runtime = FBuffer.addln ml_runtime kw_end in
  let ml_line = succ env.ml_line in

  let ml_runtime_mli = env.ml_runtime_mli in
  let ml_runtime_mli = FBuffer.addln ml_runtime_mli kw_end in

  let env = {
    env with
      last_endmodule ;
      ml_runtime ;
      ml_runtime_mli ;
      ml_line ;
      rev_path ;
  } in

  env


let fold_source_elt ~dynloader_interface ~filename env source_elt =
  let env =
    match source_elt with
    | D.Source (pos, source) ->
        let _, ml_line = FilePos.get_one_loc pos in
        let ml_runtime     = env.ml_runtime in
        let ml_runtime =
          if ml_line = env.ml_line then ml_runtime else
            FBuffer.printf ml_runtime "#%d %S@\n" ml_line filename
        in
        let ml_runtime     = FBuffer.addln ml_runtime source in
        let env = {
          env with
            ml_runtime      = ml_runtime ;
            ml_line = succ ml_line ;
        } in

        env

    | D.Directive (pos, tags, directive) -> (
        match directive with
        | D.ExternalTypeDef (skey, params, implementation) ->
            (* Register part *)
            let rt_ks = env_rt_ks env skey in
            let rt_ty_skey = env_rt_ty_skey env tags skey in

            let key = BslKey.normalize rt_ty_skey in
            (* for ocaml generation, all types are normalized *)
            let skey = BslKey.normalize_string skey in
            let tyitem = rt_ty_skey, env_ocaml_path env skey in
            let _ =
              #<If:BSL_REGISTER $equals "type">
                match tyitem with
                | opa, ocaml ->
                    debug "##extern-type %s ==> opa:%S, ocaml:%S@\n" (BslKey.to_string key) opa
                      (String.concat_map ";" (fun s -> s) ocaml)
              #<End>
            in

            let ty_spec_map =
              let replace = function
                | None ->
                    tyitem
                | Some (_, conflict_path) ->
                    (* Check of overwrite of key in ty_spec_map *)
                    warning pos (
                      "an extern-type with the same opa name is already defined:@\n"^^
                        "key: %a@\n"^^
                        "path: %a@\n"^^
                        "This is a bad practice, and will be rejected in a further version of Opa"
                    )
                      BslKey.pp key
                      (Format.pp_list "." Format.pp_print_string) conflict_path
                    ;
                    tyitem
              in
              let () = SeparatedEnv.SideEffect.add_ty_spec_map key tyitem in
              BslKeyMap.replace key replace env.ty_spec_map
            in

            let params = List.map (fun v -> B.TypeVar (pos, v)) params in
            let bslty = B.External (pos, skey, params) in

            let rt_ty = B.External (pos, rt_ty_skey, params) in
            let rt = { BPI.
              rt_ks = rt_ks ;
              rt_ty = rt_ty ;
            } in
            BPI.apply_register_type dynloader_interface.BPI.register_type rt ;

            (* Generation part *)

            let implementation =
              match implementation with
              | None -> ""
              | Some code -> " = " ^ code
            in

            let ml_runtime = env.ml_runtime in
            let ml_runtime = FBuffer.printf ml_runtime
              "%a%s@\n" BslTypesGeneration.Ocaml.pp_definition bslty implementation in
            let ml_line = succ env.ml_line in

            let ml_runtime_mli = env.ml_runtime_mli in
            let ml_runtime_mli =
              if env.property_mli
              then
                FBuffer.printf ml_runtime_mli
                  "%a%s@\n" BslTypesGeneration.Ocaml.pp_definition bslty implementation
              else
                FBuffer.printf ml_runtime_mli
                  "%a@\n" BslTypesGeneration.Ocaml.pp_definition bslty
            in

            let env = {
              env with
                ml_runtime ;
                ml_runtime_mli ;
                ml_line ;
                ty_spec_map ;
            } in

            env

        | D.OpaTypeDef (skey, params) ->
            (* Register part *)
            let () =
              if not tags.BslTags.opaname
              then
                !! pos "an opa-type cannot be @{<brigth>normalized@}"
            in

            (* for opa types, all rt_ty_skey are opaname *)
            let rt_ty_skey = skey in

            let skey = "opa_" ^ skey in
            let rt_ks = env_rt_ks env skey in

            let key = BslKey.normalize rt_ty_skey in
            (* for ocaml generation, all types are normalized *)
            let skey = BslKey.normalize_string skey in
            let tyitem = rt_ty_skey, env_ocaml_path env skey in
            let _ =
              #<If:BSL_REGISTER $equals "type">
                match tyitem with
                | opa, ocaml ->
                    debug "##opa-type %s ==> opa:%S, ocaml:%S@\n" (BslKey.to_string key) opa
                      (String.concat_map ";" (fun s -> s) ocaml)
              #<End>
            in

            let ty_spec_map =
              let replace = function
                | None ->
                    tyitem
                | Some (_opaname, conflict_path) ->
                    let path = String.concat "." conflict_path in
                    (* Check of overwrite of key in ty_spec_map *)
                    warning pos (
                      "an opa-type with the same opa name is already defined:@\n"^^
                        "key: %a@\n"^^
                        "path: %s@\n"^^
                        "This is allowed, but this is a bad practice because this hides the@ "^^
                        "previous definition, and this will lead to code duplication.@\n"^^
                        "@[<2>@{<bright>Hint@}:@\n"^^
                        "Use rather functions working on type %s"^^
                        "@]"
                    )
                      BslKey.pp key
                      path
                      path
                    ;
                    tyitem
              in
              let () = SeparatedEnv.SideEffect.add_ty_spec_map key tyitem in
              BslKeyMap.replace key replace env.ty_spec_map
            in


            let bslty_params = List.map (fun v -> B.TypeVar (pos, v)) params in
            let rt_ty = B.External (pos, rt_ty_skey, bslty_params) in
            let rt = { BPI.
              rt_ks = rt_ks ;
              rt_ty = rt_ty ;
            } in
            BPI.apply_register_type dynloader_interface.BPI.register_type rt ;


           (* Generation part *)
            let scope = BslTypes.TypeVarPrint.new_scope () in
            let typevar fmt = BslTypes.TypeVarPrint.pp scope fmt in
            let st_ty = Base.Format.sprintf "%a" (OcamlPrint.pp_parameters typevar skey) params in

            let ml_runtime = env.ml_runtime in
            let ml_runtime = FBuffer.printf ml_runtime
              "type %s = ServerLib.ty_record@\n" st_ty
            in
            let ml_runtime = FBuffer.printf ml_runtime
              "external wrap_%s : ServerLib.ty_record -> %s@ = \"%%identity\"\n" skey st_ty
            in
            let ml_runtime = FBuffer.printf ml_runtime
              "external unwrap_%s : %s -> ServerLib.ty_record = \"%%identity\"@\n" skey st_ty
            in
            (* keep coherence for line count in the ml *)
            let ml_line = env.ml_line + 2 in

            let ml_runtime_mli = env.ml_runtime_mli in
            let ml_runtime_mli = FBuffer.printf ml_runtime_mli
              "type %s  = ServerLib.ty_record@\n" st_ty
            in

            let env = {
              env with
                ml_runtime ;
                ml_runtime_mli ;
                ml_line ;
                ty_spec_map ;
            } in

            env

        | D.Module (skey, implementation) ->
            env_add_module pos skey implementation env

        | D.EndModule ->
            env_add_endmodule pos env

        | D.Register (skey, source_opt, injected, bslty) ->
            let implementation as skey_mli = Option.default skey source_opt in
            (* Register part *)
            let rp_ks = env_rp_ks env skey in
            let rp_ty = env_map_ty_reference_for_opa env pos skey bslty in
            let parsed_t = BslTags.parsed_t tags in
            let implementation = env_rp_implementation env implementation injected in
            let rp_ips = [ L.ml, filename, parsed_t, rp_ty, implementation ] in
            let rp_obj = None in
            let rp = { BPI.
              rp_ks  = rp_ks ;
              rp_ty  = rp_ty ;
              rp_ips = rp_ips ;
              rp_obj = rp_obj ;
            } in
            BPI.apply_register_primitive dynloader_interface.BPI.register_primitive rp ;

            (* Generation part *)
            let bslty = env_map_ty_reference_for_ocaml env pos skey bslty in
            let skey_ocaml =
              if OcamlUtils.Ident.is_operator skey then Printf.sprintf "( %s )" skey
              else skey in
            let ml_runtime = env.ml_runtime in
            let ml_runtime =
              if injected then (
                FBuffer.printf ml_runtime "let %s = %s@\n" skey_ocaml implementation
              )
              else FBuffer.printf ml_runtime "(* ##register %s \\ %s : %a *)@\n"
                skey implementation BslTypes.pp bslty
            in
            let ml_line = succ env.ml_line in

            let skey_ocaml_mli = if injected then skey_ocaml else skey_mli in
            let ml_runtime_mli = env.ml_runtime_mli in
            let ml_runtime_mli =
              FBuffer.printf ml_runtime_mli
                "val %s : %a@\n" skey_ocaml_mli BslTypesGeneration.Ocaml.pp bslty
            in

            let env = {
              env with
                ml_runtime ;
                ml_runtime_mli ;
                ml_line ;
            } in

            env

        (* We can actually do something in Ocaml too (currently Args is used only is Javascript)
           let f x y z =
           (
             <body>
           )
        *)
        | D.Args (name, args, bslty) ->
            ignore name ;
            ignore args ;
            ignore bslty ;
            !! pos "The directive ##args is not available in OCaml source files"

        | D.Property p ->
            let property_mli =
              if List.mem_assoc "mli" p then true
              else if List.mem_assoc "endmli" p then false
                else env.property_mli
            in
            let env = {
              env with
                property_mli ;
            } in

            env
      )
  in
  env


let env_add_file_line ~filename env =
  let ml_runtime = env.ml_runtime in
  let ml_runtime = FBuffer.printf ml_runtime "#1 %S@\n" filename in

  let ml_runtime_mli = env.ml_runtime_mli in
  let ml_runtime_mli = FBuffer.printf ml_runtime_mli "#1 \"generated interface for %s\"@\n" filename in

  let env = {
    env with
      ml_runtime      = ml_runtime ;
      ml_runtime_mli  = ml_runtime_mli ;
      ml_line         = 1 ;
  } in
  env


let fold_decorated_file ~dynloader_interface env decorated_file =
  let filename = decorated_file.D.filename in
  let implementation = OcamlUtils.Module.of_filename filename in
  (* we add a module for each file *)
  let env = env_add_module nopos implementation None env in
  let env = env_add_file_line ~filename env in
  let source = decorated_file.D.decorated_source in
  let env =
    List.fold_left (fold_source_elt ~dynloader_interface ~filename) env source
  in
  let _ =
    match env.rev_path with
    | [] | [_]-> ()
    | list ->
        let list = List.rev (List.tl (List.rev list)) in
        List.iter (
          fun (_, _, pos) ->
            OManager.printf "%a" FilePos.pp_citation pos ;
        ) list;
        OManager.error
          "File %S: unclosed module(s)@\n@[<2>@{<bright>Hint@}:@\nAdd the corresponding @{<bright>##endmodule@} directive(s)@]@\n"
          filename
  in
  let env = env_add_endmodule nopos env in
  env

(*
  update the ty_spec_map with the previous plugin (cf in options)
*)
let preprocess ~options ~plugins ~dynloader_interface decorated_files =
  if decorated_files = [] then
    let _ = SeparatedEnv.init ~ty_spec_map:empty.ty_spec_map in
    let ocaml_env = SeparatedEnv.SideEffect.get_ocaml_env () in
    ocaml_env, None, None
  else
  let env = empty in
  (*
    Add open of depends plugins
  *)
  let ml_runtime, ml_runtime_mli =
    let ml = FBuffer.addln env.ml_runtime "(* plugins depends *)" in
    let mli = FBuffer.addln env.ml_runtime_mli "(* plugins depends *)" in
    List.fold_left
      (fun (ml, mli) plugin ->
         let () =
           #<If:BSL_REGISTER $contains "open">
             debug "adding open %s" plugin.BPI.ml_runtime
           #<End>
         in
         let ml  = FBuffer.printf ml  "open %s\n" plugin.BPI.ml_runtime in
         let mli = FBuffer.printf mli "open %s\n" plugin.BPI.ml_runtime in
         ml, mli
      ) (ml, mli) plugins
  in
  let sep_env = SeparatedEnv.init ~ty_spec_map:env.ty_spec_map in
  let sep_env = List.fold_left SeparatedEnv.fold sep_env plugins in
  let ty_spec_map = SeparatedEnv.ty_spec_map sep_env in
  let runtime_module_name = OcamlUtils.Module.of_filename options.BI.ml_runtime_filename in
  let env = {
    env with
      ty_spec_map ;
      ml_runtime ;
      ml_runtime_mli ;
      runtime_module_name ;
  } in
  let env =
    List.fold_left (fold_decorated_file ~dynloader_interface) env decorated_files in
  let ocaml_env = SeparatedEnv.SideEffect.get_ocaml_env () in
  ocaml_env, Some env.ml_runtime, Some env.ml_runtime_mli
