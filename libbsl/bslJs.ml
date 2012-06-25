(*
    Copyright © 2011, 2012 MLstate

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
(* CF mli *)

(*
  Note about some duplication of code between BslOCaml and BslJs:

  The previous of libbsl was implemented using a functor for sharing some part of this process.
  The experience has shown that the tratement done on Javascript and Ocaml is quite different,
  which has lead to have a complexe code in the argument of the functor, not very natural.

  I (mathieu) think that proceding now with 2 different files is more relax, the implementation
  can be different, without hacking the interfaces of functors and args.
*)

module Format = BaseFormat
module String = BaseString
let (|>) = InfixOperator.(|>)

module BPI = BslPluginInterface
module D = BslDirectives

type filename = string
type contents = string
type module_name = string
type skey = string
type pos = FilePos.pos
type opaname = string

type js_file = FBuffer.t


let fbuffer () = FBuffer.create (8 * 1024)

type ty_spec_map = (opaname * module_name list) BslKeyMap.t
type renaming = string StringMap.t

type env = {
  (* this is just for error messages *)
  last_module         : FilePos.pos ;
  last_endmodule      : FilePos.pos ;

  (* accumulating files in a fold *)
  rev_files_js_code   : (filename * contents) list ;

  rev_path            :  ( skey * module_name * pos ) list ;

  ty_spec_map         : ty_spec_map ;
  renaming            : renaming ;
  warn_x_field        : unit ;
}

module SeparatedEnv :
sig
  type t

  val init : ty_spec_map:ty_spec_map -> renaming:renaming -> t
  val fold : t -> BslPluginInterface.plugin -> t

  val ty_spec_map : t -> ty_spec_map
  val renaming : t -> renaming

  module SideEffect :
  sig
    val get_javascript_env : unit -> BslPluginInterface.javascript_env
    val add_ty_spec_map : BslKey.t -> (opaname * module_name list) -> unit
    val add_renaming : string -> string -> unit
  end
end =
struct
  type t = {
    ty_spec_map : ty_spec_map ;
    renaming : renaming ;
  }
  let init ~ty_spec_map ~renaming = {
    ty_spec_map ;
    renaming ;
  }
  let ty_spec_map t = t.ty_spec_map
  let renaming t = t.renaming
  external wrap : t -> BslPluginInterface.javascript_env = "%identity"
  external unwrap : BslPluginInterface.javascript_env -> t = "%identity"

  let fold t plugin =
    let tp = unwrap plugin.BPI.javascript_env in
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
    let renaming =
      let fold key value renaming =
        StringMap.replace key
          (function
           | None -> value
           | Some value2 when value = value2 -> value
           | Some value2 ->
               OManager.warning ~wclass:WarningClass.bsl_register  (
                 "@[<2>The key @{<bright>%s@} is maped to different primitives:@\n"^^
                   "renaming1: %s@\n"^^
                   "renaming2: %s@\n"^^
                   "This plugin will use the renaming2.@]"
               )
                 key
                 value
                 value2
               ;
               value2
          ) renaming
      in
      StringMap.fold fold tp.renaming t.renaming
    in
    {
      renaming ;
      ty_spec_map ;
    }

  module SideEffect =
  struct
    let ty_spec_map = BslKeyTable.create 16
    let add_ty_spec_map key value = BslKeyTable.replace ty_spec_map key value

    let renaming = Hashtbl.create 16
    let add_renaming key value = Hashtbl.replace renaming key value

    let get_javascript_env () =
      let ty_spec_map = BslKeyTable.fold BslKeyMap.add ty_spec_map BslKeyMap.empty in
      let renaming = Hashtbl.fold StringMap.add renaming StringMap.empty in
      let t = {
        ty_spec_map ;
        renaming ;
      } in
      wrap t
  end
end

let nopos = FilePos.nopos "BslJs"

let empty =
  let empty = {
    last_module          = nopos ;
    last_endmodule       = nopos ;

    rev_files_js_code    = [] ;

    rev_path             = [] ;
    ty_spec_map          = BslKeyMap.empty ;
    renaming             = StringMap.empty ;
    warn_x_field         = () ;
  } in
  ( empty : env )


let (!!) pos fmt =
  OManager.printf "%a" FilePos.pp_citation pos ;
  OManager.error fmt

let warning pos fmt =
  let wclass = WarningClass.bsl_register in
  OManager.warning ~wclass ("%a"^^fmt) FilePos.pp_citation pos

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
  result

let check ~options ~depends files_js_code =
  ignore options ;
  (* appending all the code *)
  let all_files_js_code = depends @ files_js_code in
  let all_js_code = String.concat_map "\n" snd all_files_js_code in
  ignore all_js_code ;
  ()

let js_module skey =String.capitalize ( skey )
let js_module_of_filename skey =
  let skey = String.capitalize (
    Filename.chop_extension ( Filename.basename skey )
  ) in
  let skey = String.multi_replace skey [
    "-", "_dash_" ;
    ".", "_dot_" ;
  ] in
  skey

let env_add_module pos skey implementation env =
  let implementation = Option.default skey implementation in
  let name = js_module skey in
  let implementation = js_module implementation in
  let add_module = name, implementation, pos in
  let rev_path = add_module :: env.rev_path in
  let env = {
    env with
      rev_path        = rev_path ;
      last_module     = pos;
  } in

  env

let env_rt_ty_skey env tags skey =
  match tags.BslTags.opaname with
  | false ->
      let skey_rev_path = List.map (fun (s, _, _) -> s) env.rev_path in
      String.rev_concat_map "_" BslKey.normalize_string ( skey :: skey_rev_path )
  | true -> skey

let env_add_endmodule pos env =
  let rev_path =
    match env.rev_path with
    | [] ->
        let pos = List.fold_left FilePos.merge_pos pos [ env.last_module ; env.last_endmodule ] in
        !! pos "This @{<bright>##endmodule@} does not match any @{<bright>##module@}@\n"

    | _::tl -> tl
  in
  let env = {
    env with
      rev_path        = rev_path ;
      last_endmodule  = pos;
  } in

  env

let env_rp_ks env skey =
  let skey_rev_path = List.map (fun (s, _, _) -> s) env.rev_path in
  List.rev (skey :: skey_rev_path)

let env_map_ty_reference env pos skey =
  let ty_spec_map = env.ty_spec_map in
  let rev_pwd_keys = List.map (fun (s, _, _) -> s) env.rev_path in
  let _js_ocaml = List.rev_map (fun (_, m, _) -> m) env.rev_path in
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
     | opaname, path ->
         let module_path = path in
         opaname, String.concat_map "." (fun s -> s) module_path
  )


let env_map_ty_reference_for_select ~select env pos skey ty =
  let map_ref = env_map_ty_reference env pos skey in
  BslTypes.Walk.map
    (function
     | BslTypes.External (p, name, vs) ->
         let name' = select (map_ref name) in
         BslTypes.External (p, name', vs)

     | t -> t) ty

let env_map_ty_reference_for_opa env pos skey bslty =
  env_map_ty_reference_for_select ~select:fst env pos skey bslty

let env_rp_implementation env implementation injected =
  if injected then implementation
  else
    let impl_rev_path = List.map (fun (_, m, _) -> m) env.rev_path in
    let impl_rev_path = implementation :: impl_rev_path in
    let implementation = String.rev_sconcat "_" impl_rev_path in
    implementation

let env_js_path = env_rp_ks

(*
  The fold update the FBuffer
*)
let debug fmt =
  OManager.printf ("@[<2>"^^fmt^^"@]@.")



(**
   regexp used to process bypass alias : when a user use %%BslSource.BslModule.bslkey%%,
   the bsl preprocessor replace the alias by it's implementation
   Str.full_split split_regexp "texte %%BslCps.Notcps_compatibility.thread_context%%; texte ";;
*)
let split_regexp = Str.regexp "%%[ ]*[a-zA-Z\\._]+[ ]*%%";;


let rec fold_source_elt ~dynloader_interface ~filename
    ~lang (env, js_file, exports) source_elt =

  let env, js_file, exports =
    match source_elt with
    | D.Source (pos, source) ->
        let splitted = Str.full_split split_regexp source in
        let source = String.concat "" (List.map
          (function
            | Str.Text t -> t
            | Str.Delim d ->
                let d = String.sub d 2 ( (String.length d) - 4 ) in
                let d' = BslKey.normalize (String.trim d) |> BslKey.to_string in
                match StringMap.find_opt d' env.renaming with
                | Some s -> s
                | None ->
                    let _ =
                      let keys =
                        StringMap.fold
                          (fun k _v acc -> k :: acc)
                          env.renaming
                          []
                      in
                      let _ = OManager.printf "%a" (HintUtils.pp_suggestion keys) d' in
                      ()
                    in
                    OManager.error "cannot replace @{<bright>%s@} by the javascript implementation@\nKey not found, position : %a@\n@." d FilePos.pp_citation pos
          )
          splitted) in
        let js_file = FBuffer.addln js_file source in
        env, js_file, exports

    | D.Directive (pos, tags, directive) -> (
        match directive with
        | D.OpaTypeDef (skey, params) ->
            let () =
              if not tags.BslTags.opaname
              then
                !! pos "an opa-type cannot be @{<brigth>normalized@}"
            in

            (* for opa types, all rt_ty_skey are opaname *)
            let rt_ty_skey = skey in

            let skey = "opa_" ^ skey in
            let rt_ks = env_rp_ks env skey in
            let key = BslKey.normalize rt_ty_skey in
            let skey = BslKey.normalize_string skey in
            let tyitem = rt_ty_skey, env_js_path env skey in

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

            let params = List.map (fun v -> BslTypes.TypeVar (pos, v)) params in

            let rt_ty = BslTypes.External (pos, rt_ty_skey, params) in

            let rt = { BslPluginInterface.
              rt_ks ;
              rt_ty ;
            } in
            let () = BslPluginInterface.apply_register_type dynloader_interface.BslPluginInterface.register_type rt in
            let env = {
              env with
                ty_spec_map ;
            } in
            env, js_file, exports

        | D.ExternalTypeDef (skey, params, implementation) ->
            let () =
              match implementation with
              | None -> ()
              | Some code ->
                  warning pos (
                    "In javascript, type implementation are ignored.@\n"^^
                    "@[<2>@{<bright>Hint@}:@\n"^^
                    "remove this part: ' = %s'@]"
                  ) code
            in
            let rt_ks = env_rp_ks env skey in
            let rt_ty_skey = env_rt_ty_skey env tags skey in
            let key = BslKey.normalize rt_ty_skey in
            let skey = BslKey.normalize_string skey in
            let tyitem = rt_ty_skey, env_js_path env skey in

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

            let params = List.map (fun v -> BslTypes.TypeVar (pos, v)) params in

            let rt_ty = BslTypes.External (pos, rt_ty_skey, params) in

            let rt = { BslPluginInterface.
                         rt_ks ;
                       rt_ty ;
                     } in
            let () = BslPluginInterface.apply_register_type dynloader_interface.BslPluginInterface.register_type rt in
            let env = {
              env with
                ty_spec_map     = ty_spec_map ;
            } in
            env, js_file, exports

        | D.Module (skey, implementation) ->
            env_add_module pos skey implementation env, js_file, exports

        | D.EndModule ->
            env_add_endmodule pos env, js_file, exports

        | D.Register (skey, source_implementation, injected, bslty) ->
            let rp_ks = env_rp_ks env skey in
            let rp_ty = env_map_ty_reference_for_opa env pos skey bslty in
            let parsed_t = BslTags.parsed_t tags in

            let implementation =
              match source_implementation with
              | Some source -> source
              | None ->
                  if injected then assert false
                  else
                    env_rp_implementation env skey injected
            in
            let rp_ips = [ lang, filename, parsed_t, implementation ] in
            let rp = { BslPluginInterface.
              rp_ks  = rp_ks ;
              rp_ty  = rp_ty ;
              rp_ips = rp_ips ;
              rp_obj = None ;
            } in
            let key = BslKey.normalize (String.concat "." rp_ks)  |> BslKey.to_string in
            let source_option =
              match source_implementation with
              | None -> ""
              | Some source -> " \\ "^source^" "
            in
            let js_file = FBuffer.printf js_file "/* %s%s : @[%a@] */\n" (String.concat "." rp_ks) source_option BslTypes.pp bslty in

            let js_file = FBuffer.printf js_file "/* resolution: %%%%%s%%%% --> %s */\n" key implementation in
            let () = SeparatedEnv.SideEffect.add_renaming key implementation in
            let env = {env with renaming = StringMap.add key implementation env.renaming } in
            BslPluginInterface.apply_register_primitive dynloader_interface.BslPluginInterface.register_primitive rp ;
            env, js_file, exports

        | D.Args (name, args, _bslty) ->
            (* BslTypes.pp bslty *)
            let print_args li = String.concat ", " li in
            let argsname = List.map fst args in
            let funname = env_rp_implementation env name false in
            let js_file = FBuffer.printf js_file "function %s (%s)\n" funname (print_args argsname) in
            env, js_file, (funname :: exports)

        | D.Property _ ->
            (* keep coherence for line count in the js *)
            let js_file = FBuffer.add js_file "\n" in
            env, js_file, exports
      )
  in
  env, js_file, exports


let env_add_file_line ~filename env js_file =
  let js_file = FBuffer.printf js_file "// file %S, line 1@\n" filename in
  js_file, env


let fold_decorated_file ~dynloader_interface ~lang env decorated_file =
  let filename = decorated_file.D.filename in
  let source = decorated_file.D.decorated_source in
  let implementation = js_module_of_filename filename in
  (* we add a module for each file *)
  let env = env_add_module nopos implementation None env in
  (* For each file, we create a FBuffer, updated in a fold on decorated lines *)
  let js_file = fbuffer () in
  let js_file, env = env_add_file_line ~filename env js_file in

  let env, js_file, exports =
    List.fold_left (fold_source_elt ~dynloader_interface ~filename ~lang) (env, js_file, []) source
  in

  (* Export all declared primitives in the current file *)
  let add_export js_file name =
    FBuffer.printf js_file "exports.%s = %s;\n" name name
  in
  let js_file = List.fold_left add_export js_file exports in

  let js_code = FBuffer.contents js_file in
  let file_js_code = filename, js_code in
  let rev_files_js_code = env.rev_files_js_code in
  let rev_files_js_code = file_js_code :: rev_files_js_code in
  let env = { env with rev_files_js_code = rev_files_js_code } in
  let env = env_add_endmodule nopos env in
  let _ =
    match env.rev_path with
    | [] -> ()
    | list ->
        let list = List.rev (List.tl (List.rev list)) in
        List.iter (
          fun (_, _, pos) ->
            OManager.printf "%a" FilePos.pp_citation pos ;
        ) list;
        OManager.error
          "File %S: unclosed module(s)@\n@[<2>@{<bright>Hint@}:@\nAdd the corresponding @{<bright>##endmodule@}@]@\n"
          filename
  in
  env


let preprocess ~options ~plugins ~dynloader_interface ~depends ~lang decorated_files =
  let env = empty in
  let sep_env = SeparatedEnv.init ~ty_spec_map:env.ty_spec_map ~renaming:env.renaming in
  let sep_env = List.fold_left SeparatedEnv.fold sep_env plugins in
  let ty_spec_map = SeparatedEnv.ty_spec_map sep_env in
  let renaming = SeparatedEnv.renaming sep_env in
  let env = {
    env with
      ty_spec_map ;
      renaming ;
  } in
  let env =
    List.fold_left (fold_decorated_file ~dynloader_interface ~lang) env decorated_files in
  let files_js_code = List.rev env.rev_files_js_code in

  check ~options ~depends files_js_code ;

  let javascript_env = SeparatedEnv.SideEffect.get_javascript_env () in
  javascript_env, files_js_code
