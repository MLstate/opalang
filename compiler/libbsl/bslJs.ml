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

(*
  Note about some duplication of code between BslOCaml and BslJs:

  The previous of libbsl was implemented using a functor for sharing some part of this process.
  The experience has shown that the treatment done on Javascript and Ocaml is quite different,
  which led to complex code in the argument of the functor, not very natural.

  I (mathieu) think that proceding now with 2 different files is more relax, the implementation
  can be different, without hacking the interfaces of functors and args.
*)

module Format = BaseFormat
module String = BaseString
let (|>) = InfixOperator.(|>)

module BPI = BslPluginInterface
module BI = BslInterface
module D = BslDirectives
module DJ = D.Js
module J = JsAst

type filename = string
type contents = string
type module_name = string
type skey = string
type pos = FilePos.pos
type opaname = string

type js_file = FBuffer.t

type js_decorated_file = {
  directives: (FilePos.pos * BslTags.t * DJ.t) list;
  contents: J.code;
  filename: filename;
}

(* Actually, we process either files in the classic syntax only or in
   the doc-like one. However, tagging each file avoids changing lots
   of code. *)
type decorated_file =
| Classic of D.bypasslang_decorated_file
| DocLike of js_decorated_file

let fbuffer () = FBuffer.create (8 * 1024)

type ty_spec_map = (opaname * module_name list) BslKeyMap.t
type renaming = string StringMap.t

type env = {
  (* this is just for error messages *)
  last_module         : FilePos.pos ;
  last_endmodule      : FilePos.pos ;

  (* accumulating files in a fold *)
  package : JsPackage.t;

  rev_path            : (skey * module_name * pos) list ;

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

let empty ~name =
  let empty = {
    last_module          = nopos ;
    last_endmodule       = nopos ;

    package    = JsPackage.default ~name ;

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

let env_rp_implementation env implementation =
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
   the bsl preprocessor replaces the alias by its implementation
   Str.full_split split_regexp "texte %%BslCps.Notcps_compatibility.thread_context%%; texte ";;
*)
let split_regexp = Str.regexp "%%[ ]*[a-zA-Z\\._]+[ ]*%%";;


let fold_source_elt_classic ~dynloader_interface ~filename ~lang
    (env, package) source_elt =

  let env, js_file =
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
        let package = JsPackage.add_verbatim package source in
        env, package

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
            env, package

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
            env, package

        | D.Module (skey, implementation) ->
            env_add_module pos skey implementation env, package

        | D.EndModule ->
            env_add_endmodule pos env, package

        | D.Register (skey, source_implementation, injected, bslty) ->
            let rp_ks = env_rp_ks env skey in
            let rp_ty = env_map_ty_reference_for_opa env pos skey bslty in
            let parsed_t = BslTags.parsed_t tags in

            let implementation =
              match source_implementation with
              | Some source -> source
              | None ->
                  assert (not injected);
                  env_rp_implementation env skey
            in
            let rp_ips = [ lang, filename, parsed_t, rp_ty, implementation ] in
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
            let package = JsPackage.add_verbatim package
              (Format.sprintf "/* %s%s : @[%a@] */\n"
                 (String.concat "." rp_ks) source_option BslTypes.pp bslty)
            in
            let package = JsPackage.add_verbatim package
              (Format.sprintf "/* resolution: %%%%%s%%%% --> %s */\n" key implementation)
            in
            let () = SeparatedEnv.SideEffect.add_renaming key implementation in
            let env = {env with renaming = StringMap.add key implementation env.renaming } in
            BslPluginInterface.apply_register_primitive dynloader_interface.BslPluginInterface.register_primitive rp ;
            env, package

        | D.Args (name, args, _bslty) ->
            (* BslTypes.pp bslty *)
            let print_args li = String.concat ", " li in
            let argsname = List.map fst args in
            let funname = env_rp_implementation env name in
            let package = JsPackage.add_verbatim package
              (Format.sprintf "function %s (%s)\n" funname (print_args argsname))
            in
            env, package

        | D.Property _ ->
            (* keep coherence for line count in the js *)
            let package = JsPackage.add_verbatim package "\n" in
            env, package
      )
  in
  env, js_file

let file_line ~filename =
  Format.sprintf "// file %S, line 1@\n" filename

let fold_decorated_file_classic ~dynloader_interface ~lang env decorated_file =
  let filename = decorated_file.D.filename in
  let source = decorated_file.D.decorated_source in
  let implementation = js_module_of_filename filename in
  (* we add a module for each file *)
  let env = env_add_module nopos implementation None env in
  let package = env.package in
  let package = JsPackage.add_verbatim package (file_line ~filename) in
  let env, package =
    List.fold_left
      (fold_source_elt_classic ~dynloader_interface ~filename ~lang)
      (env, package) source
  in
  let env = { env with package } in
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

(* Process a directive, returning an updated environment and an
   updated map of bypasses that have been defined in this file, so
   they can be bound later

   FIXME: env contains a renaming map already, maybe we should merge
   both.
*)
let fold_source_elt_doc_like ~dynloader_interface ~filename ~lang
    (env, renaming) (pos, tags, directive) =
  match directive with
  | DJ.OpaTypeDef (skey, params) ->
    if not tags.BslTags.opaname then
      !! pos "an opa-type cannot be @{<brigth>normalized@}";

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
    BslPluginInterface.apply_register_type
      dynloader_interface.BslPluginInterface.register_type rt;
    let env = { env with ty_spec_map; } in
    env, renaming

  | DJ.ExternalTypeDef (skey, params) ->
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
      SeparatedEnv.SideEffect.add_ty_spec_map key tyitem;
      BslKeyMap.replace key replace env.ty_spec_map
    in

    let params = List.map (fun v -> BslTypes.TypeVar (pos, v)) params in

    let rt_ty = BslTypes.External (pos, rt_ty_skey, params) in

    let rt = { BslPluginInterface.
               rt_ks ;
               rt_ty ;
             } in
    BslPluginInterface.apply_register_type
      dynloader_interface.BslPluginInterface.register_type rt;
    let env = { env with ty_spec_map; } in
    env, renaming

  | DJ.Module skey ->
    env_add_module pos skey None env, renaming

  | DJ.EndModule ->
    env_add_endmodule pos env, renaming

  | DJ.Register (skey, implementation, bslty) ->
    let rp_ks = env_rp_ks env skey in
    let rp_ty = env_map_ty_reference_for_opa env pos skey bslty in
    let parsed_t = BslTags.parsed_t tags in
    let key = BslKey.normalize (String.concat "." rp_ks) |> BslKey.to_string in
    let keyed_implementation, renaming =
      (* For now, we try to export the bypasses in code with the same
         name as they would have using the classic syntax, just to make
         sure that nothing will go wrong *)
      match implementation with
      | DJ.Regular name ->
        let ki = JsCons.Ident.native_global ~pure:tags.BslTags.pure
          (env_rp_implementation env skey) in
        let orig_ident = JsIdent.to_string name in
        if StringMap.mem orig_ident renaming then
          (* FIXME: Since all modules of a plugin share the same
             namespace, we must reject double renamings *)
          OManager.error
            "Two functions with the same name (%s) defined on this plugin."
            orig_ident
        else
          JsIdent.to_string ki,
          StringMap.add orig_ident ki renaming
      | DJ.Inline source ->
        (* Since it is just an alias in this case, we don't need to
           bind it.  FIXME: This will break if the source fragment
           contains an identifier which will be renamed. *)
        source, renaming
    in
    let rp_ips = [ lang, filename, parsed_t, rp_ty, keyed_implementation ] in
    let rp = { BslPluginInterface.
               rp_ks  = rp_ks ;
               rp_ty  = rp_ty ;
               rp_ips = rp_ips ;
               rp_obj = None ;
             } in
    SeparatedEnv.SideEffect.add_renaming key keyed_implementation;
    let env = { env with
      renaming = StringMap.add key keyed_implementation env.renaming
    } in
    BslPluginInterface.apply_register_primitive
      dynloader_interface.BslPluginInterface.register_primitive rp;
    env, renaming

let rename renaming code =
  let code = List.map JsUtils.globalize_native_ident code in
  let new_name ident =
    if JsIdent.is_native_global ident then
      StringMap.find_opt (JsIdent.to_string ident) renaming
    else
      None in
  List.map (fun stm ->
    JsWalk.TStatement.map
      (fun stm ->
        match stm with
        | J.Js_function (pos, ident, args, body) -> (
          match new_name ident with
          | Some ident' -> J.Js_function (pos, ident', args, body)
          | None -> stm
        )
        | J.Js_var (pos, ident, def) -> (
          match new_name ident with
          | Some ident' -> J.Js_var (pos, ident', def)
          | None -> stm
        )
        | _ -> stm
      )
      (fun expr ->
        match expr with
        | J.Je_ident (pos, ident) -> (
          match new_name ident with
          | Some ident' -> J.Je_ident (pos, ident')
          | None -> expr
        )
        | _ -> expr
      ) stm
  ) code

(** Process directives in a decorated file, updating the renaming map *)
let process_directives_doc_like
    ~dynloader_interface ~lang (env, renaming) decorated_file =
  let filename = decorated_file.filename in
  let directives = decorated_file.directives in
  let implementation = js_module_of_filename filename in

  (* we add a module for each file *)
  let env = env_add_module nopos implementation None env in
  let fold = fold_source_elt_doc_like ~dynloader_interface ~filename ~lang in
  let env, renaming = List.fold_left fold (env, renaming) directives in
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
        OManager.error (
          "File %S: unclosed module(s)@\n@[<2>@{<bright>Hint@}:@\n" ^^
          "Add the corresponding @{<bright>##endmodule@}@]@\n"
        ) filename
  in
  env, renaming

(** Use the renaming map built in the previous stage to rename
    bypasses in all files, and serialize the resulting ASTs, updating
    the environment *)
let build_file_doc_like ~transform renaming env decorated_file =
  (* For each file, we create a FBuffer, updated in a fold on decorated lines *)
  let filename = decorated_file.filename in
  let contents = rename renaming decorated_file.contents in
  let package = JsPackage.add_verbatim env.package (file_line ~filename) in
  let package = JsPackage.add_code package (transform filename contents) in
  let env = { env with package } in
  env

let process_file ~dynloader_interface ~lang (env, renaming) decorated_file =
  match decorated_file with
  | Classic decorated_file ->
    fold_decorated_file_classic ~dynloader_interface ~lang env decorated_file,
    renaming
  | DocLike decorated_file ->
    process_directives_doc_like ~dynloader_interface
      ~lang (env, renaming) decorated_file

let build_file ~transform renaming env file =
  match file with
  | Classic _ ->
    (* In classic syntax, renaming is already done *)
    env
  | DocLike decorated_file ->
    build_file_doc_like ~transform renaming env decorated_file

let apply_conf conf code =
  let code =
    if conf.BslJsConf.localrenaming then Imp_Renaming.rename code
    else code
  in
  let code =
    if conf.BslJsConf.cleanup then Imp_CleanUp.clean ~use_shortcut_assignment:true code
    else code
  in
  let code =
    if conf.BslJsConf.cleanup then Imp_CleanUp.clean ~use_shortcut_assignment:true code
    else code in
  let code = List.map JsUtils.globalize_native_ident code in
  code

let preprocess ~options ~plugins ~dynloader_interface ~depends ~lang ~js_confs decorated_files =
  ignore depends;
  let confs = BslJsConf.export js_confs in
  let transform filename code =
    let code = match StringMap.find_opt filename confs with
      | Some (BslJsConf.Optimized conf) -> apply_conf conf code
      | _ -> code
    in
    if BslLanguage.is_nodejs lang then JsUtils.export_to_global_namespace code
    else code
  in
  let env = empty ~name:(Printf.sprintf "%s.%s"
                           (Option.get options.BI.basename)
                           BslConvention.Extension.plugin) in
  let sep_env = SeparatedEnv.init ~ty_spec_map:env.ty_spec_map ~renaming:env.renaming in
  let sep_env = List.fold_left SeparatedEnv.fold sep_env plugins in
  let ty_spec_map = SeparatedEnv.ty_spec_map sep_env in
  let renaming = SeparatedEnv.renaming sep_env in
  let env = {
    env with
      ty_spec_map ;
      renaming ;
  } in

  let env, renaming =
    List.fold_left (process_file ~dynloader_interface ~lang)
      (env, StringMap.empty) decorated_files in
  let env = List.fold_left (build_file ~transform renaming) env decorated_files in

  let javascript_env = SeparatedEnv.SideEffect.get_javascript_env () in
  javascript_env, env.package
