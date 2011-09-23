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
(* shorthands *)
module O = OpaEnv
module P = Passes
module PH = PassHandler
module Q = QmlAst

module List = BaseList

(* Define some alias for OPA pass system *)
type opa_options = OpaEnv.opa_options

type 'env opa_env = (opa_options, 'env) PassHandler.one_env

type ('env, 'env2) opa_pass =
    (opa_options, opa_options, 'env, 'env2) PassHandler.pass

type ('env, 'env2) opa_old_pass =
    (opa_options, 'env, 'env2) PassHandler.old_pass

type env_bothFinalCompile = (Passes.env_NewFinalCompile * Passes.env_NewFinalCompile)

type env_QmlCompilation = {
  qmlCompilation_options : Qml2ocamlOptions.argv_options ;
  qmlCompilation_env_ocaml_input : Qml2ocaml.env_ocaml_input ;
}

type env_OcamlSplitCode = {
  ocamlSplitCode_options : Qml2ocamlOptions.argv_options ;
  ocamlSplitCode_env_ocaml_split : Qml2ocaml.env_ocaml_split ;
}

type env_OcamlGeneration = {
  ocamlGeneration_options : Qml2ocamlOptions.argv_options ;
  ocamlGeneration_env_ocaml_output : Qml2ocaml.env_ocaml_output ;
}

type env_OcamlCompilation = {
  ocamlCompilation_returned_code : int ;
}

(* when propagating to all environment is overkill
   ensures that their is no package mismatch, and no mutability of extra env *)
let pass_extra_output_env (default:'extra_env option) =
  let r = ref None in
  (fun () -> match !r with
  | Some((pack,extra_env)) when ObjectFiles.get_current_package_name() = pack ->
    extra_env
  | _ -> match default with
    | None -> failwith "pass_extra_output_env:not initalized"
    | Some d -> d
  ),
  (fun (extra_env:'extra_env) ->
    let cur_pack = ObjectFiles.get_current_package_name() in
    match !r with
    | Some((pack,_)) when cur_pack = pack && cur_pack<>"" -> (* something wrong here *)
      failwith ("pass_extra_output_env:no mutability in <<"^pack^">>")
    | _ -> r:= Some(cur_pack,extra_env)
   )

(**********************************************************)
(* Private module : Provides some utils for make **********)
(* environnments ******************************************)
(**********************************************************)


(**********************************************************)
(* Extracter **********************************************)
(**********************************************************)
module Extract = struct
  module SAEnvGen = struct
    let code env = env.P.sa_lcode
  end
  module EnvGen = struct
    let code env = env.P.qmlAst
    let annotmap env = env.P.typerEnv.QmlTypes.annotmap
    let gamma env = env.P.typerEnv.QmlTypes.gamma

    (** Return (annotmap, code).*)
    let ac env = (annotmap env, code env)
    let agc env = (annotmap env, gamma env, code env)
    let bymap env =  env.P.bsl.BslLib.bymap
    let bypass_typer env s = BslLib.BSL.ByPassMap.bypass_typer (bymap env) s

    (** Return (bypass_typer, (code, gamma, annotmap)). Used by
        [QmlCheck.Typing.retype] condition *)
    let bcga env =
      (bypass_typer env), ((code env), (gamma env), (annotmap env))
  end
  module EnvGenSliced = struct
    let get_env_gen env = env.P.env_gen
    let code_client env = env.P.sliced_env.P.client.P.code
    let code_server env = env.P.sliced_env.P.server.P.code
    let annotmap env = EnvGen.annotmap (get_env_gen env)
    let gamma env = EnvGen.gamma (get_env_gen env)

    (** Return (annotmap, code) for client *)
    let a_client env = (annotmap env, code_client env)
    let ag_client env = (annotmap env, gamma env, code_client env)

    (** Return (annotmap, server) for server *)
    let a_server env = (annotmap env, code_server env)
    let ag_server env = (annotmap env, gamma env, code_server env)
    let bymap env =  EnvGen.bymap (get_env_gen env)
    let bypass_typer env = EnvGen.bypass_typer (get_env_gen env)
    let gamma env = EnvGen.gamma (get_env_gen env)

    (** Return (bypass_typer, (code, gamma, annotmap)) client*)
    let bcga_client env =
      (bypass_typer env), ((code_client env), (gamma env), (annotmap env))

    (** Return (bypass_typer, (code, gamma, annotmap)) server*)
    let bcga_server env =
      (bypass_typer env), ((code_server env), (gamma env), (annotmap env))
  end
end

(* TODO: Some of this should be in OpaTracker *)
(* There should not be code there ! *)
(* please update consistently with QmlTracker and passdesign *)

module EnvUtils = struct
  let make_code_printer printer _opt =
    ["code",
     fun fmt env ->
       Format.fprintf fmt "%a\n%!" printer env]

  let create_sa_both_env_gen printers e env = { PH.
    options = e.PH.options;
    env = env;
    printers =
      (fun opt ->
         let user_printers =
           printers (fun e -> e.SurfaceAstPassesTypes.lcodeUser) opt in
         let nuser_printers =
           printers (fun e -> e.SurfaceAstPassesTypes.lcodeNotUser) opt in
         List.fold_right2
           (fun (id, u) (_, nu) acc -> (
              (* The printer size should not be modified, opa track is expecting a specific format *)
              if Base.String.is_prefix "size" (PassHandler.printer_id id)
              then
                (id,
                 (fun fmt e ->
                    let code = e.SurfaceAstPassesTypes.lcodeNotUser @ e.SurfaceAstPassesTypes.lcodeUser in
                    OpaTracker.Printer.size fmt code))
              else
                (id,
                 (fun fmt e ->
                    Format.fprintf fmt "/* Not user code */@\n%a" nu e;
                    Format.fprintf fmt "/* User code */@\n%a" u e))
            )::acc)
           user_printers nuser_printers []);

    trackers = fun _ -> [] (* TODO: use OpaTracker.trackers *);
  }

  let create_sa_both_env e env = create_sa_both_env_gen OpaTracker.printers_nonuid e env
  let create_sa_both_env_uids e env = create_sa_both_env_gen OpaTracker.printers_uids e env

  let create_sa_env e env = { PH.
    options = e.PH.options;
    env = env;
    printers = OpaTracker.printers_uids Extract.SAEnvGen.code;
    trackers = OpaTracker.trackers Extract.SAEnvGen.code;
  }

  let create_env_gen e env = { PH.
    options = e.PH.options;
    env = env;
    printers = QmlTracker.printers Extract.EnvGen.agc;
    trackers = QmlTracker.trackers Extract.EnvGen.code;
  }

  let qmlTracker_sliced define getid qmlTracker extract_client extract_server _ =
    let make_printer make_filename extract =
      let list = qmlTracker extract () in
      match make_filename with
      | None ->
          list
      | Some new_name ->
          List.map (
            fun (id, prt) ->
              let id = getid id in
              let id = new_name id in
              let id = define id in
              id, prt
          )
            list
  in
    (make_printer (Some (fun name -> name ^ "_client")) extract_client)
    @ (make_printer None extract_server)
      (* using (fun name -> name) allows to can compare a non sliced code with
       * the server part of a sliced code *)

  (* keep generalized (x) *)
  let sliced_printers x = qmlTracker_sliced PassHandler.define_printer PassHandler.printer_id QmlTracker.printers x
  let sliced_trackers x = qmlTracker_sliced PassHandler.define_tracker PassHandler.tracker_id QmlTracker.trackers x

end



(**********************************************************)
(* Private module : Provides some adaptater for make an ***)
(* opa_pass with an opa_old_pass **************************)
(**********************************************************)
(** This module allows to make an [opa_pass] with a
    [opa_old_pass]. The created [opa_pass] returns an environment with
    a good behavior (printers, ...). *)
module Adapter : sig

  (* Keep sig here just for doc..*)

  val adapt_sa_both :
    ('a, (SurfaceAst.nonuid, [< SurfaceAst.all_directives ] as 'b) SurfaceAstPassesTypes.env_both_lcodes) opa_old_pass ->
    ('a, (SurfaceAst.nonuid, 'b) SurfaceAstPassesTypes.env_both_lcodes) opa_pass

  val adapt_sa :
    ('a, (SurfaceAst.uids, [< SurfaceAst.all_directives ] as 'd) P.sa_env_Gen) opa_old_pass ->
    ('a, (SurfaceAst.uids, 'd) P.sa_env_Gen) opa_pass

  val adapt_sliced :
    ?invariant:(('tmp_env P.env_Gen_sliced, 'tmp_env P.env_Gen_sliced) PassHandler.invariant list) ->
    ?precond:('tmp_env P.env_Gen_sliced PassHandler.cond list) ->
    ?postcond:('tmp_env P.env_Gen_sliced PassHandler.cond list) ->
    ('tmp_env P.env_Gen, 'tmp_env P.env_Gen) opa_old_pass ->
    ('tmp_env P.env_Gen_sliced, 'tmp_env P.env_Gen_sliced) opa_pass

  val adapt_sliced_on_client :
    ('b, 'b) opa_old_pass ->
    ('c * 'b, 'c * 'b) opa_pass

  val adapt_new_sliced_on_client :
    ('c, 'c) opa_pass ->
    ('s * 'c, 's * 'c) opa_pass

end = struct

  let adapt_sa pass =
    PassHandler.make_pass
      (fun e -> EnvUtils.create_sa_env e (pass ~options:e.PH.options e.PH.env))

  let adapt_sa_both pass =
    PassHandler.make_pass
      (fun e -> EnvUtils.create_sa_both_env e (pass ~options:e.PH.options e.PH.env))

  let adapt_sliced ?(invariant = []) ?(precond=[]) ?(postcond=[]) pass =
    PassHandler.make_pass ~invariant ~precond ~postcond
    (fun e ->
       let env_server =
         { e.PH.env.P.env_gen with P.qmlAst = e.PH.env.P.sliced_env.P.server.P.code } in
       let env_s = pass ~options:e.PH.options env_server in
       let env_client = {env_s with P.qmlAst = e.PH.env.P.sliced_env.P.client.P.code} in
       let env_c = pass ~options:e.PH.options env_client in
       { e with PH.env = {P.
           env_gen = { env_c with P.qmlAst = [] };
           sliced_env = {P.
             server = { e.PH.env.P.sliced_env.P.server with P.code = env_s.P.qmlAst };
             client = { e.PH.env.P.sliced_env.P.client with P.code = env_c.P.qmlAst };
           };
         }
       })

  let adapt_sliced_on_client pass =
    PassHandler.make_pass
      (fun e ->
        let senv, cenv = e.PH.env in
        let cenv = pass ~options:e.PH.options cenv in
        { e with PH.env = (senv, cenv) })


  let compose_pass
      (uncons : 'env -> 'env2 * ('env2 -> 'env))
      (pass : ('env2, 'env2) opa_pass)
      : ('env, 'env) opa_pass
      =
    let just_uncons x = fst ( uncons x ) in
    let map_cond cond = PH.compose_fun_condition just_uncons cond in
    let map_invariant i = PH.compose_fun_invariant just_uncons just_uncons i in

    let invariant = List.map map_invariant pass.PH.invariant in
    let precond = List.map map_cond pass.PH.precond in
    let postcond = List.map map_cond pass.PH.postcond in
    PassHandler.make_pass
      ~invariant
      ~precond
      ~postcond
      (fun e ->
        let new_env, cons = uncons e.PH.env in
        let options = e.PH.options in
        let printers = e.PH.printers in
        let trackers = e.PH.trackers in
        let map_track_print track_or_print opt =
          List.map
            (fun (id, p) -> (id, (fun f env -> p f (cons env) )))
            (track_or_print opt)
        in
        let one_env =
          { PH.
            env = new_env;
            options = options;
            printers = map_track_print printers;
            trackers = map_track_print trackers;
          } in
        let one_env = pass.PH.f one_env in
        let new_env = one_env.PH.env in
        { e with PH.env = (cons new_env) }
      )


  let adapt_new_sliced_on_client pass =
    compose_pass
      (fun (serv, cli) -> (cli, (fun cli -> (serv, cli))))
      pass

end

(** Select the good register function according to back-end *)
let register_fields options =
  match options.O.back_end with
  | `qmlflat -> Flat_Compiler.register_field_name


(**********************************************************)
(* Public : MAKE YOUR PASSES HERE *************************)
(* NO IMPLEMENTATIONS, JUST COATING (deconstruct and ******)
(* construct environnment) ********************************)
(* AND COMPLETE MLI ***************************************)
(**********************************************************)

let pass_Welcome =
  PassHandler.make_pass
    (fun {PH.env=()} ->
       Unix.putenv "OPA_VERSION" "S3"; (* this is a hack, probably deprecated (FIXME:remove) *)
       OpaEnv.Options.parse_options ();
       let options = OpaEnv.Options.get_options () in
       OManager.verbose "OPA version %s" BuildInfos.opa_version_name ;
       OManager.verbose "(c) 2007-%s MLstate, All Rights Reserved." BuildInfos.year;
       OManager.verbose "Build: %s" BuildInfos.version_id;
       PassHandler.make_env options ())

let pass_CheckOptions =
  PassHandler.make_pass
    (fun e ->
       if List.is_empty e.PH.options.O.filenames
         && ObjectFiles.Arg.no_packages ()
       then (
         OManager.printf "opa.exe: @{<bright>no opa files@}@.";
         OpaEnv.Options.echo_help ();
         OManager.printf "@[<2>@{<bright>Hint@}:@\nprecise some opa files@]@.";
         exit 1;
       ) else (
         let filenames = e.PH.options.O.filenames in
         PassHandler.make_env e.PH.options filenames
       )
    )

let pass_AddStdlibFiles =
  PassHandler.make_pass
    (fun e ->
       let options = e.PH.options in
       let filenames = e.PH.env in
       let env_AddStdlibFiles =
         let special_files =
           (*
             - In separated mode, we do not want to add the stdlib as loaded sources.
           *)
           if ObjectFiles.Arg.is_separated ()
           then []
           else
             if options.O.embedded_opa
             then Pass_AddStdlibFiles.of_static_include options
             else
               (* keep in sync with build_rules *)
               Pass_AddStdlibFiles.of_basedir [ "stdlib/core" ; "stdlib/tests" ] options
         in
         if ObjectFiles.Arg.is_separated ()
         then
           ([], special_files@filenames)
         else
           (special_files, filenames)
       in
       PassHandler.make_env options env_AddStdlibFiles
    )

let pass_PreProcess =
  PassHandler.make_pass
    (fun e ->
       let (files, ufiles) = e.PH.env in
       let ppenv =
         Pprocess.fill_with_sysenv Pprocess.empty_env in
       let ppenv =
         Pprocess.add_env "OPA_VERSION" "S3" ppenv in
       let ppenv =
         OpaEnv.Options.to_ppenv e.PH.options ppenv in
       let ppopt = Pprocess.default_options ppenv in
       let process = (Pprocess.process Pplang.opa_description ppopt) in
       let process =
         List.map
           (fun f ->
              {f with P.inputFile_content = process f.P.inputFile_content})
       in
       { e with PH.env = (process files, process ufiles) })

let pass_Parse =
  PassHandler.make_pass
    (fun e ->
       let options = e.PH.options in
       let env = e.PH.env in
       let (special_files, user_files) = env in
       let parsed_of_input_file input_file =
         let lcode =
           OpaParser.code
             ~cache:(not options.O.no_cache_parse)
             ~filename:input_file.P.inputFile_basename
             input_file.P.inputFile_content
         in
         { SurfaceAstPassesTypes.
             parsedFile_filename = input_file.P.inputFile_filename;
             parsedFile_lcode = lcode;
             parsedFile_content = input_file.P.inputFile_content;
         }
       in
       let special_parsed_files = List.map parsed_of_input_file special_files in
       let user_parsed_files = List.map parsed_of_input_file user_files in
       PassHandler.make_env options ((special_parsed_files, user_parsed_files), env)
    )

let pass_RegisterAppSrcCode =
  PassHandler.make_pass
    (fun e ->
      let options = e.PH.options in
      let ((special_files, user_files), (special_sources, user_sources)) = e.PH.env in
       (* FIXME for now we only not register the source code if option publish_src_code
          is not set, ideally we should not generate the _internal_/src_code page at
          all (now it will be empty if publish_src_code=false) *)
      if options.O.publish_src_code then
        let special_files' = Pass_RegisterAppSrcCode.register_code ~special:true
          special_files special_sources in
        let user_files' = Pass_RegisterAppSrcCode.register_code ~special:false
          user_files user_sources in
        PassHandler.make_env options (special_files', user_files')
      else
        PassHandler.make_env options (special_files, user_files)
    )

let pass_LoadObjects k =
  PH.make_pass
    (fun env ->
       let options = env.PH.options in
       let env =
         SurfaceAstPasses.pass_load_objects
           ~options
           env.PH.env
           (fun files -> k (PH.make_env options files)) in
       PH.make_env () env
    )

let pass_ConvertStructure =
  PassHandler.make_pass
    (fun env ->
       let options = env.PH.options in
       let files, env_bsl = env.PH.env in
       let both_env = { SurfaceAstPassesTypes.
         lcodeNotUser = [];
         lcodeUser = List.concat_map (fun (_, _, code) -> code) files;
         lcodeTypeRenaming = StringMap.empty;
         exported_values_idents = IdentSet.empty ;
         env_bsl;
       } in
       PassHandler.make_env options both_env
    )


let pass_CheckDuplication =
  PH.make_pass
    (fun env ->
       let options = env.PH.options in
       let both_env = SurfaceAstPasses.pass_check_duplication [] [] ~options env.PH.env in
       EnvUtils.create_sa_both_env_uids env both_env
    )

let pass_BslLoading =
  PassHandler.make_pass
    (fun e ->
       let options = e.PH.options in
       let env = e.PH.env in
       let options, env, env_bsl =
         Pass_BslLoading.process ~options ~code:env
       in
       PassHandler.make_env options (env, env_bsl)
    )

let pass_TreatNoClientCalls() =
  PassHandler.make_pass
    (fun e ->
      let options = e.PH.options in
      let env = e.PH.env in
      PassHandler.make_env options (Pass_TreatNoClientCalls.translate env)
    )

let pass_CheckServerEntryPoint =
  PassHandler.make_pass
    (fun e ->
       let (opt, env) =
         Pass_ServerDeclaration.pass_check_server_entry_point
           ~options:e.PH.options e.PH.env in
       let options = { e with PH.options = opt } in
       EnvUtils.create_sa_both_env options env)

let pass_I18nAndComputedString =
  PassHandler.make_pass
    (fun e -> EnvUtils.create_sa_both_env_uids e (I18nAndComputedString.process_directives__i18n__string ~options:e.PH.options e.PH.env))

let pass_ParserGeneration =
  Adapter.adapt_sa_both SurfaceAstPasses.pass_parser_generation

let pass_ConvertStructure2 () =
  PassHandler.make_pass
    (fun e ->
       let options = e.PH.options in
       let env = e.PH.env in
       let env2 =
         let sa_lcode = env.SurfaceAstPassesTypes.lcodeNotUser @ env.SurfaceAstPassesTypes.lcodeUser in
         let sa_doc_types = [] in
         let sa_bsl = env.SurfaceAstPassesTypes.env_bsl in
         let sa_type_renaming = env.SurfaceAstPassesTypes.lcodeTypeRenaming in
         let sa_exported_values_idents =
           env.SurfaceAstPassesTypes.exported_values_idents in
         {P.
           sa_lcode ;
           sa_doc_types ;
           sa_bsl ;
           sa_type_renaming ;
           sa_exported_values_idents ;
         }
       in
       PassHandler.make_env
         ~printers:(OpaTracker.printers_uids Extract.SAEnvGen.code)
         options
         env2)

let pass_CodingDirectives =
  let transform pass_env =
    let env = pass_env.PassHandler.env in
    let typerEnv = env.P.typerEnv in
    let gamma = typerEnv.QmlTypes.gamma in
    let annotmap = typerEnv.QmlTypes.annotmap in
    let code = env.P.qmlAst in
    let annotmap, code =
      Pass_CodingDirectives.process_code gamma annotmap code
    in
    let typerEnv = {
      typerEnv with QmlTypes.
        annotmap ;
    } in
    let env = {
      env with Passes.
        qmlAst = code ;
        typerEnv ;
    } in
    { pass_env with PassHandler.
        env ;
    }
  in
  (* invariants, and pre/post conds *)
  let invariant =
    [
    ] in
  let precond =
    [
    ] in
  let postcond =
    [
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform


let pass_AddDocApiDirectives () =
  Adapter.adapt_sa Pass_OpaDocApi.process_opa

let pass_ReorderToplevel () =
  Adapter.adapt_sa Passes.pass_reorder_toplevel

let pass_RewriteModules () =
  Adapter.adapt_sa Passes.pass_rewrite_modules



(**********************************************)
(* QML AST PASSES *****************************)
let pass_SaToQml =
  PassHandler.make_pass
    ~postcond:[
      QmlCheck.Annot.unicity Extract.EnvGen.code;
    ]
    (fun e ->
       let options = e.PH.options in
       let env = e.PH.env in

       (* new_toplevel_names are thoses that come from toplevel patterns *)
       let _new_toplevel_names, (qmlAst:QmlAst.code_elt list) =
         let options = OpaToQml.options in
         OpaToQml.UidsOpaToQml.code ~options env.P.sa_lcode
       in
       let type_renaming = SurfaceAstRenaming.ObjectType.fold StringMap.safe_merge env.P.sa_type_renaming in
       let type_renaming = StringMap.map (fun (i,_) -> OpaToQml.UidsOpaToQml.typeident_aux ~check:false i) type_renaming in
       let type_renamer ?(check=true) str =
         try StringMap.find str type_renaming
         with Not_found ->
           (* need to do that because the db uses bypasses with type [embed_info]
            * but this type is not defined in the code because the bypasses can only
            * be inserted by the compiler *)
           QmlAst.TypeIdent.of_string ~check str in
       (*
         1) Initializing the typer env
         With the mess S2/S3, passes, S3Passes,
         there are a lot of different initilization of typerEnv.
         The annotmap and the options for the typer are the only
         relevant information from the typerEnv.

         Here seems the right place for initializing the typerEnv.
         In particular, we do not use the blender in S3 anymore, so
         the 'prepare', the 'empty_env', and the 'blend_initial__Typing'
         functions from [opa_Typers] will not be used.

         Essentially, building the typerEnv there is just about passing
         the correct options depending on the command line, and keeping
         the annotations.
       *)

       let typerEnv =
         let bypass_typer =
           BslLib.BSL.ByPassMap.bypass_typer
             ~typeident:type_renamer env.P.sa_bsl.BslLib.bymap in
         QmlTyper.OfficialTyper.initial
           ~bypass_typer
           ~exception_handler:
             QmlTyperErrHandling.typechecking_exception_handler
           ~explicit_instantiation: options.O.explicit_instantiation
           ~value_restriction: options.O.value_restriction
           ~display: options.O.show_types
           ~exported_values_idents: env.P.sa_exported_values_idents () in

       let env_Gen =
         { P.
           bsl = env.P.sa_bsl;
           qmlAst = qmlAst;
           typerEnv = typerEnv;
           doc_types = env.P.sa_doc_types;
           temporary_env = ();
           local_typedefs = QmlAst.TypeIdentSet.empty;
           stdlib_gamma = QmlTypes.Env.empty;
         } in
       EnvUtils.create_env_gen e env_Gen
    )

let make_pass_raw_env pass ?(invariant=[]) ?(precond=[]) ?(postcond=[]) ()=
   let fpass e = { e with PH.
     env = pass ~options:e.PH.options e.PH.env;
   } in
   PassHandler.make_pass ~invariant ~precond ~postcond fpass

(* same as [make_pass_raw_env] but refresh printers and trackers
   hence, the start and end envs don't have to be of the same type. *)
let make_pass_raw_env_refresh pass ?(invariant=[]) ?(precond=[]) ?(postcond=[]) ()=
   let fpass e = EnvUtils.create_env_gen e (pass ~options:e.PH.options e.PH.env) in
   PassHandler.make_pass ~invariant ~precond ~postcond fpass


let make_process_code_pass process_code extract rebuild ?(invariant=[]) ?(precond=[]) ?(postcond=[]) () =
  let fpass = fun e ->
    let typerEnv, code = extract e.PH.env
    in
    let gamma = typerEnv.QmlTypes.gamma
    and annotmap = typerEnv.QmlTypes.annotmap
    and bypass_typer = typerEnv.QmlTypes.bypass_typer
    in
    let (gamma,annotmap),code = process_code bypass_typer gamma annotmap code
    in
    let typerEnv =
      { typerEnv with
        QmlTypes.gamma = gamma;
        QmlTypes.annotmap = annotmap }
    in
    { e with PH.env = rebuild e.PH.env typerEnv code
    }
  in
  PassHandler.make_pass ~invariant ~precond ~postcond fpass


(*** INVARIANT *)
let invariant = [
  PassHandler.make_cons_invariant (QmlCheck.Annot.unicity Extract.EnvGen.code);
  PassHandler.make_cons_invariant (QmlCheck.Annot.find Extract.EnvGen.ac)
]

(* Check the contents of code: filtering code_elts *)
module CodeContents =
struct
  let no_NewType =
    let contents = { QmlCheck.Code.contents_all with QmlCheck.Code.c_NewType = false } in
    QmlCheck.Code.contents contents

  (* NewVal & NewValRec *)
  let only_NewVal =
    let contents = { QmlCheck.Code.contents_all with QmlCheck.Code.
                       c_Database = false ;
                       c_NewDbValue = false ;
                       c_NewType = false ;
                   } in
    QmlCheck.Code.contents contents
end

let pass_AddCSS =
  PassHandler.make_pass
    ~invariant
    ~precond:[]
    ~postcond:[]
    (fun e ->
       let code = e.PH.env.P.qmlAst in
       let code = Pass_AddCSS.perform code in
       { e with PH.env = { e.PH.env with P.qmlAst = code } }
    )

let pass_CheckPatternMatching =
  PassHandler.make_pass
    ~invariant
    ~precond:[]
    ~postcond:[]
    (fun e ->
       let env = e.PH.env in
       let gamma = env.P.typerEnv.QmlTypes.gamma in
       let annotmap = env.P.typerEnv.QmlTypes.annotmap in
       let code = env.P.qmlAst in
       let annotmap, code = Pass_CheckPatternMatching.process_code gamma annotmap code in
       let typerEnv = {
         env.P.typerEnv with QmlTypes.
           annotmap ;
       } in
       let env = {
         env with P.
           typerEnv ;
           qmlAst = code
       } in
       { e with PH.env }
    )

let pass_EndOfSeparateCompilation k =
  PH.make_pass
    (fun env ->
       let env =
         Pass_EndOfSeparateCompilation.process_code
           env.PH.env
           (fun env_gen -> k (EnvUtils.create_env_gen env env_gen)) in
       PH.make_env () env
    )

(*** INVARIANT *)
(* The following invariants are global
   hence, they should be polymorphic wrt. the environment.
   The argument of [make_invariant] is duplicated in order to keep polymorphism. *)

let global_invariant () = [
      PassHandler.make_invariant (QmlCheck.Annot.unicity Extract.EnvGen.code)
                                 (QmlCheck.Annot.unicity Extract.EnvGen.code);
      PassHandler.make_invariant (QmlCheck.Code.valrec Extract.EnvGen.ac)
                                 (QmlCheck.Code.valrec Extract.EnvGen.ac);
      PassHandler.make_invariant (QmlCheck.Annot.find Extract.EnvGen.ac)
                                 (QmlCheck.Annot.find Extract.EnvGen.ac);
    ]
let invariant = global_invariant ()

let pass_FunActionLifting =
  PassHandler.make_pass
    ~invariant
    ~precond:[]
    ~postcond:[]
    (fun e ->
       let code = e.PH.env.P.qmlAst in
       let {QmlTypes.annotmap=annotmap} = e.PH.env.P.typerEnv in
       let annotmap, code =
         Pass_LambdaLifting.process_code_fun_action annotmap code in
       { e with PH.env = {e.PH.env with P.
                         qmlAst = code;
                         typerEnv = {e.PH.env.P.typerEnv with QmlTypes.annotmap = annotmap};
                         } })


let pass_TypesDefinitions =
  let precond =
    [
      (* TODO: add precondition *)
    ] in
  let postcond =
    [
      CodeContents.no_NewType Extract.EnvGen.ac ;
      (* TODO: add more postcondition *)
    ] in
  let invariant =
    [
      (* TODO: add postcondition *)
    ]
    @invariant
  in
  PassHandler.make_pass ~precond ~postcond ~invariant
    (fun e ->
       let env = ( e.PH.env : 'tmp_env Passes.env_Gen ) in
       let typerEnv = env.Passes.typerEnv in
       let code = env.Passes.qmlAst in
       let local_typedefs, typerEnv, code = Pass_TypeDefinition.process_code
         (register_fields e.PH.options) typerEnv code in
       let env = { env with Passes.typerEnv = typerEnv ; qmlAst = code; local_typedefs = local_typedefs } in
       { e with PH.env = env }
    )

let pass_DbSchemaGeneration =
  let precond =
    [
      CodeContents.no_NewType Extract.EnvGen.ac ;
    ] in
  let postcond =
    [
    ] in
  let invariant =
    [
      (* TODO: add invariant *)
    ]
    @invariant
  in
  PassHandler.make_pass ~precond ~postcond ~invariant
    (fun e ->
       let env = ( e.PH.env : 'tmp_env Passes.env_Gen ) in
       let typerEnv = env.Passes.typerEnv in
       let code = env.Passes.qmlAst in
       let gamma = typerEnv.QmlTypes.gamma in
       let annotmap = typerEnv.QmlTypes.annotmap in
       let schema = typerEnv.QmlTypes.schema in
       let (gamma, schema, code) =
         Pass_DbSchemaGeneration.process_code gamma annotmap schema code
       in
       let typerEnv =
         { typerEnv with QmlTypes.schema = schema; QmlTypes.gamma = gamma }
       in
       let env = { env with Passes.typerEnv = typerEnv ; qmlAst = code } in
       { e with PH.env = env }
    )

let pass_DbPathCoercion =
  let precond =
    [
      (* TODO: add precondition *)
    ] in
  let postcond =
    [
      (* TODO: add postcondition *)
    ] in
  let invariant =
    [
      (* TODO: add postcondition *)
    ]
    @invariant
  in
  PassHandler.make_pass ~precond ~postcond ~invariant
    (fun e ->
       let env = ( e.PH.env : 'tmp_env Passes.env_Gen ) in
       let typerEnv = env.Passes.typerEnv in
       let schema = typerEnv.QmlTypes.schema in
       let annotmap = typerEnv.QmlTypes.annotmap in
       let code = env.Passes.qmlAst in
       let val_ = OpaMapToIdent.val_ in
       let annotmap, code = Pass_DbPathCoercion.process_code ~val_ schema annotmap code in
       let typerEnv = { typerEnv with QmlTypes.annotmap = annotmap } in
       let env = { env with Passes.typerEnv = typerEnv ; qmlAst = code } in
       { e with PH.env = env }
    )



(** {b Descr}: The typechecking passe. *)
let pass_Typing =
  let precond =
    [
      (* TODO: add precondition *)
    ] in
  let postcond =
    [
      QmlCheck.Annot.find Extract.EnvGen.ac ;
    ] in
  let invariant =
    [
      (* TODO: add postcondition *)
    ]
    @invariant
  in
  PassHandler.make_pass ~precond ~postcond ~invariant
    (fun e ->
       let env = (e.PH.env : 'tmp_env Passes.env_Gen) in
       let env = Pass_Typing.process_code env in
       { e with PH.env = env }
    )

let pass_Retyping =
  PassHandler.make_pass
    (fun e -> {e with PH.env = Pass_Retyping.process_code e.PH.env})

(**
   Checking the directive @@warncoerce in the code, after typing.
*)
let pass_WarnCoerce =
  let precond =
    [
      (* TODO: add precondition *)
    ] in
  let postcond =
    [
      (* TODO: add postcondition *)
    ] in
  let invariant =
    [
      (* TODO: add postcondition *)
    ]
    @invariant
  in
  PassHandler.make_pass ~precond ~postcond ~invariant
    (fun e ->
       let env = (e.PH.env : 'tmp_env Passes.env_Gen) in
       let typerEnv = env.Passes.typerEnv in
       let gamma = typerEnv.QmlTypes.gamma in
       let annotmap = typerEnv.QmlTypes.annotmap in
       let code = env.Passes.qmlAst in
       let _ = Pass_WarnCoerce.process_code gamma annotmap code in
       e
    )

let pass_CompileRecursiveValues =
  PassHandler.make_pass
    (fun e ->
       let env = (e.PH.env : 'tmp_env Passes.env_Gen) in
       let typerEnv = env.Passes.typerEnv in
       let gamma = typerEnv.QmlTypes.gamma in
       let annotmap = typerEnv.QmlTypes.annotmap in
       let code = env.Passes.qmlAst in
       let val_ = OpaMapToIdent.val_ in
       let gamma, annotmap, code = Pass_CompileRecursiveValues.process_code ~val_ gamma annotmap code in
       let typerEnv = {typerEnv with QmlTypes.gamma; annotmap} in
       let env = {env with P.typerEnv; qmlAst = code} in
       {e with PH.env = env}
    )

let pass_RewriteAsyncLambda =
  PassHandler.make_pass
    (fun e ->
       let env = (e.PH.env : 'tmp_env Passes.env_Gen) in
       let typerEnv = env.Passes.typerEnv in
       let gamma = typerEnv.QmlTypes.gamma in
       let annotmap = typerEnv.QmlTypes.annotmap in
       let code = env.Passes.qmlAst in
       let val_ = OpaMapToIdent.val_ in
       let annotmap, code = Pass_RewriteAsyncLambda.process_code ~val_ gamma annotmap code in
       let typerEnv = {typerEnv with QmlTypes.annotmap} in
       let env = {env with P.typerEnv; qmlAst = code} in
       {e with PH.env = env}
    )

let pass_DbAccessorsGeneration =
  let invariant = global_invariant () in
  let precond = [
  ] in
  let postcond = [
    QmlCheck.Annot.find Extract.EnvGen.ac ;
  ] in
  make_pass_raw_env_refresh Passes.pass_DbAccessorsGeneration
    ~invariant
    ~precond
    ~postcond
    ()

let pass_DbCodeGeneration =
  let invariant = global_invariant () in
  let precond = [
  ] in
  let postcond = [
    CodeContents.only_NewVal Extract.EnvGen.ac ;
    QmlCheck.Annot.find Extract.EnvGen.ac ;
  ] in
  make_pass_raw_env_refresh Passes.pass_DbCodeGeneration
    ~invariant
    ~precond
    ~postcond
    ()

let pass_DocApiGeneration =
  make_pass_raw_env Pass_OpaDocApi.process_qml
    ~invariant ()

let pass_MacroExpansion =
  make_pass_raw_env Pass_MacroExpansion.process
    ~invariant ()

(*
  The first pass of bypass hoisting, before the slicer
  TODO(slicer):see if we can set to true the property just_expand,
  the sharing between bypass is maybe not necessary.
*)
let pass_BypassHoisting =
  make_pass_raw_env (
    Passes.pass_QmlProcessCode_env_Gen (
      (fun _bypass_typer gamma annotmap code ->
         let annotmap, code = Pass_BypassApply.process_code gamma annotmap code in
         (gamma, annotmap), code
      )
    )
  )
    ~invariant ()

let pass_RegisterFields =
  PassHandler.make_pass
    (fun e ->
       Pass_RegisterFields.perform (register_fields e.PH.options) e.PH.env.P.qmlAst;
       e
    )
    ~invariant

let pass_EnrichMagic =
  PassHandler.make_pass
    (fun e ->
       let env = e.PH.env in
       let {P.typerEnv = typerEnv; qmlAst = code; stdlib_gamma = stdlib} = env in
       let {QmlTypes.annotmap = annotmap; gamma = gamma} = typerEnv in
       let specialized_env, annotmap, code =
         Pass_EnrichMagic.process_code ~annotmap ~stdlib ~gamma code in
       let typerEnv = {typerEnv with QmlTypes.annotmap = annotmap} in
       let env = {env with Passes.typerEnv = typerEnv; qmlAst = code} in
       let env = Passes.change_temporary specialized_env env in
       EnvUtils.create_env_gen e env
    )
    ~invariant:(global_invariant ())

let pass_EnrichMagicPurge =
  PassHandler.make_pass
    (fun e ->
       let env = e.PH.env in
       let code = Pass_EnrichMagic.just_purge env.P.qmlAst in
       let env = {env with P.qmlAst = code} in
       let env = Passes.change_temporary IdentMap.empty env in
       EnvUtils.create_env_gen e env
    )
    ~invariant:(global_invariant ())

let pass_SimplifyEquality =
  PassHandler.make_pass
    (fun e ->
       let env = (e.PH.env : 'tmp_env Passes.env_Gen) in
       let {Passes.typerEnv = typerEnv; qmlAst = code} = env in
       let {QmlTypes.annotmap = annotmap; gamma = gamma} = typerEnv in
       let annotmap, code = Pass_SimplifyEquality.process_code OpaMapToIdent.val_noerr gamma annotmap code in
       let typerEnv = {typerEnv with QmlTypes.annotmap = annotmap} in
       let env = {env with Passes.typerEnv = typerEnv; qmlAst = code} in
       {e with PH.env = env}
    )
    ~invariant:(global_invariant ())

let pass_SimplifyMagic =
  PassHandler.make_pass
    (fun e ->
       let env = e.PH.env in
       let {Passes.typerEnv = typerEnv; qmlAst = code;
            temporary_env = specialized_env} = env in
       let {QmlTypes.annotmap = annotmap; gamma = gamma} = typerEnv in
       let annotmap, code = Pass_SimplifyMagic.process_code
         ~specialized_env gamma annotmap code in
       let typerEnv = {typerEnv with QmlTypes.annotmap = annotmap} in
       let env = {env with Passes.typerEnv = typerEnv; qmlAst = code} in
       let env = Passes.change_temporary () env  in
       EnvUtils.create_env_gen e env
    )
    ~invariant:(global_invariant ())

let pass_InstrumentForClosureSerialization_instrumented,
    pass_InstrumentForClosureSerialization_define_instrumented
  = pass_extra_output_env (None:IdentSet.t option)

let pass_InstrumentForClosureSerialization =
  PassHandler.make_pass
    (fun e ->
       let env = (e.PH.env : 'tmp_env Passes.env_Gen) in
       let {Passes.typerEnv = typerEnv; qmlAst = code} = env in
       let {QmlTypes.annotmap = annotmap; gamma = gamma} = typerEnv in
       let gamma, annotmap, code, instrumented =
         Pass_InstrumentForClosureSerialization.process_code gamma annotmap code
       in pass_InstrumentForClosureSerialization_define_instrumented instrumented;
       let typerEnv = {typerEnv with QmlTypes.annotmap = annotmap; gamma} in
       let env = {env with Passes.typerEnv = typerEnv; qmlAst = code} in
       {e with PH.env = env}
    )
    ~invariant:(global_invariant ())

let pass_ReorderEnvGen =
  PassHandler.make_pass
    (fun e ->
       let code = e.PH.env.Passes.qmlAst in
       let code = QmlDependencies.reorder OpaMapToIdent.val_noerr [] IdentMap.empty
         Reordering.create_group_list code in
       {e with PH.env = { e.PH.env with Passes.qmlAst = code } }
    )
    ~invariant

let pass_QmlUndot =
  make_pass_raw_env Passes.pass_QmlUndot
    ~invariant ()

(**********************************************)
(* SLICED PASSES ******************************)
module SlicedCheck = struct
  module Annot = struct
    let unicity =
      PassHandler.compose_condition
        [QmlCheck.Annot.unicity Extract.EnvGenSliced.code_client;
         QmlCheck.Annot.unicity Extract.EnvGenSliced.code_server]
    let find =
      PassHandler.compose_condition
        [QmlCheck.Annot.find Extract.EnvGenSliced.a_client;
         QmlCheck.Annot.find Extract.EnvGenSliced.a_server]
  end
  module Ident = struct
    let unbound =
      let weak = Qmljs_Serializer.JsIdent.is_toplevel_declaration in
      PassHandler.compose_condition
        [QmlAlphaConv.Check.unbound (OpaMapToIdent.val_noerr ~side:`client)
           Extract.EnvGenSliced.a_client;
         QmlAlphaConv.Check.unbound ~weak (OpaMapToIdent.val_noerr ~side:`server)
           Extract.EnvGenSliced.a_server]
  end
  module Code = struct
    let valrec =
      PassHandler.compose_condition
        [QmlCheck.Code.valrec Extract.EnvGenSliced.a_client;
         QmlCheck.Code.valrec Extract.EnvGenSliced.a_server]
  end
end
let sliced_printers =
  EnvUtils.sliced_printers
    Extract.EnvGenSliced.ag_client
    Extract.EnvGenSliced.ag_server
let pass_GenericSlicer slicer =
  PassHandler.make_pass
    ~invariant:[
      PassHandler.make_invariant
        (QmlCheck.Code.valrec Extract.EnvGen.ac)
        (SlicedCheck.Code.valrec);
      PassHandler.make_invariant
        (QmlCheck.Annot.unicity Extract.EnvGen.code)
        SlicedCheck.Annot.unicity
    ]
    ~precond:[
      QmlCheck.Annot.find Extract.EnvGen.ac ;
    ]
    ~postcond:[

    ]
    (fun e ->
       let env = slicer ~options:e.PH.options e.PH.env in
       { PH.
         env = env;
         options = e.PH.options;
         printers = sliced_printers;
         trackers =
           EnvUtils.sliced_trackers
             Extract.EnvGenSliced.code_client
             Extract.EnvGenSliced.code_server
       } )

let pass_NoSlicer = pass_GenericSlicer Passes.pass_no_slicer
let pass_SimpleSlicer = pass_GenericSlicer Passes.pass_simple_slicer

let pass_CleanLambdaLiftingDirectives =
  PassHandler.make_pass
    (fun one_env ->
       let env = one_env.PH.env in
       let milk = env.P.newFinalCompile_qml_milkshake in
       let code = milk.QmlBlender.code in
       let code = Pass_CleanLambdaLiftingDirectives.process_code code in
       let milk = {milk with QmlBlender.code} in
       let env = {env with P.newFinalCompile_qml_milkshake = milk} in
       {one_env with PH.env}
    )

let pass_Assertion =
  Adapter.adapt_sliced
    ~invariant:[
    ]
    ~precond:[
    ]
    ~postcond:[
    ]
    (fun ~options e ->
       let no_assert = options.OpaEnv.no_assert in
       let typerEnv = e.P.typerEnv in
       let gamma = typerEnv.QmlTypes.gamma in
       let annotmap = typerEnv.QmlTypes.annotmap in
       let code = e.P.qmlAst in
       let annotmap, code = Pass_Assertion.process_code ~no_assert gamma annotmap code in
       let typerEnv = { typerEnv with QmlTypes.annotmap } in
       { e with P.
           typerEnv ;
           qmlAst = code ;
       })

let pass_PurgeTypeDirectiveAfterTyping =
  PassHandler.make_pass
    ~invariant:[]
    ~precond:[]
    ~postcond:[]
    (fun one_env ->
       let env = one_env.PH.env in
       let typerEnv = env.P.typerEnv in
       let annotmap, qmlAst = Pass_Purge.process_code_after_typer typerEnv.QmlTypes.annotmap env.P.qmlAst in
       let env = {env with P.typerEnv = {typerEnv with QmlTypes.annotmap}; qmlAst} in
       {one_env with PH.env})

let pass_PurgeTypeDirectiveAfterEi =
  Adapter.adapt_sliced
    ~invariant:[
      PassHandler.make_cons_invariant SlicedCheck.Code.valrec;
      PassHandler.make_cons_invariant SlicedCheck.Annot.unicity
    ]
    ~precond:[
      SlicedCheck.Annot.unicity
    ]
    ~postcond:[
    ]
    (fun ~options:_ e ->
       let typerEnv = e.P.typerEnv in
       let annotmap, qmlAst = Pass_Purge.process_code_after_ei typerEnv.QmlTypes.annotmap e.P.qmlAst in
       {e with P.typerEnv = {typerEnv with QmlTypes.annotmap}; qmlAst})

let pass_ResolveRemoteCalls =
  let fpass =
    fun e ->
      { e with PH.
          env =
          Passes.pass_resolve_remote_calls ~options:e.PH.options e.PH.env;
      } in
  PassHandler.make_pass
    ~invariant:[
      PassHandler.make_cons_invariant SlicedCheck.Code.valrec;
      PassHandler.make_cons_invariant SlicedCheck.Annot.unicity;
    ]
    fpass

let pass_InsertMemoizedTypes =
  let fpass =
    fun e ->
      let gamma = e.PH.env.P.env_gen.P.typerEnv.QmlTypes.gamma in
      let local_typedefs = e.PH.env.P.env_gen.P.local_typedefs in
      let annotmap = e.PH.env.P.env_gen.P.typerEnv.QmlTypes.annotmap in
      (* tsc_map can be put here, because the tsc_map code is monomorphic,
         so not changed by Explicit Instantiation *)
      let (annotmap, server_updater) =
        Pass_ExplicitInstantiation.generate_tsc_map_updates ~val_:OpaMapToIdent.val_ ~side:`server ~local_typedefs gamma annotmap
      in
      let (annotmap, client_updater) =
        Pass_ExplicitInstantiation.generate_tsc_map_updates ~val_:OpaMapToIdent.val_ ~side:`client ~local_typedefs gamma annotmap
      in
      let server_code = e.PH.env.P.sliced_env.P.server.P.code in
      let client_code = e.PH.env.P.sliced_env.P.client.P.code in
      let server_code = server_updater :: server_code in
      let client_code = client_updater :: client_code in
      let gamma, new_server_code = Pass_ExplicitInstantiation.get_memoized_definitions gamma `server in
      let gamma, new_client_code = Pass_ExplicitInstantiation.get_memoized_definitions gamma `client in
      let server_code = List.tail_append new_server_code server_code in
      let client_code = List.tail_append new_client_code client_code in
      let env_gen = {e.PH.env.P.env_gen with P.typerEnv = {e.PH.env.P.env_gen.P.typerEnv with QmlTypes.gamma = gamma ; annotmap = annotmap }} in
      let client = {e.PH.env.P.sliced_env.P.client with P.code = client_code} in
      let server = {e.PH.env.P.sliced_env.P.server with P.code = server_code} in
      let sliced_env = {(*e.PH.env.sliced_env with*) P.client = client; server = server} in
      let env = {(*e.PH.env with*) P.env_gen = env_gen; sliced_env = sliced_env} in
      { e with PH.env = env } in
  PassHandler.make_pass fpass

let pass_SlicedReorder =
  let fpass e =
    let client = Extract.EnvGenSliced.code_client e.PH.env in
    let server = Extract.EnvGenSliced.code_server e.PH.env in
    let client, server = Opa_SlicedReorder.perform ~client ~server in
    { e with PH.
        env =
        { e.PH.env with P.
            sliced_env = { P.
              client = { e.PH.env.P.sliced_env.P.client with P.
                           code = client };
              server = { e.PH.env.P.sliced_env.P.server with P.
                           code = server };
            };
        }
    }
  in
  PassHandler.make_pass
    ~postcond:[
      SlicedCheck.Code.valrec;
      SlicedCheck.Ident.unbound;
    ]
    fpass

(*
  Factorizing construction and deconstruction of env for passes :
  + FunActionEnvSerialize
  + FunActionJsCallGeneration
*)
let decons_env_Gen_sliced_for_funaction_Passes env_Gen_sliced =
  let env_gen = env_Gen_sliced.P.env_gen in
  let typerEnv = env_gen.P.typerEnv in
  let gamma = typerEnv.QmlTypes.gamma in
  let annotmap = typerEnv.QmlTypes.annotmap in
  let sliced_env = env_Gen_sliced.P.sliced_env in
  let server = sliced_env.P.server in
  let code = server.P.code in
  gamma, annotmap, code

let recons_env_Gen_sliced_for_funaction_Passes env_Gen_sliced (gamma, annotmap) code =
  let env_gen = env_Gen_sliced.P.env_gen in
  let typerEnv = env_gen.P.typerEnv in
  let sliced_env = env_Gen_sliced.P.sliced_env in
  let server = sliced_env.P.server in
  let typerEnv =
    { typerEnv with QmlTypes.
        gamma = gamma ;
        annotmap = annotmap ;
    } in
  let env_gen =
    { env_gen with P.
        typerEnv = typerEnv
    } in
  let server =
    { server with P.
        code = code
    } in
  let sliced_env =
    { sliced_env with P.
        server = server
    } in
  let env =
    { P.
        env_gen = env_gen ;
        sliced_env = sliced_env ;
    } in
  env

let pass_SlicedCleaning =
  PassHandler.make_pass
    (fun e ->
       let env_gen_sliced = e.PH.env.P.sliced_env in
       let server_code = env_gen_sliced.P.server.P.code in
       let server_published = env_gen_sliced.P.server.P.published in
       let server_renaming = env_gen_sliced.P.server.P.renaming in
       let client_code = env_gen_sliced.P.client.P.code in
       let client_published = env_gen_sliced.P.client.P.published in
       let client_renaming = env_gen_sliced.P.client.P.renaming in
       let roots =
         let no_server = Option.get e.PH.options.O.no_server in
         List.filter_map (OpaMapToIdent.val_opt ~side:`server) (Opa_Roots.roots_for_s3 ~no_server) @
         List.filter_map (OpaMapToIdent.val_opt ~side:`client) (Opa_Roots.roots_for_s3 ~no_server) in
       let unreachable_idents, server_code, client_code =
         QmlDependencies.get_unreachable_idents_of_code
           OpaMapToIdent.val_noerr
           roots
           server_code
           client_code in
       let filter_fun = (fun i -> not (IdentSet.mem i unreachable_idents)) in
       let server_published =
         IdentMap.filter_keys
           filter_fun
           server_published in
       let client_published =
         IdentMap.filter_keys
           filter_fun
           client_published in
       let filter_fun_renaming = (fun _ id -> filter_fun id) in
       let server_renaming =
         QmlRenamingMap.filter server_renaming filter_fun_renaming in
       let client_renaming =
         QmlRenamingMap.filter client_renaming filter_fun_renaming in
       OpaMapToIdent.filter filter_fun;
       let server = {(* env_gen_sliced.P.server with *)
                       P.code = server_code;
                       P.published = server_published;
                       P.renaming = server_renaming;
                    } in
       let client = {(* env_gen_sliced.P.client with *)
                       P.code = client_code;
                       P.published = client_published;
                       P.renaming = client_renaming;
                    } in
       let env_gen_sliced = {(*env_gen_sliced with*) P.server = server; P.client = client} in
       let e = {e with PH.env = {e.PH.env with P.sliced_env = env_gen_sliced}} in
       e)

let pass_FunActionEnvSerialize =
  let precond =
    [
      (* TODO: add precondition *)
    ] in
  let postcond =
    [
      (* TODO: add postcondition *)
    ] in
  PassHandler.make_pass ~precond ~postcond
    (fun e ->
       let env_Gen_sliced = e.PH.env in

       (* Deconstruction of the env *)
       let gamma, annotmap, code =
         decons_env_Gen_sliced_for_funaction_Passes env_Gen_sliced
       in
       let stdlib_gamma = env_Gen_sliced.P.env_gen.P.stdlib_gamma in

       (* FunActionEnvSerialize *)
       let (gamma, annotmap), code =
         Pass_FunActionEnvSerialize.process_server_code ~stdlib_gamma gamma annotmap code
       in

       (* Reconstruction of the env *)
       let env_Gen_sliced =
         recons_env_Gen_sliced_for_funaction_Passes env_Gen_sliced (gamma, annotmap) code
       in
       let e = { e with PH.env = env_Gen_sliced } in
       e
    )

(* insert directives wherever values are instantiated or quantified
   with a nonempty set of variables *)
let pass_ExplicitInstantiation =
  let precond =
    [
      SlicedCheck.Code.valrec;
      SlicedCheck.Annot.unicity
    ] in
  let postcond =
    [

    ] in
  PassHandler.make_pass ~precond ~postcond
    (fun e ->
      let env = e.PH.env in
      let gamma = env.P.env_gen.P.typerEnv.QmlTypes.gamma in
      let annotmap = env.P.env_gen.P.typerEnv.QmlTypes.annotmap in
      let {P.code=server_code;
           published=server_published;
           renaming=server_renaming} = env.P.sliced_env.P.server in
      let {P.code=client_code;
           published=client_published;
           renaming=client_renaming} = env.P.sliced_env.P.client in
      (* TODO: optimize by adding only dummy arguments for published functions,
         if there is no explicit instantiation to be done there;
         then return *_published pointing to the versions with dummy arguments,
         but inside the server and client code use the original,
         unchanged version*)

      Pass_ExplicitInstantiation.published_ref := server_published;
      Pass_ExplicitInstantiation.renaming_map := server_renaming;

      (* to compute the have_typeof set of a piece of code, ei needs
       * the have_typeof set of its dependencies
       * so it can't look at the server code first nor the client code first *)
      let have_typeof =
        Pass_ExplicitInstantiation.have_typeof gamma annotmap
          (Opa_SlicedReorder.reorder_in_new_qml
             (server_code @ client_code)) in

      let (annotmap, gamma, qmlAst_server) =
        Pass_ExplicitInstantiation.process_code
          have_typeof gamma annotmap
          (IdentSet.from_list (IdentMap.keys server_published))
          env.P.sliced_env.P.server.P.code
      in

      let server_published = !Pass_ExplicitInstantiation.published_ref in
      let server_renaming = !Pass_ExplicitInstantiation.renaming_map in

      Pass_ExplicitInstantiation.published_ref := client_published;
      Pass_ExplicitInstantiation.renaming_map := client_renaming;

      let (annotmap, gamma, qmlAst_client) =
        Pass_ExplicitInstantiation.process_code
          have_typeof gamma annotmap
          (IdentSet.from_list (IdentMap.keys client_published))
          env.P.sliced_env.P.client.P.code
      in

      let client_published = !Pass_ExplicitInstantiation.published_ref in
      let client_renaming = !Pass_ExplicitInstantiation.renaming_map in

      let stdlib_gamma = Pass_ExplicitInstantiation.get_stdlib_gamma gamma in

      let env_gen =
        { env.P.env_gen with P.
          stdlib_gamma = stdlib_gamma;
          typerEnv = { env.P.env_gen.P.typerEnv with
                       QmlTypes.annotmap = annotmap;
                       QmlTypes.gamma = gamma;
          };
        }
      in
      { e with PH.env = { P.
          env_gen = env_gen;
          sliced_env =
          { P.server = { P.code = qmlAst_server;
                         published = server_published;
                         renaming = server_renaming;
                       };
            P.client = { P.code = qmlAst_client;
                         published = client_published;
                         renaming = client_renaming;
                       }
          };
        }
      })

(* rewrite the directives to normal QML code; insert code generating
   gamma accessible from OPA code *)
let pass_OptimizeExplicitInstantiation =
  let precond =
    [
      (* TODO: add pre condition *)
    ] in
  let postcond =
    [
      SlicedCheck.Code.valrec;
      SlicedCheck.Annot.unicity;
    ] in
  PassHandler.make_pass ~precond ~postcond
    (fun e ->
      let env = e.PH.env in
      let gamma = env.P.env_gen.P.typerEnv.QmlTypes.gamma in
      let annotmap = env.P.env_gen.P.typerEnv.QmlTypes.annotmap in
      let (annotmap, qmlAst_server) =
        Pass_ExplicitInstantiation.unprocess_code
          ~val_:OpaMapToIdent.val_
          ~side:`server gamma annotmap env.P.sliced_env.P.server.P.code
      in
      let (annotmap, qmlAst_client) =
        Pass_ExplicitInstantiation.unprocess_code
          ~val_:OpaMapToIdent.val_
          ~side:`client gamma annotmap env.P.sliced_env.P.client.P.code
      in
      let env_gen =
        { env.P.env_gen with P.
          typerEnv = { env.P.env_gen.P.typerEnv with
                       QmlTypes.annotmap = annotmap;
                     };
        }
      in
      { e with PH.env =
        { P.env_gen = env_gen;
          sliced_env =
          { P.server = { env.P.sliced_env.P.server with P.
                         code = qmlAst_server
                       };
            P.client = { env.P.sliced_env.P.client with P.
                         code = qmlAst_client
                       }
          };
        }
      })

let pass_FunActionJsCallGeneration =
  let precond =
    [
      (* TODO: add precondition *)
    ] in
  let postcond =
    [
      (* TODO: add postcondition *)
    ] in
  PassHandler.make_pass ~precond ~postcond
    (fun e ->
       let env_Gen_sliced = e.PH.env in

       (* Deconstruction of the env *)
       let gamma, annotmap, code =
         decons_env_Gen_sliced_for_funaction_Passes env_Gen_sliced
       in
       let stdlib_gamma = env_Gen_sliced.P.env_gen.P.stdlib_gamma in

       (* FunActionEnvSerialize *)
       let (gamma, annotmap), code =
         Pass_FunActionJsCallGeneration.process_server_code stdlib_gamma gamma annotmap code
       in

       (* Reconstruction of the env *)
       let env_Gen_sliced =
         recons_env_Gen_sliced_for_funaction_Passes env_Gen_sliced (gamma, annotmap) code
       in
       let e = { e with PH.env = env_Gen_sliced } in
       e
    )

(* ***********************************************)
(* FINAL COMPILATION *****************************)

let extract_client_agc (_, cenv) =
  (cenv.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.annotmap,
   cenv.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.gamma,
   cenv.P.newFinalCompile_qml_milkshake.QmlBlender.code)
let extract_server_agc (senv, _) =
  (senv.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.annotmap,
   senv.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.gamma,
   senv.P.newFinalCompile_qml_milkshake.QmlBlender.code)
let extract_client (_, cenv) = cenv.P.newFinalCompile_qml_milkshake.QmlBlender.code
let extract_server (senv, _) = senv.P.newFinalCompile_qml_milkshake.QmlBlender.code
let final_printers = EnvUtils.sliced_printers extract_client_agc extract_server_agc
let pass_SlicedToFinal =
  PassHandler.make_pass
    (fun e ->
       let env =
         let eenv = e.PH.env in
         let mk_blender typerEnv sliced_aux =
           let blender_milkshake : QmlBlender.qml_milkshake =
             { QmlBlender.env = typerEnv;
               QmlBlender.code = sliced_aux.P.code;
             } in
           { P.
             newFinalCompile_bsl = e.PH.env.P.env_gen.P.bsl ;
             newFinalCompile_qml_milkshake = blender_milkshake;
             newFinalCompile_renaming_server = eenv.P.sliced_env.P.server.P.renaming;
             newFinalCompile_renaming_client = eenv.P.sliced_env.P.client.P.renaming;
             newFinalCompile_closure_map = IdentMap.empty;
             newFinalCompile_stdlib_gamma = e.PH.env.P.env_gen.P.stdlib_gamma;
           } in
         ((mk_blender eenv.P.env_gen.P.typerEnv eenv.P.sliced_env.P.server),
          (mk_blender eenv.P.env_gen.P.typerEnv eenv.P.sliced_env.P.client)) in
       {
         PH.
         env = env;
         options = e.PH.options;
         printers = final_printers;
         trackers = EnvUtils.sliced_trackers extract_client extract_server;
       }
    )

(* ***********************************************)
(* FINAL CLIENT COMPILATION **********************)

let pass_ClientCpsRewriter =
  Adapter.adapt_sliced_on_client (P.pass_QmlCpsRewriter true)

let pass_ClientLambdaLifting =
  Adapter.adapt_sliced_on_client (P.pass_LambdaLifting2 ~typed:true ~side:`client)

let pass_ClientQmlUncurry =
  Adapter.adapt_sliced_on_client (P.pass_QmlUncurry2 ~typed:true ~side:`client)

let pass_ClientQmlClosure =
  Adapter.adapt_sliced_on_client (P.pass_QmlClosure2 ~typed:true ~side:`client)

let pass_JavascriptCompilation =
  let make_extract make env =
    let env = make env in
    env.P.newFinalCompile_qml_milkshake.QmlBlender.code in
  let make_extract_ac make env =
    let env = make env in
    (env.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.annotmap,
     env.P.newFinalCompile_qml_milkshake.QmlBlender.code) in
  let make_extract_agc make env =
    let env = make env in
    let gamma = env.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.gamma in
    let annotmap, code = make_extract_ac make env in
    annotmap, gamma, code in
  let pass pass_env =
    let options = pass_env.PassHandler.options in
    let mk_final_server_env env = { PH.
      env = env;
      options = options;
      printers = QmlTracker.printers (make_extract_agc (fun senv -> senv)) ;
      trackers = QmlTracker.trackers (make_extract (fun senv -> senv)) ;
    } in
    let (server_finalenv, client_finalenv) = pass_env.PassHandler.env in
    match Option.get options.O.no_server, ObjectFiles.compilation_mode () with
    | true, (`linking | `prelude | `init) -> mk_final_server_env server_finalenv
    | false, (`linking | `prelude | `init)
    | _, `compilation ->
      (* FIXME: i don't think we still need those roots
       * and if it breaks without, it should be solved properly *)
      let client_roots = List.fold_left (
        fun set root ->
          match OpaMapToIdent.val_opt ~side:`client root with
          | Some id -> IdentSet.add id set
          | None -> set
      ) IdentSet.empty (Opa_Roots.roots_for_s3 ~no_server:false) in
      (* instrumented closure should not be cleaned *)
      let client_roots = IdentSet.fold (fun id set ->
        let id = QmlRenamingMap.new_from_original client_finalenv.P.newFinalCompile_renaming_client id in
        IdentSet.add id set
      ) (pass_InstrumentForClosureSerialization_instrumented()) client_roots
      in
      let typing = server_finalenv.P.newFinalCompile_qml_milkshake.QmlBlender.env in
      let bsl_client = client_finalenv.P.newFinalCompile_bsl in
      let server = server_finalenv.P.newFinalCompile_qml_milkshake in
      let client = client_finalenv.P.newFinalCompile_qml_milkshake in
      let closure_map = client_finalenv.P.newFinalCompile_closure_map in
      let renaming_server = client_finalenv.P.newFinalCompile_renaming_server in
      let renaming_client = client_finalenv.P.newFinalCompile_renaming_client in
      let bsl_pp =
        let ppenv = Pprocess.fill_with_sysenv Pprocess.empty_env in
        let ppenv = OpaEnv.Options.to_ppenv options ppenv in
        let ppopt = Pprocess.default_options ppenv in
        Pprocess.process Pplang.js_description ppopt in
      let server =
        Pass_JavascriptCompilation.process
          ~options
          ~closure_map
          ~renaming_server
          ~renaming_client
          ~client_roots
          ~typing
          ~bsl_pp
          ~bsl_client
          ~server
          ~client
      in
      let server_finalenv = {
        server_finalenv with P.
          newFinalCompile_qml_milkshake = server ;
      } in
      mk_final_server_env server_finalenv
  in
  (* invariants, and pre/post conds *)
  let pre_extract_ac env = make_extract_ac (fun (_, cenv) -> cenv) env in
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
      QmlCheck.Code.valrec pre_extract_ac ;
      QmlAlphaConv.Check.unbound (OpaMapToIdent.val_noerr ~side:`client) pre_extract_ac ;
    ] in
  let postcond =
    [
      (* TODO: add postcond *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond pass

let pass_ResolveJsIdent =
  PassHandler.make_pass
    (fun e ->
       let env = e.PH.env in
       let annotmap, code =
         Opa_ResolveJsIdent.perform
           env.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.annotmap
           env.P.newFinalCompile_qml_milkshake.QmlBlender.code in
       let env =
         { env with P.
             newFinalCompile_qml_milkshake =
             { QmlBlender.
                 code = code ;
                 env =
                   { env.P.newFinalCompile_qml_milkshake.QmlBlender.env with
                       QmlTypes.annotmap = annotmap;
                   }
             }
         } in
       { e with PH.env = env }
    )

let pass_GenerateServerAst generate =
  PassHandler.make_pass
    (fun e ->
       let env = e.PH.env in
       let {P.
         newFinalCompile_qml_milkshake = {
           QmlBlender.code;
           env = {QmlTypes.annotmap; gamma}
         };
         newFinalCompile_stdlib_gamma = stdlib_gamma;
         newFinalCompile_renaming_server = server_renaming;
         newFinalCompile_renaming_client = client_renaming;
       } = env in
       let val_ = OpaMapToIdent.val_noerr in
       let gamma, annotmap, code = Pass_GenerateServerAst.process ~generate ~gamma ~stdlib_gamma ~annotmap ~val_ ~server_renaming ~client_renaming ~code in
       let env =
         { env with P.
             newFinalCompile_qml_milkshake =
             { QmlBlender.
                 code ;
                 env =
                 { env.P.newFinalCompile_qml_milkshake.QmlBlender.env with QmlTypes.
                     annotmap;
                     gamma;
                 }
             }
         } in
       {e with PH.env = env}
    )


(* ***********************************************)
(* FINAL SERVER COMPILATION **********************)

(* extractors *)
let extract_final_ac env =
  env.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.annotmap,
  env.P.newFinalCompile_qml_milkshake.QmlBlender.code
let extract_final_gamma env =
  env.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.gamma
let extract_final_code env = env.P.newFinalCompile_qml_milkshake.QmlBlender.code
let extract_final_bypass_typer env = env.P.newFinalCompile_qml_milkshake.QmlBlender.env.QmlTypes.bypass_typer
let extract pass_env = (extract_final_bypass_typer pass_env, extract_final_code pass_env)

let pass_QmlLiftDeepRecords =
  make_process_code_pass
    (fun _ -> Pass_LiftDeepRecords.process_code ~typed:true)
    P.extract_env_NewFinalCompile
    P.rebuild_env_NewFinalCompile
    ()

let pass_InitializeBslValues =
  PH.make_pass
    (fun e ->
       let env =  e.PH.env in
       let bypass_typer = extract_final_bypass_typer env in
       let annotmap, code = extract_final_ac env in
       let gamma = extract_final_gamma env in
       let gamma, annotmap, code = Pass_InitializeBslValues.process_code bypass_typer gamma annotmap code in
       let milkshake = { QmlBlender.
                          code = code;
                          env = {env.P.newFinalCompile_qml_milkshake.QmlBlender.env with QmlTypes.
                                   annotmap = annotmap;
                                   gamma = gamma;
                                }
                       } in
       let env = {env with P.newFinalCompile_qml_milkshake = milkshake} in
       {e with PH.env = env}
    )

let pass_ServerCpsRewriter =
  let transform pass_env =
    let options = pass_env.PassHandler.options in
    let env = pass_env.PassHandler.env in
    { pass_env with PassHandler.
        env = Passes.pass_QmlCpsRewriter false ~options env
    }
  in
  (* invariants, and pre/post conds *)
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
    ] in
  let postcond =
    [
      (*
        This post condition is currently broken because of the second_order bypasses.
        In bsl_ocaml_init.ml they take 1 extra argument, but the bypass_typer does not know
        about it. This condition is desactivated until we solve this probleme.
        QmlCheck.Bypass.applied extract
      *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform

let pass_ServerQmlClosure =
  PassHandler.make_pass
    (fun e ->
       { e with PH.env = P.pass_QmlClosure2 ~typed:false ~side:`server ~options:e.PH.options e.PH.env })

let pass_QmlConstantSharing_gen side =
  let transform pass_env =
    let typerEnv, code = P.extract_env_NewFinalCompile pass_env.PH.env in
    let gamma = typerEnv.QmlTypes.gamma
    and annotmap = typerEnv.QmlTypes.annotmap in
    let (gamma,annotmap), code =
      Pass_ConstantSharing.process_code ~side ~typed:false gamma annotmap code in
    let typerEnv =
      { typerEnv with
        QmlTypes.gamma = gamma;
        QmlTypes.annotmap = annotmap }
    in
    { pass_env with PH.env = P.rebuild_env_NewFinalCompile pass_env.PH.env typerEnv code
    }
  in
  let precond =
    [
      (* TODO: add pre condition *)
    ] in
  let postcond =
    [
      QmlAlphaConv.Check.alpha extract_final_ac ;
    ] in
  PassHandler.make_pass ~precond ~postcond transform

let pass_QmlConstantSharing =
  pass_QmlConstantSharing_gen `server
let pass_ClientQmlConstantSharing =
  let pass_QmlConstantSharing = pass_QmlConstantSharing_gen `client in
  Adapter.adapt_new_sliced_on_client pass_QmlConstantSharing

let pass_QmlCompilation =
  let transform pass_env =
    let options = pass_env.PH.options in
    let env = pass_env.PH.env in
    (* get env entities *)
    let qml2ocaml_env_bsl = env.Passes.newFinalCompile_bsl in
    let qml2ocaml_qml_milkshake = env.Passes.newFinalCompile_qml_milkshake in
    (* renaming is not used *)
    (* 1) transform options *)
    let qmlCompilation_options = Passes.pass_OpaOptionsToQmlOptions ~options qml2ocaml_qml_milkshake in
    (* 2) selection of the back-end *)
    let qml_to_ocaml =
      match options.O.back_end with
      | `qmlflat -> Flat_Compiler.qml_to_ocaml in
    (* proceed *)
    let qmlCompilation_env_ocaml_input = qml_to_ocaml qmlCompilation_options qml2ocaml_env_bsl qml2ocaml_qml_milkshake in
    (* build env *)
    let qmlCompilation_env =
      {
        qmlCompilation_options = qmlCompilation_options ;
        qmlCompilation_env_ocaml_input = qmlCompilation_env_ocaml_input
      }
    in
    let empty _ = [] in
    {
      pass_env with PassHandler.
        env = qmlCompilation_env ;
        printers = OcamlTrack.printers (fun env -> env.qmlCompilation_env_ocaml_input.Qml2ocaml.ocaml_code) ;
        trackers = empty
    }
  in
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
      (* TODO: add pre conditions *)
    ] in
  let postcond =
    [
      (* TODO: add pre conditions *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform

let pass_OcamlSplitCode =
  let transform pass_env =
    let env = pass_env.PassHandler.env in
    (* get env entities *)
    let ocamlSplitCode_options = env.qmlCompilation_options in
    let qmlCompilation_env_ocaml_input = env.qmlCompilation_env_ocaml_input in
    (* proceed *)
    let ocamlSplitCode_env_ocaml_split =
      Qml2ocaml.OcamlCompilation.ocaml_split_code ocamlSplitCode_options qmlCompilation_env_ocaml_input in
    (* build env *)
    let env = {
      ocamlSplitCode_options = ocamlSplitCode_options ;
      ocamlSplitCode_env_ocaml_split = ocamlSplitCode_env_ocaml_split ;
    }
    in
    let empty _ = [] in
    {
      pass_env with PassHandler.
        env = env ;
        printers = empty ;
        trackers = empty
    }
  in
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
      (* TODO: add pre conditions *)
    ] in
  let postcond =
    [
      (* TODO: add pre conditions *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform

let pass_OcamlGeneration =
  let transform pass_env =
    let env = pass_env.PassHandler.env in
    (* get env entities *)
    let ocamlGeneration_options = env.ocamlSplitCode_options in
    let ocamlSplitCode_env_ocaml_split = env.ocamlSplitCode_env_ocaml_split in
    (* proceed *)
    let ocamlGeneration_env_ocaml_output =
      Qml2ocaml.OcamlCompilation.ocaml_generation ocamlGeneration_options ocamlSplitCode_env_ocaml_split in
    (* build env *)
    let ocamlGeneration_env =
      {
        ocamlGeneration_options = ocamlGeneration_options ;
        ocamlGeneration_env_ocaml_output = ocamlGeneration_env_ocaml_output
      }
    in
    let empty _ = [] in
    {
      pass_env with PassHandler.
        env = ocamlGeneration_env ;
        printers = empty ;
        trackers = empty
    }
  in
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
      (* TODO: add pre conditions *)
    ] in
  let postcond =
    [
      (* TODO: add pre conditions *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform

let pass_OcamlCompilation =
  let transform pass_env =
    let env = pass_env.PassHandler.env in
    (* get env entities *)
    let ocamlGeneration_options = env.ocamlGeneration_options in
    let ocamlGeneration_env_ocaml_output = env.ocamlGeneration_env_ocaml_output in
    (* proceed *)
    let ocamlCompilation_returned_code =
      Qml2ocaml.OcamlCompilation.ocaml_compilation ocamlGeneration_options ocamlGeneration_env_ocaml_output in
    (* build env *)
    let ocamlCompilation_env =
      {
        ocamlCompilation_returned_code = ocamlCompilation_returned_code
      }
    in
    let empty _ = [] in
    {
      pass_env with PassHandler.
        env = ocamlCompilation_env ;
        printers = empty ;
        trackers = empty
    }
  in
  let invariant =
    [
      (* TODO: add invariants *)
    ] in
  let precond =
    [
      (* TODO: add pre conditions *)
    ] in
  let postcond =
    [
      (* TODO: add pre conditions *)
    ] in
  PassHandler.make_pass ~invariant ~precond ~postcond transform

let pass_CleanUp =
  { PH.
      invariant = [];
      precond = [];
      postcond = [];
      f = (fun e ->
             QmlRefresh.clear ();
             OpaMapToIdent.reset ();
             let code = e.PH.env.ocamlCompilation_returned_code in
             if code = 0 then ObjectFiles.compilation_is_successfull ();
             e)
  }

let pass_ByeBye =
  (* Ocaml typer is stupid! (or I'm) *)
  (* make_pass (fun _ -> exit 0) *)
  { PH.
    invariant = [];
    precond = [];
    postcond = [];
    f = (fun env ->
           let code = env.PH.env.ocamlCompilation_returned_code in
           if code <> 0 then OManager.exit code;
           PH.make_env () ()
        );
  }


(*-----------------------------*)
(*---- register printers ------*)
(*-----------------------------*)
(*grep '".*"' main.ml | sed 's/.*"\(.*\)".*/     | "\1" ->/g'*)
let () =
  PassHandler.register_printer
    (function
     | "Opa.exe"
     | "Welcome"
     | "CheckOptions"
     | "AddStdlibFiles"
     | "OpenFiles"
     | "Parse"
     | "DumpParsed"
     | "ConvertStructure"
     | "LoadObjects"
     | "CheckTypesDuplication"
     | "CheckServerEntryPoint"
     | "ParserGeneration"
     | "CheckDuplication"
     | "BslLoading"
     | "AddingRoots"
     | "AddingCompileEnvVars"
     | "StaticInclusionDirectory"
     | "StaticFileMimeType"
     | "StaticInclusions"
     | "ServerEntryPoint"
     | "AddDocApiDirectives"
     | "TupleTypeGeneration"
     | "Reorder"
     | "RewriteModules"
     | "AddingServer"
     | "CheckBypass"
     | "SAtoQML" -> None

     | "AddCSS"
     | "CheckPatternMatching"
     | "ExtractDbExpr"
     | "FunActionLifting"
     | "ExtractLocalTypes"
     | "TypesDefinitions"
     | "DbSchemaGeneration"
     | "DbPathCoercion"
     | "Typing"
     | "BlenderDB"
     | "DocApiGeneration"
     | "WarnCoerce"
     | "MacroExpansion"
     | "BypassHoisting"
     | "Undot" -> Some (QmlTracker.printers Extract.EnvGen.agc)

     | "NoSlicerRemoveJsIdent"
     | "Slicing"
     | "NoSlicing"
     | "PrepareSlicedCleaning"
     | "FunActionEnvSerialize"
     | "ExplicitInstantiation"
     | "OptimizeExplicitInstantiation"
     | "SlicedCleaning"
     | "FunActionJsCallGeneration"
     | "PurgeTypeDirectives"
     | "ResolveRemoteCalls"
     | "DiscardSlicerDirectives"
     | "InsertFunInfos"
     | "JustReorder2" -> Some (Obj.magic sliced_printers)

     | "ClientQmlCpsRewriter"
     | "ClientLambdaLifting"
     | "ClientQmlBypassHoisting"
     | "ClientQmlUncurry"
     | "ClientQmlClosure"
     | "JavascriptCompilation" -> None

     | "ResolveJsIdent"
     | "QmlLiftDeepRecords"
     | "NoSlicerCleanClientBypass"
     | "ServerQmlCpsRewriter"
     | "ServerLambdaLifting"
     | "ServerQmlBypassHoisting"
     | "ServerQmlUncurry"
     | "ServerQmlClosure"
     | "QmlConstantSharing" -> Some (Obj.magic final_printers)

     | "QmlCompilation"
     | "OcamlGeneration"
     | "OcamlCompilation"
     | "ByeBye" -> None

     | _ -> None)
