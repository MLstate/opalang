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
(*
    @author Cedric Soulas
    @author Damien Lefortier
    @author Rudy Sicard
    @author Mathieu Barbin
    @author Mehdi Bouaziz
    @author Corentin Gallet
    @author Audouin Maxime

    @review David Teller (08-27-2009)
*)

(**
   The definition of compiler passes.

   This module gives access to all the features of the compiler as passes.
   This module should {i only} contain passes.

   @author Cedric Soulas
   @author Damien Lefortier
   @author Rudy Sicard
   @author Mathieu Barbin
   @author Mehdi Bouaziz
   @author David Rajchenbach-Teller, 2010 (simplifications)

   @review David Teller (08-27-2009)
*)

(*
  TODO: this file is a big mess,
  implementations should be removed from there, this file should contains only
  box for passHander.
  Actually, s3Passes.ml[i] will replace this file as soon as s3 is working good enough
  to remove s2, and all libold.
*)

(* depends *)
module Format = BaseFormat
module String = BaseString
module List = BaseList

(* refactoring in progress *)

(* shorthands *)
module Q = QmlAst
module BPI = BslPluginInterface

(* -- *)

type opa_options = OpaEnv.opa_options

let search str ast =
  prerr_endline ("searching... " ^ str);
  List.iter
    (function
      | Q.NewVal (_, bindings)
      | Q.NewValRec (_, bindings) ->
          List.iter (fun (ident, _) ->
            let s = Ident.to_string ident in
            if String.is_contained str s then
              begin
                prerr_endline "OK";
                prerr_endline s;
              end
          ) bindings
      | _ -> ()
    )
  ast

type env_ArgParse = string list

(**
   {6 BSL}

   Resulting environment for the BSL, load the Bypass Standard Library.

   This is the first non-trivial pass (i.e. the first pass after displaying the copyright, etc.).
   It loads both the standard library of bypasses and any plugin specified with command-line
   arguments {v *.opp v}
*)

(**
   BSL environment with a non backend specific bymap. (for typing only)
*)
type env_Bsl = BslLib.env_bsl

type env_AddStdlibFiles = (string list * string list)

(**
   {6 Open files}

   This pass deals with opening and loading the contents of .opa files. Text is
   only parsed in an ulterior pass. Some files are embedded into the executable
   compiler and are automatically included, unless option {OpaEnv.embedded_opa}
   is unset.
*)

(**
   An input file, i.e. a source file with extension {v .opa v}.
*)
type input_file = {
  inputFile_filename : string (**The complete name of the input file.*);
  inputFile_basename : string (**The base name of the input file -- for the moment, identical to the previous field.*) ;
  inputFile_content :  string; (**The complete contents of the file.*)
  inputFile_org_content : string (**The original content of the file (the previous
                                    field will be modified in the PreProcess pass,
                                    whereas this field will continue to hold the
                                    original content of the file) *)
}

(** A list if special .opa files and a list of user-specified .opa
    files.
*)
type env_OpenFiles = (input_file list * input_file list)

(**
   Open and load the contents of files.

   Depending on the option {OpaEnv.embedded_opa}, the content of the files are search:
   + [embedded_opa] is set : load from .opa files which have been embedded into the compiler
   + [embedded_opa] is unset : load from the real content on the disk.

   Remark: only the stdlib has this "special" status.
   This is simply replaced by an assertion in case of the option [separated] is set.
*)
let pass_OpenFiles ~(options:opa_options) (env:env_AddStdlibFiles) : env_OpenFiles =
  let (special_files, filenames) = env in
  let input_file_of_filename ?(warn_no_embedded=false) filename =
    let content =
      if options.OpaEnv.embedded_opa then
        CompilationUtils.file_content_and_embedded ~warn_no_embedded filename
      else
        File.content filename
    in
    {
      inputFile_filename = filename ;
      inputFile_basename = (* Filename.basename *) filename ;
      inputFile_content  = content ;
      inputFile_org_content  = content
    }
  in
  let special_files = List.map (input_file_of_filename ~warn_no_embedded:true) special_files in
  let user_files = List.map input_file_of_filename filenames in
  (special_files, user_files)



(**
   {6 Addition of roots}


*)

type 'a doc_types = 'a SurfaceAst.typedef list (** opadoc : contains all type declaration *)

type ('c, 'd) sa_env_Gen = {
  sa_lcode : ('c,'d) SurfaceAst.code; (** The surface AST source code*)
  sa_doc_types : Ident.t doc_types; (** opadoc data *)
  sa_bsl : env_Bsl                ;(**BSL environment with dummy backend, only for introspection of bypasses*)
  sa_type_renaming : (Ident.t * FilePos.pos) StringMap.t;
  (** The set of values identifiers that are exported outside this package.
      It contains all toplevel values definitions that are not marked by a
      @private directive. *)
  sa_exported_values_idents : IdentSet.t
}


(**
   The environment used for various rewriting phases.
*)
type 'tmp_env env_Gen = {
  bsl : env_Bsl                ;(**BSL environment with dummy backend, only for introspection of bypasses*)
  qmlAst : QmlAst.code ;
  typerEnv : QmlTyper.env ; (* contains Gamma, db_schema, and annotMap *)
  local_typedefs : QmlAst.TypeIdentSet.t;
  stdlib_gamma : QmlTypes.gamma; (* used to get the tsc of the stdlib ident *)
  doc_types : Ident.t doc_types; (** opadoc specific data *)
  exported : IdentSet.t;
  temporary_env : 'tmp_env;
}

type env_Sliced_aux = QmlSimpleSlicer.splitted_code = {
  code : QmlAst.code;
  published : Pass_ExplicitInstantiation.published_map;
  original_renaming : QmlRenamingMap.t;
  renaming : QmlRenamingMap.t;
}

type env_Sliced = {
  client : env_Sliced_aux;
  server : env_Sliced_aux;
}

type 'tmp_env env_Gen_sliced = {
  env_gen : 'tmp_env env_Gen;
  sliced_env : env_Sliced;
}

let env_getGamma    env = env.typerEnv.QmlTypes.gamma
let env_getSchema   env = env.typerEnv.QmlTypes.schema
let env_getAnnotMap env = env.typerEnv.QmlTypes.annotmap
let env_getBypassTyper env = env.typerEnv.QmlTypes.bypass_typer

let change_temporary (temporary_env : 'b) (env_gen : 'a env_Gen)  = ({
  bsl = env_gen.bsl;
  qmlAst = env_gen.qmlAst;
  typerEnv = env_gen.typerEnv;
  local_typedefs = env_gen.local_typedefs;
  stdlib_gamma = env_gen.stdlib_gamma;
  doc_types = env_gen.doc_types;
  exported = env_gen.exported;
  temporary_env = temporary_env;
} : ('b env_Gen))

let pass_resolve_server_entry_point ~options env =
    {env with sa_lcode = Pass_ServerDeclaration.pass_resolve_server_entry_point ~options env.sa_lcode}
let pass_adding_server ~options env =
    {env with sa_lcode = Pass_ServerDeclaration.pass_adding_server ~options env.sa_lcode}

(**
   Take care of the special function 'static_source_content' and 'static_binary_content'

   @TODO Should have a proper error message with position
*)
let pass_static_inclusions ~(options:opa_options) env =
  {env with sa_lcode = SurfaceAstStaticInclude.pass_static_inclusions ~options env.sa_lcode}

(**
   Take care of the special function 'static_include_directory'

   @TODO Should have a proper error message with position
*)
let pass_static_inclusion_directory ~(options:opa_options) env =
  {env with sa_lcode = SurfaceAstStaticInclude.pass_static_inclusion_directory ~options env.sa_lcode}

let pass_no_slicer ~options:(_:opa_options) (env:'tmp_env env_Gen) =
  let rec is_a_bypass = function
    | Q.Directive (_, (`restricted_bypass _ | `may_cps),[e],_) -> is_a_bypass e
    | Q.Bypass (_,key) -> Some key
    | _ -> None in
  let client_key key =
    match BslLib.BSL.ByPassMap.find_opt env.bsl.BslLib.bymap key with
    | Some bypass ->
        not (BslLib.BSL.ByPass.implemented_in_any bypass ~lang:[OpaEnv.Parameters.bsl_server_language])
    | None -> assert false in
  let annotmap, code =
    QmlAstWalk.CodeExpr.fold_map
      (QmlAstWalk.Expr.foldmap_up
         (fun annotmap -> function
          | Q.Directive (label, `sliced_expr, [_(*client*); e(*server*)], _)
          | Q.Directive (label, (#Q.slicer_directive | `fun_action _), [e], _) ->
              let tsc_gen = QmlAnnotMap.find_tsc_opt_label label annotmap in
              let annotmap = QmlAnnotMap.add_tsc_opt_label (Q.Label.expr e) tsc_gen annotmap in
              annotmap, e
          | Q.Directive (_, (#Q.slicer_directive | `sliced_expr | `fun_action _), _, _) ->
              assert false
          | Q.Directive (_, `js_ident, _, []) ->
              QmlAstCons.TypedExpr.string annotmap "Compiler forgot to change js event to value case"
          | e ->
              match is_a_bypass e with
              | Some key when client_key key ->
                  let annotmap, s =
                    QmlAstCons.TypedExpr.string
                      annotmap
                      (Printf.sprintf "%s is not wanted on server" (BslKey.to_string key)) in
                  annotmap, Q.Directive (Q.Label.expr e,`fail,[s],[])
              | _ ->
                  annotmap, e
         )
      ) env.typerEnv.QmlTypes.annotmap env.qmlAst in
   let env_gen = env in
    { env_gen = {env_gen with typerEnv = {env_gen.typerEnv with QmlTypes.annotmap}};
      sliced_env = {
        server = {
          code;
          published = IdentMap.empty;
          original_renaming = QmlRenamingMap.empty;
          renaming = QmlRenamingMap.empty;
        };
        client = {
          code = [];
          published = IdentMap.empty;
          original_renaming = QmlRenamingMap.empty;
          renaming = QmlRenamingMap.empty;
        };
      }
    }


let pass_simple_slicer ~(options:opa_options) (env:'tmp_env env_Gen) =
  let make_sliced_env env_gen ~server ~client =
    { env_gen;
      sliced_env = {
        server = server;
        client = client;
      }
    }
  in
  let client_bsl_lang = BslLanguage.js in
  let server_bsl_lang = match options.OpaEnv.back_end with
    | `qmlflat -> BslLanguage.ml
    | `qmljs   -> BslLanguage.nodejs
  in
  let stdlib_gamma, typer_env, client, server =
       QmlSimpleSlicer.process_code
      ~test_mode:options.OpaEnv.slicer_test
      ~dump:options.OpaEnv.slicer_dump
      ~stdlib_gamma:env.stdlib_gamma
      ~typer_env:env.typerEnv
      ~client_bsl_lang
      ~server_bsl_lang
      ~bymap:env.bsl.BslLib.bymap
      ~code:env.qmlAst in

  (* updating toplevel renaming map with renamed server ident *)
  let update_maptoident =
    let map_before = OpaMapToIdent.get_val_map () in
    fun side renaming ->
      let updated =
        StringMap.fold
          (fun k v updated ->
             let v = List.filter_map (QmlRenamingMap.new_from_original_opt renaming) v in
             match  v with
             | [] -> updated
             | r -> StringMap.add k r updated

          )
          map_before StringMap.empty
      in
      OpaMapToIdent.set_val_map ~side updated;
  in
  update_maptoident `server server.QmlSimpleSlicer.renaming;
  update_maptoident `client client.QmlSimpleSlicer.renaming;
  (* Debug - print contains of maps *)
(*   OpaMapToIdent.iter_val_map *)
(*     ~side:`server *)
(*     (fun str ident -> *)
(*        prerr_endline str; *)
(*        debug_ident ident; *)
(*        prerr_endline "__server__" *)
(*     ); *)
(*   OpaMapToIdent.iter_val_map *)
(*     ~side:(`client) *)
(*     (fun str ident -> *)
(*        prerr_endline str; *)
(*        debug_ident ident; *)
(*        prerr_endline "__client__" *)
(*     ); *)
  let env_gen =
    {env with
       qmlAst = server.QmlSimpleSlicer.code;
       stdlib_gamma = stdlib_gamma;
       typerEnv = typer_env;
    } in

  make_sliced_env env_gen ~server ~client


type env_NewFinalCompile = {
  newFinalCompile_bsl : BslLib.env_bsl ;
  newFinalCompile_qml_milkshake : QmlBlender.qml_milkshake ;
  newFinalCompile_renaming_server : QmlRenamingMap.t;
  newFinalCompile_renaming_client : QmlRenamingMap.t;
  newFinalCompile_exported : IdentSet.t;
  newFinalCompile_closure_map : Ident.t IdentMap.t; (* see QmlUncurry.mli *)
  newFinalCompile_stdlib_gamma : QmlTypes.gamma;
}

let extract_env_Gen (env:'tmp_env env_Gen) =
  env.typerEnv, env.qmlAst

let rebuild_env_Gen (env:'tmp_env env_Gen) typerEnv qmlAst : 'tmp_env env_Gen =
  { env with typerEnv; qmlAst }

let extract_env_NewFinalCompile (env:env_NewFinalCompile) =
  env.newFinalCompile_qml_milkshake.QmlBlender.env,
  env.newFinalCompile_qml_milkshake.QmlBlender.code

let rebuild_env_NewFinalCompile (env:env_NewFinalCompile) typerEnv code : env_NewFinalCompile =
  let milkshake =
    { QmlBlender.
      env = typerEnv ;
      code ;
    }
  in
    { env with
        newFinalCompile_qml_milkshake = milkshake }

let pass_QmlProcessCode extract rebuild process_code ~options:(_:opa_options) env =
  let typerEnv, code = extract env in
  let gamma = typerEnv.QmlTypes.gamma
  and annotmap = typerEnv.QmlTypes.annotmap
  and bypass_typer = typerEnv.QmlTypes.bypass_typer in
  let (gamma,annotmap),code = process_code bypass_typer gamma annotmap code in
  let typerEnv =
    { typerEnv with
        QmlTypes.gamma = gamma;
        QmlTypes.annotmap = annotmap }
  in
    rebuild env typerEnv code


let pass_QmlProcessCode_env_Gen ~options env =
  pass_QmlProcessCode extract_env_Gen rebuild_env_Gen ~options env
let pass_QmlProcessCode_env_NewFinalCompile ~options env =
  pass_QmlProcessCode extract_env_NewFinalCompile rebuild_env_NewFinalCompile ~options env
let pass_QmlLiftDeepRecords =
  pass_QmlProcessCode_env_NewFinalCompile (fun _ -> Pass_LiftDeepRecords.process_code ~typed:true)
let pass_QmlUndot =
  pass_QmlProcessCode_env_Gen (fun _ -> Pass_Undot.process_code)
let pass_EarlyLambdaLifting =
  pass_QmlProcessCode_env_Gen (fun _ -> Pass_LambdaLifting.process_code ~early:true ~side:`server ~typed:true)
let pass_QmlClosure ~side ~options env =
  pass_QmlProcessCode_env_Gen
    (Pass_Closure.process_code ~typed:false ~side ~renaming_server:QmlRenamingMap.empty ~renaming_client:QmlRenamingMap.empty) ~options env

let extract_renaming_map ~side env =
  match side with
  | `client -> env.newFinalCompile_renaming_client
  | `server -> env.newFinalCompile_renaming_server
let pass_LambdaLifting2 ~typed ~side =
  pass_QmlProcessCode_env_NewFinalCompile (fun _ -> Pass_LambdaLifting.process_code ~early:false ~side ~typed)
let pass_QmlUncurry2 ~typed ~side ~options:_ env =
  let renaming_server = extract_renaming_map ~side:`server env in
  let renaming_client = extract_renaming_map ~side:`client env in
  let renamingmap, renamingmap_other =
    match side with
    | `client -> renaming_client, renaming_server
    | `server -> renaming_server, renaming_client in
  (* We can clean on linking and if the closure is not defined on the
     other side.*)
  let l = ObjectFiles.compilation_mode () = `linking in
  let can_be_cleaned i = l &&
    try
      let original = QmlRenamingMap.original_from_new renamingmap i in
      ignore (QmlRenamingMap.new_from_original renamingmap_other original);
      false
    with Not_found -> true in
  let {QmlTypes.gamma; annotmap} as typerEnv, code = extract_env_NewFinalCompile env in
  let (gamma,annotmap),newFinalCompile_closure_map,code =
    Pass_Uncurry.process_code ~side ~typed ~can_be_cleaned gamma annotmap code in
  let typerEnv = {typerEnv with QmlTypes.gamma; annotmap} in
  let newFinalCompile_qml_milkshake =
    { QmlBlender.
        env = typerEnv ;
        code ;
    } in
  { env with newFinalCompile_qml_milkshake; newFinalCompile_closure_map }
let pass_QmlClosure2 ~typed ~side ~options env =
  (* assert (env.newFinalCompile_renaming_client!=env.newFinalCompile_renaming_server); *)
  let renaming_server = extract_renaming_map ~side:`server env in
  let renaming_client = extract_renaming_map ~side:`client env in
  pass_QmlProcessCode_env_NewFinalCompile
    (Pass_Closure.process_code ~typed
       ~side
       ~renaming_server
       ~renaming_client
    ) ~options env

let pass_tuple_types ~options env =
  {env with sa_lcode = SurfaceAstPasses.pass_tuple_types ~options env.sa_lcode}

let arg_for_surfaceAstDependencies =
  (fun ?roots deps -> Reordering.create_group_list (Option.default [] roots) IntMap.empty deps)
let pass_reorder_toplevel ~options:_ env =
  {env with sa_lcode = SurfaceAstDependencies.reorder_toplevel arg_for_surfaceAstDependencies env.sa_lcode}

let pass_rewrite_modules ~options:_ env =
  let sa_exported_values_idents, sa_lcode =
    SurfaceAstDependencies.rewrite_modules arg_for_surfaceAstDependencies
      env.sa_exported_values_idents
      env.sa_lcode
  in
  {env with sa_lcode; sa_exported_values_idents}



let wrap_newfinalcompile pass ~options:(_:OpaEnv.opa_options) (env:env_NewFinalCompile) : env_NewFinalCompile  =
let qml_milkshake = env.newFinalCompile_qml_milkshake in
  let code = qml_milkshake.QmlBlender.code in
  let code = pass code in
  let qml_milkshake = {qml_milkshake with QmlBlender.code = code} in
  {env with newFinalCompile_qml_milkshake = qml_milkshake}


let pass_ReplaceCompileTimeDirective ~options code =
  {code with sa_lcode = Pass_CompileTimeDirective.process_code ~options code.sa_lcode}

let pass_DbAccessorsGeneration ~options:(_ : opa_options) env =
  (** About alpha conv : use opa ones, but do not apply it since calling blender final only*)
  let alphaconv_opt =
    let map =
      OpaMapToIdent.fold_val_map
        (fun k v map ->
           match v with
           | [] -> map
           | v::_ -> IdentMap.add (Ident.source k) v map)
        IdentMap.empty in
    let revmap =
      OpaMapToIdent.fold_val_map
        (fun k v revmap ->
           List.fold_left
             (fun revmap v -> IdentMap.add v (Ident.source k) revmap)
             revmap v)
        IdentMap.empty in
    Some (QmlAlphaConv.create_from_maps ~map:map ~revmap:revmap)
  in
  let () = env.temporary_env in
  let typerEnv = env.typerEnv in
  let gamma = typerEnv.QmlTypes.gamma in
  let annotmap = typerEnv.QmlTypes.annotmap in
  let schema = typerEnv.QmlTypes.schema in
  let code = env.qmlAst in
  let (dbinfo, gamma, annotmap(* , alphaconv_opt *), code) = Pass_DbAccessorsGeneration.process_code gamma annotmap schema code alphaconv_opt in
  let typerEnv = { typerEnv with QmlTypes.
                     gamma = gamma;
                     annotmap = annotmap} in
  {env with qmlAst = code; typerEnv = typerEnv; temporary_env = (dbinfo, alphaconv_opt)}


let pass_DbCodeGeneration ~options:(_:opa_options) env =
  let (dbinfo, alphaconv_opt) = env.temporary_env in
  let typerEnv = env.typerEnv in
  let gamma = typerEnv.QmlTypes.gamma in
  let annotmap = typerEnv.QmlTypes.annotmap in
  let schema = typerEnv.QmlTypes.schema in
  let code = env.qmlAst in
  let (gamma, annotmap, _alpha_opt, code) = Pass_DbCodeGeneration.process_code gamma annotmap schema code dbinfo alphaconv_opt in
  let typerEnv = { typerEnv with QmlTypes.
                     gamma = gamma;
                     annotmap = annotmap} in
  {env with qmlAst = code; typerEnv = typerEnv; temporary_env = ()}

let pass_QmlCpsRewriter client ~(options:opa_options) (env:env_NewFinalCompile) : env_NewFinalCompile =
  (* Passing options to qmlCpsRewriter : use syntax { with } like ever *)
  let opaoptions = options in
  let qml_closure =
    match options.OpaEnv.back_end with
    | `qmljs -> false
    | `qmlflat -> options.OpaEnv.closure
  in
  let server_side =
    match options.OpaEnv.back_end with
    | `qmljs -> false
    | `qmlflat -> not client;
  in
  let lang =
    match options.OpaEnv.back_end with
    | `qmljs -> BslLanguage.nodejs
    | `qmlflat -> BslLanguage.ml
  in
  let options =
    { QmlCpsRewriter.default_options with QmlCpsRewriter.
        no_assert = options.OpaEnv.no_assert ;
        no_server = Option.get options.OpaEnv.no_server;
        qml_closure ;
        toplevel_concurrency = options.OpaEnv.cps_toplevel_concurrency ;
        server_side ;
    } in
  let bsl_bypass_typer key =
    match BslLib.BSL.ByPassMap.bsl_bypass_typer env.newFinalCompile_bsl.BslLib.bymap key with
    | None -> (
        OManager.i_error "bypass : %%%% %s %%%%
User bypasses are not allowed using CPS transformation
because the types of all primitives are required.
Please use a bsl plugin@\n" (BslKey.to_string key)
      )
    | Some t -> t in
  let bsl_bypass_tags key =
    match BslLib.BSL.ByPassMap.bsl_bypass_tags ~lang
      env.newFinalCompile_bsl.BslLib.bymap key with
      | None ->
          OManager.i_error "bypass : %%%% %s %%%% is not found in lang %a"
            (BslKey.to_string key) BslLanguage.pp lang
      | Some t -> t in
  let bsl_bypass_cps = BslLib.BSL.ByPassMap.bsl_bypass_cps
    ~lang env.newFinalCompile_bsl.BslLib.bymap in
  let env_cps =
    QmlCpsRewriter.env_initial
      ~options
      ~bsl_bypass_typer
      ~bsl_bypass_tags
      ~bsl_bypass_cps
      ~typing:(env.newFinalCompile_qml_milkshake.QmlBlender.env)
      () in
  let code = env.newFinalCompile_qml_milkshake.QmlBlender.code in
  let private_env, code =
    let side = if client then `client else `server in
    try
      (if opaoptions.OpaEnv.cps then QmlCpsRewriter.cps_pass ~side
       else QmlCpsRewriter.no_cps_pass)
        env_cps code
    with
    | ( QmlCpsRewriter.Exception error ) as e ->
        OManager.printf "During CPS transformation :@\n%s@\n" (QmlCpsRewriter.error_message error) ;
        raise e  (** plus + : very usefull to see the backtrace *)
  in
  let update_exported exported =
    IdentSet.fold
      (fun i exported ->
         match QmlCpsRewriter.private_env_get_skipped_ident private_env i with
         | Some skip_id -> IdentSet.add skip_id exported
         | None -> match QmlRenamingMap.new_from_original_opt
             env.newFinalCompile_renaming_server i with
             | None -> exported
             | Some i2 -> match QmlCpsRewriter.private_env_get_skipped_ident private_env i2 with
               | Some skip_id -> IdentSet.add skip_id exported
               | None -> exported
      ) exported exported
  in
  let exported = update_exported env.newFinalCompile_exported in
  (* ignore (PassTracker.print ~passname:"CPSEXPORTED" ~printer_id:"js_exported" (IdentSet.pp ", " QmlPrint.pp#ident) exported); *)
  let qml_milkshake = { env.newFinalCompile_qml_milkshake with QmlBlender.code = code } in
  { env with
      newFinalCompile_qml_milkshake = qml_milkshake;
      newFinalCompile_exported = exported;
  }


type env_BinaryGeneration = {
  binaryGeneration_success : bool
}

let pass_OpaOptionsToQmlOptions ~(options:opa_options) qml_milkshake =
  (** PASSING OPTIONS TO QML2OCAML : we must absolutly here use the syntax \{ with \} because
      the interface of qmlfake options is unfrozen (and need to stay so) (don't need to update this code) *)
  let argv_options = Qml2ocamlOptions.ArgvOptions.default (OpaEnv.string_of_available_back_end options.OpaEnv.back_end) in
  let argv_options =
    let compilation_directory =
      if options.OpaEnv.build_dir = ""
      then Filename.concat "_build" (File.chop_extension options.OpaEnv.target)
      else options.OpaEnv.build_dir
    in
    let split_ocaml_value =
      let mindeclfile = 20 (* desire *) in
      let maxfiles = 500 (* approximate constraint *) in
      let nb_decl = List.length qml_milkshake.QmlBlender.code in
      let split_for_maxfiles =  int_of_float (ceil ((float nb_decl ) /. (float maxfiles))) in
        (* min 2 *) (max mindeclfile split_for_maxfiles)
    in
    let cclib = List.map (Printf.sprintf "-cclib %s") options.OpaEnv.cclib in
    let ccopt = List.map (Printf.sprintf "-ccopt %s") options.OpaEnv.ccopt in
    { argv_options with Qml2ocamlOptions.
        bypass_plugin = options.OpaEnv.bypass_plugin ;
        camlp4o = false ; (* options.OpaEnv.camlp4o *)
        compilation_directory = compilation_directory ; (* options.OpaEnv.build_dir *)
        compile_via_makefile = true; (* not Base.is_windows; *) (** use _build/Makefile except on Windows *)
        cps = options.OpaEnv.cps ;
        cps_toplevel_concurrency = options.OpaEnv.cps_toplevel_concurrency ;
        server_side = true;
        display_schema = options.OpaEnv.dump_dbgen_schema ;
        exe_argv = Option.default [] options.OpaEnv.run_server_options ;
        extra_lib = argv_options.Qml2ocamlOptions.extra_lib @ options.OpaEnv.extralibs ;
        extra_path = OpaEnv.Parameters.server_include_dir @
        (List.map (Filename.concat (Lazy.force InstallDir.getenv)) OpaEnv.Parameters.server_include_mlstate_dir) @
        options.OpaEnv.extrapath ;
        exe_run = Option.is_some options.OpaEnv.run_server_options ;
        hacker_mode = options.OpaEnv.hacker_mode ;
        makefile_rule = options.OpaEnv.makefile_rule ;
        mlcopt = argv_options.Qml2ocamlOptions.mlcopt @ options.OpaEnv.mlcopt @ ccopt ;
        mllopt = argv_options.Qml2ocamlOptions.mllopt @ options.OpaEnv.mllopt @ ccopt @ cclib ;
        no_assert = options.OpaEnv.no_assert ;
        ocamlc = options.OpaEnv.ocamlc ;
        ocamlopt = options.OpaEnv.ocamlopt ;
        profile = options.OpaEnv.profile ;
        qml_closure = options.OpaEnv.closure;
        show_compilation = options.OpaEnv.show_compilation ;
        split_ocaml_value =  split_ocaml_value ;
        target = options.OpaEnv.target ;
        (* hack for "cannot generalize"; in rare cases of Opa code helps, and can be complementary with eta_expand *)
        top_magic = true ;
    } in
  argv_options

let pass_QmlCompiler ~(options:opa_options) (env:env_NewFinalCompile) : env_BinaryGeneration =
  let qml_milkshake = env.newFinalCompile_qml_milkshake in
  let env_bsl = env.newFinalCompile_bsl in
  let argv_options = pass_OpaOptionsToQmlOptions ~options qml_milkshake in
  (** Choice of back-end *)
  assert (`qmlflat = options.OpaEnv.back_end);
  let qml_to_ocaml = Flat_Compiler.qml_to_ocaml in
  (* This pass is splitten in 3 in opas3 *)
  let return = Qml2ocaml.Sugar.for_opa qml_to_ocaml argv_options env_bsl qml_milkshake in
  let out = {
    binaryGeneration_success = return = 0
  }
  in let () = if return <> 0 then prerr_endline "OCAML COMPILER FAIL"
  in out

let macro_pass_CompilationSuccess ((options:opa_options), _) =
  flush stderr; flush stdout;
  (*List.iter (fun s->Printf.printf "%s" s)  (get_warnings ());flush stdout;*)
  let cwd = Sys.getcwd () in
(*   let exe = Filename.basename options.OpaEnv.target in *)
  let target_exe = options.OpaEnv.target in
  let exe_path = if Filename.is_relative target_exe then Filename.concat cwd target_exe else target_exe in
  OManager.printf "@{<green>Compilation is ok, result is current directory : %s@}" exe_path; flush stderr;
  0

let macro_pass_CompilationFailure _ =
  flush stderr; flush stdout;
  OManager.i_error "Ocaml Compilation Failed"


let pass_resolve_remote_calls ~options (env:'tmp_env env_Gen_sliced) =
  (* Get data from env *)
  let server_code, client_code =
    env.sliced_env.server.code, env.sliced_env.client.code in
  let annotmap, gamma =
    env_getAnnotMap env.env_gen, env_getGamma env.env_gen in
  let server_renaming, client_renaming =
    env.sliced_env.server.renaming, env.sliced_env.client.renaming in
  let server_published, client_published =
    env.sliced_env.server.published, env.sliced_env.client.published in
  let stdlib_gamma = env.env_gen.stdlib_gamma in
  let options =
    let i, p, c = options.OpaEnv.rpc_options in {
      Opa_InsertRemote.optimize_insert = i;
      Opa_InsertRemote.optimize_publish = p;
      Opa_InsertRemote.optimize_call = c;
    } in

  (* this could possibly be done inside of perform_code instead of before
   * but it seems that to process one code, you need both explicit maps *)
  Opa_InsertRemote.R2.save ~side:`server annotmap server_published;
  Opa_InsertRemote.R2.save ~side:`client annotmap client_published;

  if Opa_InsertRemote.need_to_process_code server_code client_code then (
    Opa_InsertRemote.prelude ~gamma ~annotmap server_code client_code;
    let annotmap, server_published = Opa_InsertRemote.R2.load ~side:`server annotmap server_published in
    let annotmap, client_published = Opa_InsertRemote.R2.load ~side:`client annotmap client_published in

    (* Perform passe *)
    let annotmap, gamma, server_code, more_client_code =
      Opa_InsertRemote.perform_on_server_code ~options ~annotmap ~stdlib_gamma ~gamma
        server_published
        client_published
        server_renaming
        client_renaming
        server_code in

    let annotmap, gamma, client_code, more_server_code =
      Opa_InsertRemote.perform_on_client_code ~options ~annotmap ~stdlib_gamma ~gamma
        client_published
        server_published
        client_renaming
        server_renaming
        client_code in
    (* Reconstruct env *)

    let server_code = more_server_code @ server_code in
    let client_code = more_client_code @ client_code in

    let exported = env.env_gen.exported in

    let client_renaming, server_renaming, exported =
      Opa_InsertRemote.postlude client_renaming server_renaming exported in

    let sliced_env =
      {
        server = {
          env.sliced_env.server with
            code = server_code;
            renaming = server_renaming;
        };
        client = {
          env.sliced_env.client with
            code = client_code;
            renaming = client_renaming;
        }
      } in
    let env_gen =
      { env.env_gen with
          exported;
          qmlAst = [];
          typerEnv = { env.env_gen.typerEnv with
                         QmlTypes.annotmap = annotmap;
                         QmlTypes.gamma = gamma;
                     };
      } in
    {
      sliced_env = sliced_env;
      env_gen = env_gen;
    }
  ) else (
    env
  )
