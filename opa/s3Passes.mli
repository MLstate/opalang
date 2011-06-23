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
open Passes

(** Pass and pass utils for OPA S3. *)

(** {6 Type alias to passHandler for OPA}*)
(** Shortcut for [opa_options] *)
type opa_options = OpaEnv.opa_options

(** Type of OPA environments. *)
type 'env opa_env = (opa_options, 'env) PassHandler.one_env

(** Type of OPA passes. *)
type ('env, 'env2) opa_pass =
    (opa_options, opa_options, 'env, 'env2) PassHandler.pass

(** Type of old OPA passes. *)
type ('env, 'env2) opa_old_pass =
    (opa_options, 'env, 'env2) PassHandler.old_pass

(** {6 Some environment} *)

(**
   Environment for keeping (server * client) environments.
*)
type env_bothFinalCompile = (env_NewFinalCompile * env_NewFinalCompile)

(**
   Environment returned by the QmlCompilation.
*)
type env_QmlCompilation = {
  qmlCompilation_options : Qml2ocamlOptions.argv_options ;
  qmlCompilation_env_ocaml_input : Qml2ocaml.env_ocaml_input ;
}

(**
   Environment returned after splitting ocaml code into smaller files
*)
type env_OcamlSplitCode = {
  ocamlSplitCode_options : Qml2ocamlOptions.argv_options ;
  ocamlSplitCode_env_ocaml_split : Qml2ocaml.env_ocaml_split ;
}

(**
   Environment returned by the OcamlGeneration.
*)
type env_OcamlGeneration = {
  ocamlGeneration_options : Qml2ocamlOptions.argv_options ;
  ocamlGeneration_env_ocaml_output : Qml2ocaml.env_ocaml_output ;
}

(**
   Environment returned by the OcamlCompilation.
*)
type env_OcamlCompilation = {
  ocamlCompilation_returned_code : int ;
}

(**{6 S3 Passes} All value bellow should be type of
   [opa_pass].*)

val pass_Welcome : (unit, opa_options, unit, unit) PassHandler.pass

val pass_CheckOptions : (unit, env_ArgParse) opa_pass

(**
   {6 Stdlib files}

   This pass adds OPA's standard include list.
*)
val pass_AddStdlibFiles : (env_ArgParse, env_AddStdlibFiles) opa_pass

val pass_PreProcess : (env_OpenFiles, env_OpenFiles) opa_pass

val pass_Parse :
  (
    env_OpenFiles,
    ( SurfaceAst.parsing_directive SurfaceAstPassesTypes.parsed_file list
    * SurfaceAst.parsing_directive SurfaceAstPassesTypes.parsed_file list
    ) * env_OpenFiles
  )
  opa_pass

val pass_RegisterAppSrcCode :
  (
    (
      ( SurfaceAst.parsing_directive SurfaceAstPassesTypes.parsed_file list
      * SurfaceAst.parsing_directive SurfaceAstPassesTypes.parsed_file list
      ) as 'parsed_files
    )
    * env_OpenFiles
  ,
    'parsed_files
  ) opa_pass

val pass_BslLoading :
  ((((SurfaceAst.nonuid, SurfaceAst.parsing_directive)
    SurfaceAst.code_elt) ObjectFiles.parsed_code) as 'parsed_code
    ,
  'parsed_code * BslLib.env_bsl) opa_pass

val pass_ConvertStructure :
  (
    ((SurfaceAst.nonuid, SurfaceAst.parsing_directive as 'directive)
       SurfaceAst.code_elt) ObjectFiles.parsed_code * BslLib.env_bsl
      ,
    (SurfaceAst.nonuid, 'directive) SurfaceAstPassesTypes.env_both_lcodes
  ) opa_pass

val pass_LoadObjects :
  (
    (
      SurfaceAstPassesTypes.options,
      (SurfaceAst.nonuid,
       [< SurfaceAst.all_directives >
            `static_content `static_content_directory `static_resource `static_resource_directory ] as 'a)
        SurfaceAst.code_elt ObjectFiles.parsed_code
    ) PassHandler.one_env
    -> unit
  )
  ->
  (
    SurfaceAstPassesTypes.options, unit,
    ('a SurfaceAstPassesTypes.parsed_file list * 'a SurfaceAstPassesTypes.parsed_file list)
      , unit
  )
    PassHandler.pass

val pass_CheckServerEntryPoint :
  (
   (SurfaceAst.nonuid, SurfaceAst.parsing_directive) SurfaceAstPassesTypes.env_both_lcodes,
   (SurfaceAst.nonuid, SurfaceAst.parsing_directive) SurfaceAstPassesTypes.env_both_lcodes
  )
  opa_pass

val pass_ParserGeneration :
  (
   (SurfaceAst.nonuid, SurfaceAst.parsing_directive)  SurfaceAstPassesTypes.env_both_lcodes,
   (SurfaceAst.nonuid, SurfaceAst.renaming_directive) SurfaceAstPassesTypes.env_both_lcodes
  )
  opa_pass

val pass_CheckDuplication :
  (
    (SurfaceAst.nonuid, SurfaceAst.renaming_directive)   SurfaceAstPassesTypes.env_both_lcodes,
    (SurfaceAst.uids,   SurfaceAst.dependency_directive) SurfaceAstPassesTypes.env_both_lcodes
  )
  opa_pass

val pass_I18nAndComputedString :
  (
   (SurfaceAst.uids, SurfaceAst.dependency_directive) SurfaceAstPassesTypes.env_both_lcodes,
   (SurfaceAst.uids, SurfaceAst.dependency_directive) SurfaceAstPassesTypes.env_both_lcodes
  )
  opa_pass

val pass_TreatNoClientCalls : unit ->
  (
    (SurfaceAstCons.ExprIdentCons.ident,  [< SurfaceAst.all_directives >
      `no_client_calls `with_thread_context] as 'a ) Passes.sa_env_Gen,
    (SurfaceAstCons.ExprIdentCons.ident, 'a) Passes.sa_env_Gen
  )
  opa_pass

val pass_ConvertStructure2 : unit ->
  (
    (Ident.t,  [< SurfaceAst.all_directives ] as 'a) SurfaceAstPassesTypes.env_both_lcodes,
    (Ident.t, 'a) Passes.sa_env_Gen
  )
  opa_pass

val pass_AddDocApiDirectives : unit ->
  (
   (SurfaceAstCons.ExprIdentCons.ident,
    [< SurfaceAst.all_directives
       > `coerce `deprecated `doctype `local `module_ `package
       `private_ `public `side_annotation `visibility_annotation ]
      as 'c) Passes.sa_env_Gen,
   (SurfaceAstCons.ExprIdentCons.ident, 'c) Passes.sa_env_Gen)
    opa_pass

val pass_ReorderToplevel : unit ->
  (
   (Ident.t, SurfaceAst.dependency_directive) Passes.sa_env_Gen,
   (Ident.t, SurfaceAst.dependency_directive) Passes.sa_env_Gen)
    opa_pass

val pass_RewriteModules : unit ->
  (
   (Ident.t, SurfaceAst.dependency_directive) Passes.sa_env_Gen,
   (Ident.t, SurfaceAst.basic_directive) Passes.sa_env_Gen)
    opa_pass

(**********************************************)
(* QML AST PASSES *****************************)

val pass_SaToQml :
  (
   (SurfaceAst.uids, SurfaceAst.basic_directive) Passes.sa_env_Gen,
   unit Passes.env_Gen) opa_pass

val pass_AddCSS :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_CheckPatternMatching :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_EndOfSeparateCompilation :
  ((SurfaceAstPassesTypes.options, unit Passes.env_Gen) PassHandler.one_env -> unit) ->
  (opa_options, unit, unit Passes.env_Gen, unit) PassHandler.pass

(*
  (Passes.env_Gen, Passes.env_Gen) opa_pass

((SurfaceAstPassesTypes.options, ('a, 'b, SurfaceAstCons.ExprIdentCons.ident, SurfaceAst.basic_directive) Passes.sa_env_Gen_aux) PassHandler.one_env -> unit) ->
(SurfaceAstPassesTypes.options, unit, ('a, 'b, SurfaceAstCons.ExprIdentCons.ident, SurfaceAst.basic_directive) Passes.sa_env_Gen_aux, unit) PassHandler.pass
*)

val pass_FunActionLifting :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_TypesDefinitions :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_DbSchemaGeneration :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_DbPathCoercion :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

(** {b Descr}: The typechecking passe. *)
val pass_Typing :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_Retyping :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_PurgeTypeDirectiveAfterTyping :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_DbAccessorsGeneration :
  (unit Passes.env_Gen, (QmlDbGen.dbinfo StringListMap.t * QmlAlphaConv.t option) Passes.env_Gen) opa_pass

val pass_DbCodeGeneration :
  ((QmlDbGen.dbinfo StringListMap.t * QmlAlphaConv.t option) Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_DocApiGeneration :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_WarnCoerce :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_CompileRecursiveValues :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_RewriteAsyncLambda :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_MacroExpansion :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_EnrichMagic :
  (unit Passes.env_Gen, Pass_SimplifyMagic.env Passes.env_Gen) opa_pass

val pass_EnrichMagicPurge :
  (unit Passes.env_Gen, Pass_SimplifyMagic.env Passes.env_Gen) opa_pass

val pass_SimplifyEquality :
  (Pass_SimplifyMagic.env Passes.env_Gen,
   Pass_SimplifyMagic.env Passes.env_Gen) opa_pass

val pass_SimplifyMagic :
  (Pass_SimplifyMagic.env Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_InstrumentForClosureSerialization :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_ReorderEnvGen :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_BypassHoisting :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_RegisterFields :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_QmlUndot :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

val pass_CodingDirectives :
  (unit Passes.env_Gen, unit Passes.env_Gen) opa_pass

(**********************************************)
(* SLICED PASSES ******************************)

val pass_NoSlicer :
  (unit env_Gen, unit env_Gen_sliced) opa_pass

val pass_SimpleSlicer :
  (unit env_Gen, unit env_Gen_sliced) opa_pass

val pass_ExplicitInstantiation :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

val pass_FunActionEnvSerialize :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

val pass_OptimizeExplicitInstantiation :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

val pass_Assertion :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

val pass_SlicedCleaning :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

val pass_FunActionJsCallGeneration :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

val pass_PurgeTypeDirectiveAfterEi :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

val pass_ResolveRemoteCalls :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

val pass_InsertMemoizedTypes :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

val pass_SlicedReorder :
  (unit env_Gen_sliced, unit env_Gen_sliced) opa_pass

(* ***********************************************)
(* FINAL COMPILATION *****************************)

val pass_SlicedToFinal :
  (unit env_Gen_sliced, env_bothFinalCompile) opa_pass

(* ***********************************************)
(* FINAL CLIENT COMPILATION **********************)

val pass_ClientCpsRewriter :
  (env_bothFinalCompile, env_bothFinalCompile) opa_pass

val pass_ClientLambdaLifting :
  (env_bothFinalCompile, env_bothFinalCompile) opa_pass

val pass_ClientQmlUncurry :
  (env_bothFinalCompile, env_bothFinalCompile) opa_pass

val pass_ClientQmlClosure :
  (env_bothFinalCompile, env_bothFinalCompile) opa_pass

val pass_ClientQmlConstantSharing :
  (env_bothFinalCompile, env_bothFinalCompile) opa_pass

val pass_JavascriptCompilation :
  (env_bothFinalCompile, env_NewFinalCompile) opa_pass

val pass_ResolveJsIdent :
  (env_NewFinalCompile, env_NewFinalCompile) opa_pass

val pass_GenerateServerAst : bool ->
  (env_NewFinalCompile, env_NewFinalCompile) opa_pass

(* ***********************************************)
(* FINAL SERVER COMPILATION **********************)

val pass_CleanLambdaLiftingDirectives :
  (env_NewFinalCompile, env_NewFinalCompile) opa_pass

val pass_InitializeBslValues :
  (env_NewFinalCompile, env_NewFinalCompile) opa_pass

val pass_ServerCpsRewriter :
  (env_NewFinalCompile, env_NewFinalCompile) opa_pass

val pass_ServerQmlClosure :
  (env_NewFinalCompile, env_NewFinalCompile) opa_pass

val pass_QmlLiftDeepRecords :
  (env_NewFinalCompile, env_NewFinalCompile) opa_pass

val pass_QmlConstantSharing :
  (env_NewFinalCompile, env_NewFinalCompile) opa_pass

val pass_QmlCompilation :
  (env_NewFinalCompile, env_QmlCompilation) opa_pass

val pass_OcamlSplitCode :
  (env_QmlCompilation, env_OcamlSplitCode) opa_pass

val pass_OcamlGeneration :
  (env_OcamlSplitCode, env_OcamlGeneration) opa_pass

val pass_OcamlCompilation :
  (env_OcamlGeneration, env_OcamlCompilation) opa_pass

val pass_CleanUp : ('opt, 'opt, env_OcamlCompilation, env_OcamlCompilation) PassHandler.pass

val pass_ByeBye : (_, unit, env_OcamlCompilation, unit) PassHandler.pass
