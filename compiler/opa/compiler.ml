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

(** The main program for the OPA compiler. S3 version. *)

(* Opening the generic pass system. *)
module PH = PassHandler

(* FIXME: define a module InfixOperators in PassHandler *)
(* this could by the only case an 'open' is allowed *)
let (|+>) = PH.(|+>)
let (|>) = PH.(|>)
let (<?>) = PH.(<?>)
let (&) = PH.(&)
let (|?>) = PH.(|?>)
let (|?|) = PH.(|?|)
let (or) = PH.(or)

(* Shorthands for accessing options of compilation *)
module If = Main_utils.If
module Switch = Main_utils.Switch

(* The deprecated passes *)
(* FIXME: adapt to the new PassHandler *)
module S2 = Passes

(* S3 implementations. *)
module S3 = S3Passes

type ('opt, 'opt2, 'env, 'env2) backend_handler = {
  name : string;
  aliases : string list;
  pass : ('opt, 'opt2, 'env, 'env2) PH.pass;
  dynloader : (BslPluginInterface.plugin -> unit) option;
  bsl_lang : BslLanguage.t;
  register_fields : string -> unit;
}

let make_backend name ?(aliases=[]) pass dynloader bsl_lang register_fields = {
  name = name;
  aliases = aliases;
  pass = pass;
  dynloader = dynloader;
  bsl_lang = bsl_lang;
  register_fields = register_fields;
}

(* Run all passes, except the backend specific passes *)
let compile backend_handlers =

  let available_back_end_list =
    List.map (
      fun backend_handler ->
	backend_handler.name :: backend_handler.aliases
    ) backend_handlers
  in

  let generic_find f =
    fun (OpaEnv.Backend b) ->
      let rec find_backend l =
	match l with
	| hd :: tl ->
	    if hd.name = b || (List.mem b hd.aliases) then f hd
	    else find_backend tl
	| [] -> OManager.error "The back-end @{<bright>%s@} was not recognized" b;
      in find_backend backend_handlers in
  let unify_backend_name = (* translate alias to backend name *)
    generic_find (fun b -> OpaEnv.Backend b.name)
  in
  let backend_pass_switcher = (* choose the backend pass *)
    generic_find (fun b -> ((Printf.sprintf "%sCompilation" b.name), b.pass))
  in
  let backend_dynloader_switcher = (* choose the dynloader and bsl_lang *)
    generic_find (fun b -> b.dynloader)
  in
  let backend_register_fields_switcher = (* choose fields register function *)
    generic_find (fun b -> b.register_fields)
  in
  let backend_bsl_lang_switcher = (* choose bsl_lang *)
    generic_find (fun b -> b.bsl_lang)
  in

  (**********************************************)
  (* INITIALIZATION *****************************)
  PH.init

  |+> ("Welcome", (S3.pass_Welcome available_back_end_list unify_backend_name))

  |+> ("CheckOptions", (S3.pass_CheckOptions unify_backend_name))

  |+> ("AddStdlibFiles", S3.pass_AddStdlibFiles)

  |> PH.old_handler
      "OpenFiles" S2.pass_OpenFiles

  |+> ("PreProcess", S3.pass_PreProcess)

  |+> ("Parse", S3.pass_Parse)

  |+> ("PluginCompilation", PH.make_pass Pass_PluginCompilation.process)

  (**********************************************)
  (* SURFACE AST PASSES *************************)
  |> PH.handler ~count_time:false "LoadObjects" (S3.pass_LoadObjects (
    fun e -> e

    |+> ("DbEngineImportation", S3.pass_DbEngineImportation)

    |+> ("BslLoading", S3.pass_BslLoading backend_dynloader_switcher backend_bsl_lang_switcher)

    |+> ("ConvertStructure", S3.pass_ConvertStructure)

    |> PH.old_handler
        "CheckOptionsConsistency" (Pass_CheckOptionsConsistency.process_code)

    |+> ("CheckServerEntryPoint", S3.pass_CheckServerEntryPoint)

    |+> ("ParserGeneration", S3.pass_ParserGeneration)

    |+> ("CheckDuplication", S3.pass_CheckDuplication)

    (* I18n, exit if generating template *)
    |+> ("I18nAndComputedString", S3.pass_I18nAndComputedString)

    |+> ("ConvertStructure2", S3.pass_ConvertStructure2 ())

    |+> ("TreatNoClientCalls",S3.pass_TreatNoClientCalls ())

    |> PH.old_handler
        "ReplaceCompileTimeDirective" S2.pass_ReplaceCompileTimeDirective

    |> PH.old_if_handler
        "StaticInclusionDirectory" S2.pass_static_inclusion_directory

    |> PH.old_if_handler
        "StaticInclusions" S2.pass_static_inclusions

    |> PH.old_if_handler ~if_:If.server
        "ServerEntryPoint" S2.pass_resolve_server_entry_point

    (* inserting doctype directive for a collection later
       currently always enable until an automated test verify that nobody breaks it
    *)
    |+> ((*PH.old_if_handler  ~if_:If.generate_interface,  *)
        "AddDocApiDirectives", (S3.pass_AddDocApiDirectives ()))

    |> PH.old_if_handler
        "TupleTypeGeneration" S2.pass_tuple_types

    |+> ("Reorder", (S3.pass_ReorderToplevel ()))

    |+> ("RewriteModules", (S3.pass_RewriteModules ()))

    |> PH.old_if_handler ~if_:If.server
        "AddingServer" S2.pass_adding_server

    (**********************************************)
    (* QML AST PASSES *****************************)
    |+> ("SAtoQML", S3.pass_SaToQml)

    |+> ("AddCSS", S3.pass_AddCSS)

    |+> ("FunActionLifting", S3.pass_FunActionLifting)

    |+> ("TypesDefinitions", (S3.pass_TypesDefinitions backend_register_fields_switcher))

    |+> ("DbSchemaGeneration", S3.pass_DbSchemaGeneration)

    |+> ("DbPathCoercion", S3.pass_DbPathCoercion)

    |+> ("MacroExpansion", S3.pass_MacroExpansion)

    |+> ("Typing", S3.pass_Typing)

    (* Extracting interesting types for documentation *)
    |+> ("DocApiGeneration",  S3.pass_DocApiGeneration)(*~if_:If.generate_interface*)

    |+> ("CheckPatternMatching", S3.pass_CheckPatternMatching)

    |+> ("WarnCoerce", S3.pass_WarnCoerce)

    |+> ("CompileRecursiveValues", S3.pass_CompileRecursiveValues)

    |+> ("RewriteAsyncLambda", S3.pass_RewriteAsyncLambda)

    (*|+> ("Retyping", S3.pass_Retyping)*)

    |?> (If.database `db3,
         "BadopCodeGeneration", S3.pass_BadopCodeGeneration)

    |?> (If.database `mongo,
         "MongoCodeGeneration", S3.pass_MongoCodeGeneration)

    |?> (If.database `dropbox,
         "DropBoxCodeGeneration", S3.pass_DropBoxCodeGeneration)

    (* could be just after typing, if dbgen didn't complain that it can't find its coercions :/ *)
    |+> ("PurgeTypeDirectivesAfterTyping", S3.pass_PurgeTypeDirectiveAfterTyping)

    |> PH.handler ~count_time:false "EndOfSeparateCompilation" (S3.pass_EndOfSeparateCompilation (fun e -> e

      |+> ("BypassHoisting", S3.pass_BypassHoisting)

      |+> ("RegisterFields", (S3.pass_RegisterFields backend_register_fields_switcher))

      |?> (If.undot,
           "Undot", S3.pass_QmlUndot)

      |+> ("CodingDirectives", S3.pass_CodingDirectives)

      <?> (If.closure,
          ("EnrichMagic", S3.pass_EnrichMagic),
          ("EnrichMagicPurge", S3.pass_EnrichMagicPurge))

      |+> ("SimplifyEquality", S3.pass_SimplifyEquality)

      |+> ("SimplifyMagic", S3.pass_SimplifyMagic)

      |+> ("JustReorder1", S3.pass_ReorderEnvGen)

      |> PH.old_if_handler
          "EarlyLambdaLifting" S2.pass_EarlyLambdaLifting

      |+> ("InstrumentForClosureSerialization", S3.pass_InstrumentForClosureSerialization)

      (**********************************************)
      (* SLICED PASSES ******************************)
      <?> (If.server or If.separated or If.slicer_test,
          ("Slicing"  , S3.pass_SimpleSlicer backend_bsl_lang_switcher),
          ("NoSlicing", S3.pass_NoSlicer))

      |+> ("Assertion", S3.pass_Assertion)

      |?> (PH.neg (If.no_discard_of_unused_stdlib or If.separated),
           "SlicedCleaning", S3.pass_SlicedCleaning)

      (* Fun action resolution, step 2/3 *)
      |?> (If.server or If.separated,
           "FunActionEnvSerialize", S3.pass_FunActionEnvSerialize)

      (* Explicit instantiation *)
      |?> (If.explicit_instantiation,
           "ExplicitInstantiation", S3.pass_ExplicitInstantiation)

      |?> (If.explicit_instantiation,
           "OptimizeExplicitInstantiation", S3.pass_OptimizeExplicitInstantiation)

      (* Fun action resolution, step 3/3 *)
      |?> (If.server or If.separated,
           "FunActionJsCallGeneration", S3.pass_FunActionJsCallGeneration)

      |+> ("PurgeTypeDirectivesAfterEi", S3.pass_PurgeTypeDirectiveAfterEi)

      |?> (If.explicit_instantiation & (If.server or If.separated),
           "ResolveRemoteCalls", S3.pass_ResolveRemoteCalls)

      |?> (If.explicit_instantiation,
           "InsertMemoizedTypes", S3.pass_InsertMemoizedTypes)

      |+> ("JustReorder2", S3.pass_SlicedReorder)

      (* ***********************************************)
      (* FINAL COMPILATION *****************************)
      |+> ("SlicedToFinal", S3.pass_SlicedToFinal)

      (* ***********************************************)
      (* FINAL CLIENT COMPILATION **********************)

      |?> (If.cps_client,
           "ClientQmlCpsRewriter", S3.pass_ClientCpsRewriter)

      |?> (If.closure,
           "ClientQmlLambdaLifting", S3.pass_ClientLambdaLifting)

      |?> (If.constant_sharing_client,
           "QmlClientConstantSharing", S3.pass_ClientQmlConstantSharing)
      (* Insert client code like a js string on server (if
         necessary) - After that client qml code have no more
         place to exist and it dropped *)
      |+> ("JavascriptCompilation", S3.pass_JavascriptCompilation)

      |?> (If.server or If.separated,
           "ResolveJsIdent", S3.pass_ResolveJsIdent)

      <?> (If.server or If.separated,
           ("GenerateServerAst", S3.pass_GenerateServerAst true),
           ("DontGenerateServerAst", S3.pass_GenerateServerAst false))

      (* ***********************************************)
      (* FINAL SERVER COMPILATION **********************)

      (* |+> ("CleanLambdaLiftingDirectives", S3.pass_CleanLambdaLiftingDirectives) *)

      |?> (If.init & If.server,
           "InitializeBslValues", S3.pass_InitializeBslValues)

      |+> ("ServerQmlCpsRewriter", (S3.pass_ServerCpsRewriter backend_bsl_lang_switcher))

      |?| (Switch.back_end, backend_pass_switcher)

      |+> ("CleanUp", S3.pass_CleanUp)

      |+> ("ByeBye", S3.pass_ByeBye)

      |> PH.return )) (* end of the pass endOfSeparateCompilation *)
    |> PH.return )) (* end of the pass loadObjects *)
  |> PH.return;
  OManager.exit 0

(* Set title of generic pass system. *)
let _ = PH.set_title "Opa.exe"

(* Load warnings of opa s3 applications *)
let _ = WarningClass.load_set S3Warnings.warning_set
