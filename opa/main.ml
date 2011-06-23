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
let (or) = PH.(or)

(* Shorthands for accessing options of compilation *)
module If = Main_utils.If

(* The deprecated passes *)
(* FIXME: adapt to the new PassHandler *)
module S2 = Passes

(* S3 implementations. *)
module S3 = S3Passes

(* Set title of generic pass system. *)
let _ = PH.set_title "Opa.exe"

(* Load warnings of opa s3 applications *)
let _ = WarningClass.load_set S3Warnings.warning_set

(* Run all passes *)
let () =
  (**********************************************)
  (* INITIALIZATION *****************************)
  PH.init

  |+> ("Welcome", S3.pass_Welcome)

  |+> ("CheckOptions", S3.pass_CheckOptions)

  |+> ("AddStdlibFiles", S3.pass_AddStdlibFiles)

  |> PH.old_handler
      "OpenFiles" S2.pass_OpenFiles

  |+> ("PreProcess", S3.pass_PreProcess)

  |+> ("Parse", S3.pass_Parse)

  |+> ("RegisterAppSrcCode", S3.pass_RegisterAppSrcCode)

  (**********************************************)
  (* SURFACE AST PASSES *************************)
  |> PH.handler ~count_time:false "LoadObjects" (S3.pass_LoadObjects (fun e -> e

    |+> ("BslLoading", S3.pass_BslLoading)

    |+> ("ConvertStructure", S3.pass_ConvertStructure)

    |> PH.old_handler
        "CheckOptionsConsistency" Pass_CheckOptionsConsistency.process_code

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

    |+> ("TypesDefinitions", S3.pass_TypesDefinitions)

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

      |+> ("DbAccessorsGeneration", S3.pass_DbAccessorsGeneration)

      |+> ("DbCodeGeneration", S3.pass_DbCodeGeneration)

    (* could be just after typing, if dbgen didn't complain that it can't find its coercions :/ *)
    |+> ("PurgeTypeDirectivesAfterTyping", S3.pass_PurgeTypeDirectiveAfterTyping)

    |> PH.handler ~count_time:false "EndOfSeparateCompilation" (S3.pass_EndOfSeparateCompilation (fun e -> e

      |+> ("BypassHoisting", S3.pass_BypassHoisting)

      |+> ("RegisterFields", S3.pass_RegisterFields)

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
          ("Slicing"  , S3.pass_SimpleSlicer),
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
      (* ~precond:[check_ident_final_client] *)

      |?> (If.cps_client,
           "ClientQmlCpsRewriter", S3.pass_ClientCpsRewriter)

      |?> (If.closure,
           "ClientQmlLambdaLifting", S3.pass_ClientLambdaLifting)

(*      |?> (If.closure,
           "ClientQmlUncurry", S3.pass_ClientQmlUncurry)

      |?> (If.closure,
           "ClientQmlClosure", S3.pass_ClientQmlClosure)*)

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

      |+> ("CleanLambdaLiftingDirectives", S3.pass_CleanLambdaLiftingDirectives)

      |?> (If.init,
           "InitializeBslValues", S3.pass_InitializeBslValues)

      |+>  ("ServerQmlCpsRewriter", S3.pass_ServerCpsRewriter)

      |> PH.old_if_handler ~if_:If.closure (* ~precond:[check_ident_final] *)
          "ServerQmlLambdaLifting" (S2.pass_LambdaLifting2 ~typed:false ~side:`server)

      |> PH.old_if_handler ~if_:If.closure
          "ServerQmlUncurry" (S2.pass_QmlUncurry2 ~typed:false ~side:`server)

      |?> (If.closure,
           "ServerQmlClosure", S3.pass_ServerQmlClosure)

      |?> (If.constant_sharing,
           "QmlConstantSharing", S3.pass_QmlConstantSharing)

      |+> ("QmlCompilation", S3.pass_QmlCompilation)

      |+> ("OcamlSplitCode", S3.pass_OcamlSplitCode)

      |+> ("OcamlGeneration", S3.pass_OcamlGeneration)

      |+> ("OcamlCompilation", S3.pass_OcamlCompilation)

      |+> ("CleanUp", S3.pass_CleanUp)

      |+> ("ByeBye", S3.pass_ByeBye)

      |> PH.return )) (* end of the pass endOfSeparateCompilation *)
    |> PH.return )) (* end of the pass loadObjects *)
  |> PH.return

let () = OManager.exit 0
